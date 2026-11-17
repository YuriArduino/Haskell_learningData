from pathlib import Path
import hashlib
import hmac
import os
import sys
import tempfile

import framing
import message_pb2


def parse_message(payload):
    message = message_pb2.TransferMessage()
    message.ParseFromString(payload)
    return message


def validate_filename(name):
    if not name or name in (".", "..") or "/" in name or "\\" in name or "\0" in name:
        raise ValueError(f"Invalid file name: {name!r}")


def receive_file(stream, output_dir):
    # 1. Receive FileStart.
    payload = framing.receive_frame(stream)

    if payload is None:
        raise EOFError("Missing FileStart")

    message = parse_message(payload)

    if message.WhichOneof("body") != "start":
        raise ValueError("Expected FileStart")

    start = message.start

    validate_filename(start.file_name)

    if start.chunk_size == 0:
        raise ValueError("Invalid chunk size")

    if start.chunk_size > framing.MAX_FRAME_SIZE:
        raise ValueError("Chunk size exceeds maximum frame size")

    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    target = output_dir / start.file_name

    if target.exists():
        raise FileExistsError(f"Destination already exists: {target}")

    # 2. Create a temporary file.
    fd, temp_name = tempfile.mkstemp(prefix=f".{start.file_name}.", suffix=".part", dir=output_dir)

    try:
        digest = hashlib.sha256()
        total_bytes = 0
        expected_index = 0

        with os.fdopen(fd, "wb") as output:

            # 3. Receive chunks until FileEnd.
            while True:
                payload = framing.receive_frame(stream)

                if payload is None:
                    raise EOFError("Missing FileEnd")

                message = parse_message(payload)
                body = message.WhichOneof("body")

                if body == "chunk":
                    chunk = message.chunk

                    if chunk.index != expected_index:
                        raise ValueError("Unexpected chunk index")

                    if total_bytes >= start.file_size:
                        raise ValueError("Unexpected extra chunk")

                    remaining = start.file_size - total_bytes

                    expected_size = min(start.chunk_size, remaining)

                    if len(chunk.data) != expected_size:
                        raise ValueError("Invalid chunk payload size")

                    output.write(chunk.data)

                    digest.update(chunk.data)
                    total_bytes += len(chunk.data)
                    expected_index += 1

                elif body == "end":
                    if total_bytes != start.file_size:
                        raise ValueError("File size mismatch")

                    if len(message.end.sha256) != 32:
                        raise ValueError("Invalid SHA-256 length")

                    if not hmac.compare_digest(digest.digest(), message.end.sha256):
                        raise ValueError("SHA-256 mismatch")

                    # FileEnd must be the last frame.
                    if framing.receive_frame(stream) is not None:
                        raise ValueError("Unexpected data after FileEnd")

                    break

                else:
                    raise ValueError(f"Unexpected transfer event: {body}")

            output.flush()
            os.fsync(output.fileno())

        # 4. Publish atomically without overwriting existing files.
        os.link(temp_name, target)
        os.unlink(temp_name)

        print(f"File successfully received: {target}", file=sys.stderr)

        print(f"Received {total_bytes} bytes in " f"{expected_index} chunks", file=sys.stderr)

        return target

    except BaseException:
        try:
            os.unlink(temp_name)
        except FileNotFoundError:
            pass

        raise


def main():
    output_dir = sys.argv[1] if len(sys.argv) > 1 else "received-python"

    if len(sys.argv) > 2:
        raise SystemExit("Usage: python file_receiver.py [output-directory]")

    receive_file(sys.stdin.buffer, output_dir)


if __name__ == "__main__":
    main()
