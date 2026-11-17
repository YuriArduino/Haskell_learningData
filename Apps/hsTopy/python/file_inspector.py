from pathlib import Path
import hashlib
import hmac
import sys

import framing
import message_pb2


def parse_message(payload):
    message = message_pb2.TransferMessage()
    message.ParseFromString(payload)
    return message


def inspect_file_stream(path: Path):
    digest = hashlib.sha256()
    total_bytes = 0
    expected_index = 0

    with path.open("rb") as stream:

        # 1. Read FileStart
        payload = framing.receive_frame(stream)

        if payload is None:
            raise ValueError("Missing FileStart")

        message = parse_message(payload)

        if message.WhichOneof("body") != "start":
            raise ValueError("Expected FileStart")

        start = message.start

        if not start.file_name:
            raise ValueError("Missing file name")

        if start.chunk_size == 0:
            raise ValueError("Invalid chunk size")

        print(f"File: {start.file_name}")
        print(f"Expected size: {start.file_size}")
        print(f"Chunk size: {start.chunk_size}")

        # 2. Read chunks until FileEnd
        while True:
            payload = framing.receive_frame(stream)

            if payload is None:
                raise EOFError("Missing FileEnd")

            message = parse_message(payload)
            body = message.WhichOneof("body")

            if body == "chunk":
                chunk = message.chunk

                if chunk.index != expected_index:
                    raise ValueError(f"Unexpected chunk index: {chunk.index}")

                remaining = start.file_size - total_bytes

                if remaining <= 0:
                    raise ValueError("Unexpected extra chunk")

                expected_size = min(start.chunk_size, remaining)

                if len(chunk.data) != expected_size:
                    raise ValueError(
                        f"Invalid chunk size: expected "
                        f"{expected_size}, received {len(chunk.data)}"
                    )

                digest.update(chunk.data)
                total_bytes += len(chunk.data)
                expected_index += 1

            elif body == "end":
                if total_bytes != start.file_size:
                    raise ValueError(
                        f"File size mismatch: expected "
                        f"{start.file_size}, received {total_bytes}"
                    )

                if len(message.end.sha256) != 32:
                    raise ValueError("Invalid SHA-256 length")

                if not hmac.compare_digest(digest.digest(), message.end.sha256):
                    raise ValueError("SHA-256 mismatch")

                if framing.receive_frame(stream) is not None:
                    raise ValueError("Unexpected data after FileEnd")

                break

            else:
                raise ValueError(f"Unexpected transfer event: {body}")

    print("Transfer stream validated successfully.")
    print(f"Chunks: {expected_index}")
    print(f"Received bytes: {total_bytes}")
    print(f"SHA-256: {digest.hexdigest()}")


if __name__ == "__main__":
    file_path = Path(sys.argv[1] if len(sys.argv) > 1 else "test/files/file_1.stream")

    inspect_file_stream(file_path)
