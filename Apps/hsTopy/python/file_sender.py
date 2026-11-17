from pathlib import Path
import hashlib
import sys

import message_pb2
import framing

CHUNK_SIZE = 256 * 1024


def send_file(path: Path, output=None):
    if output is None:
        output = sys.stdout.buffer
    file_size = path.stat().st_size

    # 1. Announce the transfer
    start = message_pb2.TransferMessage()
    start.start.file_name = path.name
    start.start.file_size = file_size
    start.start.chunk_size = CHUNK_SIZE

    framing.write_frame(output, start.SerializeToString())

    # 2. Stream file contents in chunks
    digest = hashlib.sha256()
    total_bytes = 0
    chunk_index = 0

    with path.open("rb") as source:
        while True:
            data = source.read(CHUNK_SIZE)

            if not data:
                break

            digest.update(data)
            total_bytes += len(data)

            chunk = message_pb2.TransferMessage()
            chunk.chunk.index = chunk_index
            chunk.chunk.data = data

            framing.write_frame(output, chunk.SerializeToString())

            chunk_index += 1

    # Ensure the file was not truncated or extended during reading.
    if total_bytes != file_size:
        raise RuntimeError(f"File size changed: expected {file_size}, " f"read {total_bytes}")

    # 3. Announce completion and send checksum
    end = message_pb2.TransferMessage()
    end.end.sha256 = digest.digest()

    framing.write_frame(output, end.SerializeToString())

    print(
        f"Transfer completed: {path.name} " f"({total_bytes} bytes, {chunk_index} chunks)",
        file=sys.stderr,
    )


if __name__ == "__main__":
    file_path = Path(sys.argv[1] if len(sys.argv) > 1 else "test/files/file_1.txt")

    if not file_path.is_file():
        raise SystemExit(f"File not found: {file_path}")

    send_file(file_path)
