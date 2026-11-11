"""Module for frame-based protocol communication over binary streams."""

import struct

HEADER_SIZE = 4
MAX_FRAME_SIZE = 16 * 1024 * 1024

# Padrão de fragmentação simulada para testes de estresse
FRAGMENT_PATTERN = (1, 2, 1, 3, 1, 2, 4)


def read_exact(stream, size):
    """Read exactly size bytes from the stream, or detect EOF/truncation."""
    if size < 0:
        raise ValueError("Invalid read size")

    data = bytearray()

    while len(data) < size:
        chunk = stream.read(size - len(data))

        if not chunk:
            if not data:
                return None
            raise EOFError(f"Truncated stream: expected {size} bytes, received {len(data)}")

        data.extend(chunk)

    return bytes(data)


def receive_frame(stream) -> bytes | None:
    """Receive a single frame payload. Returns None on clean EOF."""
    header = read_exact(stream, HEADER_SIZE)
    if header is None:
        return None

    frame_size = struct.unpack(">I", header)[0]
    if frame_size > MAX_FRAME_SIZE:
        raise ValueError(f"Frame too large: {frame_size} bytes")

    payload = read_exact(stream, frame_size)
    if payload is None:
        raise EOFError("Unexpected EOF while reading payload")

    return payload


def write_frame(stream, payload: bytes):
    """Write a payload to the stream adding the size header and simulating fragmentation."""
    size = len(payload)
    if size > MAX_FRAME_SIZE:
        raise ValueError(f"Frame too large: {size} bytes")

    # Reconstrói o frame completo (Cabeçalho + Payload)
    frame = struct.pack(">I", size) + payload

    stream.write(frame)
    stream.flush()
