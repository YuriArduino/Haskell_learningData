import io
import struct
from itertools import cycle
from pathlib import Path

import pytest

import framing
import message_pb2

from google.protobuf.message import DecodeError

VECTOR_DIR = Path(__file__).resolve().parents[2] / "test-vectors"


def read_vector(name: str) -> bytes:
    return bytes.fromhex((VECTOR_DIR / name).read_text())


def encode_test_frame(payload: bytes) -> bytes:
    """Independent reference encoder for test inputs."""
    return struct.pack(">I", len(payload)) + payload


class FragmentedStream(io.BytesIO):
    """Simulates a stream that returns small chunks on each read."""

    def __init__(self, data: bytes, pattern=(1, 2, 1, 3, 1, 2, 4)):
        super().__init__(data)
        self.pattern = cycle(pattern)

    def read(self, size=-1):
        chunk_size = next(self.pattern)

        if size < 0:
            size = chunk_size
        else:
            size = min(size, chunk_size)

        return super().read(size)


# F01: Encoding


def test_f01_big_endian_header():
    stream = io.BytesIO()

    framing.write_frame(stream, b"abc")

    assert stream.getvalue() == b"\x00\x00\x00\x03abc"


# F02: Payload


def test_f02_empty_payload():
    stream = io.BytesIO()

    framing.write_frame(stream, b"")
    stream.seek(0)

    assert framing.receive_frame(stream) == b""


def test_f02_valid_payload():
    stream = io.BytesIO(encode_test_frame(b"hello"))

    assert framing.receive_frame(stream) == b"hello"


# F03: Fragmentation


def test_f03_fragmented_header_and_payload():
    payload = b"fragmented payload"
    stream = FragmentedStream(encode_test_frame(payload))

    assert framing.receive_frame(stream) == payload


# F04: Multiple frames


def test_f04_multiple_frames():
    payloads = [b"first", b"second", b"third"]

    wire = b"".join(encode_test_frame(p) for p in payloads)
    stream = FragmentedStream(wire)

    received = [framing.receive_frame(stream) for _ in payloads]

    assert received == payloads
    assert framing.receive_frame(stream) is None


# F05: Clean EOF


def test_f05_clean_eof():
    stream = io.BytesIO()

    assert framing.receive_frame(stream) is None


# F06: Truncated header


def test_f06_truncated_header():
    stream = io.BytesIO(b"\x00\x00")

    with pytest.raises(EOFError, match="Truncated stream"):
        framing.receive_frame(stream)


# F07: Truncated payload


def test_f07_truncated_payload():
    # Header declares 3 bytes, but only 2 are transmitted.
    stream = io.BytesIO(b"\x00\x00\x00\x03ab")

    with pytest.raises(EOFError, match="Truncated stream"):
        framing.receive_frame(stream)


def test_f07_missing_payload():
    # Valid header, but no payload bytes.
    stream = io.BytesIO(b"\x00\x00\x00\x03")

    with pytest.raises(EOFError, match="Unexpected EOF"):
        framing.receive_frame(stream)


# F08: Validation


def test_f08_rejects_oversized_incoming_frame():
    oversized_header = struct.pack(">I", framing.MAX_FRAME_SIZE + 1)

    stream = io.BytesIO(oversized_header)

    with pytest.raises(ValueError, match="Frame too large"):
        framing.receive_frame(stream)


def test_f08_rejects_oversized_outgoing_frame():
    payload = bytes(framing.MAX_FRAME_SIZE + 1)

    with pytest.raises(ValueError, match="Frame too large"):
        framing.write_frame(io.BytesIO(), payload)


# Additional validation


def test_read_exact_rejects_negative_size():
    with pytest.raises(ValueError, match="Invalid read size"):
        framing.read_exact(io.BytesIO(), -1)


# I01: Python produces the shared framing vector.


def test_i01_frame_encoding():
    expected = read_vector("frame_abc.hex")

    stream = io.BytesIO()
    framing.write_frame(stream, b"abc")

    assert stream.getvalue() == expected


# I02: Python consumes the shared framing vector.


def test_i02_frame_decoding():
    wire = read_vector("frame_abc.hex")

    stream = io.BytesIO(wire)

    assert framing.receive_frame(stream) == b"abc"


# I03: Python produces the shared protobuf vector.


def test_i03_protobuf_encoding():
    expected = read_vector("protobuf_hello_42.hex")

    message = message_pb2.Message(content="Hello", value=42)

    assert message.SerializeToString() == expected


# I04: Python consumes the shared protobuf vector.


def test_i04_protobuf_decoding():
    payload = read_vector("protobuf_hello_42.hex")

    message = message_pb2.Message()
    message.ParseFromString(payload)

    assert message.content == "Hello"
    assert message.value == 42


# I05: Python produces and consumes multiple shared frames.


def test_i05_multiple_frames():
    expected = read_vector("multi_message_stream.hex")

    messages = [
        message_pb2.Message(content="A", value=1),
        message_pb2.Message(content="BC", value=2),
    ]

    # Validate Python frame encoding.
    outgoing = io.BytesIO()

    for message in messages:
        framing.write_frame(outgoing, message.SerializeToString())

    assert outgoing.getvalue() == expected

    # Validate Python frame decoding.
    incoming = io.BytesIO(expected)
    received = []

    while True:
        payload = framing.receive_frame(incoming)

        if payload is None:
            break

        message = message_pb2.Message()
        message.ParseFromString(payload)

        received.append((message.content, message.value))

    assert received == [
        ("A", 1),
        ("BC", 2),
    ]


def test_f09_invalid_protobuf():
    malformed = bytes([0x0A, 0x05, 0x41])

    # Framing preserves arbitrary payload bytes.
    stream = io.BytesIO(encode_test_frame(malformed))

    payload = framing.receive_frame(stream)

    assert payload == malformed

    # Protobuf parser rejects malformed content.
    message = message_pb2.Message()

    with pytest.raises(DecodeError):
        message.ParseFromString(payload)


def test_f10_valid_frame_followed_by_truncated_frame():
    valid_frame = encode_test_frame(b"first")

    incomplete_header = bytes([0, 0])

    stream = io.BytesIO(valid_frame + incomplete_header)

    first = framing.receive_frame(stream)

    assert first == b"first"

    with pytest.raises(EOFError, match="Truncated stream"):
        framing.receive_frame(stream)
