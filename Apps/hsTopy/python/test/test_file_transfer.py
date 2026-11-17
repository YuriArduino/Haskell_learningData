import hashlib
import io

import pytest

from file_sender import send_file, CHUNK_SIZE
from file_receiver import receive_file


def binary_pattern(size: int) -> bytes:
    pattern = bytes(range(256))
    return (pattern * ((size + 255) // 256))[:size]


@pytest.mark.parametrize(
    ("case", "payload"),
    [
        pytest.param("empty", b"", id="FT01-empty-file"),
        pytest.param("small", b"Hello from hsTopy\n", id="FT02-small-text"),
        pytest.param("exact", binary_pattern(CHUNK_SIZE), id="FT03-exact-chunk"),
        pytest.param("partial", binary_pattern(CHUNK_SIZE + 1), id="FT04-chunk-plus-one"),
        pytest.param("multiple", binary_pattern(5 * 1024 * 1024 + 123), id="FT05-multiple-chunks"),
        pytest.param("binary", binary_pattern(4096), id="FT06-arbitrary-binary"),
    ],
)
def test_file_transfer_roundtrip(tmp_path, case, payload):

    source_dir = tmp_path / "source"
    output_dir = tmp_path / "received"

    source_dir.mkdir()
    output_dir.mkdir()

    source_path = source_dir / f"{case}.bin"
    source_path.write_bytes(payload)

    # Sender: file -> protobuf -> framing.
    stream = io.BytesIO()
    send_file(source_path, stream)

    # Receiver: framing -> protobuf -> file.
    stream.seek(0)
    received_path = receive_file(stream, output_dir)

    actual = received_path.read_bytes()

    assert actual == payload

    assert hashlib.sha256(actual).digest() == hashlib.sha256(payload).digest()

    # No temporary files should remain.
    assert list(output_dir.iterdir()) == [received_path]
