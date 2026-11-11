"""Module to receive and process messages."""

import sys
import message_pb2
from framing import receive_frame


def receive_frames():
    stream = sys.stdin.buffer
    index = 0

    while True:
        # A complexidade de ler e validar os frames agora está isolada aqui:
        payload = receive_frame(stream)

        # Clean EOF
        if payload is None:
            break

        # Deserialize the protobuf payload
        message = message_pb2.Message()
        message.ParseFromString(payload)

        index += 1
        print(f"[{index}] content: {message.content}")
        print(f"[{index}] value: {message.value}")


if __name__ == "__main__":
    receive_frames()
