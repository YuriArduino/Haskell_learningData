"""Message sender utilizing the modular framing protocol."""

# import time
import sys
import message_pb2
from framing import write_frame

# Lista de mensagens estruturadas com Protobuf
messages = [
    message_pb2.Message(content="Hello from Python #1", value=1),
    message_pb2.Message(content="Hello from Python #2", value=2),
    message_pb2.Message(content="Hello from Python #3", value=3),
]

if __name__ == "__main__":
    stream = sys.stdout.buffer

    for message in messages:
        # 1. Serializa a mensagem para bytes brutos
        payload = message.SerializeToString()

        # 2. Envia através do nosso módulo de enquadramento
        write_frame(stream, payload)

        # Mantém o intervalo entre o envio de frames individuais
        # time.sleep(2)
