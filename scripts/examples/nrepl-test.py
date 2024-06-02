import socket
import struct
from time import sleep

clientSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM);
clientSocket.connect(("127.0.0.1", 8181))
print(clientSocket)
data = clientSocket.recv(1024)
print(data.decode())

form = "(:status)"

header = struct.pack('<II', len(form), 10)

clientSocket.sendall(header + form.encode())
sleep(1)
clientSocket.sendall(header + form.encode())
