import socket
import struct
clientSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM);
clientSocket.connect(("127.0.0.1", 8181))
print(clientSocket)
data = clientSocket.recv(1024)
print(data.decode())
print("...?")

form = "(repl-help)"

header = struct.pack('<II', len(form), 10)

num_sent = clientSocket.sendall(header + form.encode())

