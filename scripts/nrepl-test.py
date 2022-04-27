import socket
clientSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM);
clientSocket.connect(("127.0.0.1", 8181))
print(clientSocket)
num_sent = clientSocket.send(b'\x01\x02\x03\x04')
print("Sent {} bytes".format(num_sent))
