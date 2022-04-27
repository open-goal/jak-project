import socket
import time
clientSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM);
clientSocket.connect(("127.0.0.1", 8181))
print(clientSocket)
data = clientSocket.recv(1024)
print(data.decode())

form = "(repl-help)"

num_sent = clientSocket.send(b'\x0B\x00\x00\x00\x0A\x00\x00\x00' + form.encode())
print("Sent {} bytes".format(num_sent))

# this shouldn't be necessary but...this is just a test script
# might imply something is wrong in the C++ code!
time.sleep(1000000)
