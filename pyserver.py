from http.server import BaseHTTPRequestHandler, HTTPServer
import json
from urllib.parse import urlparse, parse_qs

server_address = ('127.0.0.1', 8080)

class RequestHandler(BaseHTTPRequestHandler):

  # Initialize the dictionary from the file
  PLAYER_IDX_LOOKUP = {}
  PLAYER_LIST = []

  def send_response_bad_request_400(self):
    self.send_response(400)
    self.send_header('Content-type', 'application/json')
    self.end_headers()
    self.wfile.write(bytes("400 Bad Request", "UTF-8"))
    self.wfile.flush()

  def send_response_not_found_404(self):
    self.send_response(404)
    self.send_header('Content-type', 'text/html')
    self.end_headers()
    self.wfile.write(bytes("404 Not Found", "UTF-8"))
    self.wfile.flush()
  
  def do_GET(self):
    url = urlparse(self.path)

    # Extract parameters from the query string
    query = parse_qs(url.query)

    # routing
    match url.path:

      # get 
      case "/get":
        username = query.get('username', [])

        if len(username) > 0 and username[0] in self.PLAYER_IDX_LOOKUP:
          # existing user, we won't return their info
          player_num = self.PLAYER_IDX_LOOKUP[username[0]]
        else:
          player_num = -1

        response_data = {}
        for i in range(len(self.PLAYER_LIST)):
          if i == int(player_num):
            # skip player's own data
            continue
          response_data[i] = self.PLAYER_LIST[i]

        # Return JSON response
        self.send_response(200)
        self.send_header('Content-type', 'application/json')
        self.end_headers()

        # Convert the dictionary to JSON format
        json_data = json.dumps(response_data)
        # Write JSON data to the response body
        self.wfile.write(json_data.encode())
        self.wfile.flush()

      # else unknown path
      case _: 
        self.send_response_not_found_404()

  def do_POST(self):
    # Get content length
    content_length = int(self.headers['Content-Length'])

    url = urlparse(self.path)

    # Extract parameters from the query string
    query = parse_qs(url.query)

    # routing
    match url.path:
      # clear
      case "/clear":
        self.PLAYER_LIST.clear()
        # Send response status code
        self.send_response(200)
        self.send_header('Content-type', 'application/json')
        self.end_headers()
        self.wfile.flush()

      # register
      case "/register":
        username = query.get('username', [])

        if len(username) == 0 or len(username[0]) == 0:
          self.send_response_bad_request_400()
        elif username[0] in self.PLAYER_IDX_LOOKUP:
          # existing user, treat as rejoin
          player_num = self.PLAYER_IDX_LOOKUP[username[0]]
        else:
          # new user
          player_num = len(self.PLAYER_LIST)
          self.PLAYER_IDX_LOOKUP[username[0]] = player_num
          # fill out empty keys
          self.PLAYER_LIST.append({})

        self.send_response(200)
        self.send_header('Content-type', 'application/json')
        self.end_headers()
        
        response_data = {
          "player_num": player_num
        }

        json_data = json.dumps(response_data)
        # Write JSON data to the response body
        self.wfile.write(json_data.encode())
        self.wfile.flush()

      # update
      case "/update":
        username = query.get('username', [])

        if len(username) == 0 or len(username[0]) == 0 or not username[0] in self.PLAYER_IDX_LOOKUP:
          # unknown player
          self.send_response_bad_request_400()
        else:
          player_num = self.PLAYER_IDX_LOOKUP[username[0]]
          # Get raw body data
          raw_data = self.rfile.read(content_length)
          # Parse JSON data into dictionary
          data = json.loads(raw_data.decode('utf-8'))
        
          for k in data:
            self.PLAYER_LIST[player_num][k] = data[k]
      
          # Send response status code
          self.send_response(200)
          self.send_header('Content-type', 'application/json')
          self.end_headers()
          self.wfile.flush()
      
      # else unknown path
      case _:
        self.send_response_not_found_404()
       
def run():
    print('Starting server...')

    # Server settings
    httpd = HTTPServer(server_address, RequestHandler)
    print('Server running at ' + server_address [0])
    httpd.serve_forever()

if __name__ == '__main__':
    run()