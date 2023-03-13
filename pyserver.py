from http.server import BaseHTTPRequestHandler, HTTPServer
import json
from urllib.parse import urlparse, parse_qs

server_address = ('127.0.0.1', 8080)

# Initialize the dictionary from the file
PLAYERS = []
EXPECTED_KEYS = {
  "trans_x",
  "trans_y",
  "trans_z",
  "quat_x",
  "quat_y",
  "quat_z",
  "quat_w"
}

class RequestHandler(BaseHTTPRequestHandler):
   
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

    # routing
    match url.path:

      # get 
      case "/get":
        # Extract parameters from the query string
        query = parse_qs(url.query)
        player_num = query.get('player_num', [])

        if len(player_num) == 0 or (not player_num[0].isnumeric()) or int(player_num[0]) >= len(PLAYERS):
          self.send_response_bad_request_400()
        else:
          player_num = int(player_num[0])
          response_data = {}
          for i in range(len(PLAYERS)):
            if i == int(player_num):
              # skip player's own data
              continue
            response_data[i] = PLAYERS[i]

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

    # routing
    match url.path:
      
      # register
      case "/register":
        player_num = len(PLAYERS)
        # fill out empty keys
        PLAYERS.append({})
        for k in EXPECTED_KEYS:
          PLAYERS[player_num][k] = None

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
        # Extract parameters from the query string
        query = parse_qs(url.query)
        player_num = query.get('player_num', [])

        if len(player_num) == 0 or (not player_num[0].isnumeric()) or int(player_num[0]) >= len(PLAYERS):
          self.send_response_bad_request_400()
        else:
          player_num = int(player_num[0])
          # Get raw body data
          raw_data = self.rfile.read(content_length)
          # Parse JSON data into dictionary
          data = json.loads(raw_data.decode('utf-8'))
        
          for k in EXPECTED_KEYS:
            if k not in data:
              self.send_response_bad_request_400()
            else:
              PLAYERS[player_num][k] = data[k]
      
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