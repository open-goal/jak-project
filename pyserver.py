from http.server import BaseHTTPRequestHandler, HTTPServer
import json
import time
import threading
from enum import Enum
from urllib.parse import urlparse, parse_qs

server_address = ('127.0.0.1', 8080)

class MpGameRole(Enum):
  LOBBY = 0
  HIDER = 1
  SEEKER = 2
  FOUND = 3

class MpGameState(Enum):
  INVALID = 0
  LOBBY = 1
  STARTING_SOON = 2
  PLAY_HIDE = 3
  PLAY_SEEK = 4
  END = 5

class MpTargetState(Enum):
  INVALID = 0
  LOBBY = 1
  READY = 2
  START = 3
  HIDER_START = 4
  HIDER_PLAY = 5
  HIDER_FOUND = 6
  SEEKER_WAIT = 7
  SEEKER_START = 8
  SEEKER_PLAY = 9

MP_INFO = {
  "state": MpGameState.INVALID
}
PLAYER_IDX_LOOKUP = {}
PLAYER_LIST = []

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

    # Extract parameters from the query string
    query = parse_qs(url.query)

    # routing
    match url.path:

      # get 
      case "/get":
        response_data = {
          "game_state": MP_INFO["state"].value,
          "players": {}
        }
        for i in range(len(PLAYER_LIST)):
          response_data["players"][i] = PLAYER_LIST[i]

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
        PLAYER_LIST.clear()
        # Send response status code
        self.send_response(200)
        self.send_header('Content-type', 'application/json')
        self.end_headers()
        self.wfile.flush()

      # register
      case "/register":
        username = query.get('username', [])

        if len(PLAYER_LIST) == 0:
          # first player, setup lobby
          MP_INFO["state"] = MpGameState.LOBBY

        if len(username) == 0 or len(username[0]) == 0:
          self.send_response_bad_request_400()
        elif username[0] in PLAYER_IDX_LOOKUP:
          # existing user, treat as rejoin
          player_num = PLAYER_IDX_LOOKUP[username[0]]
        else:
          # new user
          player_num = len(PLAYER_LIST)  # TODO: loop to find next open slot (after dropping players)
          PLAYER_IDX_LOOKUP[username[0]] = player_num

          # fill out empty keys
          PLAYER_LIST.append({})

        self.send_response(200)
        self.send_header('Content-type', 'application/json')
        self.end_headers()
        
        response_data = {
          "game_state": MP_INFO["state"].value,
          "player_num": player_num
        }

        json_data = json.dumps(response_data)
        # Write JSON data to the response body
        self.wfile.write(json_data.encode())
        self.wfile.flush()

      # update (either player updating themselves or seeker marking hider as found)
      case "/update":
        username = query.get('username', [])

        if len(username) == 0 or len(username[0]) == 0 or not username[0] in PLAYER_IDX_LOOKUP:
          # unknown player
          self.send_response_bad_request_400()
        else:
          player_num = PLAYER_IDX_LOOKUP[username[0]]
          # Get raw body data
          raw_data = self.rfile.read(content_length)
          # Parse JSON data into dictionary
          data = json.loads(raw_data.decode('utf-8'))
        
          for k in data:
            PLAYER_LIST[player_num][k] = data[k]
      
          # Send response status code
          self.send_response(200)
          self.send_header('Content-type', 'application/json')
          self.end_headers()
          self.wfile.flush()
      
      case "/mark_found":
        # Get raw body data
        raw_data = self.rfile.read(content_length)
        # Parse JSON data into dictionary
        data = json.loads(raw_data.decode('utf-8'))
      
        seeker = data["seeker_username"]
        found = data["found_username"]

        if found in PLAYER_IDX_LOOKUP:
          PLAYER_LIST[PLAYER_IDX_LOOKUP[found]]["role"] = MpGameRole.FOUND.value
        else:
          print("couldn't find player {found} to mark FOUND")

        # TODO: track last find for alert
    
        # Send response status code
        self.send_response(200)
        self.send_header('Content-type', 'application/json')
        self.end_headers()
        self.wfile.flush()

      # else unknown path
      case _:
        self.send_response_not_found_404()
       
def game_loop():
  last_state_change_time = time.time()  # seconds
  while True:
    # collect some info
    first_player_start = False
    total_players = 0
    player_counts = {
      MpTargetState.LOBBY: 0,
      MpTargetState.READY: 0,
      MpTargetState.START: 0,
      MpTargetState.HIDER_START: 0,
      MpTargetState.HIDER_PLAY: 0,
      MpTargetState.HIDER_FOUND: 0,
      MpTargetState.SEEKER_WAIT: 0,
      MpTargetState.SEEKER_START: 0,
      MpTargetState.SEEKER_PLAY: 0
    }

    for i in range(len(PLAYER_LIST)):
      if PLAYER_LIST[i] is None or PLAYER_LIST[i] == {} or MpTargetState(PLAYER_LIST[i]["mp_state"]) == MpTargetState.INVALID:
        # dont count this player as joined
        continue

      total_players += 1
      state = MpTargetState(PLAYER_LIST[i]["mp_state"])
      if state not in player_counts:
        player_counts[state] = 0
      player_counts[state] += 1

      match state:
        case MpTargetState.START:
          if total_players == 1:
            first_player_start = True

    # print(MP_INFO["state"], total_players, player_counts)

    # update state conditionally
    match MP_INFO["state"]:
      case MpGameState.LOBBY:
        # go to STARTING_SOON if either:
        # - first player wants to start
        # - 50% are ready/start and anyone wants to start
        if first_player_start or (player_counts[MpTargetState.START] > 0 and (player_counts[MpTargetState.READY] + player_counts[MpTargetState.START]) * 2 >= total_players):
          print("LOBBY -> STARTING_SOON")
          MP_INFO["state"] = MpGameState.STARTING_SOON
          last_state_change_time = time.time()
      case MpGameState.STARTING_SOON:
        # see if 10s timer is up and we should begin hiding
        if time.time() - last_state_change_time >= 10:
          # assign hiders/seekers
          # TODO: make this random
          print("starting game, assigning roles")
          for i in range(len(PLAYER_LIST)):
            # skip players who weren't in start state
            if PLAYER_LIST[i] is None or PLAYER_LIST[i] == {} or PLAYER_LIST[i]["mp_state"] != MpTargetState.START.value:
              continue
            if i == 1:
              print("player {i} is seeker")
              PLAYER_LIST[i]["role"] = MpGameRole.SEEKER.value
            else:
              PLAYER_LIST[i]["role"] = MpGameRole.HIDER.value
          print("STARTING_SOON -> PLAY_HIDE")
          MP_INFO["state"] = MpGameState.PLAY_HIDE
          last_state_change_time = time.time()
      case MpGameState.PLAY_HIDE:
        # see if 30s timer is up and we should begin seeking
        if time.time() - last_state_change_time >= 10:
          print("PLAY_HIDE -> PLAY_SEEK")
          MP_INFO["state"] = MpGameState.PLAY_SEEK
          last_state_change_time = time.time()
      case MpGameState.PLAY_SEEK:
        # see if 300s timer is up and we should end game
        if time.time() - last_state_change_time >= 300:
          print("PLAY_SEEK -> END")
          MP_INFO["state"] = MpGameState.END
          last_state_change_time = time.time()
        # see if all hiders found, then we should end game
        if player_counts[MpTargetState.HIDER_FOUND] != 0: # todo revert back
          print("PLAY_SEEK -> END")
          MP_INFO["state"] = MpGameState.END
          last_state_change_time = time.time()
      case MpGameState.END:
        # see if 10s timer is up and we should go back to lobby 
        if time.time() - last_state_change_time >= 10:
          print("END -> LOBBY")
          MP_INFO["state"] = MpGameState.LOBBY
          last_state_change_time = time.time()

      # any clients should then update their own player states accordingly after seeing a game state change here

def run():
    print('Starting server...')

    game_thread = threading.Thread(target=game_loop)
    game_thread.start()

    # Server settings
    with HTTPServer(server_address, RequestHandler) as httpd:
      print('Server running at ' + server_address [0])
      server_thread = threading.Thread(target=httpd.serve_forever())
      # server_thread.daemon = True
      server_thread.start()

if __name__ == '__main__':
    run()