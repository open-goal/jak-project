from http.server import BaseHTTPRequestHandler, HTTPServer
import json
from urllib.parse import urlparse, parse_qs

pathToDataTxt = "DICT.txt"
server_address = ('127.0.0.1', 8080)

# Initialize the dictionary from the file
pos_dict = {}

def reset_file_values(file_path, keys_to_skip):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    with open(file_path, 'w') as file:
        for line in lines:
            if ':' not in line:
                print(f"Skipping line '{line}'")
                file.write(line)
                continue
            key, value = line.split(':')
            key = key.strip().lower() # convert key to lowercase
            if key in [skip_key.lower() for skip_key in keys_to_skip]:
                print(f"Skipping key reset '{key}'")
                file.write(line.lower()) # convert skipped line to lowercase
                continue
            value = value.strip()
            if '.' in value:
                file.write(f'{key}: 0.0\n'.lower()) # convert line to lowercase
            elif '#' in value:
                file.write(f'{key}: #f\n'.lower()) # convert line to lowercase
            elif value.isdigit():
                file.write(f'{key}: 0\n'.lower()) # convert line to lowercase
            else:
                file.write(f'{key}: \n'.lower()) # convert line to lowercase

    # Reset the dictionary from the file
    global pos_dict
    pos_dict = initialize_var_dict(file_path)

def initialize_var_dict(file_path):
    with open(file_path, 'r') as f:
        var_dict = {}
        for line in f:
            key, value = line.strip().split(':')
            if value.startswith('[') and value.endswith(']'):
                value = [int(x) for x in value[1:-1].split(',')]
            elif value.lower() == '#t':
                value = True
            elif value.lower() == '#f':
                value = False
            elif '.' in value:
                value = float(value)
            else:
                value = value.strip()
            var_dict[key.strip()] = value
        return var_dict

class RequestHandler(BaseHTTPRequestHandler):
    def do_GET(self):
        # Check if user agent is Chrome
        is_chrome = False
        if 'User-Agent' in self.headers:
            user_agent = self.headers['User-Agent']
            is_chrome = 'Chrome' in user_agent
    
        if is_chrome:
            # Read dictionary file and Display variables in the browser
            content = '<html><body>'
            for key, value in pos_dict.items():
                content += f'{key}={value} <BR>'
            content += '</body></html>'
            self.send_response(200)
            self.send_header('Content-type', 'text/html')
            self.end_headers()
            self.wfile.write(content.encode())
            self.wfile.flush()
        else:
            # Return JSON response
            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.end_headers()
    
            # Extract the "keys" parameter from the query string
            url = urlparse(self.path)
            print("URL from GET ", url)
            query = url.query
            keys = parse_qs(query).get('keys', [])
    
            # Create a dictionary with the requested data
            response_data = {}
            if len(keys) == 0:
                response_data = pos_dict
            else:
                for key in keys:
                    if key in pos_dict:
                        response_data[key] = pos_dict[key]
                    elif key.lower() in [k.lower() for k in pos_dict.keys()]:
                        key_lower = key.lower()
                        for k, v in pos_dict.items():
                            if k.lower() == key_lower:
                                response_data[k] = v
    
            # Convert the dictionary to JSON format
            json_data = json.dumps(response_data)
    
            # Write JSON data to the response body
            self.wfile.write(json_data.encode())
            self.wfile.flush()

    def do_POST(self):

        # Get content length
        content_length = int(self.headers['Content-Length'])
        # Get raw body data
        raw_data = self.rfile.read(content_length)
        # Parse JSON data into dictionary
        data = json.loads(raw_data.decode('utf-8'))
    
        # Check if the key already exists in the dictionary (case insensitive)
       
        for key in data.keys():
            pos_dict_keys_lower = [k.lower() for k in pos_dict.keys()]
            if key.lower() in pos_dict_keys_lower:
                # Update the value
                pos_dict[list(pos_dict.keys())[pos_dict_keys_lower.index(key.lower())]] = data[key]
            else:
                # Add the key-value pair to the dictionary
                pos_dict[key] = data[key]
    
        # Write the updated dictionary to the DICT.txt file
        with open(pathToDataTxt, "w") as f:
            for key, value in pos_dict.items():
                f.write(f"{key}:{value}\n")
    
        # Send response status code
        self.send_response(200)
    
        # Send headers
        self.send_header('Content-type', 'application/json')
        self.end_headers()
    
        # Create a dictionary with requested values

        response_data = pos_dict
    
        # Convert the dictionary to JSON format
        json_data = json.dumps(response_data)
    
        # Write JSON data to the response body
        self.wfile.write(json_data.encode())
        

def run():
    print('Reseting values')
    reset_file_values(pathToDataTxt, ['x1','X2'])

    print('Starting server...')

    # Server settings
    httpd = HTTPServer(server_address, RequestHandler)
    print('Server running at ' + server_address [0])
    httpd.serve_forever()

if __name__ == '__main__':
    run()