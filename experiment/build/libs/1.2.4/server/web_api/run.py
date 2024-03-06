#!/usr/bin/env python
import argparse
from http.server import HTTPServer, BaseHTTPRequestHandler
from urllib.parse import urlparse, parse_qs

def get_html():
    content = read_file("main.html")
    return content

def get_status():
    mod_status = read_file("mod_write.txt")
    return mod_status.encode("utf8")

def get_log():
    mod_status = tail("../logs/debug.log", n=15)
    return mod_status.encode("utf8")

def read_file(path):
    with open (path, "r") as myfile:
        data=myfile.read()
        return data

def append_file(path, s):
    with open(path, "a") as myfile:
        myfile.write(s+"\n")

def tail(file, n=1, bs=1024):
    f = open(file)
    f.seek(0,2)
    l = 1-f.read(1).count('\n')
    B = f.tell()
    while n >= l and B > 0:
            block = min(bs, B)
            B -= block
            f.seek(B, 0)
            l += f.read(block).count('\n')
    f.seek(B, 0)
    l = min(l,n)
    lines = f.readlines()[-l:]
    f.close()
    return '<br>'.join(lines)

class S(BaseHTTPRequestHandler):
    def _set_headers(self):
        self.send_response(200)
        self.send_header("Content-type", "text/html")
        self.end_headers()

    def _html(self):
        """This just generates an HTML document that includes `message`
        in the body. Override, or re-write this do do more interesting stuff.
        """
        #content = f"<html><body><h1>{message}</h1></body></html>"
        content = get_html()
        return content.encode("utf8")  # NOTE: must return a bytes object!

    def do_GET(self):
        self._set_headers()
        if self.path=="/status":
            self.wfile.write(get_status())
        elif self.path=="/log":
            self.wfile.write(get_log())
        else:
            self.wfile.write(self._html())

    def do_HEAD(self):
        self._set_headers()

    def do_POST(self):
        content_length = int(self.headers['Content-Length'])
        post_data = self.rfile.read(content_length)
        data = parse_qs(post_data)

        command=""
        if self.path=="/setup":
            s = data[b'session'][0].decode("utf-8")
            l = data[b'lang'][0].decode("utf-8")
            if l=="En": command = "setup "+s
            else: command = "setup "+s+"_ge"
        elif self.path=="/command":
            if b'command' in data.keys(): command = data[b'command'][0].decode("utf-8")
        else:
            print("ERR: Unknown path.")

        append_file("mod_read.txt", command)
        self._set_headers()
        self.wfile.write(self._html())


def run(server_class=HTTPServer, handler_class=S, addr="localhost", port=8000):
    server_address = (addr, port)
    httpd = server_class(server_address, handler_class)

    print(f"Starting httpd server on {addr}:{port}")
    httpd.serve_forever()


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="Run a simple HTTP server")
    parser.add_argument(
        "-l",
        "--listen",
        default="localhost",
        help="Specify the IP address on which the server listens",
    )
    parser.add_argument(
        "-p",
        "--port",
        type=int,
        default=8000,
        help="Specify the port on which the server listens",
    )
    args = parser.parse_args()
    run(addr=args.listen, port=args.port)
