#!/usr/bin/env python

import pygame
import random
import socket
import sys
import time

canvas_x = 600
canvas_y = 600

def decode_token_draw(token, world):

    tokens = token.split(' ')
    tokens = [t for t in tokens if t != '']

    cmd = tokens[0]
    if cmd not in ['PIXEL', 'LINE', 'CIRCLE', 'TEXT']:
        print("Command: %s not found. Ignoring!" % cmd)
        return

    tokens = tokens[1:]
    if cmd == "PIXEL":
        (x, y, r, g, b) = [int(t) for t in tokens]
        world.draw_pixel(coords=(x,y), color=(r, g, b))
    elif cmd == "LINE":
        (x1, y1, x2, y2, r, g, b) = [int(t) for t in tokens]
        world.draw_line(pt1=(x1, y1), pt2=(x2, y2), color=(r, g, b))
    elif cmd == "CIRCLE":
        (x, y, rad, width, r, g, b) = [int(t) for t in tokens]
        world.draw_circle(rad, width, coords=(x,y), color=(r,g,b))
    elif cmd == "TEXT":
        text = tokens[0]
        tokens = tokens[1:]
        (x, y, r, g, b) = [int(t) for t in tokens]
        world.draw_text(text, coords=(x, y), color=(r, g, b))
        
class World:
    def __init__(self, world_x_dims = (-1000, 1000),
                 world_y_dims = (-1000, 1000),
                 canvas_dims = (canvas_x, canvas_y)):

        self.world_x_min = world_x_dims[0]
        self.world_y_min = world_y_dims[0]
        self.world_width = sum([abs(p) for p in world_x_dims])
        self.world_height = sum([abs(p) for p in world_y_dims])
        self.canvas_width, self.canvas_height = canvas_dims
        
        self.screen = pygame.display.set_mode(canvas_dims, 0, 32)
        self.bg_image = None
        self.font = pygame.font.SysFont(None, 16)

        print("[+] World dimensions:")
        print(world_x_dims)
        print(world_y_dims)
        print("[+] Canvas dimensions:")
        print(canvas_dims)

    def set_background_image(self, bg_image_f):
        self.bg_image = pygame.image.load(bg_image_f).convert()
        self.bg_image = pygame.transform.scale(self.bg_image, (self.canvas_width, self.canvas_height))      
        self.screen.blit(self.bg_image, (0,0))
        
    def world_to_canvas_x(self, x):
        
        dist_to_x_min = abs(self.world_x_min - x)
        dist_x_ratio = float(dist_to_x_min) / float(self.world_width)
        canvas_x = int(round(dist_x_ratio * self.canvas_width))

        return canvas_x

    def world_to_canvas_y(self, y):
        
        dist_to_y_min = abs(self.world_y_min - y)
        dist_y_ratio = float(dist_to_y_min) / float(self.world_height)
        canvas_y = int(round(dist_y_ratio * self.canvas_height))

        return canvas_y

    def draw_pixel(self, coords, color):
        x, y = coords
        world_pt = (self.world_to_canvas_x(x),
                    self.world_to_canvas_y(y))
        try:
            self.screen.set_at(world_pt, color)
        except Exception as e:
            mesg = "Exception with draw_pixel(): %s" % str(e)
            print(mesg)

    def draw_circle(self, radius, width, coords, color):
        x, y = coords
        world_pt = (self.world_to_canvas_x(x),
                    self.world_to_canvas_y(y))
        try:
            pygame.draw.circle(self.screen, color, world_pt, radius, width)
        except Exception as e:
            mesg = "Exception with draw_circle(): %s" % str(e)
            print(mesg)

    def draw_line(self, pt1, pt2, color):
        x1, y1 = pt1
        x2, y2 = pt2
        world_pt1 = (self.world_to_canvas_x(x1),
                     self.world_to_canvas_y(y1))
        world_pt2 = (self.world_to_canvas_x(x2),
                     self.world_to_canvas_y(y2))
        try:
            pygame.draw.line(self.screen, color, world_pt1, world_pt2)
        except Exception as e:
            mesg = "Exception with draw_line(): %s" % str(e)
            print(mesg)

    def draw_text(self, text, coords, color):
        x, y = coords
        world_pt = (self.world_to_canvas_x(x),
                    self.world_to_canvas_y(y))      
        try:
            text_img = self.font.render(text, True, color)
            self.screen.blit(text_img, world_pt)
        except Exception as e:
            mesg = "Exception with draw_text(): %s" % str(e)
            print(mesg)

def main(args):

    host = "127.0.0.1"
    port = 54321
    
    # Create the socket object.
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    # Set the socket options.
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)

    # Bind to a port and interface.
    sock.bind( (host, port) )

    y_min = -1000
    y_max = 1000
    x_min = -1000
    x_max = 1000

    world_x_dims=(x_min, x_max)
    world_y_dims=(y_min, y_max)
    
    pygame.init()

    my_world = World(world_x_dims, world_y_dims)
    # my_world.set_background_image(bg_image_f)

    # Listen for connections.
    print( "Waiting for connections..." )
    sock.listen(1)
    connection, client_address = sock.accept()

    # use the pygame clock.
    clock = pygame.time.Clock()
    # rewrote the event loop.

    token = ""
    while True:
        # clock.tick(500)      
        
        events = pygame.event.get()
        if len(events) == 0:
            pass
        else:
            if pygame.QUIT in [e.type for e in events]:
                sys.exit()
        
        key_events = [e for e in events if e.type == pygame.KEYDOWN]
        for e in key_events:
            if e.key == pygame.K_t:
                exit()

        # receive some data.
        data = connection.recv(1)
        if len(data) > 0:
            byte = str(data, 'utf-8')
            if byte == "{":
                token = ""
            elif byte == "}":
                decode_token_draw(token, my_world)
                # update the frame.
                pygame.display.update()
            else:
                token += byte

    print("Press [ENTER] to continue...")
    w = input()
    sys.exit(1)


if __name__ == "__main__":
    try:
        main(sys.argv)
    except Exception as e:
        mesg = "Exception in pixel_server.py: %s" % str(e)
        print(mesg)

