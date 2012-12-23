#!/usr/bin/env python
# -*- coding: utf-8 -*-


import sys, struct, subprocess

# connect to the server
server = subprocess.Popen(['hg', '--config', 'ui.interactive=True', 'serve', '--cmdserver', 'pipe'],
                                                    stdin=subprocess.PIPE, stdout=subprocess.PIPE)

def readchannel(server):
    channel, length = struct.unpack('>cI', server.stdout.read(5))
    if channel in 'IL': # input
        return channel, length
    return channel, server.stdout.read(length)

def writeblock(data):
    server.stdin.write(struct.pack('>I', len(data)))
    server.stdin.write(data)
    server.stdin.flush()
    
# read the hello block
hello = readchannel(server)
print "hello block:", repr(hello)

# write the command
server.stdin.write('runcommand\n')
writeblock('\0'.join(sys.argv[1:]))

# receive the response
while True:
    channel, val = readchannel(server)
    if channel == 'o':
        print "output:", val
    elif channel == 'e':
        print "error:", repr(val)
    elif channel == 'r':
        print "exit code:", struct.unpack(">l", val)[0]
        break
    elif channel == 'L':
        print "(line read request)"
        writeblock(sys.stdin.readline(val))
    elif channel == 'I':
        print "(block read request)"
        writeblock(sys.stdin.read(val))
    else:
        print "unexpected channel:", channel, val
        if channel.isupper(): # required?
            break
        
# shut down the server
server.stdin.close()
