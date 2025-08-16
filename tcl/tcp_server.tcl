#!/usr/bin/env tclsh
## ===============================================================================================================
## This server opens a socket on any available port.
## Everything received on this socket is printed to stdout, evaluated by parent and returned through stdin.
## It is meant to be started by Virtuoso, Python or any other parent to build a small ipc server.
##
## A. Buchet - August 2025
## ===============================================================================================================

fconfigure stdin -buffering none -blocking 0

## Create a socket to listen on any available port
set server [socket -server accept_client 0]
fconfigure $server -buffering none -blocking 1
set port [lindex [fconfigure $server -sockname] 2]
set host [exec hostname -f]

puts stderr "Listening on $host:$port"
flush stderr

## Define the procedure to accept client connections
proc accept_client {sock addr port} {
  fileevent $sock readable [list handle_request $sock]
}

## Define the procedure to handle incoming requests
proc handle_request {sock} {
  ## Read message from socket, print it to stdout
  set msg [read $sock]
  puts stdout "$msg"
  flush stdout
  ## Wait for evaluation answer
  fileevent stdin readable [list handle_answer stdin $sock]
  fileevent $sock readable ""
}

proc handle_answer {fd sock} {
  ## Send the response to the client
  set response [read $fd]
  puts $sock "$response"
  flush $sock
  close $sock
}

## Keep the event loop running indefinitely
vwait forever

