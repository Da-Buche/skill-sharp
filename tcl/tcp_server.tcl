#!/usr/bin/env tclsh
## ===============================================================================================================
## This server opens a socket on any available port.
## Everything received on this socket is printed to stdout, evaluated by parent and returned through stdin.
## It is meant to be started by Virtuoso, Python or any other parent to build a small ipc server.
##
## A. Buchet - August 2025
## ===============================================================================================================

## This script was intended to be used with `ipcSkillProcess' which reads from fd3 and writes to fd4
## To make it work wiht python, those file descriptors were simply replaced by stdout and stdin
## stderr is used to write messages directly to the parent process

set fd2 [open /dev/stderr w+]
set fd3 [open /dev/stdout w+] ; #[open /dev/fd/3 w+]
set fd4 [open /dev/stdin  r ] ; #[open /dev/fd/4 r ]
fconfigure $fd4 -buffering none -blocking 0

## Create a socket to listen on any available port
set server [socket -server accept_client 0]
fconfigure $server -buffering none -blocking 1
set port [lindex [fconfigure $server -sockname] 2]
set host [exec hostname -f]

puts -nonewline $fd2 "Listening on $host:$port\n"
flush $fd2

## Define the procedure to accept client connections
proc accept_client {sock addr port} {
  fileevent $sock readable [list handle_request $sock]
}

## Define the procedure to handle incoming requests
proc handle_request {sock} {
  global fd3 fd4
  ## Read message from socket, print it to stdout
  set msg [read $sock]
  puts $fd3 "$msg"
  flush $fd3
  ## Wait for evaluation answer
  fileevent $fd4 readable [list handle_answer $fd4 $sock]
  fileevent $sock readable ""
}

proc handle_answer {fd sock} {
  ## Send the response to the client
  set response [read $fd]
  puts $sock "$response"
  close $sock
}

## Keep the event loop running indefinitely
vwait forever

