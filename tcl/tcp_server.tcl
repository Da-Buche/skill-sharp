#!/usr/bin/env tclsh
## ===============================================================================================================
## This server opens a socket on any available port.
## Everything received on this socket is printed to stdout, evaluated by parent and returned through stdin.
## It is meant to be started by Virtuoso, Python or any other parent to build a small TCP server.
##
## A. Buchet - August 2025
## ===============================================================================================================

## =======================================================
## Parse input arguments
## =======================================================

proc print_help {} {
  ## Print help message and exit
  puts "Usage: [file tail [info script]] \[OPTIONS\]"
  puts ""
  puts "Start TCP server:"
  puts "Listen on PORT, write every received message to stdout."
  puts "Wait for answer from stdin, forward it through PORT."
  puts ""
  puts "Options:"
  puts "  -f, --file            FILE     : CSV file to store used port.       (default: \$TCP_SERVER_FILE     or \"~/.tcp_servers.csv\" )"
  puts "  -k, --key, --password PASSWORD : Password used to encrypt messages. (default: \$TCP_SERVER_PASSWORD or random string        )"
  puts "  -l, --language        LANGUAGE : Server language.                   (default: \$TCP_SERVER_LANGUAGE or \"UNKNOWN\"            )"
  puts "  -n, --name, --project PROJECT  : Current project name.              (default: \$TCP_SERVER_PROJECT  or \"NO_PROJECT\"         )"
  puts "  -p, --port            PORT     : TCP Server Port number.            (default: \$TCP_SERVER_PORT     or any available port   )"
  puts "  -s, --setup, --config CONFIG   : Current project config.            (default: \$TCP_SERVER_CONFIG   or \"NO_CONFIG\"          )"
  puts "  -v, --verbose                  : Toggle verbose.                    (default: \$TCP_SERVER_VERBOSE  or 0                    )"
  puts "  -h, --help                     : Show this help message and exits."
  exit 0
}

proc random_string {} {
  ## Generate a randowm password
  #exec openssl rand -base64 12 | tr , -
  set charset "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!@#%^-_=:."
  set charset [string map {"," ""} $charset]
  set password ""
  for {set i 0} {$i < 16} {incr i} {
    append password [string index $charset [expr {int(rand() * [string length $charset])}]]
  }
  return $password
}

## Set arguments default values from env or fallbacks
if {[info exists ::env(TCP_SERVER_PORT)    ]} {set port     "$::env(TCP_SERVER_PORT)"    } else {set port     0                    }
if {[info exists ::env(TCP_SERVER_FILE)    ]} {set file     "$::env(TCP_SERVER_FILE)"    } else {set file     "~/.tcp_servers.csv" }
if {[info exists ::env(TCP_SERVER_PASSWORD)]} {set password "$::env(TCP_SERVER_PASSWORD)"} else {set password [random_string]      }
if {[info exists ::env(TCP_SERVER_LANGUAGE)]} {set language "$::env(TCP_SERVER_LANGUAGE)"} else {set language "UNKNOWN"            }
if {[info exists ::env(TCP_SERVER_PROJECT) ]} {set project  "$::env(TCP_SERVER_PROJECT)" } else {set project  "NO_PROJECT"         }
if {[info exists ::env(TCP_SERVER_CONFIG)  ]} {set setup    "$::env(TCP_SERVER_CONFIG)"  } else {set config   "NO_CONFIG"          }
if {[info exists ::env(TCP_SERVER_VERBOSE) ]} {set verbose  "$::env(TCP_SERVER_VERBOSE)" } else {set verbose  0                    }

## Parse command line arguments
for {set i 0} {$i < [llength $argv]} {incr i} {
  set arg [lindex $argv $i]
  switch -- $arg {
    -h - -help - --help     { print_help                                  ; }
    -f - --file             { incr i ; set file     [lindex $argv $i]     ; }
    -k - --key - --password { incr i ; set password [lindex $argv $i]     ; }
    -l - --language         { incr i ; set language [lindex $argv $i]     ; }
    -n - --name - --project { incr i ; set project  [lindex $argv $i]     ; }
    -p - --port             { incr i ; set port     [lindex $argv $i]     ; }
    -s - --setup - --config { incr i ; set config   [lindex $argv $i]     ; }
    -v - --verbose          { set verbose 1                               ; }
    default                 { puts "Unknown argument: $arg" ;  print_help ; }
  }
}

## =======================================================
## Clean down servers
## =======================================================

## Make sure csv files exists and has the right access
exec bash -c "touch $file ; chmod 600 $file"
exec bash -c "\$(realpath \$(dirname [info script])/../bin/tcp_client) --clean"

## =======================================================
## Start the server:
## Listen on port, write every received message to stdout.
## Wait for answer from stdin, forward it through port.
## =======================================================

fconfigure stdin -buffering none -blocking 0

## Create a socket to listen on any available port
set server [socket -server accept_client $port]

## Make sure socket is well closed when program is exited
proc cleanup {} {
  global server
  if {[info exists server]} {
    close $server
    puts stderr "Server closed."
    flush stderr
  }
}

# Override exit to add cleanup
rename exit _exit
proc exit {args} {
  cleanup
  eval _exit $args
}

## Configure socket and retrieve port
fconfigure $server -buffering none -blocking 1
set port [lindex [fconfigure $server -sockname] 2]
set host [exec hostname -f]

if {$verbose} {
  puts stderr "Listening on $host:$port decrypting with $password"
  flush stderr
}

## Define the procedure to accept client connections
proc accept_client {sock addr port} {
  fileevent $sock readable [list handle_request $sock $addr $port]
}

## Define the procedure to handle incoming requests
proc handle_request {sock addr port} {
  ## Read message from socket, print it to stdout
  global password verbose
  fconfigure $sock -translation binary -encoding binary
  set msg [read $sock]

  ## Write the encrypted message to a temporary file
  set tmp_msg [exec mktemp /tmp/gpg.XXXXX]
  set fh [open $tmp_msg "w"]
  fconfigure $fh -translation binary -encoding binary
  puts -nonewline $fh $msg
  close $fh

  ## Decrypt message
  set msg [exec gpg --decrypt --batch --yes --passphrase $password $tmp_msg 2>/dev/null]
  file delete $tmp_msg

  if {$verbose} {
    flush stderr
  }
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

## =======================================================
## Write server data to CSV file
## =======================================================

set timestamp [clock format [clock seconds] -format "%Y-%m-%d_%H-%M-%S"]

## Write server data to csv file
set tmp_file [exec mktemp /tmp/csv.XXXXX]
exec bash -c "{\
  echo  'timestamp, host, port, password, language, project, config' ;\
  echo '$timestamp,$host,$port,$password,$language,$project,$config' ;\
  grep -v '^timestamp' $file || :                                    ;\
  } >> $tmp_file && mv -f $tmp_file $file"
exec bash -c "chmod 600 $file"

## =======================================================
## Keep the event loop running indefinitely
## =======================================================

vwait forever

