#!/bin/bash
## Header to use the proper version when running this Python script directly
""":" Main definitions for project_boilerplate.
export SKILL_SHARP_ROOT=$(dirname $(dirname $(realpath $0))) ;
/usr/bin/env python3 "$0" $@ ; exit $status ; ".
"""
## ===============================================================================================================
## Cadence SKILL `ipcBeginProcess' Python equivalent.
##
## A. Buchet - August 2025
## ===============================================================================================================

import os
import asyncio
import threading
from typing import Callable, Optional

class PID:
  """
  PID class to manage a shell command as a subprocess with callback handlers.
  """

  def __init__(
      self,
      command        : str                       ,
      stdout_handler : Optional[Callable] = None ,
      stderr_handler : Optional[Callable] = None ,
      end_handler    : Optional[Callable] = None ,
      ):
    self.command = command
    self.stdout_handler = stdout_handler
    self.stderr_handler = stderr_handler
    self.end_handler = end_handler
    self.thread = threading.Thread(target=self._thread, daemon=True)
    self.thread.start()

  def _thread(self):
    """Run subprocess in a thread"""
    asyncio.run(self._async())

  async def _async(self):
    """Start subprocess in the background"""
    self.process = await asyncio.create_subprocess_shell(
      self.command                   ,
      stdin=asyncio.subprocess.PIPE  ,
      stdout=asyncio.subprocess.PIPE ,
      stderr=asyncio.subprocess.PIPE ,
    )
    ## Start handlers
    async def handle_stdout():
      if self.stdout_handler:
        async for line in self.process.stdout:
          self.stdout_handler(self, line.decode())

    async def handle_stderr():
      if self.stderr_handler:
        async for line in self.process.stderr:
          self.stderr_handler(self, line.decode())

    ## Run stdout and stderr readers concurrently
    await asyncio.gather(handle_stdout(), handle_stderr())

    status = await self.process.wait()
    if self.end_handler:
      self.end_handler(self, status)


  def wait(self):
    """Wait for child process to terminate."""
    self.thread.join()


  def write(self, data: str):
    """Write to child process stdin."""
    stdin = self.process.stdin
    if stdin:
      stdin.write(data.encode())

def info(msg):
  print(msg, flush=True)

def python_server():
  """
  Start Python server, it opens an available port and evaluates all incoming data as Python and return it through the same port.
  """

  def stdout_handler(pid: PID, data: str):
    try:
      data=data.strip()
      if data:
        #info(f"Evaluating |{data}|")
        res = eval(data)
      else:
        res = None
    except Exception as e:
      res = e
    pid.write(f"{res}")

  def stderr_handler(pid: PID, data: str):
    """Print DATA"""
    info(data)

  script = os.path.expandvars("$SKILL_SHARP_ROOT/bin/tcp_server")
  return PID(script, stdout_handler, stderr_handler)


if __name__ == "__main__":
  python_server().wait()

