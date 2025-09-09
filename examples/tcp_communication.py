#!/bin/bash
## Run current script in a new terminal
""":" Example of communication between Python and Virtuoso.
$(dirname $0)/../bin/terminal "python3 $0" ; exit $status ; ".
"""
## ===============================================================================================================
## Cadence SKILL `ipcBeginProcess' Python equivalent.
##
## A. Buchet - August 2025
## ===============================================================================================================

import os
import sys

## Define $SKILL_SHARP_ROOT and update $PYTHONPATH accordingly
current_script_path = os.path.realpath(__file__)
skill_sharp_root = os.path.dirname(os.path.dirname(current_script_path))
python_path_to_add = os.path.join(skill_sharp_root, 'python')
if python_path_to_add not in sys.path:
  sys.path.insert(0, python_path_to_add)
os.environ['SKILL_SHARP_ROOT'] = skill_sharp_root
os.environ['PYTHONPATH'] = f"{python_path_to_add}:{os.environ.get('PYTHONPATH', '')}"

import ipc

from textual import on, work
from textual.app import App, ComposeResult
from textual.containers import VerticalScroll
from textual.widgets import Footer, Header, Input, Markdown

class Prompt(Markdown):
  """Markdown for the user prompt."""


class Response(Markdown):
  """Markdown for the reply from the LLM."""
  BORDER_TITLE = "SKILL++"


class SkillApp(App):
  """Simple app to demonstrate chatting to an LLM."""

  AUTO_FOCUS = "Input"

  CSS = """
  Prompt {
    background: $primary 10%;
    color: $text;
    margin: 1;
    margin-right: 8;
    padding: 1 2 0 2;
  }

  Response {
    border: wide $success;
    background: $success 10%;
    color: $text;
    margin: 1;
    margin-left: 8;
    padding: 1 2 0 2;
  }
  """

  def compose(self) -> ComposeResult:
    yield Header()
    with VerticalScroll(id="chat-view"):
      yield Response("All Inputs Will Be Evaluated by SKILL++ Interpreter")
    yield Input(placeholder="SKILL++ input goes here...")
    yield Footer()

  def on_mount(self) -> None:
    """You might want to change the model if you don't have access to it."""
    #self.model = llm.get_model("gpt-4o")
    self.query_one("#chat-view").anchor()

  @on(Input.Submitted)
  async def on_input(self, event: Input.Submitted) -> None:
    """When the user hits return."""
    chat_view = self.query_one("#chat-view")
    event.input.clear()
    await chat_view.mount(Prompt(event.value))
    await chat_view.mount(response := Response())
    ## Evaluate input in SKILL++, display result
    self.send_to_skill(event.value, response)

  @work(thread=True)
  def send_to_skill(self, message: str, response: Response) -> None:
    """Get SKILL response in a thread."""
    response_content = ipc.skill_client(message)
    self.call_from_thread(response.update, response_content)



if __name__ == "__main__":
  app = SkillApp()
  ## Start TCP server in parallel
  ipc.python_server(verbose=True)
  ## Run app
  app.run()


