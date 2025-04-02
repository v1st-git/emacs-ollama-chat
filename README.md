# emacs-ollama-chat
yet another (trivial-minimal) emacs client to chat with LLM using Ollama API
# README.md for `ollama-chat-mode` Emacs Package
# (created from and by ollama-chat-mode.el using phi4:latest LLM)

## Overview

`ollama-chat-mode` is an Emacs package that allows users to interact with a local Large Language Model (LLM) using the Ollama API. This mode provides a conversational interface within Emacs, similar to classic chatbots like ELIZA.

## Features

- **Local LLM Interaction**: Communicate with a local language model via the Ollama API.
- **Customizable Settings**: Configure endpoint, model name, keystrings, and more.
- **Emacs Integration**: Seamlessly integrates into Emacs as a major mode.
- **Interactive Chat**: Supports interactive chatting within Emacs buffers.

## Installation

### Prerequisites

Ensure you have the following installed:

- GNU Emacs
- Ollama package from [ollama GitHub repository](http://github.com/zweifisch/ollama)

### Setup

1. Clone this repository to your desired location.
2. Add the following line to your `.emacs` or `init.el` file to load the package:

   ```elisp
   (add-to-list 'load-path "/path/to/ollama-chat-mode")
   (require 'ollama-chat-mode)
   ```

3. Customize settings in your Emacs configuration if needed:

   ```elisp
   (setq ollama-chat-mode:endpoint "http://localhost:11434/api/generate"
         ollama-chat-mode:model-name "phi4:latest")
   ```

## Usage

1. Start a chat session by running `M-x emacs-ollama-chat`.
2. Interact with the LLM by typing your messages.
3. Press `RET` twice to send your message and receive a response.

## Customization

You can customize various aspects of `ollama-chat-mode`:

- **Endpoint**: Set the Ollama HTTP service endpoint using `ollama-chat-mode:endpoint`.
- **Model Name**: Choose the LLM model with `ollama-chat-mode:model-name`.
- **Keystrings**: Customize human and bot keystrings for message formatting.
- **Buffer Name**: Change the chat buffer name via `ollama-chat-mode:ollama-chat-buffer`.

## Contact

For further information or issues, please check the [GitHub repository](https://github.com/v1st-git/emacs-ollama-chat) or contact the maintainer via [LinkedIn](https://www.linkedin.com/in/vladimir-stavrov-a9803b115).

## License

`ollama-chat-mode` is licensed under the GNU General Public License v3.0 or later.

For more details, see the [GNU General Public License](https://www.gnu.org/licenses/).

---
