;	$Id: ollama-chat-md-mode.el,v 1.3 2025/04/18 09:36:35 vst Exp vst $	
;;; ollama-chat-md-mode.el --- chat with local LLM using ollama API, markdown mode

;; Copyright (C) 2025- Vladimir Stavrov

;; Maintainer: https://github.com/v1st-git
;; Keywords: Emacs, Eliza, chat-bot, ollama, LLM

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The single entry point `emacs-ollama-chat', implements a chat with LLM using ollama API.
;; It is successor of the classic ELIZA demonstration of pseudo-AI and doctor.el.

;;; Code:
;; Requires ollama.el from URL: http://github.com/zweifisch/ollama
;; Uses ollama-prompt
;;
(require 'ollama)
(require 'markdown-mode)

(defgroup ollama-chat-mode nil
  "Ollama chat client for Emacs."
  :group 'ollama-chat-mode)

(defcustom ollama-chat-mode:ollama-endpoint "http://localhost:11434/api/generate"
  "Ollama http service endpoint."
  :group 'ollama-chat-mode
  :type 'string)

(defcustom ollama-chat-mode:model-name "phi4:latest"
  "Ollama model."
  :group 'ollama-chat-mode
  :type 'string)

(defcustom ollama-chat-mode:human-keystring "\n<!--<human>\n" ;; emulate md comments
  "Ollama human keystring."
  :group 'ollama-chat-mode
  :type 'string)

(defcustom ollama-chat-mode:bot-keystring "<bot>:
-->" ;; close md comments
  "Ollama bot keystring."
  :group 'ollama-chat-mode
  :type 'string)

(defcustom ollama-chat-mode:time-format "%Y-%m-%d %H:%M:%S "
  "Ollama bot keystring."
  :group 'ollama-chat-mode
  :type 'string)

(defcustom ollama-chat-mode:ollama-chat-buffer "*ollama-chat-buffer*"
  "Chat output will by default go to that buffer."
  :group 'ollama-chat-mode
  :type 'string)


(defvar ollama-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\n" 'ollama-chat-read-print)
    (define-key map "\r" 'ollama-chat-ret-or-read)
    map))

(define-derived-mode ollama-chat-mode markdown-mode "Ollama-Chat"
  "Major mode for running the Emacs-Ollama-Chat (ex. Eliza) program.
Like Text mode with Auto Fill mode except that RET when point is after a newline, 
or LFD at any time, reads the sentence before point until <human> keystring, 
and prints the answer, received from Ollama API"
  (turn-on-auto-fill)
  (goto-char (point-max))
  (ollama-chat-type (list "<!--" "Here you can interact with LLM model by Ollama api."
			  "Each time you are finished talking, type RET twice ." "-->"))
  (insert "\n"))



;;;###autoload
(defun emacs-ollama-chat ()
  "Switch to *emacs-ollama-chat* buffer and start chatting."
  (interactive)
  (switch-to-buffer ollama-chat-mode:ollama-chat-buffer)
  (ollama-chat-mode))

(defun ollama-chat-ret-or-read (arg)
  "Insert a newline if preceding character is not a newline.
 Otherwise call the Ollama-Chat to parse preceding sentence."
  (interactive "*p")
  (if (= (preceding-char) ?\n)
      (ollama-chat-read-print)
    (newline arg)))

(defun ollama-chat-read-print ()
  "Top level loop."
  (interactive)
  (setq ollama-chat-sent (ollama-chat-readin))
  (insert "\n")
  (ollama-chat-doc-ollama ollama-chat-sent)
  (insert "\n")
  (setq ollama-chat--bak ollama-chat-sent))

(defun ollama-chat-readin ()
  "Read a sentence.  Return it as a string."
  (goto-char (point-max))
  (search-backward ollama-chat-mode:human-keystring) ; find latest human keystring
  (search-forward ollama-chat-mode:human-keystring) ; set point after keystring
  (let ((sentence
	 (buffer-substring-no-properties (point) (goto-char (point-max)))))
    sentence))


;; Main processing function for sentences that have been read.
(defun ollama-process-query (query-text)
  "Get answer to input query text."
  (ollama-prompt ollama-chat-mode:ollama-endpoint query-text ollama-chat-mode:model-name))

(defun ollama-chat-doc-ollama (query-text)
  "Insert LLM answer, received from Ollama API, to chat buffer"
  (ollama-chat-type (list (ollama-process-query query-text))))

;; Output of replies.

(defun ollama-chat-type (ans)
  "Output to buffer a list of symbols or strings as a sentence with time and model name"
  (let* ((bot-header-format
	  (concat ollama-chat-mode:time-format
		  "(" ollama-chat-mode:model-name ")"
		  ollama-chat-mode:bot-keystring
		  ))
	 (ans-str
	  (mapconcat
	   (lambda(x) (if (stringp x) x (prin1-to-string x)))
	   (cons (format-time-string bot-header-format)
		 ans)
	   "\n")))
    (insert ans-str)
    (insert "\n")
    (insert ollama-chat-mode:human-keystring)
    ))



(provide 'ollama-chat-md-mode)

;;; ollama-chat-md-mode.el ends here
