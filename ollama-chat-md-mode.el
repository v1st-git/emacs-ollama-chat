;;	$Id: ollama-chat-md-mode.el,v 1.8 2025/07/14 11:59:30 vst Exp vst $	
;;; ollama-chat-md-mode.el --- chat with local LLM using Ollama API, markdown mode

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


(require 'json)
(require 'cl-lib)
(require 'url)
(require 'markdown-mode)

(defgroup ollama-chat-md-mode nil
  "Ollama chat client for Emacs."
  :group 'ollama-chat-md-mode)

(defcustom ollama-chat-md-mode:ollama-endpoint "http://localhost:11434/api/generate"
  "Ollama http service endpoint."
  :group 'ollama-chat-md-mode
  :type 'string)

(defcustom ollama-chat-md-mode:model-name "gemma3:12b" ;; "phi4:latest"
  "Ollama model."
  :group 'ollama-chat-md-mode
  :type 'string)

(defcustom ollama-chat-md-mode:human-keystring "<!--<human>" ;; emulate md comments
  "Ollama human keystring."
  :group 'ollama-chat-md-mode
  :type 'string)

(defcustom ollama-chat-md-mode:bot-keystring "<bot>-->" ;; close md comments
  "Ollama bot keystring."
  :group 'ollama-chat-md-mode
  :type 'string)

(defcustom ollama-chat-md-mode:time-format "%Y-%m-%d %H:%M:%S "
  "Ollama bot keystring."
  :group 'ollama-chat-md-mode
  :type 'string)

(defcustom ollama-chat-md-mode:ollama-chat-buffer "*ollama-chat-buffer*"
  "Chat output will by default go to that buffer."
  :group 'ollama-chat-md-mode
  :type 'string)

(defcustom ollama-chat-md-mode:stream_trigger nil
  "Ollama response stream trigger:
   if true, ask Ollama API to return response in multiple lines."
  :group 'ollama-chat-md-mode
  :type 'boolean)



(defvar ollama-chat-md-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\n" 'ollama-chat-read-print)
    (define-key map "\r" 'ollama-chat-ret-or-read)
    map))

(define-derived-mode ollama-chat-md-mode markdown-mode "Ollama-Chat"
  "Major mode for running the Emacs-Ollama-Chat (ex. Eliza) program.
Like Markdown mode except that RET when point is after a newline, 
or LFD at any time, reads the sentence before point until <human> keystring, 
and prints the answer, received from Ollama API"
  (turn-on-auto-fill)
  (goto-char (point-max))
  (ollama-chat-type (list "<!--" "Here you can interact with LLM model by Ollama API."
			  "Customize LLM model name etc in customization group"
			  "'ollama-chat-md-mode'"
			  "Each time you are finished talking, type RET twice ." "-->"))
  (insert "\n"))



;;;###autoload
(defun emacs-ollama-chat ()
  "Switch to *emacs-ollama-chat* buffer and start chatting."
  (interactive)
  (switch-to-buffer ollama-chat-md-mode:ollama-chat-buffer)
  (ollama-chat-md-mode))

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
  (let* ((ollama_chat_sent (ollama-chat-readin))
	 (ollama_answer (concat "\n" (ollama-chat-doc-ollama ollama_chat_sent) "\n"))
	 (ollama-chat-read-print_result (insert ollama_answer)) ;; var to add watcher if needed
	 )
    ))

(defun ollama-chat-readin ()
  "Read a sentence.  Return it as a string."
  (goto-char (point-max))
  (search-backward ollama-chat-md-mode:human-keystring) ; find latest human keystring
  (search-forward ollama-chat-md-mode:human-keystring) ; set point after keystring
  (while (and (not (eobp))         ; Ensure we don't go past end of buffer
              (looking-at "^[ \t\n]*$")) ; Check if line is only whitespace
    (forward-line 1))             ; Move to the next line

  (let ((sentence
	 (buffer-substring-no-properties (point) (goto-char (point-max)))))
    sentence
    ))


(defun ollama-chat-fetch (url prompt model)
  "sends HTTP POST request to url with body, containing prompt and model, wrapped to json.
   returns response body (json expected) or error message if error occurs"
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
	 (json-body (json-encode `((model . ,model) (prompt . ,prompt))))
	 (json-body
	  (if ollama-chat-md-mode:stream_trigger
	      json-body
	    (replace-regexp-in-string "}$" ",\"stream\":false}" json-body)))
         (url-request-data
          (encode-coding-string json-body 'utf-8))
	 (response-buffer (url-retrieve-synchronously url)))
    (with-current-buffer response-buffer
      (goto-char url-http-end-of-headers)
      (let ((content (decode-coding-string
                      (buffer-substring-no-properties
                       (point) (point-max))
                      'utf-8)))
        content))
    ))

(defun ollama-chat-get-json-tag-from-string (json_string tag_string)
  "return content of 'tag' key in input json string or error message if it occurs"
  (let ((tag_name (intern tag_string)))
    (condition-case err
	(cdr
	 (assoc tag_name
		(json-read-from-string (format "%s" json_string))))
      (error
       (concat
	(format "\nollama-chat-get-json-response-from-string error occurs: %s\n"
  		(error-message-string err))
	(format "in the string:\n%s\n" json_string))
       )
      )))

(defun ollama-chat-json-extract-text-response (json_data_string)
  "apply 'ollama-chat-get-json-tag-from-string' function to each line in input json-data-string,
  return concatenated results, exclude empty lines"
  (mapconcat (lambda(str)
	       (ollama-chat-get-json-tag-from-string str "response"))
             (cl-remove-if #'(lambda (str) (string= str "")) 
                           (split-string json_data_string "\n"))
	     ""))

(defun ollama-chat-process-query (query_text)
  "Get LLM answer to input query text.
   Use Ollama API URL and model from customization constants."
  (let* ((response_string
	  (ollama-chat-fetch ollama-chat-md-mode:ollama-endpoint
			     query_text
			     ollama-chat-md-mode:model-name))
	 (response_text (ollama-chat-json-extract-text-response response_string)))
    response_text
    ))

;; Main processing function for sentences that have been read.

(defun ollama-chat-doc-ollama (query-text)
  "Return LLM answer, received from Ollama API"
  (let* ((answer_string
	  (ollama-chat-answer query-text)))
    answer_string
    ))

;; Output of replies.

(defun ollama-chat-object-to-string (object)
  "return value as a string in any cases: convert to string if object is not a string"
  (if (stringp object) object (prin1-to-string object))
  )

(defun ollama-chat-answer (query_text)
  "Returns concatenated string with time and model name from a list of symbols or strings"
  (let* ((start_timestamp (format-time-string ollama-chat-md-mode:time-format))
	 (start_time (current-time))
	 (llm_answer_list
	   (list (ollama-chat-process-query query-text)))
	 (elapsed_time
	  (float-time (time-subtract (current-time) start_time)))
	 (bot_header_format
	  (concat start_timestamp "-"
		  ollama-chat-md-mode:time-format
		  "="
		  (format "%.3fs\n" elapsed_time)
		  "(" ollama-chat-md-mode:model-name ")"
		  ollama-chat-md-mode:bot-keystring
		  "\n"
		  ))
	 (llm_answer_string
	  (mapconcat
	   #'ollama-chat-object-to-string
	   llm_answer_list))
	 (llm_answer_string
	  (concat (format-time-string bot_header_format)
		  llm_answer_string
		  "\n\n"
		  ollama-chat-md-mode:human-keystring))
	 )
    llm_answer_string
    ))



(provide 'ollama-chat-md-mode)

;;; ollama-chat-md-mode.el ends here
