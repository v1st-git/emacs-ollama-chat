;;
;;; mceval-benchmark-process.el --- read McEval Emacs Lisp tests json file;
;;; call LLM by Ollama API to generate response from instruction;
;;; add response to source json line
;;; see https://github.com/MCEVAL/McEval.git

;; Copyright (C) 2025- Vladimir Stavrov

;; Maintainer: https://github.com/v1st-git
;; Keywords: Emacs, ollama, LLM, code generation benchmark

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

(require 'ollama-chat-md-mode)

(defun find-extract-defun (input-text)
  "Extract the first function definition using regular expressions."
  (with-temp-buffer
    (insert input-text)
    (goto-char (point-min))
    (when (re-search-forward "(defun" nil t)
      (condition-case nil
      (let* ((start (progn (beginning-of-defun) (point)))
             (end (progn (forward-sexp 1) (point))))
        ;; Extracting the region as a string
        (buffer-substring-no-properties start end)
	)
      (error nil)))
    ))


(defun enrich-mceval-json-file (input-file-path out-file-path)
  "Read FILE-PATH line by line, extract 'instruction' tag content from each line,
send it as a prompt to ollama API and add raw_generation in the current buffer."
  (with-current-buffer (get-buffer-create "*llm-results*")
    (erase-buffer)
    (insert-file-contents input-file-path)
    (goto-char (point-min))
    (while (not (eobp))
      ;; Read each line which is expected to be a JSON string.
      (let* ((json-line
	      (buffer-substring-no-properties
	       (line-beginning-position)
	       (line-end-position)))
             ;; Parse the JSON object from the current line.
             (parsed-json (condition-case nil
                               (json-read-from-string json-line)
                             (error nil)))
             ;; ;; Extract the 'instruction' field, if available.
             (instruction (and parsed-json (assoc-default 'instruction parsed-json)))
             (task_id (and parsed-json (assoc-default 'task_id parsed-json)))
	     )

        (when instruction
	  ;; (delete-region
	  ;;  (line-beginning-position) (line-end-position))

          ;; Send the extracted instruction to ollama API and get response.
          (let* ((response-text (ollama-chat-process-query instruction))
		 (response-function (find-extract-defun response-text))
		 (response-json (when response-function
				  (json-encode `((raw_generation . ,response-function)))))
		)
            ;; Insert received response into current buffer after each line of JSON processed.
	    (when response-json
	      (goto-char (1- (line-end-position)))
	      (insert (concat ", " (string-trim response-json "{" "}")))
	      (goto-char (line-end-position))
	      )
            ;; (insert (json-encode `((task_id . ,task_id)
	    ;; 		       (response . ,response-text)
	    ;; 		       (model . , ollama-chat-md-mode:model-name)
	    ;; 		       )))
	    (message "%s Task %s processed" (current-time-string) task_id)
	    (print (format "%s Task %s processed" (current-time-string) task_id))
	    ))
        
        ;; Move to the next line in the file.
        (forward-line 1)
	))
    (write-region (point-min) (point-max) out-file-path)
    ))
