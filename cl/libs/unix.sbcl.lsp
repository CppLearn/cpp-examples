;; -------------------------------------------------------------------------
;;  File:    unix.sbcl.lsp
;;  Created: Fri Mar 21 22:15:41 2025
;;  By:      Rick <rick@bee>
;;  Comment: Steel Bank Common Lisp (sbcl) specific system calls
;; -------------------------------------------------------------------------

(require "asdf")

(defun get-args ()
	sb-ext:*posix-argv*)

(defun run-internal (cmd args-string)
	(ignore-errors (let* ((output (uiop:run-program (concatenate 'string cmd " " args-string) :output :string :ignore-error-status t :force-shell t))
												(output-list (slip:split-string output #\Newline)))
									 output-list)))





