;; -------------------------------------------------------------------------
;;  File:    unix.ccl.lsp
;;  Created: Fri Mar 21 21:52:40 2025
;;  By:      Rick <rick@bee>
;;  Comment: Clozure (ccl) specific system calls
;; -------------------------------------------------------------------------

(require "asdf")

(defun get-args ()
	(ccl:*command-line-argument-list*))

(defun run-internal (cmd args-string)
	(let ((full-cmd (concatenate 'string cmd " " args-string))
				(output nil)
				(output-list nil))
		(setf output (uiop:run-program full-cmd :output :string))
		(setf output-list (slip:split-string output #\Newline))
		output-list))



