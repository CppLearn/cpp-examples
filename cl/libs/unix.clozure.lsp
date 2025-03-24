;; -------------------------------------------------------------------------
;;  File:    unix.ccl.lsp
;;  Created: Fri Mar 21 21:52:40 2025
;;  By:      Rick <rick@bee>
;;  Comment: Clozure (ccl) specific system calls
;; -------------------------------------------------------------------------

(require "asdf")

(defun run (cmd args-string)
	(let* ((output (uiop:run-program (append (list cmd) (list args-string)) :output :string))
				 (output-list (slip:split-string output #\Newline)))
		output-list))


