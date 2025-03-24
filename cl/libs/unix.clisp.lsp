;; -------------------------------------------------------------------------
;;  File:    unix.clisp.lsp
;;  Created: Fri Mar 21 21:48:41 2025
;;  By:      Rick <rick@bee>
;;  Comment: CLISP specific system calls
;; -------------------------------------------------------------------------

(defun get-args ()
	ext:*args*)

(defun run (cmd args-string)
  (let ((args (slip:split-string args-string #\ ))
        (*output-file* "clisp.run.out")
        (output-list nil))

    (ext:run-program cmd :arguments args
                         :output *output-file*
                         :if-output-exists :overwrite)
    (if (probe-file *output-file*)
        (setf output-list (slip:file-to-list *output-file* )))
    output-list))

(defun chdir (path)
	(ext:cd path))

																				; ext:delete-directory
																				; ext:dir
																				; ext:probe-directory
																				; ext:edit-file
																				; ext:editor-name
																				; ext:editor-tempfile
																				; ext:physical-memory
																				; ext:make-directory

																				; ext:copy-file        
																				; ext:copy-file-info   
																				; ext:copy-file-stat    
