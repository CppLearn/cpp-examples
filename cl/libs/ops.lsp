
(defpackage :ops

  ( :use :common-lisp :slip )

  ( :export 
   ; constants
																				; OS
		:get-args
		:run
		:run-out
		
  ))

(in-package ops)

; constants

; (defconstant +speed-of-light+ 299792458) ; speed of light m/s

(defun get-args ()
  (or 
   #+CLISP ext:*args*
   #+SBCL *posix-argv*  
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))

(defun run (cmd args-string)
	(let ((args (slip:split-string args-string #\ )))
		(ext:run-program cmd :arguments args)))

(defun run-out (cmd args-string)
	(let ((args (slip:split-string args-string #\ ))
				(*output-file* "clisp.run.out")
				(output-list nil))

		(ext:run-program cmd :arguments args
												 :output *output-file*
												 :if-output-exists :overwrite)
		(if (probe-file *output-file*)
				(setf output-list (slip:file-to-list *output-file* )))
		output-list))

																				;(defun ls (path)
;	(let ((args nil))
;		(push "-l" args)		
;		(push path args)
;		(reverse args)
;		(ext:run-program "ls" :arguments args)))

