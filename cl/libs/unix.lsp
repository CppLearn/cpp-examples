;; -------------------------------------------------------------------------
;;  File:    unix.lsp
;;  Created: Fri Mar 21 21:51:33 2025
;;  By:      Rick <rick@bee>
;;  Comment: Library for OS/System calls.
;; -------------------------------------------------------------------------

(defpackage :unix

  ( :use :common-lisp :slip )
  ( :export 
                                        ; OS
    :get-args
    :run
		:runu   ; experimental with uiop
                                        ; file system
		:chdir
    :lart
    :ls
    :get-files
    :files
    :get-dirs
    :peek-file
		:join-path
    :rm-ext
                                        ; file edits
    :gedit-file
    :libedit
                                        ; utilities
    :date
    
                                        ; sounds
    :play-sound
																				; misc
		:figlet
		:mac-figlet
		:fig
		:mac-fig

    ))

(in-package unix)

#+clisp (load "/home/rick/git/lisp/libs/unix.clisp.lsp")
#+sbcl (load "/home/rick/git/lisp/libs/unix.sbcl.lsp")
#+CCL (load "/home/rick/git/lisp/libs/unix.clozure.lsp")

																				; processes
(defun run (cmd &optional (args nil))
	(let ((parts nil)
				(cmd-string "")
				(arg-string ""))
		(if (null args)  ; everything is in cmd.
				(progn 
					(setf parts (slip:split-string cmd #\ ))
					(setf cmd-string (car parts))
					(if (not (null (cdr parts)))
							(setf arg-string (slip:join-strings (cdr parts))))
					(run-internal cmd-string arg-string))
				(progn       ; args is separate.
					(run-internal cmd args)))))

                                        ; file system
(defun lart ()
  (run "ls" "-lart"))

(defun ls (path)
  (let ((dir-entries (run "ls" (concatenate 'string "-lart " path))))
    (loop for d in dir-entries do (format t "~% ~a" d))))

(defun get-files ()
  (let ((dir-entries (run "ls -l")))
    (loop for e in dir-entries if (and
																	 (> (length e) 0)
                                   (not (slip:starts-with "d" e))
                                   (not (slip:starts-with "total" e)))
          collect (slip:last-word e))))

(defun get-dirs ()
  (let ((dir-entries (run "ls -l")))
    (loop for e in dir-entries if (and
																	 (> (length e) 0)
																	 (slip:starts-with "d" e))
          collect (slip:last-word e))))

(defun files ()
  (loop for f in (unix:get-files) do (format t "~% :: ~a" (run "file" (format nil "~a" f)))))

(defun rm-ext (f)
  (let ((parts (slip:split-string f #\.)))
    (car parts)))

(defun peek-file (f n)
  (let ((fh (open f :direction :input)))
    (loop repeat n do
      (format t "~% : ~a" (read-line fh)))
    (close fh)))

(defun join-path (p1 p2)
	(let ((new-path (concatenate 'string p1 "/" p2)))
		new-path))

                                        ; file edits
(defun get-lisp-dir ()
  (let ((lisp-config nil)
        (lisp-dir nil)
        (f nil))
    (cond ( (probe-file "/home/rick/.lispdir") (setf lisp-config "/home/rick/.lispdir") )
          ( (probe-file "/home/rickde/.lispdir") (setf lisp-config "/home/rickcde/.lispdir" ))
          ( t (setf msg (format nil "~%~% :: [warning] .lispdir not found! create containing path to lisp libraries."))
              (error msg)))
    (with-open-file (f lisp-config :direction :input)
      (setf lisp-dir (read-line f)))))

(defun libedit (lib)
  (gedit-file (concatenate 'string (get-lisp-dir) "/" lib ".lsp")))

(defun gedit-file (f)
  (let ((args (concatenate 'string f " &")))
    (run "gedit" args)))

                                        ; utilities
(defun date ()
  (run "date" "-d now"))

                                        ; sounds
(defun play-sound (wav)
  (run "aplay" wav))

																				; misc
(defun figlet (font mesg)
	(if (probe-file "/usr/bin/figlet")
			(run "/usr/bin/figlet" (format nil "-f ~a ~a" font mesg))
																				;
			(format t "~% [warning] sorry no figlet installed!~%")))

(defun fig (mesg)
	(let ((figlet-lines (unix:figlet "small" mesg)))
		(loop for line in (unix:figlet "small" mesg) do (format t "~% ~a" line))))

(defun mac-figlet (mesg)
	(let ((args (format nil "~a" mesg)))
		(run "figlet" args)))

(defun mac-fig (mesg)
	(loop for line in (unix:mac-figlet mesg) do
		(format t "~% ~a" line)))

