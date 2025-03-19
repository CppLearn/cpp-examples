
(defpackage :unix

  ( :use :common-lisp :slip )

  ( :export 
   ; constants
                                        ; OS
    :get-args
    :run
                                        ; file system
		:chdir
    :lart
    :ls
    :get-files
    :files
    :get-dirs
    :peek-file
    :rm-ext
                                        ; file edits
    :gedit-file
    :libedit
                                        ; utilities
    :date
    
                                        ; sounds
    :play-sound
    
    ))

(in-package unix)

; constants

; (defconstant +speed-of-light+ 299792458) ; speed of light m/s

(defun get-args ()
  (or 
   #+CLISP ext:*args*
   #+SBCL sb-ext:*posix-argv*  
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))

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

                                        ; file system
(defun chdir (path)
	(ext:cd path))

(defun lart ()
  (run "ls" "-lart"))

(defun ls (path)
  (let ((dir-entries (run "ls" (concatenate 'string "-lart " path))))
    (loop for d in dir-entries do (format t "~% ~a" d))))

(defun get-files ()
  (let ((dir-entries (run "ls" "-l")))
    (loop for e in dir-entries if (and
                                   (not (slip:starts-with "d" e))
                                   (not (slip:starts-with "total" e)))
          collect (slip:last-word e))))

(defun get-dirs ()
  (let ((dir-entries (run "ls" "-l")))
    (loop for e in dir-entries if (slip:starts-with "d" e)
          collect (slip:last-word e))))

(defun files ()
  (loop for f in (unix:get-files) do (format t "~% :: ~a" (unix:run "file" f))))

(defun rm-ext (f)
  (let ((parts (slip:split-string f #\.)))
    (car parts)))

(defun peek-file (f n)
  (let ((fh (open f :direction :input)))
    (loop repeat n do
      (format t "~% : ~a" (read-line fh)))
    (close fh)))

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
                                        ;(defun ls (path)
; (let ((args nil))
;   (push "-l" args)    
;   (push path args)
;   (reverse args)
;   (ext:run-program "ls" :arguments args)))

                                        ; sounds
(defun play-sound (wav)
  (unix:run "aplay" wav))


