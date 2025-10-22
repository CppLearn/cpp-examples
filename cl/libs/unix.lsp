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
    :runu                               ; experimental with uiop
                                        ; file system

    :is-mac-p                           ; OS version checks
    
    :chdir
    :lart
    :ls
    :get-files
    :files
    :get-dirs
    :less
    :peek-file
    :join-path
    :join-paths
    :file-only
    :file-stem
    :file-ext
    :add-ext
    
    :pathname-as-file
    :pathname-as-directory
    :list-directory
    :file-exists-p
    :walk-directory
                                        ; file edits
    :gedit-file
    :libedit
                                        ; utilities
    :date
    :universal-time-to-local

    :play-gif                           ; images
    
                                        ; sounds
    :play-sound
    :message   
                                        ; misc
    :figlet
    :mac-figlet
    :fig
    :mac-fig

    :nedry
    
    ))

(in-package unix)

(defun get-lisp-dir ()
  (let ((lisp-config nil)
        (lisp-dir nil)
        (f nil))
    (cond ( (probe-file "/home/rick/.lispdir") (setf lisp-config "/home/rick/.lispdir") )
          ( (probe-file "/home/rickcde/.lispdir") (setf lisp-config "/home/rickcde/.lispdir" ))
          ( (probe-file "/Users/rickcde/.lispdir") (setf lisp-config "/Users/rickcde/.lispdir"))
          ( t (setf msg (format nil "~%~% :: [warning] .lispdir not found! create containing path to lisp libraries."))
              (error msg)))
          
    (with-open-file (f lisp-config :direction :input)
      (setf lisp-dir (read-line f)))
    lisp-dir))

(defvar unix-dialect-fname)
(defvar unix-final-fname)

#+clisp (setf unix-dialect-fname "unix.clisp.lsp")
#+sbcl  (setf unix-dialect-fname "unix.sbcl.lsp")
#+sbcl  (require :sb-posix)
#+CCL   (setf unix-dialect-fname "unix.clozure.lsp")

(setf unix-final-name (concatenate 'string (get-lisp-dir) "/" unix-dialect-fname))
(format t "~% ---------------------------------------------------------------------")
(format t "~% loading unix dialect: ~A" unix-final-name)
(format t "~% ---------------------------------------------------------------------")
(load unix-final-name)
                                        ; processes
(defun run (cmd &optional (args nil))
  (let ((parts nil)
        (cmd-string "")
        (arg-string ""))
    (if (null args)                     ; everything is in cmd.
        (progn 
          (setf parts (slip:split-string cmd #\ ))
          (setf cmd-string (car parts))
          (if (not (null (cdr parts)))
              (setf arg-string (slip:join-strings (cdr parts))))
          (run-internal cmd-string arg-string))
        (progn                          ; args is separate.
          (run-internal cmd args)))))

                                        ; detect different operating systems
(defun is-mac-p ()
  (probe-file "/System/Library/CoreServices/SystemVersion.plist"))

                                        ; file system
(defun chdir (path)
  #+clisp (ext:cd path)
  #+sbcl (sb-posix:chdir path)
  #+CCL (ccl:cwd path))

(defun lart ()
  (run "ls" "-lart"))

(defun ls (path)
  (let ((dir-entries (run "ls" (concatenate 'string "-lart " path))))
    (loop for d in dir-entries do (format t "~% ~a" d))))

(defun get-files (&optional (dir "./") &key (path nil add-path-p) (no-ext nil no-ext-p))
  (let* ((target (concatenate 'string "-l " dir))
         (dir-entries (run "ls" target))
         (files nil))

    (setf files (loop for e in dir-entries if (and
                                               (> (length e) 0)
                                               (not (slip:starts-with "d" e))
                                               (not (slip:starts-with "total" e)))
                  collect (slip:last-word e)))
    (if no-ext-p
        (setf files (loop for f in files collect (car (slip:split-string f #\.)))))
    (if add-path-p
        (mapcar (lambda (f) (concatenate 'string dir "/" f)) files)
        files)))

(defun get-dirs ()
  (let ((dir-entries (run "ls -l")))
    (loop for e in dir-entries if (and
                                   (> (length e) 0)
                                   (slip:starts-with "d" e))
       collect (slip:last-word e))))

(defun files ()
  (loop for f in (unix:get-files) do (format t "~% :: ~a" (run "file" (format nil "~a" f)))))

(defun file-only (path)
  (car (last (slip:split-string path #\/))))

(defun file-stem (f)
  (car (slip:split-string f #\.)))

(defun file-ext (f)
  (cadr (slip:split-string f #\.)))

(defun add-ext (f ext)
  (concatenate 'string f "." ext))

(defun less (f)
  (unix:run "less" f))

(defun peek-file (f n)
  (let ((fh (open f :direction :input)))
    (loop repeat n do
      (format t "~% : ~a" (read-line fh)))
    (close fh)))

(defun join-path (p1 p2)
  (let ((new-path (concatenate 'string p1 "/" p2)))
    new-path))

(defun join-paths (&rest components)
  (let ((new-path (car components)))
    (loop for c in (cdr components) do
         (setf new-path (concatenate 'string new-path (if (not (slip:starts-with "/" c)) "/"
                                                          "")
                                     c)))
    new-path))

                                        ; These file functions are from Practical Common Lisp

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

(defun pathname-as-file (name)
  (let ((pathname name)))
  (when (wild-pathname-p pathname)
    (error "Can't reliably convert wild pathnames."))
  (if (directory-pathname-p name)
      (let* ((directory (pathname-directory pathname))
             (name-and-type (pathname (first (last directory)))))
        (make-pathname
         :directory (butlast directory)
         :name (pathname-name name-and-type)
         :type (pathname-type name-and-type)
         :defaults pathname))
      pathname))

(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
        (make-pathname
         :directory (append (or (pathname-directory pathname)
                                (list :relative))
                            (list (file-namestring pathname)))
         :name nil
         :type nil
         :defaults pathname)
        pathname)))

(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
         :defaults (pathname-as-directory dirname)))

#+clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname
   :directory (append (pathname-directory wildcard) (list :wild))
   :name nil
   :type nil
   :defaults wildcard))

(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (let ((wildcard (directory-wildcard dirname)))

    #+ (or sbcl cmu lispworks)
    (directory wildcard)

    #+openmcl
    (directory wildcard :directories t)

    #+allegro
    (directory wildcard :directories-are-files nil)

    #+clisp
    (nconc
     (directory wildcard)
     (directory (clisp-subdirectories-wildcard wildcard)))

    #- (or sbcl cmu lispworks openmcl allegro clisp)
    (error "list-directory not implented")))


(defun file-exists-p (pathname)
  #+ (or sbcl lispworks openmcl)        ; simple probe-file
  (probe-file pathname)

  #+ (or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  (or (ignore-errors
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext:probe-directory directory-form)
            directory-form))))

  #- (or sbcl cmu lispworks openmcl allegro clisp)
  (error "file-exists-p not implemented"))

(defun walk-directory (dirname fn &key directories (test (constantly t)))
  (labels
      ((walk (name)
         (cond
           ((directory-pathname-p name)
            (when (and directories (funcall test name))
              (funcall fn name))
            (dolist (x (list-directory name)) (walk x)))

           ((funcall test name) (funcall fn name)))))
    (walk (pathname-as-directory dirname))))

                                        ; file edits

(defun libedit (lib)
  (gedit-file (concatenate 'string (get-lisp-dir) "/" lib ".lsp")))

(defun gedit-file (f)
  (let ((args (concatenate 'string f " &")))
    (run "gedit" args)))

                                        ; utilities
(defun date ()
  (run "date" "-d now"))

(defun universal-time-to-local (universal-time)
  "Convert universal time to a readable local time string"
  (multiple-value-bind (second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time universal-time)
    (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month date hour minute second)))

                                         ; images
(defun play-gif (gif)
  (run (format nil "gifview --min-delay 20 --animate ~A" gif)))

                                        ; sounds
(defun play-sound (wav &key (show nil show-p))
  (if show-p
      (format t "~% playing sound: ~a" wav))
  (setf cmd (cond ((is-mac-p) "afplay ~a")
                  (t "aplay --quiet ~a")))  
  (unix:run (format nil cmd wav)))

                                        ; misc

(defun message (msg)
  (let ((ui-beep (concatenate 'string (get-lisp-dir) "/" "ui-beep.wav")))
    (play-sound ui-beep)
    (slip:color (format nil "~%[~A] " (unix:universal-time-to-local (get-universal-time))) slip:green)
    (slip:color msg slip:blue)))

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

(defun nedry ()
  (let ((nedry-gif (concatenate 'string (get-lisp-dir) "/" "nedry.gif")))
    (if (probe-file nedry-gif) (unix:run (format nil "gifview --animate ~A" nedry-gif))
      (format t "~% Ah Ah Ah... You didn't say the magic word!"))))

