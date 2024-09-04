
(defparameter *f* nil)
(defparameter *fname* nil)

(defun get-date ()
	(multiple-value-bind (second minute hour day month year) (get-decoded-time)
		(list month day year)))

(defun strip-ext (s)
	(let ((ext-pos (position #\. s :test #'equal)))
		(subseq s 0 ext-pos)))

(defun slides (project &optional (title project) (author "Rick") (date (get-date)))
	(progn
		(setf *fname* (format nil "~a.tex" project))
		(setf *f* (open *fname* :direction :output :if-does-not-exist :create :if-exists :overwrite))
		(format *f* "~% \\documentclass{beamer}")
		(format *f* "~% \\usetheme{DarkConsole}")
		(format *f* "~% \\usepackage{graphicx}")
		(format *f* "~% \\title{~a}" title)
		(format *f* "~% \\author{~a}" author)
		(format *f* "~% \\date{~a}" date)
		(format *f* "~%~% \\begin{document}")
		(format *f* "~%\\frame{\\titlepage}")))

(defun end-slides ()
	(format *f* "~%~% \\end{document}"))

(defun slide (title)
	(format *f* "~%~% \\begin{frame}[fragile]")
	(format *f* "~% \\frametitle{~a}" title))

(defun end-slide ()
	(format *f* "~%~% \\end{frame}"))

(defun itemize (items)
 	(progn
 		(format *f* "~% \\begin{itemize}")
 		(dolist (i items)
 			(format *f* "~% \\item ~a" i))
 		(format *f* "~% \\end{itemize}")))

(defun enumerate (items)
 	(progn
 		(format *f* "~% \\begin{enumerate}")
 		(dolist (i items)
 			(format *f* "~% \\item ~a" i))
 		(format *f* "~% \\end{enumerate}")))

(defun output (s)
	(format *f* "~% ~a" s))

(defun text (text)
  (output text))
																				;(mapcar #'output text-list))

(defun note (title text)
  (format *f* "~% \\begin{block}{~a}" title)
  (format *f* "~% ~a" text)
  (format *f* "~% \\end{block}"))

(defun alert (title text)
  (format *f* "~% \\begin{alertblock}{~a}" title)
  (format *f* "~% ~a" text)
  (format *f* "~% \\end{alertblock}"))

(defun image (image-path &key (width 60) (caption image-path)) 
 	(progn
		(format *f* "~% \\begin{figure}[ht!]")
		(format *f* "~% \\centering")
		(format *f* "~% \\includegraphics[width=~amm]{~a}" width image-path)
		(format *f* "~% \\caption{~a}" caption)
		(format *f* "~% \\label{overflow}")
		(format *f* "~% \\end{figure}")))


;; (defun publish ()
;; 	(setf cmd (format nil "~a ~a" "./late.py" (strip-ext *fname*)))
;;   (ext:run-shell-command cmd))



