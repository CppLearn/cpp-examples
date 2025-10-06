;; -------------------------------------------------------------------------
;;  File:    grid.lsp
;;  Created: Sun Sep 18 18:08:36 2016
;;  Comment: Package for plotting and visualization.
;; -------------------------------------------------------------------------

(defpackage :grid

  ( :use :common-lisp :slip :unix )

  ( :export
    
    :plot-list               ; plot list of values.
    :plot-2d-func        ; plot 2-d function.
    :plot-xy                  ; plot two lists vs. each other.
    :plot-scatterplot ; plot two lists vs. each other as a scatterplot.
		:plot-functions    ; plot all the functions in a list on same plot.
    ) )

(in-package grid)

(defun plot-list (l)
  "Plot a list of values."
  (let ((fname "clisp-one-var-plot.dat")
				(script (concatenate 'string (slip:bin-dir) "clisp_gplot_one_var.sh")) )
    (slip:list-to-file l fname)
		(sleep 0.5)
		(unix:run script fname)
		(unix:run "/usr/bin/rm" (concatenate 'string "-vf" fname))))

(defun plot-2d-func (f &optional (start -10) (end 10))
  "Plot a function f."
  (let ((samples (loop for x from start to end by 0.1 collect (funcall f x))))
    (plot-list samples)))

(defun plot-xy (a b)
  "Plot two lists: list a on x-axis, list b on y-axis."
  (let ((fname "clisp-xy-plot.dat")
				(script (concatenate 'string (slip:bin-dir) "clisp_gplot_xy.sh")))
    (with-open-file (f fname :direction :output)
      (mapcar (lambda (x y)
                (format f "~% ~a ~a" x y)) a b))
		(sleep 0.5)		
		(unix:run script fname)
		(unix:run "/usr/bin/rm" (concatenate 'string "-vf " fname))))

(defun plot-scatterplot (a b)
  "Create a scatter plot: list a on x-axis, list b on y-axis."
  (let ((fname "clisp-scatterplot.dat")
				(script (concatenate 'string (slip:bin-dir) "clisp_gplot_scatterplot.sh")))
    (with-open-file (f fname :direction :output)
      (mapcar (lambda (x y)
                (format f "~% ~a ~a" x y)) a b))
		(sleep 0.5)		
		(unix:run script fname)
		(unix:run "/usr/bin/rm" (concatenate 'string "-vf " fname))))

(defun plot-functions (list-of-functions)
	"Use gnuplot to plot a list of functions on the same plot."
  (let ((fname "clisp-plot-functions.dat"))
		(with-open-file (plot-file fname :direction :output :if-exists :supersede)
			(write-string "plot " plot-file)
			(labels ((write-functions (function-list f)
								 (if (> (length function-list) 1)
										 (progn
											 (write-string (car function-list) f)
											 (write-string ", " f)
											 (write-functions (cdr function-list) f))
										 ; else last one
										 (write-string (car function-list) f))))
				(write-functions list-of-functions plot-file)))
																				; run gnuplot on our script
		(unix:run "gnuplot" (format nil "-p -c ~a" fname))
		; remove file when done
		(unix:run "/usr/bin/rm" (concatenate 'string "-vf " fname))))

