;; -------------------------------------------------------------------------
;;  File:    grid.lsp
;;  Created: Sun Sep 18 18:08:36 2016
;;  Comment: Package for plotting and visualization.
;; -------------------------------------------------------------------------

(defpackage :grid

  ( :use :common-lisp :slip )

  ( :export
		
    :plot-list      ; plot 1-d list
		:plot-1d-func   ; plot 1-d function.
		:plot-two-lists ; plot two lists vs. each other.

		) )

(in-package grid)

(defun plot-list (l)
	"Plot a 1-d list."
	(let ( (fname "clisp-one-var-plot.dat") )
		(slip:list-to-file l fname)
		(ext:run-program "clisp_gplot_one_var.sh" :arguments
										 (list fname))		
		(ext:run-program "/usr/bin/rm" :arguments
										 (list "-vf" fname))))

(defun plot-1d-func (f &optional (start -10) (end 10))
	"Plot a 1-dimensional function."
	(let ( (samples (loop for x from start to end by 0.1 collect (funcall f x))) )
		(plot-list samples)))

(defun plot-two-lists (a b)
	"Plot two lists: list a on x-axis, list b on y-axis."
	(let ( (fname "clisp-two-lists-plot.dat") )
		(with-open-file (f fname :direction :output)
			(mapcar (lambda (x y)
								(format f "~% ~a ~a" x y)) a b))
		(ext:run-program "clisp_gplot_two_lists.sh" :arguments
										 (list fname))		
		(ext:run-program "/usr/bin/rm" :arguments
										 (list "-vf" fname))))
		
		

