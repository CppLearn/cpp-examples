;; -------------------------------------------------------------------------
;;  File:    grid.lsp
;;  Created: Sun Sep 18 18:08:36 2016
;;  Comment: Package for plotting and visualization.
;; -------------------------------------------------------------------------

(defpackage :grid

  ( :use :common-lisp :slip )

  ( :export
    
    :plot-list        ; plot list of values.
    :plot-2d-func     ; plot 2-d function.
    :plot-xy          ; plot two lists vs. each other.
    :plot-scatterplot ; plot two lists vs. each other as a scatterplot.

    ) )

(in-package grid)

(defun plot-list (l)
  "Plot a list of values."
  (let ( (fname "clisp-one-var-plot.dat") )
    (slip:list-to-file l fname)
    (ext:run-program "clisp_gplot_one_var.sh" :arguments
                     (list fname))    
    (ext:run-program "/usr/bin/rm" :arguments
                     (list "-vf" fname))))

(defun plot-2d-func (f &optional (start -10) (end 10))
  "Plot a function f."
  (let ( (samples (loop for x from start to end by 0.1 collect (funcall f x))) )
    (plot-list samples)))

(defun plot-xy (a b)
  "Plot two lists: list a on x-axis, list b on y-axis."
  (let ( (fname "clisp-xy-plot.dat") )
    (with-open-file (f fname :direction :output)
      (mapcar (lambda (x y)
                (format f "~% ~a ~a" x y)) a b))
    (ext:run-program "clisp_gplot_xy.sh" :arguments
                     (list fname))    
    (ext:run-program "/usr/bin/rm" :arguments
                     (list "-vf" fname))))

(defun plot-scatterplot (a b)
  "Create a scatter plot: list a on x-axis, list b on y-axis."
  (let ( (fname "clisp-scatterplot.dat") )
    (with-open-file (f fname :direction :output)
      (mapcar (lambda (x y)
                (format f "~% ~a ~a" x y)) a b))
    (ext:run-program "clisp_gplot_scatterplot.sh" :arguments
                     (list fname))    
    (ext:run-program "/usr/bin/rm" :arguments
                     (list "-vf" fname))))

