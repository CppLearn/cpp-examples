;; -------------------------------------------------------------------------
;;  File:    grid.lsp
;;  Created: Sun Sep 18 18:08:36 2016
;;  Comment: Package for plotting and visualization.
;; -------------------------------------------------------------------------

(defpackage :grid

  ( :use :common-lisp :slip :unix )

  ( :export
    
    :plot-list         ; plot list of values.
    :plot-2d-func      ; plot 2-d function.
    :plot-xy           ; plot two lists vs. each other.
    :plot-scatterplot  ; plot two lists vs. each other as a scatterplot.
		:plot-functions    ; plot all the functions in a list on same plot.

																				; graph/tree visualizations
																				; these graph functions are with the plotting functions because
																				; this package already depends on the unix package.
		
		:graph-viz   ; undirected graph
		:wgraph-viz  ; undirected weighted graph
		:tree-viz    ; tree (directed graph)
		
    ))

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

																				; tree visualization functions.

(defun tree-viz (root)
	"Display tree using graphviz."
  (let* ((fname "clisp-tnode.dot")
				 (png-name (concatenate 'string fname ".png")))
		(with-open-file (dot-file fname :direction :output :if-exists :supersede)
			(format dot-file "digraph G {~%")
      (format dot-file "~%rankdir=TB;")
      (format dot-file "~%bgcolor=\"lightyellow\";")
			(format dot-file "~%pad=\"0.5\";")
      (format dot-file "~%node [shape=ellipse, style=filled, fillcolor=\"lightblue\", fontname=\"Arial\", fontsize=10];")
			(format dot-file "~%edge [color=\"black\", arrowhead=normal];")
			
			(let ((current-parent nil)
						(tree-nodes nil)
						(*print-case* :downcase))
				(labels ((traverse (subtree)
									 (if (listp subtree)
											 (loop for branch in subtree do
														(if (atom branch) ; down to a leaf
																(if current-parent
																		(format dot-file "~a->~a~%" current-parent branch))
																(progn ; if branch is list, tree it as a subtree
																	(setf current-parent (car subtree))
																	(traverse branch)))))))
					(traverse root)))
			
			(format dot-file "~% ~%}"))       ; close dot file before trying to run dot on it
																				; or else it hangs.
		
		(unix:run (format nil "/usr/bin/dot -Tpng ./~a -o ~a" fname png-name))
																				; remove file when done			
																				; (unix:run "/usr/bin/rm" (concatenate 'string "-vf " fname))
		(sleep 1)
		(unix:run "/usr/bin/eog" (format nil "~a" png-name))))

(defun graph-viz (graph)
	"Display graph using graphviz."
	
  (let* ((fname "clisp-gnode.dot")
				 (png-name (concatenate 'string fname ".png")))
		(with-open-file (dot-file fname :direction :output :if-exists :supersede)
			(format dot-file "graph G {~%")
      (format dot-file "~%bgcolor=\"lightyellow\";")
			(format dot-file "~%pad=\"0.5\";")
      (format dot-file "~%node [shape=ellipse, style=filled, fillcolor=\"lightblue\", fontname=\"Arial\", fontsize=10];")
			(format dot-file "~%edge [color=\"black\", arrowhead=normal];")
			
			(let ((*print-case* :downcase))
				(loop for node in graph do
					(format dot-file "~% ~a -- ~a" (car node) (cadr node)))
			
			(format dot-file "~% ~%}")))       ; close dot file before trying to run dot on it
																				; or else it hangs.
		
		(unix:run (format nil "/usr/bin/dot -Tpng ./~a -o ~a" fname png-name))
																				; remove file when done			
																				; (unix:run "/usr/bin/rm" (concatenate 'string "-vf " fname))
		(sleep 1)
		(unix:run "/usr/bin/eog" (format nil "~a" png-name))))

(defun wgraph-viz (wgraph)
	"Display weighted graph using graphviz."
	
  (let* ((fname "clisp-gnode.dot")
				 (png-name (concatenate 'string fname ".png")))
		(with-open-file (dot-file fname :direction :output :if-exists :supersede)
			(format dot-file "graph G {~%")
      (format dot-file "~%bgcolor=\"lightyellow\";")
			(format dot-file "~%pad=\"0.5\";")
      (format dot-file "~%node [shape=ellipse, style=filled, fillcolor=\"lightblue\", fontname=\"Arial\", fontsize=10];")
			(format dot-file "~%edge [color=\"black\", arrowhead=normal];")
			
			(let ((*print-case* :downcase))
				(loop for node in wgraph do
					(format dot-file "~% ~a -- ~a [label = \"~a\", fontsize=\"9\"];" (car node) (cadr node) (caddr node))))
			
			(format dot-file "~% ~%}"))       ; close dot file before trying to run dot on it
																				; or else it hangs.
		
		(unix:run (format nil "/usr/bin/dot -Tpng ./~a -o ~a" fname png-name))
																				; remove file when done			
																				; (unix:run "/usr/bin/rm" (concatenate 'string "-vf " fname))
		(sleep 1)
		(unix:run "/usr/bin/eog" (format nil "~a" png-name))))
