;; -------------------------------------------------------------------------
;;  File:    pixel.lsp
;;  Created: 02.26.21
;;  Comment: Library to support graphics with a Python/Pygame graphics server.
;; -------------------------------------------------------------------------

(defpackage :pixel

  ( :use :common-lisp )

  ( :export   
    ;;
    ;; -- screen --
    :screen
    :build-screen
    ;;
    ;; -- world --
    :world
    :build-world
		;; -- graphics context --
		:gc
		:build-gc
                                        ;
    :init
    :shutdown
    ;;
    ;; -- methods --
    ;;
    :draw-pixel
    :draw-line
    :draw-circle
    :draw-text
																				; with world scaling
		:wdraw-pixel
		:wdraw-line
		:wdraw-circle
		:wdraw-text
		
    ;;
    ;; -- colors --
    :colors   
    :*black*
    :*white*
    :*red*
    :*green*
    :*blue*
    :*yellow*
    :*cyan*
    :*magenta*
    ))

(in-package pixel)

(defconstant *socket* 54321)
(defconstant *ip* "127.0.0.1")

(defconstant *black*    (vector 0 0 0))
(defconstant *white*    (vector 255 255 255))
(defconstant *red*      (vector 255 0 0))
(defconstant *green*    (vector 0 255 0))
(defconstant *blue*     (vector 0 0 255))
(defconstant *yellow*   (vector 255 255 0))
(defconstant *cyan*     (vector 0 255 255))
(defconstant *magenta*  (vector 255 0 255))

(defvar colors '(*black* *white* *red* *green* *blue* *yellow* *cyan* *magenta*))

;; (defun dwarf (n &key weight gamma) 
;;     (format t "~% n = ~a, weight = ~a, gamma = ~a" n weight gamma))

(defun launch-server (canvas)
  (progn
    (format t "~% [*] Launching Lisp Graphics Server...")
    (format t "~% [*] canvas size: ~a x ~a"
            (first canvas)
            (second canvas))
    (format t "~%~%")
    (defvar args (loop for arg in canvas collect (write-to-string arg)))
    (append args '("&"))
																				; pygame
																				; (ext:run-program "/home/rick/git/lisp/libs/launch-pixel-server.sh" :arguments args)

    (ext:run-program "/home/rick/git/lisp/libs/launch-raylib-server.sh" :arguments args)
    (sleep 4)))

                                        ; -- screen class to handle
                                        ; -- physical screen resolution.

(defstruct screen
	"Stores screen dimensions."
	x-min
	x-max
	y-min
	y-max
	(x-range 0)
	(y-range 0))

(defun build-screen (width height)
  (defvar new-screen (make-screen :x-min 0 :x-max width :y-min 0 :y-max height))
  (setf (screen-x-range new-screen) (- (screen-x-max new-screen)
                                      (screen-x-min new-screen)))
  (setf (screen-y-range new-screen) (- (screen-y-max new-screen)
                                      (screen-y-min new-screen)))
  new-screen)

                                        ; -- world to handle
                                        ; -- world coordinates.

(defstruct world
	"Stores world coordinates."
	x-min
	x-max
	y-min
	y-max
	(x-range 0)
	(y-range 0))

(defun build-world (xmin xmax ymin ymax)
	(defvar new-world (make-world :x-min xmin :x-max xmax :y-min ymin :y-max ymax))
  (setf (world-x-range new-world) (- (world-x-max new-world)
                                    (world-x-min new-world)))
  (setf (world-y-range new-world) (- (world-y-max new-world)
                                    (world-y-min new-world)))
  new-world)

(defun get-scale-x (s w)
  "Return function that computes world to screen transform: x coord"  
  (lambda (x)
    (let* ((dist-to-x-min (abs (- (world-x-min w)
                                  x)))
           (dist-x-ratio (float (/ dist-to-x-min
                                   (world-x-range w))))
           (canvas-x (round (* dist-x-ratio
                               (screen-x-range s)))))
      canvas-x)))

(defun get-scale-y (s w)
  "Return function that computes world to screen transform: y coord"  
  (lambda (y)
    (let* ((dist-to-y-min (abs (- (world-y-min w)
                                  y)))
           (dist-y-ratio (float (/ dist-to-y-min
                                   (world-y-range w))))
           (canvas-y (round (* dist-y-ratio
                               (screen-y-range s)))))
      canvas-y)))

(defstruct gc
	"Graphics Controller."
	screen
	world
	scale-x
	scale-y)

(defun build-gc (s w)
	(defvar gc (make-gc :screen s :world w))
	(setf (gc-scale-x gc) (get-scale-x s w))
	(setf (gc-scale-y gc) (get-scale-y s w))
	gc)

(defun init (gc)
  "Initialize graphics with screen s"
  (progn
    (launch-server (list (screen-x-max
													(gc-screen gc))
                         (screen-y-max
													(gc-screen gc)))))
													
  (format t "~% [*] Connecting Lisp to Pixel Server with ip: ~a socket: ~a" *ip* *socket*)
  (defvar *pixel-stream* (ext:socket-connect *socket* *ip*)))

(defun shutdown ()
  (close *pixel-stream*))


; //  ---------------------------------------------------------------  //
; //  pixel                                                            //
; //  ---------------------------------------------------------------  //

(defun draw-pixel (x y color)
  (setf mesg (format nil "{ PIXEL ~a ~a ~a ~a ~a }"
										 x y
										 (svref color 0)
										 (svref color 1)
										 (svref color 2)
										 ))
  (print mesg *pixel-stream*))

(defmacro wdraw-pixel (px1 py1 color)
	`(let ((x1 (funcall (gc-scale-x gc) ,px1))
				 (y1 (funcall (gc-scale-y gc) ,py1)))
		 (pixel:draw-pixel x1 y1 ,color)))

; //  ---------------------------------------------------------------  //
; //  line                                                             //
; //  ---------------------------------------------------------------  //

(defun draw-line (x1 y1 x2 y2 color)
  (setf mesg (format nil "{ LINE ~a ~a ~a ~a ~a ~a ~a }"
										 x1 y1 x2 y2
										 (svref color 0)
										 (svref color 1)
										 (svref color 2)))  
  (print mesg *pixel-stream*))

(defmacro wdraw-line (px1 py1 px2 py2 color)
	`(let ((x1 (funcall (gc-scale-x gc) ,px1))
				 (x2 (funcall (gc-scale-x gc) ,px2))
				 (y1 (funcall (gc-scale-y gc) ,py1))
				 (y2 (funcall (gc-scale-y gc) ,py2)))
		 (format t "~% line: ~a ~a ~a ~a" x1 y1 x2 y2)
		 (pixel:draw-line x1 y1 x2 y2 ,color)))


; //  ---------------------------------------------------------------  //
; //  circle                                                           //
; //  ---------------------------------------------------------------  //

(defun draw-circle (x y radius color)
  (setf mesg (format nil "{ CIRCLE ~a ~a ~a ~a ~a ~a }"
										 x y
										 radius
										 (svref color 0)
										 (svref color 1)
										 (svref color 2)))
  (print mesg *pixel-stream*))

(defmacro wdraw-circle (px1 py1 radius color)
	`(let ((x1 (funcall (gc-scale-x gc) ,px1))
				 (y1 (funcall (gc-scale-y gc) ,py1)))
		 (pixel:draw-circle x1 y1 ,radius ,color)))

; //  ---------------------------------------------------------------  //
; //  text                                                             //
; //  ---------------------------------------------------------------  //

(defun draw-text (text x y color)
  (setf mesg (format nil "{ TEXT ~a ~a ~a ~a ~a ~a }"
                     text                    
                     x y
                     (svref color 0)
                     (svref color 1)
                     (svref color 2)))
  (print mesg *pixel-stream*))

(defmacro wdraw-text (text px1 py1 color)
	`(let ((x1 (funcall (gc-scale-x gc) ,px1))
				 (y1 (funcall (gc-scale-y gc) ,py1)))
		 (pixel:draw-text ,text x1 y1 ,color)))

