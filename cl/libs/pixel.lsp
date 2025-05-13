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
		:build-gc
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
		:*silver*
		:*gray*
		:*maroon*
		:*olive*
		:*purple*
		:*teal*
		:*navy*
		:*orange*
		:*pink*
		:*brown*
		:*firebrick*
		:*crimson*
		:*tomato*
		:*coral*
		:*indian-red*
		:*light-coral*
		:*dark-salmon*
		:*salmon*
		:*light-salmon*
		:*dark-orange*
		:*gold*
		:*dark-golden-rod*
		:*golden-rod*
		:*pale-golden-rod*
		:*dark-khaki*
		:*khaki*
		:*yellow-green*
		:*dark-olive-green*
		:*olive-drab*
		:*lawn-green*
		:*chart-reuse*
		:*green-yellow*
		:*dark-green*
		:*forest-green*
		:*lime*
		:*lime-green*
		:*light-green*
		:*pale-green*
		:*dark-sea-green*
		:*medium-spring-green*
		:*spring-green*
		:*sea-green*
		:*medium-aqua-marine*
		:*medium-sea-green*
		:*light-sea-green*
		:*dark-slate-gray*
		:*dark-cyan*
		:*aqua*
		:*light-cyan*
		:*dark-turquoise*
		:*turquoise*
		:*medium-turquoise*
		:*pale-turquoise*
		:*aqua-marine*
		:*powder-blue*
		:*cadet-blue*
		:*steel-blue*
		:*corn-flower-blue*
		:*deep-sky-blue*
		:*dodger-blue*
		:*light-blue*
		:*sky-blue*
		:*light-sky-blue*
		:*midnight-blue*
		:*dark-blue*
		:*medium-blue*
		:*royal-blue*
		:*blue-violet*
		:*indigo*
		:*dark-slate-blue*
		:*slate-blue*
		:*medium-slate-blue*
		:*medium-purple*
		:*dark-magenta*
		:*dark-violet*
		:*dark-orchid*
		:*medium-orchid*
		:*thistle*
		:*plum*
		:*violet*
		:*orchid*
		:*medium-violet-red*
		:*pale-violet-red*
		:*deep-pink*
		:*hot-pink*
		:*light-pink*
		:*antique-white*
		:*beige*
		:*bisque*
		:*blanched-almond*
		:*wheat*
		:*corn-silk*
		:*lemon-chiffon*
 		:*light-golden-rod-yellow*

		)) ; end package

(in-package pixel)

(defconstant *socket* 54321)
(defconstant *ip* "127.0.0.1")

(defconstant *black* (vector 0 0 0))
(defconstant *white* (vector 255 255 255))
(defconstant *red* (vector 255 0 0))
(defconstant *green* (vector 0 128 0))
(defconstant *blue* (vector 0 0 255))
(defconstant *yellow* (vector 255 255 0))
(defconstant *cyan* (vector 0 255 255))
(defconstant *magenta* (vector 255 0 255))
(defconstant *silver* (vector 192 192 192))
(defconstant *gray* (vector 128 128 128))
(defconstant *maroon* (vector 128 0 0))
(defconstant *olive* (vector 128 128 0))
(defconstant *purple* (vector 128 0 128))
(defconstant *teal* (vector 0 128 128))
(defconstant *navy* (vector 0 0 128))
(defconstant *orange* (vector 255 165 0))
(defconstant *pink* (vector 255 192 203))
(defconstant *brown* (vector 165 42 42))
(defconstant *firebrick* (vector 178 34 34))
(defconstant *crimson* (vector 220 20 60))
(defconstant *tomato* (vector 255 99 71))
(defconstant *coral* (vector 255 127 80))
(defconstant *indian-red* (vector 205 92 92))
(defconstant *light-coral* (vector 240 128 128))
(defconstant *dark-salmon* (vector 233 150 122))
(defconstant *salmon* (vector 250 128 114))
(defconstant *light-salmon* (vector 255 160 122))
(defconstant *dark-orange* (vector 255 140 0))
(defconstant *gold* (vector 255 215 0))
(defconstant *dark-golden-rod* (vector 184 134 11))
(defconstant *golden-rod* (vector 218 165 32))
(defconstant *pale-golden-rod* (vector 238 232 170))
(defconstant *dark-khaki* (vector 189 183 107))
(defconstant *khaki* (vector 240 230 140))
(defconstant *yellow-green* (vector 154 205 50))
(defconstant *dark-olive-green* (vector 85 107 47))
(defconstant *olive-drab* (vector 107 142 35))
(defconstant *lawn-green* (vector 124 252 0))
(defconstant *chart-reuse* (vector 127 255 0))
(defconstant *green-yellow* (vector 173 255 47))
(defconstant *dark-green* (vector 0 100 0))
(defconstant *forest-green* (vector 34 139 34))
(defconstant *lime* (vector 0 255 0))
(defconstant *lime-green* (vector 50 205 50))
(defconstant *light-green* (vector 144 238 144))
(defconstant *pale-green* (vector 152 251 152))
(defconstant *dark-sea-green* (vector 143 188 143))
(defconstant *medium-spring-green* (vector 0 250 154))
(defconstant *spring-green* (vector 0 255 127))
(defconstant *sea-green* (vector 46 139 87))
(defconstant *medium-aqua-marine* (vector 102 205 170))
(defconstant *medium-sea-green* (vector 60 179 113))
(defconstant *light-sea-green* (vector 32 178 170))
(defconstant *dark-slate-gray* (vector 47 79 79))
(defconstant *dark-cyan* (vector 0 139 139))
(defconstant *aqua* (vector 0 255 255))
(defconstant *light-cyan* (vector 224 255 255))
(defconstant *dark-turquoise* (vector 0 206 209))
(defconstant *turquoise* (vector 64 224 208))
(defconstant *medium-turquoise* (vector 72 209 204))
(defconstant *pale-turquoise* (vector 175 238 238))
(defconstant *aqua-marine* (vector 127 255 212))
(defconstant *powder-blue* (vector 176 224 230))
(defconstant *cadet-blue* (vector 95 158 160))
(defconstant *steel-blue* (vector 70 130 180))
(defconstant *corn-flower-blue* (vector 100 149 237))
(defconstant *deep-sky-blue* (vector 0 191 255))
(defconstant *dodger-blue* (vector 30 144 255))
(defconstant *light-blue* (vector 173 216 230))
(defconstant *sky-blue* (vector 135 206 235))
(defconstant *light-sky-blue* (vector 135 206 250))
(defconstant *midnight-blue* (vector 25 25 112))
(defconstant *dark-blue* (vector 0 0 139))
(defconstant *medium-blue* (vector 0 0 205))
(defconstant *royal-blue* (vector 65 105 225))
(defconstant *blue-violet* (vector 138 43 226))
(defconstant *indigo* (vector 75 0 130))
(defconstant *dark-slate-blue* (vector 72 61 139))
(defconstant *slate-blue* (vector 106 90 205))
(defconstant *medium-slate-blue* (vector 123 104 238))
(defconstant *medium-purple* (vector 147 112 219))
(defconstant *dark-magenta* (vector 139 0 139))
(defconstant *dark-violet* (vector 148 0 211))
(defconstant *dark-orchid* (vector 153 50 204))
(defconstant *medium-orchid* (vector 186 85 211))
(defconstant *thistle* (vector 216 191 216))
(defconstant *plum* (vector 221 160 221))
(defconstant *violet* (vector 238 130 238))
(defconstant *orchid* (vector 218 112 214))
(defconstant *medium-violet-red* (vector 199 21 133))
(defconstant *pale-violet-red* (vector 219 112 147))
(defconstant *deep-pink* (vector 255 20 147))
(defconstant *hot-pink* (vector 255 105 180))
(defconstant *light-pink* (vector 255 182 193))
(defconstant *antique-white* (vector 250 235 215))
(defconstant *beige* (vector 245 245 220))
(defconstant *bisque* (vector 255 228 196))
(defconstant *blanched-almond* (vector 255 235 205))
(defconstant *wheat* (vector 245 222 179))
(defconstant *corn-silk* (vector 255 248 220))
(defconstant *lemon-chiffon* (vector 255 250 205))
(defconstant *light-golden-rod-yellow* (vector 250 250 210))

(defvar colors '(*black*
								 *white*
								 *red*
								 *green*
								 *blue*
								 *yellow*
								 *cyan*
								 *magenta*
								 *silver*
								 *gray*
								 *maroon*
								 *olive*
								 *purple*
								 *teal*
								 *navy*
								 *orange*
								 *pink*
								 *brown*
								 *firebrick*
								 *crimson*
								 *tomato*
								 *coral*
								 *indian-red*
								 *light-coral*
								 *dark-salmon*
								 *salmon*
								 *light-salmon*
								 *dark-orange*
								 *gold*
								 *dark-golden-rod*
								 *golden-rod*
								 *pale-golden-rod*
								 *dark-khaki*
								 *khaki*
								 *yellow-green*
								 *dark-olive-green*
								 *olive-drab*
								 *lawn-green*
								 *chart-reuse*
								 *green-yellow*
								 *dark-green*
								 *forest-green*
								 *lime*
								 *lime-green*
								 *light-green*
								 *pale-green*
								 *dark-sea-green*
								 *medium-spring-green*
								 *spring-green*
								 *sea-green*
								 *medium-aqua-marine*
								 *medium-sea-green*
								 *light-sea-green*
								 *dark-slate-gray*
								 *dark-cyan*
								 *aqua*
								 *light-cyan*
								 *dark-turquoise*
								 *turquoise*
								 *medium-turquoise*
								 *pale-turquoise*
								 *aqua-marine*
								 *powder-blue*
								 *cadet-blue*
								 *steel-blue*
								 *corn-flower-blue*
								 *deep-sky-blue*
								 *dodger-blue*
								 *light-blue*
								 *sky-blue*
								 *light-sky-blue*
								 *midnight-blue*
								 *dark-blue*
								 *medium-blue*
								 *royal-blue*
								 *blue-violet*
								 *indigo*
								 *dark-slate-blue*
								 *slate-blue*
								 *medium-slate-blue*
								 *medium-purple*
								 *dark-magenta*
								 *dark-violet*
								 *dark-orchid*
								 *medium-orchid*
								 *thistle*
								 *plum*
								 *violet*
								 *orchid*
								 *medium-violet-red*
								 *pale-violet-red*
								 *deep-pink*
								 *hot-pink*
								 *light-pink*
								 *antique-white*
								 *beige*
								 *bisque*
								 *blanched-almond*
								 *wheat*
								 *corn-silk*
								 *lemon-chiffon*
								 *light-golden-rod-yellow*))
	
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

(defun create-gc (&key (screen nil) (world nil))
	(let ((gc nil)
				(screen-width (car screen))
				 (screen-height (cadr screen))
				 (world-min (car world))
				(world-max (cadr world)))
		(format t "~% [*] Pixel: creating graphics context...")
		(setf gc (pixel:build-gc
							(pixel:build-screen screen-width screen-height)
							(pixel:build-world  world-min world-max world-min world-max)))
		gc))

(defun init (&key (screen nil) (world nil))
  "Initialize graphics with screen s"
	(let ((gc (create-gc :screen screen :world world)))
		(launch-server (list (screen-x-max
													(gc-screen gc))
												 (screen-y-max
													(gc-screen gc))))
		(format t "~% [*] Connecting Lisp to Pixel Server with ip: ~a socket: ~a" *ip* *socket*)
		(defvar *pixel-stream* (ext:socket-connect *socket* *ip*))))

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

