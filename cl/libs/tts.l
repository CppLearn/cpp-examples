(load "slip.lsp")
(load "unix.lsp")

(defun detect-tts-cmd ()
  (cond
    ((probe-file "/usr/bin/espeak-ng") "/usr/bin/espeak-ng")
    ((probe-file "/usr/bin/espeak") "/usr/bin/espeak")
    ((probe-file "/usr/bin/say") "/usr/bin/say")
    (t nil)))

(defparameter *tts-cmd* (detect-tts-cmd))

(defun trim-word (w)
  (string-trim '(#\Space #\Tab #\Newline #\, #\. #\! #\? #\; #\: #\" #\') w))

(defun speak-word (word &key (rate 170) (pitch 50) (gap 5))
  (let ((w (trim-word word)))
    (when (and *tts-cmd* (> (length w) 0))
      (cond
        ((string= *tts-cmd* "/usr/bin/say")
         ;; GNUstep say on Linux often has no -r/-p/-g flags.
         (unix:run *tts-cmd* w))
        (t
         (unix:run *tts-cmd* (format nil "-s ~a -p ~a -g ~a ~a" rate pitch gap w)))))))

(defun say-text (text &key (rate 170) (pitch 50) (gap 5))
  (if (null *tts-cmd*)
      (format t "~%No TTS program found. Install espeak-ng, espeak, or say.")
      (dolist (word (slip:split-string text #\Space))
        (speak-word word :rate rate :pitch pitch :gap gap))))

;; Demo
(say-text "hello rick this is common lisp
speaking" :rate 185 :pitch 55 :gap 4)
(say-text "now your lisp programs can talk!" :rate
	220 :pitch 55 :gap 1)

