;; -------------------------------------------------------------------------
;;  File:    timewarp.lsp
;;  Created: 2026-04-09
;;  Comment: DateTime library inspired by Python's datetime module.
;;           Core data structure is a struct (no CLOS).
;;           Covers: datetime creation, timedelta arithmetic, comparison,
;;           formatting, parsing, weekday, day-of-year, ISO week.
;; -------------------------------------------------------------------------

(defpackage :timewarp

  ( :use :common-lisp )

  ( :export

    ;; structs  (dtime = datetime)
    :dtime
    :make-dtime
    :dtime-year
    :dtime-month
    :dtime-day
    :dtime-hour
    :dtime-minute
    :dtime-second

    :delta
    :make-delta
    :delta-days
    :delta-seconds

    ;; construction / current time
    :now
    :today

    ;; calendar helpers
    :leap-year-p
    :days-in-month

    ;; internal conversion (exported for testing)
    :dt-to-ordinal
    :ordinal-to-date
    :dt-to-total-seconds
    :total-seconds-to-dt

    ;; arithmetic
    :dt-add
    :dt-subtract
    :delta-add
    :delta-negate
    :delta-total-seconds
    :normalize-delta

    ;; comparison
    :dt-equal
    :dt-before
    :dt-after
    :dt-before-or-equal
    :dt-after-or-equal

    ;; properties
    :dt-weekday        ; 0=Monday, 6=Sunday  (same as Python)
    :dt-isoweekday     ; 1=Monday, 7=Sunday
    :dt-day-of-year
    :dt-week-of-year

    ;; formatting
    :dt-isoformat
    :dt-format
    :dt-to-string

    ;; parsing
    :dt-parse

    ))

(in-package timewarp)

;; -------------------------------------------------------------------------
;;  Structs
;; -------------------------------------------------------------------------

(defstruct dtime ; dtime = datetime
  "A date and time value (proleptic Gregorian calendar).
   Fields match Python's datetime: year month day hour minute second."
  (year   1900 :type integer)
  (month     1 :type integer)
  (day       1 :type integer)
  (hour      0 :type integer)
  (minute    0 :type integer)
  (second    0 :type integer))

(defstruct delta
  "A duration, stored as days + seconds (seconds in [0, 86399]).
   Mirrors Python's timedelta internals (minus microseconds)."
  (days    0 :type integer)
  (seconds 0 :type integer))

;; -------------------------------------------------------------------------
;;  Calendar helpers
;; -------------------------------------------------------------------------

(defun leap-year-p (year)
  "Return T if year is a leap year (proleptic Gregorian calendar).
   Matches Python: calendar.isleap(year)."
  (or (and (zerop (mod year 4))
           (not (zerop (mod year 100))))
      (zerop (mod year 400))))

(defun days-in-month (month year)
  "Return the number of days in the given month of year.
   Matches Python: calendar.monthrange(year, month)[1]."
  (cond
    ((member month '(1 3 5 7 8 10 12)) 31)
    ((member month '(4 6 9 11))        30)
    ((= month 2) (if (leap-year-p year) 29 28))
    (t (error "days-in-month: invalid month ~a" month))))

;; Cumulative days before each month in a non-leap year (1-indexed, slot 0 unused).
(defvar *days-before-month*
  #(0 0 31 59 90 120 151 181 212 243 273 304 334))

(defun days-before-month (month year)
  "Number of days in year before the 1st of month."
  (let ((d (aref *days-before-month* month)))
    (if (and (> month 2) (leap-year-p year))
        (1+ d)
        d)))

(defun days-before-year (year)
  "Days from ordinal epoch (0001-01-01) to Jan 1 of year.
   Equivalent to Python's _days_before_year."
  (let ((y (1- year)))
    (+ (* y 365)
       (floor y 4)
       (- (floor y 100))
       (floor y 400))))

;; -------------------------------------------------------------------------
;;  Ordinal conversion  (proleptic Gregorian, day 1 = 0001-01-01)
;; -------------------------------------------------------------------------

(defun dt-to-ordinal (dt)
  "Convert dt to its proleptic Gregorian ordinal.
   Matches Python's datetime.toordinal()."
  (+ (days-before-year (dtime-year dt))
     (days-before-month (dtime-month dt) (dtime-year dt))
     (dtime-day dt)))

(defun ordinal-to-date (n)
  "Convert a proleptic Gregorian ordinal to (values year month day).
   Matches Python's datetime.fromordinal()."
  (let ((year (max 1 (floor n 366))))
    ;; Walk year forward until days-before-year(year+1) > n-1.
    (loop while (<= (days-before-year (1+ year)) (1- n)) do
      (incf year))
    (let ((day-of-year (- n (days-before-year year)))
          (month 1))
      (loop while (> day-of-year (days-in-month month year)) do
        (decf day-of-year (days-in-month month year))
        (incf month))
      (values year month day-of-year))))

;; -------------------------------------------------------------------------
;;  Total-seconds conversion  (seconds since 0001-01-01 00:00:00)
;; -------------------------------------------------------------------------

(defun dt-to-total-seconds (dt)
  "Convert a dt to total seconds since 0001-01-01 00:00:00."
  (+ (* (1- (dt-to-ordinal dt)) 86400)
     (* (dtime-hour dt) 3600)
     (* (dtime-minute dt) 60)
     (dtime-second dt)))

(defun total-seconds-to-dt (total-secs)
  "Convert total seconds (since 0001-01-01 00:00:00) back to a dt."
  (let* ((ordinal  (1+ (floor total-secs 86400)))
         (day-secs (mod total-secs 86400))
         (hour     (floor day-secs 3600))
         (rem      (mod day-secs 3600))
         (minute   (floor rem 60))
         (second   (mod rem 60)))
    (multiple-value-bind (year month day) (ordinal-to-date ordinal)
      (make-dtime :year year :month month :day day
                     :hour hour :minute minute :second second))))

;; -------------------------------------------------------------------------
;;  Current time / date   (uses CL's decode-universal-time → local time)
;; -------------------------------------------------------------------------

(defun now ()
  "Return current local date and time as a dt struct.
   Matches Python's datetime.now()."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time))
    (make-dtime :year year :month month :day day
                   :hour hour :minute minute :second second)))

(defun today ()
  "Return today's date with time set to midnight.
   Matches Python's date.today() (or dt.combine(date.today(), time()))."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore second minute hour))
    (make-dtime :year year :month month :day day)))

;; -------------------------------------------------------------------------
;;  Timedelta helpers
;; -------------------------------------------------------------------------

(defun normalize-delta (td)
  "Return a new delta with seconds in [0, 86399].
   Matches Python's timedelta normalization."
  (let* ((total (+ (* (delta-days td) 86400)
                   (delta-seconds td)))
         (days  (floor total 86400))
         (secs  (mod   total 86400)))
    (make-delta :days days :seconds secs)))

(defun delta-total-seconds (td)
  "Return total seconds represented by delta.
   Matches Python's timedelta.total_seconds()."
  (+ (* (delta-days td) 86400)
     (delta-seconds td)))

(defun delta-add (a b)
  "Add two deltas. Returns a new normalized delta.
   Matches Python's timedelta + timedelta."
  (normalize-delta
   (make-delta :days    (+ (delta-days    a) (delta-days    b))
                   :seconds (+ (delta-seconds a) (delta-seconds b)))))

(defun delta-negate (td)
  "Negate a delta.
   Matches Python's -timedelta."
  (normalize-delta
   (make-delta :days    (- (delta-days    td))
                   :seconds (- (delta-seconds td)))))

;; -------------------------------------------------------------------------
;;  Datetime arithmetic
;; -------------------------------------------------------------------------

(defun dt-add (dt td)
  "Add a delta to a dt. Returns a new dt.
   Matches Python's datetime + timedelta."
  (total-seconds-to-dt
   (+ (dt-to-total-seconds dt)
      (delta-total-seconds td))))

(defun dt-subtract (a b)
  "Subtract two dts → delta, or dt − delta → dt.
   Matches Python's datetime − datetime and datetime − timedelta."
  (cond
    ((and (dtime-p a) (dtime-p b))
     (let* ((diff  (- (dt-to-total-seconds a)
                      (dt-to-total-seconds b)))
            (days  (floor diff 86400))
            (secs  (mod   diff 86400)))
       (make-delta :days days :seconds secs)))
    ((and (dtime-p a) (delta-p b))
     (dt-add a (delta-negate b)))
    (t (error "dt-subtract: expected (dt dt) or (dt delta)"))))

;; -------------------------------------------------------------------------
;;  Comparison
;; -------------------------------------------------------------------------

(defun dt-compare (a b)
  "Return -1 if a < b, 0 if a = b, 1 if a > b."
  (let ((sa (dt-to-total-seconds a))
        (sb (dt-to-total-seconds b)))
    (cond ((< sa sb) -1)
          ((> sa sb)  1)
          (t           0))))

(defun dt-equal (a b)
  "Return T if a == b. Matches Python's datetime == datetime."
  (= 0 (dt-compare a b)))

(defun dt-before (a b)
  "Return T if a < b. Matches Python's datetime < datetime."
  (= -1 (dt-compare a b)))

(defun dt-after (a b)
  "Return T if a > b. Matches Python's datetime > datetime."
  (= 1 (dt-compare a b)))

(defun dt-before-or-equal (a b)
  "Return T if a <= b."
  (<= (dt-compare a b) 0))

(defun dt-after-or-equal (a b)
  "Return T if a >= b."
  (>= (dt-compare a b) 0))

;; -------------------------------------------------------------------------
;;  Calendar properties
;; -------------------------------------------------------------------------

(defun dt-weekday (dt)
  "Return day of week: 0=Monday … 6=Sunday.
   Matches Python's datetime.weekday().
   Note: 0001-01-01 was a Monday (ordinal 1 → weekday 0)."
  (mod (1- (dt-to-ordinal dt)) 7))

(defun dt-isoweekday (dt)
  "Return day of week: 1=Monday … 7=Sunday.
   Matches Python's datetime.isoweekday()."
  (1+ (dt-weekday dt)))

(defun dt-day-of-year (dt)
  "Return the day of the year (1–366).
   Matches Python's datetime.timetuple().tm_yday."
  (+ (days-before-month (dtime-month dt) (dtime-year dt))
     (dtime-day dt)))

(defun dt-week-of-year (dt)
  "Return the ISO 8601 week number (1–53).
   Matches Python's datetime.isocalendar().week."
  (let* ((ordinal  (dt-to-ordinal dt))
         (weekday  (dt-weekday dt))          ; 0=Mon
         ;; Ordinal of the Thursday in this ISO week
         (thu-ord  (+ ordinal (- 3 weekday)))
         ;; What year does that Thursday belong to?
         (thu-year (dtime-year (total-seconds-to-dt
                                   (* (1- thu-ord) 86400))))
         ;; Ordinal of Jan 4 of that year (always in week 1)
         (jan4-ord (dt-to-ordinal
                    (make-dtime :year thu-year :month 1 :day 4)))
         ;; Monday that starts week 1
         (w1-start (- jan4-ord (mod (1- jan4-ord) 7))))
    (1+ (floor (- ordinal w1-start) 7))))

;; -------------------------------------------------------------------------
;;  Formatting
;; -------------------------------------------------------------------------

(defvar *weekday-names*
  #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(defvar *weekday-abbrevs*
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defvar *month-names*
  #("" "January" "February" "March" "April" "May" "June"
      "July" "August" "September" "October" "November" "December"))

(defvar *month-abbrevs*
  #("" "Jan" "Feb" "Mar" "Apr" "May" "Jun"
      "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun replace-all (str old new)
  "Replace every occurrence of old in str with new."
  (let ((result str)
        (len-old (length old)))
    (loop
      (let ((pos (search old result)))
        (unless pos (return result))
        (setf result (concatenate 'string
                                  (subseq result 0 pos)
                                  new
                                  (subseq result (+ pos len-old))))))))

(defun dt-isoformat (dt &optional (sep #\T))
  "Return ISO 8601 string e.g. '2026-04-09T14:30:00'.
   Matches Python's datetime.isoformat(sep='T')."
  (format nil "~4,'0d-~2,'0d-~2,'0d~c~2,'0d:~2,'0d:~2,'0d"
          (dtime-year dt)  (dtime-month dt)  (dtime-day dt)
          sep
          (dtime-hour dt)  (dtime-minute dt) (dtime-second dt)))

(defun dt-to-string (dt)
  "Return a human-readable string for dt.
   Matches Python's str(datetime)."
  (dt-isoformat dt #\Space))

(defun dt-format (dt fmt)
  "Format a dt using a strftime-style format string.
   Supported directives: %Y %m %d %H %M %S %A %a %B %b %j %w %W.
   Matches Python's datetime.strftime(fmt)."
  (let ((result fmt))
    (setf result (replace-all result "%Y"
                   (format nil "~4,'0d" (dtime-year dt))))
    (setf result (replace-all result "%m"
                   (format nil "~2,'0d" (dtime-month dt))))
    (setf result (replace-all result "%d"
                   (format nil "~2,'0d" (dtime-day dt))))
    (setf result (replace-all result "%H"
                   (format nil "~2,'0d" (dtime-hour dt))))
    (setf result (replace-all result "%M"
                   (format nil "~2,'0d" (dtime-minute dt))))
    (setf result (replace-all result "%S"
                   (format nil "~2,'0d" (dtime-second dt))))
    (setf result (replace-all result "%A"
                   (aref *weekday-names* (dt-weekday dt))))
    (setf result (replace-all result "%a"
                   (aref *weekday-abbrevs* (dt-weekday dt))))
    (setf result (replace-all result "%B"
                   (aref *month-names* (dtime-month dt))))
    (setf result (replace-all result "%b"
                   (aref *month-abbrevs* (dtime-month dt))))
    (setf result (replace-all result "%j"
                   (format nil "~3,'0d" (dt-day-of-year dt))))
    ;; %w: Sunday=0 … Saturday=6  (Python convention)
    (setf result (replace-all result "%w"
                   (format nil "~a" (mod (1+ (dt-weekday dt)) 7))))
    (setf result (replace-all result "%W"
                   (format nil "~2,'0d" (dt-week-of-year dt))))
    result))

;; -------------------------------------------------------------------------
;;  Parsing
;; -------------------------------------------------------------------------

(defun dt-parse (str &optional (fmt "%Y-%m-%d %H:%M:%S"))
  "Parse a dt string.
   Supported directives: %Y (4 chars) %m %d %H %M %S (2 chars each).
   Matches Python's datetime.strptime(str, fmt)."
  (let ((year 1900) (month 1) (day 1)
        (hour 0)    (minute 0) (second 0)
        (si 0) (fi 0)
        (flen (length fmt))
        (slen (length str)))
    (loop while (and (< fi flen) (< si slen)) do
      (let ((fc (char fmt fi)))
        (cond
          ((char= fc #\%)
           (incf fi)
           (when (< fi flen)
             (let* ((directive (char fmt fi))
                    (width  (if (char= directive #\Y) 4 2))
                    (end-si (min (+ si width) slen))
                    (token  (subseq str si end-si)))
               (incf fi)
               (setf si end-si)
               (let ((val (or (ignore-errors (parse-integer token)) 0)))
                 (cond
                   ((char= directive #\Y) (setf year   val))
                   ((char= directive #\m) (setf month  val))
                   ((char= directive #\d) (setf day    val))
                   ((char= directive #\H) (setf hour   val))
                   ((char= directive #\M) (setf minute val))
                   ((char= directive #\S) (setf second val)))))))
          (t
           ;; literal character — advance both pointers
           (incf fi)
           (incf si)))))
    (make-dtime :year year :month month :day day
                   :hour hour :minute minute :second second)))
