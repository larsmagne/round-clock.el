;;; round-clock.el --- make a clock -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'svg)
(require 'json)

(defun make-clock (&optional testing)
  (let* ((dia 720)
	 (rad (/ dia 2))
	 (svg (svg-create dia dia))
	 ;; The next minute -- we're generating in advance.
	 (time (decode-time (+ (time-convert (current-time) 'integer) 60)))
	 (fore "white"))
    (svg-gradient svg "gradient" 'nope '((0 . "#000080") (100 . "black")))
    (svg-rectangle svg 0 0 dia dia :fill "black")
    (svg-circle svg rad rad rad  :gradient "gradient")
    ;; Draw the minute markers.
    (dotimes (i 60)
      (svg-line svg 0 0 0 rad
		:stroke-width "5px"
		:stroke "grey"
		:transform (format "translate(%d,%d) rotate(%d)"
				   rad rad
				   (* i (/ 360 60)))))
    (svg-circle svg rad rad (- rad 10) :gradient "gradient")
    ;; Draw the hour markers.
    (dotimes (i 60)
      (when (zerop (% i 5))
	(svg-line svg 0 0 0 rad
		  :stroke-width "10px"
		  :stroke fore
		  :transform (format "translate(%d,%d) rotate(%d)"
				     rad rad
				     (* i (/ 360 60))))))
    (svg-circle svg rad rad (- rad 20) :gradient "gradient")
    ;; Draw the minute hand.
    (svg-line svg 0 0 0 (+ (- rad) 60)
	      :stroke-width "20px"
	      :stroke fore
	      :stroke-linecap "round"
	      :transform (format "translate(%d,%d) rotate(%d)"
				 rad rad
				 (* (decoded-time-minute time)
				    (/ 360 60))))
    ;; Draw the hour hand.
    (svg-line svg 0 0 0 (+ (- rad) 150)
	      :stroke-width "20px"
	      :stroke fore
	      :stroke-linecap "round"
	      :transform (format "translate(%d,%d) rotate(%d)"
				 rad rad
				 (* (+ (decoded-time-hour time)
				       (/ (decoded-time-minute time) 60.0))
				    (/ 360 12))))
    ;; Draw a square behind the date.
    (when nil
      (svg-rectangle svg (- dia 160) (- rad 40) 110 80
		     :stroke-width "0px"
		     :stroke fore
		     :fill "grey"))
    ;; Display the date.
    (svg-text svg (format "%s %d"
			  (elt
			   '("dim" "lun" "mar" "mer" "jeu" "ven" "sam")
			   (decoded-time-weekday time))
			  (decoded-time-day time))
	      :x (- dia 50)
	      :y (+ rad 20)
	      :font-size 60
	      :stroke "black"
	      :stroke-width "0px"
	      :font-weight "bold"
	      :text-anchor "end"
	      :fill "grey"
	      :font-family "futura")
    ;; Display the temperature.
    (svg-text svg (round-clock--temp-from-file)
	      :x 50
	      :y (+ rad 20)
	      :font-size 60
	      :stroke "grey"
	      :stroke-width "0px"
	      :font-weight "bold"
	      :fill "grey"
	      :font-family "futura")
    ;; Output to a file.
    (with-temp-buffer
      (svg-print svg)
      (call-process-region (point-min) (point-max) "rsvg-convert"
			   t (current-buffer))
      (write-region (point-min) (point-max) "/mnt/tmpfs/clock.png"
		    nil 'silent))
    (when testing
      (find-file "/stage/tmp/clock.png"))))

(defun round-clock--temp-from-file ()
  (if (not (file-exists-p "/mnt/tmpfs/data.json"))
      ""
    (with-temp-buffer
      (insert-file-contents "/mnt/tmpfs/data.json")
      (let ((data (json-read)))
	(format "%.1f°" (string-to-number (cdr (assq 'temp data))))))))

