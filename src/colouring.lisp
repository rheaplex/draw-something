;; colouring.lisp - Colour chord generation and application.
;; Copyright Rhea Myers 2007
;; Copyright Myers Studio, Ltd. 2023
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :draw-something)

;; Types and default values for keywords???
;; Min & max bounds for saturation & lightness?value

;; Gamut for inkjet?
;; Tween colours, esp. complements?

;; For colour: plane (fore/mid/back/etc),
;; object (ground/table/tree),
;; group (edges/top/legs/trunk/branches),
;; element (branch1..20/leg1..4/leaf1..200)

;; So a red pot in the blue distance will need to compose the plane,
;; object and element colour generators

(defvar acceptible-value-deviation 0.1)

(defmethod combined-value ((col <colour>))
  "Get the value 0.0..1.0 by combining lightness and saturation."
  (/ (+ (lightness col)
    (- 1.0 (saturation col)))
     2.0))

;; Change to (write) methods

(defmethod dump ((col <colour>))
  (format t
      "Colour hue: ~a saturation: ~a lightness ~a"
      (hue col)
      (saturation col)
      (lightness col)))

(defmethod dump (cols)
  (dolist (col cols)
    (dump col)
    (format t "~%")))

(defmethod fixed-generator (val)
  "Make a function to always return the same value."
  (lambda ()
    val))

(defmethod random-generator ((from float) (to float))
  "make a function to generate random numbers between from and to."
  (let ((base (min from to))
    (range (abs (- to from))))
    (lambda ()
      (+ base
     (random-number range)))))

(defmethod step-generator ((from float) (to float) (steps integer))
  "Make a function to return a value that steps from from to to in steps steps."
  (let ((step (/ (abs (- to from))
         steps))
    (i 0))
    (lambda ()
      (let ((current-value (+ (* i step)
                  from)))
    (incf i)
    current-value))))

(defmethod colour-generator (hue-fun saturation-fun lightness-fun)
  "Make a function to make a new instance of colour."
  (lambda ()
    (make-instance '<colour>
           :hue (funcall hue-fun)
           :saturation (funcall saturation-fun)
           :lightness (funcall lightness-fun))))

(defmethod random-colour-generator (&key (min-hue 0.0) (max-hue 1.0)
                    (min-saturation 0.0) (max-saturation 1.0)
                    (min-lightness 0.0) (max-lightness 1.0))
  "Make a function to make a random colour."
  (colour-generator (random-generator min-hue max-hue)
            (random-generator min-saturation max-saturation)
            (random-generator min-lightness max-lightness)))

(defmethod n-random-colours ((n integer) &key (min-hue 0.0)
                                           (max-hue 359.0)
                                           (min-saturation 0.0)
                                           (max-saturation 1.0)
                                           (min-lightness 0.0)
                                           (max-lightness 1.0))
  "Make a list of n random colours."
  (let ((generate (random-colour-generator :min-hue min-hue
                       :max-hue max-hue
                       :min-saturation min-saturation
                       :max-saturation max-saturation
                       :min-lightness min-lightness
                       :max-lightness max-lightness)))
    (loop repeat n
          collect (funcall generate))))

(defmethod hue-step-colour-generator ((n integer)
                      &key (min-hue 0.0) (max-hue 1.0)
                      (min-saturation 0.0) (max-saturation 1.0)
                      (min-lightness 0.0) (max-lightness 1.0))
  "Make a function to make colours with stepped hue, random sat / lightness."
  (colour-generator (step-generator min-hue max-hue n)
            (random-generator min-saturation max-saturation)
            (random-generator min-lightness max-lightness)))

(defmethod n-hue-step-colours ((n integer) &key (min-hue 0.0) (max-hue 1.0)
                   (min-saturation 0.0) (max-saturation 1.0)
                   (min-lightness 0.0) (max-lightness 1.0))
  "Make a list of n colours equally space around hue, random sat / lightness."
  (let ((generate
     (hue-step-colour-generator n
                    :min-hue min-hue
                    :max-hue max-hue
                    :min-saturation min-saturation
                    :max-saturation max-saturation
                    :min-lightness min-lightness
                    :max-lightness max-lightness))
    (i 0))
    (loop repeat n
       collect (funcall generate)
       do (incf i))))

(defmethod n-hue-step-colours-start-offset
    ((n integer) &key (min-hue 0.0) (max-hue 1.0)
                   (min-saturation 0.0) (max-saturation 1.0)
                   (min-lightness 0.0) (max-lightness 1.0))
  "Make a list of n colours equally space around hue, random sat / lightness."
  (let ((offset (random (- 1.0
               (min (- 1.0 ;; Smaller distance from range to limit
                   max-hue)
                min-hue)))))
    (n-hue-step-colours n :min-hue (+ min-hue offset)
            :max-hue (+ max-hue offset)
            :min-saturation min-saturation
            :max-saturation max-saturation
            :min-lightness min-lightness
            :max-lightness max-lightness)))

;; Random in bounds
;; Like step, but with each colour offset randomly from step with min gap

;; Additive series, use complements etc. but avoid already used colours

(defmethod list-property-range (items key increasing)
  (let ((lowest (funcall key (car items)))
    (highest (funcall key (car items)))
    (count 0))
    (dolist (item items)
      (let ((val (funcall key item)))
    (if (> val highest)
        (setf highest val))
    (if (< val lowest)
        (setf lowest val)))
      (incf count))
    (unless increasing
      (let ((temp highest))
        (setf highest lowest)
        (setf lowest temp)))
    (values lowest highest (- highest lowest) count)))

(defmethod colours-lightness-range (colours increasing)
  "Get the range of lightness for the colours in the list."
  (list-property-range colours #'lightness increasing))

(defmethod colours-value-range (colours increasing)
  "Get the range of value for the colours in the list."
  (list-property-range colours #'combined-value increasing))

(defmethod converge-value ((col <colour>) (value-target real))
  "Get the colour as close as possible to the target value."
  nil)
#|  (let ((delta (- value-target
          (combined-value col))))
    (when (> delta acceptible-value-deviation)
      ;; Try to change the lightness, but not too much
      ;; Avoid a change of > .2 if possible
      ;; If we'd have to change it too much,
      ;;change the saturation as well
      ;; If both > 0.2, change lightness most
      ;; Or something
      (when (> (delta (lightness col))
           (setf (lightness col
                 )
             ) |#

(defmethod dolist-index (items fun)
  "Iterate through items calling (fun item index-of-item) ."
  (let ((count 0))
    (dolist (item items)
      (funcall fun item count)
      (incf count))))

(defmethod make-linearise-property (colours key increasing)
  "Make a function which when called with a colour and index will set the colour's lightness to its linear position in the range from darkest to lightest."
  (multiple-value-bind
    (from to range steps) (list-property-range colours key increasing)
    (declare (ignore to))
    (let ((step (/ range steps)))
      (lambda (col i)
    (setf (slot-value col key)
          (+ from
             (* i step)))))))

(defmethod linearise-lightness (colours increasing)
  "Set each colour's lightness to its linear position in the range from darkest to lightest."
  (dolist-index colours (make-linearise-property colours 'lightness increasing)))

(defmethod linearise-saturation (colours increasing)
  "Set each colour's saturation to its linear position in the range."
  (dolist-index colours (make-linearise-property colours 'saturation increasing)))

(defmethod make-linearise-value (colours increasing)
  "Make a function which when called with a colour and index will set the colour's lightness to its linear position in the range from least to most saturated-and-bright."
  (multiple-value-bind (from to range count) (colours-value-range colours increasing)
    (declare (ignore range))
    (let ((step (/ (- to from) ;; Get the range in value-range? Yes.
           count)))
      (lambda (col i)
    (converge-value col (* i step))))))

(defmethod linearise-value (colours increasing)
  "Make a function which when called with a colour and index will set the colour's lightness to its linear position in the range from least to most saturated-and-bright."
  (dolist-index colours (make-linearise-value colours increasing)))

(defmethod sort-by-lightness (colours increasing)
  "Sort the colours from darkest to lightest."
  (stable-sort colours (if increasing #'< #'>) :key #'lightness))

(defmethod sort-by-value-increasing (colours increasing)
  "Sort the colours into ascending value (lightness + saturation)."
  (stable-sort colours (if increasing #'< #'>) :key #'combined-value))

(defmethod difference ((a <colour>) (b <colour>))
  "Decide how different two colours are. 0 .. approx 2.1"
  (+ (* (abs (- (hue a)
                (hue b)))
        0.00028)
     (abs (- (saturation a)
             (saturation b)))
     (abs (- (lightness a)
             (lightness b)))))

(defmethod value-difference ((a <colour>) (b <colour>))
  "The difference in value between the two colours."
  (/ (+ (saturation a)
    (saturation b)
    (lightness a)
    (lightness b))
     4.0))

(defmethod different-value ((target float) (separation float) (wrap float))
  "Make a value 0 .. wrap where value is < or > target +/- separation."
  (cond
    ;; Range is from 0 .. target - separation
    ((> (+ target separation)
    wrap)
     (random (- target separation)))
    ;; Range is from target + separation .. wrap
    ((< (- target separation)
    0.0)
     (+ target
    separation
    (random (- wrap
           target
           separation))))
    ;; Range is either of the above
    (t
     (mod (+ (+ target
         separation)
          (random (- wrap
             (* 2.0
                separation))))
      wrap))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colour chord (palette) generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-buckets (bucket-count per-bucket)
  (make-array (list bucket-count per-bucket)))

(defun assign-colours-to-buckets (colours bucket-count per-bucket)
  (let ((buckets (make-buckets bucket-count per-bucket)))
    (dotimes (i bucket-count)
      (dotimes (j per-bucket)
        (setf (aref buckets  i j)
              (nth (+ j (* i per-bucket)) colours))))
    (format t "~a~%~a~%" colours buckets)
    buckets))

(defmethod create-colours (bucket-count per-bucket)
  "Sort by value, but adjust only lightness."
  (let ((increasing (choose-one-of '(t nil)))
        (colours (n-random-colours (* bucket-count
                                      per-bucket))))
    (linearise-lightness colours increasing)
    ;;(dump colours)
    (assign-colours-to-buckets colours
                               bucket-count
                               per-bucket)))

(defun choose-colour-for (colours bucket-index per-bucket)
  (aref colours
        bucket-index
        (random-number per-bucket)))
