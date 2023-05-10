;; neptune.lisp - Turtle graphics point seeking.
;; Copyright (C) 2016 Rhea Myers
;; Copyright (C) 2023 Myers Studio, Ltd.
;;
;; This file is part of draw-something.
;;
;; draw-something is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; draw-something is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(ql:quickload '(:draw-something :sdl2 :sdl2kit :sketch))

(defpackage :neptune
  (:use :cl :sketch :draw-something))

(in-package :neptune)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +width+ 700)
(defconstant +height+ 700)

(defconstant +max-step+ 10.0)
(defconstant +max-turn+ 0.1)
(defconstant +close-enough+ 20.0)

(defconstant +pen-target-x+ 350)
(defconstant +pen-target-y+ 375)
(defconstant +pen-target-marker-size+ 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From utilities.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-vector (initial-size)
  "Make a stretchy vector."
  (make-array initial-size
              :adjustable t
              :fill-pointer 0))

(defun closest-to-zero (a b)
  "Return value of a or b that is closest to zero."
  (if (< (abs a) (abs b))
      a
      b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <point> ()
  ((x
    :accessor x
    :initarg :x
    :initform 0.0)
   (y
    :accessor y
    :initarg :y
    :initform 0.0)))

(defun copy-point (source)
  "Make a point with the same x and y as the source."
  (make-instance '<point> :x (x source) :y (y source)))

(defconstant +radian+ (* pi 2.0))

(defun norm-radian (value)
  "Clamp a value to 0..2pi ."
  (mod value +radian+))

(defun distance-co-ordinates (x1 y1 x2 y2)
  "The distance between two points."
  (sqrt (+ (expt (- x2 x1) 2)
           (expt (- y2 y1) 2))))

(defun distance-points (p1 p2)
  (distance-co-ordinates (x p1) (y p1) (x p2) (y p2)))

(defun angle-between-co-ordinates (x0 y0 x1 y1)
  (let ((angle (atan (- y1 y0) (- x1 x0))))
    (if (< angle 0)
        (+ angle +radian+)
        angle)))

(defun angle-between-points (p1 p2)
  (angle-between-co-ordinates (x p1) (y p1) (x p2) (y p2)))

(defun point-line-side (p0 p2 p1)
  "Find out which side of an infinite line through p1 and p2 that p0 lies on.
   < 0 = left, > 0 = right, == 0 = exactly on."
  (- (* (-  (x p1) (x p0)) (- (y p2) (y p0)))
     (* (- (x p2) (x p0)) (- (y p1) (y p0)))))

(defun turn-positive-p (a1 a2)
  "Is the shortest turn direction positive (t) or negative (nil)?"
  (> (mod (+ (- a1 a2) +radian+) +radian+) pi))

(defun shortest-angle-difference (a1 a2)
  "Find the shortest positive or negative distance between two angles."
  ;; This was slowly assembled from various online sources, none of which worked
  ;; Normalize angles to 0..2pi
  (let ((from (mod a1 +radian+))
        (to (mod a2 +radian+)))
    ;; If from and to are equal (0 = 2pi radians) the angle is zero
    (if (or (= from to)
            (= (+ from to) +radian+))
        0.0d0
        (let ((angle (- to from)))
          (if (> (abs angle) pi)
              ;; Please simplify me
              (* (- (signum angle)) (- +radian+ (abs angle)))
              angle)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <pen> ()
  ((position
    :accessor pen-position
    :initarg :position
    :initform (make-instance '<point>))
   (angle
    :accessor pen-angle
    :initarg :angle
    :initform 0.0)))

(defmethod print-object ((obj <pen>) stream)
  (print-unreadable-object (obj stream)
    (format stream "x: ~g y: ~g angle: ~g"
            (x (pen-position obj)) (y (pen-position obj)) (pen-angle obj))))

(defun pen-x-at-distance (pen distance)
  "The x value if the pen goes forward distance."
  (+ (x (pen-position pen))
     (* distance (cos (pen-angle pen)))))

(defun pen-y-at-distance (pen distance)
  "The y value if the pen goes forward distance."
  (+ (y (pen-position pen))
     (* distance (sin (pen-angle pen)))))

(defun pen-at-distance (pen distance)
  (make-instance '<point>
                 :x (pen-x-at-distance pen distance)
                 :y (pen-y-at-distance pen distance)))

(defun update-pen-position (pen distance)
  (setf (x (pen-position pen)) (pen-x-at-distance pen distance))
  (setf (y (pen-position pen)) (pen-y-at-distance pen distance)))

(defun pen-turn-angle (pen next-point amount)
  "The angle the pen should turn to face more towards the current waypoint of
   line, up to a maximum of amount"
  (let* ((waypoint-angle (angle-between-points (pen-position pen)
                                               next-point))
         (angle (shortest-angle-difference (pen-angle pen)
                                           waypoint-angle))
         (amount-angle (* (signum angle) amount)))
    (closest-to-zero angle amount-angle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line (drawing)
;; Needs a better name?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <line> ()
  ((from
    :accessor line-from
    :initarg :from
    :documentation "The start of the line.")
   (to
    :accessor line-to
    :initarg :to
    :documentation "The end of the line.")
   (waypoint
    :accessor line-waypoint
    :documentation "The current waypoint target for the pen.")
   (length
    :accessor line-length
    :documentation "The line's length.")
   (length-scale
    :accessor line-length-scale
    :documentation "1.0 / line-length.")))

(defmethod initialize-instance :after ((instance <line>) &key)
  "Set the calculated properties of the line"
  (setf (line-waypoint instance) (line-to instance))
  (setf (line-length instance)
        (distance-points (line-from instance) (line-to instance)))
  (setf (line-length-scale instance) (/ 1.0 (line-length instance))))

(defun line-completion-scale (line pen)
  "A value that goes from around 0.0 to 1.0 as pen approaches line's to"
  (max 0.0 (min 1.0 (* (- (distance-points (pen-position pen) (line-to line))
                          (line-length line))
                       (line-length-scale line)))))

(defun line-completion-scale-complement (line pen)
  "A value that goes from around 1.0 to 0.0 as pen approaches line's to"
  (- 1.0 (line-completion-scale line pen)))

(defun line-completion-scale-amount (line pen amount base)
  "Scale amount by 0.0ish..1.0 as pen approaches lines' to and add to base."
  (+ base (* (line-completion-scale line pen)
             amount)))

(defun line-completion-scale-complement-amount (line pen amount base)
  "Scale amount by 1.0ish..0.0 as pen approaches lines' to and add to base."
  (+ base (* (line-completion-scale-complement line pen)
             amount)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing (geometry)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun update-pen-angle (pen line)
  (setf (pen-angle pen)
        (+ (pen-angle pen)
           (pen-turn-angle pen (line-waypoint line)
                           +max-turn+))))

(defun append-pen-point (pen points)
  "Push the current pen position onto the points vector."
  (vector-push-extend (copy-point (pen-position pen))
                      points))

(defun pen-close-enough-p (pen line)
  (<= (distance-points (pen-position pen) (line-to line))
      +close-enough+))

(defun pen-over-the-edge-p (pen)
  (or (<= (x (pen-position pen)) 0) (>= (x (pen-position pen)) +width+)
      (<= (y (pen-position pen)) 0) (>= (y (pen-position pen)) +height+)))

(defun should-finish-p (pen line)
  (or (pen-close-enough-p pen line)
      (pen-over-the-edge-p pen)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun draw-angle-to-target (pen line)
  "Draw a line showing the angle between pen position and line's to."
  (with-pen (make-pen :stroke +magenta+ :weight 3)
    (let ((angle (angle-between-points (pen-position pen)
                                       (line-waypoint line))))
      (line (x (pen-position pen))
            (y (pen-position pen))
            (+ (x (pen-position pen))
               (* 25 (cos angle)))
            (+ (y (pen-position pen))
               (* 25 (sin angle)))))))

(defun draw-pen-angle (pen)
  "Draw a line from the pen position at the pen's current angle."
  (with-pen (make-pen :stroke +cyan+)
    (line (x (pen-position pen))
          (y (pen-position pen))
          (pen-x-at-distance pen 25)
          (pen-y-at-distance pen 25))))

(defun draw-target-position (line)
  "Draw a mark at the line's to point."
  (with-pen (make-pen :fill +yellow+)
    (rect (- (x (line-to line))
             (/ +pen-target-marker-size+ 2.0))
          (- (y (line-to line))
             (/ +pen-target-marker-size+ 2.0))
          +pen-target-marker-size+
          +pen-target-marker-size+)))


(defun draw-points (points)
  "When there are enough points to form a line, draw the lines."
  (when (>= (length points) 2)
    (let ((prev-x (x (aref points 0)))
          (prev-y (y (aref points 0))))
      (with-pen (make-pen :stroke +black+)
        ;; We don't use sketch:polyline as that produces artifacts sometimes?
        (loop for i from 1 below (length points)
              do (let ((curr-x (x (aref points i)))
                       (curr-y (y (aref points i))))
                   (line prev-x prev-y curr-x curr-y)
                   (setf prev-x curr-x)
                   (setf prev-y curr-y)))))))

(defclass <seeking> ()
  ((pen
    :accessor seeking-pen
    :initarg :pen)
   (line
    :accessor seeking-line
    :initarg :line)
   (points
    :accessor seeking-points
    :initarg :points)))

(defun pen-seek-line (seeking)
  (update-pen-angle (seeking-pen seeking) (seeking-line seeking))
  (update-pen-position (seeking-pen seeking) +max-step+)
  (append-pen-point (seeking-pen seeking) (seeking-points seeking)))

(defun draw-state (seeking)
  "Draw the current drawing state."
  (draw-angle-to-target (seeking-pen seeking) (seeking-line seeking))
  (draw-pen-angle (seeking-pen seeking))
  (draw-target-position (seeking-line seeking))
  (draw-points (seeking-points seeking)))

(defmethod initialize-instance :after ((instance <seeking>) &rest args)
  (declare (ignore args))
  ;; Append the starting point
  (append-pen-point (seeking-pen instance) (seeking-points instance)))

(defvar *seeking* nil)

(defun init-seeking ()
  "Set up the state, create the GUI resources, and go."
  (let* ((line (make-instance '<line>
                              :from (make-instance '<point>
                                                   :x (random +width+)
                                                   :y (random +height+))
                              :to (make-instance '<point>
                                                 :x +pen-target-x+
                                                 :y +pen-target-y+)))
         (pen (make-instance '<pen>
                             :position (copy-point (line-from line))))
         (points (make-vector 1)))
    (setf *seeking* (make-instance '<seeking>
                                   :pen pen
                                   :line line
                                   :points points))))

(init-seeking)

(defvar *frame* 0)

(defsketch neptune ((title "neptune")
                    (width +width+)
                    (height +height+)
                    (y-axis :up))
  (incf *frame*)
  (draw-state *seeking*)
  (unless (should-finish-p (seeking-pen *seeking*) (seeking-line *seeking*))
    ;; Slow down from 60fps
    (when (= (mod *frame* 10) 0)
      (pen-seek-line *seeking*))))

(defmethod kit.sdl2:mousebutton-event ((window neptune) state ts b x y)
  (when (eq state :mousebuttondown)
    (init-seeking)))

#-sbcl (make-instance 'neptune)
#+sbcl (sdl2:make-this-thread-main #'(lambda ()
                                       (make-instance 'neptune)))
