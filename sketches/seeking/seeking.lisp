;; seeking.lisp - Turtle graphics point seeking.
;; Copyright (C) 2016 Rhea Myers
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

(in-package #:seeking)

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
;; From/for point.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
 (- (* (- (x p1) (x p0)) (- (y p2) (y p0)))
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

(defconstant +width+ 700)
(defconstant +height+ 700)

(defconstant +max-step+ 10.0)
(defconstant +max-turn+ 0.1)
(defconstant +close-enough+ 20.0)

(defconstant +pen-target-x+ 350)
(defconstant +pen-target-y+ 375)
(defconstant +pen-target-marker-size+ 4)

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

(defun pen-turn-angle (pen line amount)
  "The angle the pen should turn to face more towards the current waypoint of
   line, up to a maximum of amount"
  (let* ((waypoint-angle (angle-between-points (pen-position pen)
                                               (line-waypoint line)))
         (angle (shortest-angle-difference (pen-angle pen)
                                           waypoint-angle))
         (amount-angle (* (signum angle) amount)))
    (closest-to-zero angle amount-angle)))

(defun update-pen-angle (pen line)
  (setf (pen-angle pen)
        (+ (pen-angle pen) (pen-turn-angle pen line +max-turn+))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing (geometry)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; Drawing (cairo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun draw-angle-to-target (cr pen line)
  "Draw a line showing the angle between pen position and line's to."
  (cairo-set-source-rgb cr 0.0 1.0 0.0)
  (cairo-move-to cr (x (pen-position pen)) (y (pen-position pen)))
  (let ((angle (angle-between-points (pen-position pen) (line-waypoint line))))
    (cairo-line-to cr
                   (+ (x (pen-position pen))
                      (* +width+ (cos angle)))
                   (+ (y (pen-position pen))
                      (* +width+ (sin angle)))))
  (cairo-stroke cr))

(defun draw-pen-angle (cr pen)
  "Draw a line from the pen position at the pen's current angle."
  (cairo-set-source-rgb cr 1.0 0.0 1.0)
  (cairo-move-to cr (x (pen-position pen)) (y (pen-position pen)))
  (cairo-line-to cr (pen-x-at-distance pen 25)
                 (pen-y-at-distance pen 25))
  (cairo-stroke cr))

(defun draw-target-position (cr line)
  "Draw a mark at the line's to point."
  (cairo-set-source-rgb cr 1.0 0.0 0.0)
  (cairo-rectangle cr
                   (- (x (line-to line))
                      (/ +pen-target-marker-size+ 2.0))
                   (- (y (line-to line))
                      (/ +pen-target-marker-size+ 2.0))
                   +pen-target-marker-size+
                   +pen-target-marker-size+)
  (cairo-fill cr))

(defun draw-points (cr points)
  "When there are enough points to form a line, draw the lines."
  (when (>= (length points) 2)
    (cairo-set-source-rgb cr 0.0 0.0 0.0)
    (cairo-set-line-width cr 1.0)
    ;; Loop through the points, treating the first and the rest differently
    (loop
       for point across points
       for i = 0 then (+ i 1)
       when (= i 0) do (cairo-move-to cr (x point) (y point))
       else do (cairo-line-to cr (x point) (y point)))
    (cairo-stroke cr)))

(defclass <seeking> (gtk-drawing-area)
  ((pen
    :accessor seeking-pen
    :initarg :pen)
   (line
    :accessor seeking-line
    :initarg :line)
   (points
    :accessor seeking-points
    :initarg :points))
  (:metaclass gobject-class))

(defun pen-seek-line (seeking)
  (unless (should-finish-p (seeking-pen seeking) (seeking-line seeking))
    (update-pen-angle (seeking-pen seeking) (seeking-line seeking))
    (update-pen-position (seeking-pen seeking) +max-step+)
    (append-pen-point (seeking-pen seeking) (seeking-points seeking))))

(defun draw-state (cr seeking)
  "Draw the current drawing state."
  ;; Erase
  (cairo-set-source-rgb cr 1.0 1.0 1.0)
  (cairo-paint cr)
  ;; draw
  (draw-angle-to-target cr (seeking-pen seeking) (seeking-line seeking))
  (draw-pen-angle cr (seeking-pen seeking))
  (draw-target-position cr (seeking-line seeking))
  (draw-points cr (seeking-points seeking)))

(defmethod initialize-instance :after ((instance <seeking>) &rest args)
  (declare (ignore args))
  ;; Append the starting point
  (append-pen-point (seeking-pen instance) (seeking-points instance))
  ;; Update timer
  ;;TODO: stop when drawing finishes
  (g-timeout-add 25
                 (lambda ()
                   (pen-seek-line instance)
                   (gtk-widget-queue-draw instance)
                   +g-source-continue+))
  ;; Draw state
  (g-signal-connect instance "draw"
                    (lambda (widget cr)
                      (let ((window (gtk-widget-window widget))
                            (cr (pointer cr)))
                        ;; Positive y-axis
                        (cairo-translate cr 0 (gdk-window-get-height window))
                        (cairo-scale cr 1.0 -1.0)
                        (draw-state cr instance)
                        (cairo-destroy cr))
                        t)))

(defun run (&optional (u (random +width+)) (v (random +height+)))
  "Set up the state, create the GUI resources, and go."
  (let* ((line (make-instance '<line>
                              :from (make-instance '<point>
                                                   :x u
                                                   :y v)
                              :to (make-instance '<point>
                                                 :x +pen-target-x+
                                                 :y +pen-target-y+)))
         (pen (make-instance '<pen>
                             :position (copy-point (line-from line))))
         (points (make-vector 1)))
    (within-main-loop
      (let ((window (make-instance 'gtk-window
                                   :title "Seeking"
                                   :default-width +width+
                                   :default-height +height+))
            (seeking (make-instance '<seeking>
                                    :pen pen
                                    :line line
                                    :points points)))
        (g-signal-connect window "destroy"
                          (lambda (widget)
                            (declare (ignore widget))
                            (leave-gtk-main)))
        (g-signal-connect window "key_press_event"
                          (lambda (widget event)
                            (declare (ignore widget event))
                            (format t "state: ~a~%target: ~$,~$ angle to target: ~$~%"
                                    (seeking-pen seeking)
                                    (x (line-waypoint line))
                                    (y (line-waypoint line))
                                    (angle-between-points
                                     (pen-position pen)
                                     (line-waypoint line)))))
        (gtk-container-add window seeking)
        (gtk-widget-show-all window)))))
