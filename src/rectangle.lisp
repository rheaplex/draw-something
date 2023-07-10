;; rectangle.lisp - A 2D rectangle.
;; Copyright (C) 2006, 2016, 2021 Rhea Myers.
;; Copyright (C) 2023 Myers Studio Ltd.
;;
;; This file is part of draw-something.
;;
;; draw-something is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; draw-something is distributed in the hopesvg that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :draw-something)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rectangle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <rectangle> (<geometry>)
  ((x :accessor x
      :type float
      :initform  0.0
      :initarg :x
      :documentation "The lower left x co-ordinate of the rectangle.")
   (y :accessor y
      :type float
      :initform 0.0
      :initarg :y
      :documentation "The lower left y co-ordinate of the rectangle.")
   (width :accessor width
      :type float
      :initform  0.0
      :initarg :width
      :documentation "The width of the rectangle.")
   (height :accessor height
      :type float
      :initform 0.0
      :initarg :height
      :documentation "The height of the rectangle."))
  (:documentation "A simple rectangle"))

(defmethod print-object ((object <rectangle>) stream)
  "Make a human readable string describing the rectangle."
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(X: ~,2f, Y: ~,2f WIDTH: ~,2f HEIGHT: ~,2f)"
          (x object)
          (y object)
          (width object)
          (height object))))

(defun make-rectangle (&key x y width height)
  "Constructor function."
  (make-instance '<rectangle> :x x :y y :width width :height height))

(defun copy-rectangle (r)
  "Make a copy of the rectangle."
  (make-rectangle :x (x r) :y (y r) :width (width r) :height (height r)))

(defun random-point-in-rectangle (bounds-rect)
  "Make a point placed randomly within the given bounding rectangle."
  (make-point :x (+ (x bounds-rect) (random-number (width bounds-rect)))
              :y (+ (y bounds-rect) (random-number (height bounds-rect)))))

(defun random-points-in-rectangle (bounds-rect count)
  "Create count points placed randomly within bounds-rect."
  (map-into (make-array count)
        (lambda ()(random-point-in-rectangle bounds-rect))))

(defun random-rectangle-in-rectangle (bounds-rect)
  "Make a random rectangle of at least size 1x1 in another rectangle."
  (let* ((new-width (random-number (width bounds-rect)))
     (new-height (random-number (height bounds-rect)))
     (new-x (+ (x bounds-rect)
           (random-number (- (width bounds-rect) new-width))))
     (new-y (+ (y bounds-rect)
           (random-number (- (height bounds-rect) new-height)))))
    (make-rectangle :x new-x
                    :y new-y
                    :width new-width
                    :height new-height)))

(defun random-rectangle-in-rectangle-size (in new-width new-height)
  "Make a random rectangle of the given size in the given bounds."
  (assert (<= new-width (width in)))
  (assert (<= new-height (height in)))
  (let ((new-x (+ (x in)
          (random-number (- (width in) new-width))))
    (new-y (+ (y in)
          (random-number (- (height in) new-height)))))
    (make-rectangle :x new-x
                    :y new-y
                    :width new-width
                    :height new-height)))

(defun inset-rectangle (source offset)
  "Trim a rectangle by the given amount."
  (make-rectangle :x (+ (x source) offset)
                  :y (+ (y source) offset)
                  :width (- (width source) (* offset 2.0))
                  :height (- (height source) (* offset 2.0))))

(defmethod area ((rect <rectangle>))
  "Get the rectangle's area."
  (* (width rect) (height rect)))

(defun rect-as-points (rect)
  "Get the rectangle as an array of four points"
  (vector (make-point :x (x rect) :y (y rect))
          (make-point :x (+ (x rect) (width rect)) :y (y rect))
          (make-instance '<point> :x (+ (x rect) (width rect))
                                  :y (+ (y rect) (height rect)))
          (make-point :x (x rect) :y (+ (y rect) (height rect)))))

(defun contains-point-co-ordinates (rect x y)
  "Find whether the rectangle contains the point."
  (and (>= x (y rect))
       (< x (+ (x rect) (width rect)))
       (>= y (y rect))
       (< y (+ (y rect) (height rect)))))

(defmethod contains ((rect <rectangle>) (p <point>))
  "Find whether the rectangle contains the point."
  (contains-point-co-ordinates rect (x p) (y p)))

(defmethod contains ((container <rectangle>) (r <rectangle>))
  "Find whether container contains r."
  (and (<= (x container) (x r))
       (<= (y container) (y r))
       (>= (+ (x container) (width container)) (+ (x r) (width r)))
       (>= (+ (y container) (height container)) (+ (y r) (height r)))))

(defun points-in-rectangle (rect points)
  "Get the vector of points within the rectangle."
  (let ((contained (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for p across points
          when (contains rect p)
            do (vector-push-extend p contained))
    contained))

(defmethod intersects ((rect1 <rectangle>) (rect2 <rectangle>))
  "Find whether the rectangles intersect."
  (and (<= (x rect1) (+ (x rect2) (width rect2)))
       (<= (y rect1) (+ (y rect2) (height rect2)))
       (>= (+ (x rect1) (width rect1)) (x rect2))
       (>= (+ (y rect1) (height rect1)) (y rect2))))

(defun include-point (rect p)
  "Destructively expand the rectangle to include the point."
  (let ((right (+ (x rect) (width rect)))
    (top (+ (y rect) (height rect))))
    (cond
      ((< (x p) (x rect))
       (setf (width rect)
         (+ (width rect) (- (x rect) (x p))))
       (setf (x rect) (x p)))
      ((> (x p) right)
       (setf (width rect)
         (+ (width rect) (- (x p) right))))
      ((< (y p) (y rect))
       (setf (height rect)
         (+ (height rect) (- (y rect) (y p))))
       (setf (y rect) (y p)))
      ((> (y p) top)
       (setf (height rect)
         (+ (height rect) (- (y p) top)))))))

(defun include-rectangle (rect include)
  "Expand the first rectangle to include the second."
  (include-point rect (make-point :x (x include) :y (y include)))
  (include-point rect (make-point :x (x include)
                                  :y (+ (y include) (height include))))
  (include-point rect (make-point  :x (+ (x include) (width include))
                                   :y (+ (y include) (height include))))
  (include-point rect (make-point :x (+ (x include) (width include))
                                  :y (y include))))

(defun rectangle-from-point (p)
  "Make a zero-size rectangle for a point."
  (make-rectangle :x (x p) :y (y p) :width 0.0 :height 0.0))

(defun random-point-on-rectangle (r)
  "Make a random point somewhere on the border of a rectangle."
  (random-point-on-line (case (random-number 4)
                          (0 (make-line :from (make-point :x (x r)
                                                          :y (y r))
                                        :to (make-point :x (x r)
                                                        :y (+ (y r)
                                                              (height r)
                                                              -1))))
                          (1  (make-line :from (make-point :x (x r)
                                                           :y (+ (y r)
                                                                 (height r)
                                                                 -1))
                                         :to (make-point :x (+ (x r)
                                                               (width r)
                                                               -1)
                                                         :y (+ (y r)
                                                               (height r)
                                                               -1))))
                          (2  (make-line :from (make-point :x (+ (x r)
                                                                 (width r)
                                                                 -1)
                                                           :y (+ (y r)
                                                                 (height r)
                                                                 -1))
                                         :to (make-point :x (+ (x r)
                                                               (width r)
                                                               -1)
                                                         :y (y r))))
                          (3 (make-line :from (make-point :x (+ (x r)
                                                                (width r)
                                                                -1)
                                                          :y (y r))
                                        :to (make-point :x (x r)
                                                        :y (y r)))))))

(defun random-points-on-rectangle (r count)
  "Generate count points on a rectangle's outline. These will not be ordered."
  (map-into (make-array count) (lambda () (random-point-on-rectangle r))))

(defun random-points-at-rectangle-corners (r count)
  "Return from 0 to 4 corner points of a rectangle, clamping out-of-range."
  ;; Inefficient but easy to code and easy to read ;-]
  (choose-n-of count
               (vector (make-point :x (x r)
                                   :y (y r))
                       (make-point :x (x r)
                                   :y (+ (y r) (height r) -1))
                       (make-point :x (+ (x r) (width r) -1)
                                   :y (+ (y r) (height r) -1))
                       (make-point :x (+ (x r) (width r) -1)
                                   :y (y r)))))

(defclass <grow-rectangle> (ds::<rectangle>)
  ((directions :initform '(left right up down)
               :accessor directions)
   (min :initarg :min
        :accessor min-rect)
   (max :initarg :max
        :accessor max-rect)
   (xstep :initarg :xstep
          :initform 1.0
          :accessor xstep)
   (ystep :initarg :ystep
          :initform 1.0
          :accessor ystep)
   (within :initarg :within
           :accessor within-rect)
   (avoid :initarg :avoid
          :accessor avoid-rects))
  (:documentation "A rectangle that grows with constraints."))

(defmethod initialize-instance :after ((o <grow-rectangle>)
                                       &key
                                         initial-x
                                         initial-y)
  (setf (x o) initial-x)
  (setf (y o) initial-y)
  (setf (width o) 1)
  (setf (height o) 1))

(defun can-grow-rectangle-p (rect)
  (not (null (directions rect))))

(defun found-rectangle-p (rect)
  (and (>= (width rect) (width (min-rect rect)))
       (>= (height rect) (height (min-rect rect)))))

(defun search-finished-p (rect)
  (or (found-rectangle-p rect)
      (not (can-grow-rectangle-p rect))))

(defun grow-rect (rect new-rect which-way)
  (case which-way
      ;; Try to grow in direction.
      (up
       (incf (height new-rect)) (ystep rect))
      (down
       (incf (height new-rect) (ystep rect))
       (decf (y new-rect)) (ystep rect))
      (left
       (decf (x new-rect) (xstep rect))
       (incf (width new-rect)) (xstep rect))
      (right
       (incf (width new-rect)) (xstep rect))))

(defun stop-growing-direction-max (rect)
  ;; If the rect has reached its maximum width,
  ;; don't grow any further to the left or right
  (when (= (width rect) (width (max-rect rect)))
    (setf (directions rect)
          (remove-if #'(lambda (x) (member x '(left right)))
                     (directions rect))))
  ;; If the rect has reached its maximum height,
  ;; don't grow any further at the top or bottom.
  (when (= (height rect) (height (max-rect rect)))
    (setf (directions rect)
          (remove-if #'(lambda (x) (member x '(up down)))
                     (directions rect)))))

(defun set-grown-size (rect new-rect which-way)
  (case which-way
    (up
     (setf (height rect) (height new-rect)))
    (down
     (setf (height rect) (height new-rect)
           (y rect) (y new-rect)))
    (left
     (setf (x rect) (x new-rect)
           (width rect) (width new-rect)))
    (right
     (setf (width rect) (width new-rect)))))

(defun grow-rectangle-step (rect)
  (let ((new-rect (copy-rectangle rect))
        (which-way (choose-one-of (directions rect))))
    ;; Grow in a random direction.
    (grow-rect rect new-rect which-way)
    ;; If we collided or spilled over, stop growing in that direction.
    (when (or (intersects-any new-rect (avoid-rects rect))
              (not (contains (within-rect rect) new-rect)))
      (setf new-rect nil)
      (setf (directions rect)
            (remove which-way (directions rect))))
    (when new-rect
      ;; Copy our new dimensions in.
      (set-grown-size rect new-rect which-way)
      ;; If we reach max size in any direction, stop growing that way.
      (stop-growing-direction-max rect)))
  rect)

(defun grow-rectangle (min max
                       initial-x initial-y
                       within avoid
                       &optional (xstep 1) (ystep 1))
  (let ((grow (make-instance '<grow-rectangle>
                             :min min :max max
                             :initial-x initial-x :initial-y initial-y
                             :xstep xstep :ystep ystep
                             :within within :avoid avoid)))
    (loop while (not (search-finished-p grow))
          do (grow-rectangle-step grow))
    (if (found-rectangle-p grow)
        (copy-rectangle grow)
     nil)))
