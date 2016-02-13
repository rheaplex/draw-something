;;  rectangle.lisp - A 2D rectangle.
;;  Copyright (C) 2006, 2016 Rhea Myers rhea@myers.studio
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

(in-package "DRAW-SOMETHING")

(defclass rectangle (geometry)
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

(defun copy-rectangle (r)
  "Make a copy of the rectangle."
  (make-instance 'rectangle 
		 :x (x r) :y (y r) :width (width r) :height (height r)))

(defun random-point-in-rectangle (bounds-rect)
  "Make a point placed randomly within the given bounding rectangle."
  (make-instance 'point 
		 :x (+ (x bounds-rect) (random (width bounds-rect)))
		 :y (+ (y bounds-rect) (random (height bounds-rect)))))

(defun random-points-in-rectangle (bounds-rect count)
  "Create count points placed randomly within bounds-rect."
  (map-into (make-vector count)
	    (lambda ()(random-point-in-rectangle bounds-rect))))

(defun random-rectangle-in-rectangle (bounds-rect)
  "Make a random rectangle of at least size 1x1 in another rectangle."
  (let* ((new-width (random (width bounds-rect)))
	 (new-height (random (height bounds-rect)))
	 (new-x (+ (x bounds-rect)
		   (random (- (width bounds-rect) new-width))))
	 (new-y (+ (y bounds-rect)
		   (random (- (height bounds-rect) new-height)))))
    (make-instance 'rectangle
		   :x new-x
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
    (make-instance 'rectangle
		   :x new-x
		   :y new-y
		   :width new-width
		   :height new-height)))
  
(defun random-rectangle-in-rectangle (bounds-rect)
  "Make a random rectangle of at least size 1x1 in another rectangle."
  (let* ((new-width (random (width bounds-rect)))
	 (new-height (random (height bounds-rect)))
	 (new-x (+ (x bounds-rect)
		   (random (- (width bounds-rect) new-width))))
	 (new-y (+ (y bounds-rect)
		   (random (- (height bounds-rect) new-height)))))
    (make-instance 'rectangle
		   :x new-x
		   :y new-y
		   :width new-width
		   :height new-height)))

(defun inset-rectangle (source offset)
  "Trim a rectangle by the given amount."
  (make-instance 'rectangle
		 :x (+ (x source) offset)
		 :y (+ (y source) offset)
		 :width (- (width source) (* offset 2.0))
		 :height (- (height source) (* offset 2.0))))

(defmethod area ((rect rectangle))
  "Get the rectangle's area."
  (* (width rect) (height rect)))

(defun rect-as-points (rect)
  "Get the rectangle as an array of four points"
  (vector (make-instance 'point :x (x rect) :y (y rect))
	  (make-instance 'point :x (+ (x rect) (width rect)) :y (y rect))
	  (make-instance 'point :x (+ (x rect) (width rect)) 
			 :y (+ (y rect) (height rect)))
	  (make-instance 'point :x (x rect) :y (+ (y rect) (height rect)))))

(defun contains-point-co-ordinates (rect x y)
  "Find whether the rectangle contains the point."
  (and (>= x (y rect))
       (< x (+ (x rect) (width rect)))
       (>= y (y rect))
       (< y (+ (y rect) (height rect)))))

(defmethod contains ((rect rectangle) (p point))
  "Find whether the rectangle contains the point."
  (contains-point-co-ordinates rect (x p) (y p)))

(defun points-in-rectangle (rect points)
  "Get the vector of points within the rectangle"
  (let ((contained (make-vector 0)))
    (loop for p across points
	  when (contains rect p)
	  do (vector-push-extend p contained))
    contained))

(defmethod intersects ((rect1 rectangle) (rect2 rectangle))
  "Find whether the rectangles intersect."
  (and (< (x rect1) (+ (x rect2) (width rect2)))
       (< (y rect1) (+ (y rect2) (height rect2)))
       (> (+ (x rect1) (width rect1)) (x rect2))
       (> (+ (y rect1) (height rect1)) (y rect2))))

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
  (include-point rect (make-instance 'point :x (x include) :y (y include)))
  (include-point rect (make-instance 'point :x (x include) 
				     :y (+ (y include) (height include))))
  (include-point rect (make-instance 'point :x (+ (x include) (width include)) 
				     :y (+ (y include) (height include))))
  (include-point rect (make-instance 'point :x (+ (x include) (width include))
				     :y (y include))))

(defun rectangle-from-point (p)
  "Make a zero-size rectangle for a point."
  (make-instance 'rectangle :x (x p) :y (y p) :width 0.0 :height 0.0))

(defun random-point-on-rectangle (r)
  "Make a random point somewhere on the border of a rectangle."
  (case (random 4)
    (0 (random-point-on-line
	(make-instance 'line
		       :from (make-instance 'point
					    :x (x r)
					    :y (y r))
		       :to (make-instance 'point
					  :x (x r)
					  :y (+ (y r)
						(height r)
						-1)))))
    (1 (random-point-on-line
	(make-instance 'line
		       :from (make-instance 'point
					  :x (x r)
					  :y (+ (y r)
						(height r)
						-1))
		       :to (make-instance 'point
					  :x (+ (x r)
						(width r)
						-1)
					  :y (+ (y r)
						(height r)
						-1)))))
    (2 (random-point-on-line
	(make-instance 'line
		       :from (make-instance 'point
					  :x (+ (x r)
						(width r)
						-1)
					  :y (+ (y r)
						(height r)
						-1))
		       :to (make-instance 'point
					  :x (+ (x r)
						(width r)
						-1)
					  :y (y r)))))
    (3 (random-point-on-line
	(make-instance 'line
		       :from (make-instance 'point
					    :x (+ (x r)
						(width r)
						-1)
					    :y (y r))
		       :to (make-instance 'point
					  :x (x r)
					  :y (y r)))))))
		       
(defun random-points-on-rectangle (r count)
  "Generate count points on a rectangle's outline. These will not be ordered."
  (map-into (make-vector count) (lambda () (random-point-on-rectangle r))))

(defun random-points-at-rectangle-corners (r count) 
  "Return from 0 to 4 corner points of a rectangle, clamping out-of-range."
  ;; Inefficient but easy to code and easy to read ;-]
  (choose-n-of count
	       (vector (make-instance 'point 
				      :x (x r) 
				      :y (y r))
		       (make-instance 'point 
				      :x (x r) 
				      :y (+ (y r) (height r) -1))
		       (make-instance 'point 
				      :x (+ (x r) (width r) -1) 
				      :y (+ (y r) (height r) -1))
		       (make-instance 'point 
				      :x (+ (x r) (width r) -1) 
				      :y (y r)))))
