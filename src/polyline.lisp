;;  polyline.lisp - A classic computer graphics polyline.
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

(defclass <polyline> (<geometry>)
  ;; Optimised to use arrays not lists to avoid terrible (distance) consing
  ;; For speed set initial dimension to a size unlikely to need increasing
  ((points :accessor points
           :initform (make-vector 1000)
           :initarg :points
           :documentation "The points of the polyline")
   (bounds :accessor bounds
           :type rectangle
           :initarg :bounds
           :documentation "The bounds of the polyline."))
  (:documentation "A polyline or polygon. A series of joined line segments."))

(defun append-point (poly pt)
  "Append a point to the polyline."
  (vector-push-extend pt (points poly))
  (if (slot-boundp poly 'bounds)
      (include-point (bounds poly) pt)
      (setf (bounds poly) (rectangle-from-point pt))))

(defun point-count (poly)
  "Get the number of points in the polyline"
  (length (points poly)))

(defun first-point (poly)
  "Get the first point of the polyline."
  (aref (points poly) 0))

(defun last-point (poly)
  "Get the last point of the polyline."
  (aref (points poly)
        (- (point-count poly) 1)))

(defun make-random-polyline-in-rectangle (rect count)
  "Create a polyline with the given number of points in the given bounds."
  (let ((poly (make-instance '<polyline>)))
    (dotimes (i count)
      (append-point poly (random-point-in-rectangle rect)))
    poly))

(defun make-polyline-from-points (points)
  "Create a polyline with the given points."
  (let ((poly (make-instance '<polyline>)))
    (loop for p across points
      do (append-point poly p))
    poly))

(defmethod distance ((p <point>) (poly <polyline>))
  "The distance from a point to a polyline."
  (cond ((= (length (points poly)) 0)
         nil) ;; Infinite distance? Zero?
        ((= (length (points poly)) 1)
         (distance p (aref (points poly) 0)))
        (t ;; More than 1 point
         (let ((distance-to-poly nil)
               (pts (points poly)))
           (do ((i 1 (+ i 1)))
               ((= i (length pts)))
             (let ((d (distance-point-line p
                                           (aref pts (- i 1))
                                           (aref pts i))))
               (if (or (not distance-to-poly)
                       (< d distance-to-poly))
                   (setf distance-to-poly d))))
           distance-to-poly))))

(defmethod highest-leftmost-point ((poly <polyline>))
  "The highest point, or highest and leftmost point (if several are highest)."
  (highest-leftmost-point-in-list (points poly)))

(defmethod area ((poly <polyline>))
  "Get the area of the POLYGON"
  ;; Cleanme!
  (if (< (length (points poly)) 3)
      0.0
      (let ((pts (points poly))
            (numpts (length (points poly)))
            (area 0.0))
        (dotimes (i (- numpts 1))
          (let ((j (mod (+ i 1)
                        numpts)))
            (setf area (+ area
                          (* (x (aref pts i))
                             (y (aref pts j)))))
            (setf area (- area
                          (* (y (aref pts i))
                             (x (aref pts j)))))))
        (setf area (/ area 2.0))
        (abs area))))

(defmethod contains ((poly <polyline>) (p <point>))
  "Find whether the POLYGON contains the point."
  ;; Count ray-poly-line intersections. Odd = inside, 0 or even = outside.
  (if (> (length (points poly)) 2)
      (let ((pts (points poly))
            (numpts (length (points poly)))
            (ray-line (make-instance '<line>
                                     :from p
                                     :to (translate-point p 10000.0 0.0)))
            (crossings 0))
        (dotimes (i (- numpts 1))
          (let ((j (mod (+ i 1)
                        numpts)))
            (when (line-intersects-line-points ray-line
                                               (aref pts i)
                                               (aref pts j))
              (setf crossings(+ crossings 1)))))
        (oddp crossings))))

(defgeneric as-polyline (geometry)
  (:documentation "Convert the geometry to a polyline approximation."))

(defmethod as-polyline ((rect <rectangle>))
  "Convert a rectangle into a five-point (closed) polyline"
  (make-instance '<polyline>
		 :bounds rect
		 :points (vector (make-instance '<point> 
						:x (x rect) 
						:y (y rect))
				 (make-instance '<point> 
						:x (+ (x rect) 
						      (width rect)) 
						:y (y rect))
				 (make-instance '<point> 
						:x (+ (x rect) 
						      (width rect)) 
						:y (+ (y rect) 
						      (height rect)))
				 (make-instance '<point> 
						:x (x rect) 
						:y (+ (y rect) 
						      (height rect)))
				 (make-instance '<point> 
						:x (x rect) 
						:y (y rect)))))

(defmacro do-poly-lines ((sym poly) &body body)
  "Apply fun to each line in the polyline, or not if there is only 1 point."
  (with-gensyms (poly-points previous-point current-point i)
   `(let ((,poly-points (points ,poly)))
      (when (> (point-count ,poly) 1)
        (let ((,previous-point (first-point ,poly))
              (,current-point nil)
              (,sym nil))
          (dotimes (,i (- (point-count ,poly) 1))
            (setf ,current-point (aref ,poly-points (+ ,i 1)))
            (setf ,sym (make-instance '<line>
                                      :from ,previous-point
                                      :to ,current-point))
            ,@body
            (setf ,previous-point ,current-point)))))))

(defmethod intersects ((l <line>) (poly <polyline>))
  "Find whether the line intersects or is contained by the polyline."
  ;; Currently only intersects
  (let ((result nil))
    (do-poly-lines (l2 poly)
      (when (intersects l l2)
        (setf result t)
        (return)))
    result))

(defun polyline-contains-points (poly1 poly2)
  "Find whether poly1 contians any points of poly2"
  ;; Currently only intersects
  (dolist (p (points poly2))
    (when (contains poly1 p)
      (return t))))

(defun polyline-lines-intersects (poly1 poly2)
  "Find whether any lines of the polylines intersect"
  (let ((result nil))
    (block outside-loops
      (do-poly-lines (l1 poly1)
	(do-poly-lines (l2 poly2)
	  (when (intersects l1 l2)
	    (setf result t)
	    (return-from outside-loops)))))
    result))

(defmethod intersects ((poly1 <polyline>) (poly2 <polyline>))
  "Find whether the two POLYGONS intersect or contain each other."
  (and (intersects (bounds poly1) (bounds poly2))
       (or 
	;; If any lines intersect
	(polyline-lines-intersects poly1 poly2)
	;; If one polyline contains any point of the other
	(some (lambda (p) (contains poly1 p)) (points poly1))
	(some (lambda (p) (contains poly2 p)) (points poly2)))))

(defmethod intersects ((poly <polyline>) (rect <rectangle>))
  "Does the polyline overlap the rectangle?"
  ;; Tests in order of computational expense
  ;; See whether the bounds overlap, then continue
  (and (intersects (bounds poly) rect)
       (or 
	;; If the rectangle contains any point of the polyline
	(some (lambda (p) (contains rect p)) (points poly))
	;; If any line of the rectangle intersects any line of the polyline
	(intersects poly (as-polyline rect))
	;; If any point of the square is inside the polyline
	(some (lambda (p) (contains poly p)) (rect-as-points rect)))))

(defun adding-point-would-cause-self-intersection (poly p)
  "Check if adding the point to the polyline would make it self-intersect."
  (let ((l (make-instance '<line>
                          :from (last-point poly)
                          :to p))
        (result nil))
    (do-poly-lines (ll poly)
             (when (intersects l ll)
               (setf result t)
               (break)))
    result))
