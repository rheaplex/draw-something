;; polyline.lisp - A classic computer graphics polyline.
;; Copyright (C) 2006, 2016 Rhea Myers.
;; Copyright (C) 2023 Myers Studio Ltd.
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

(in-package :draw-something)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polyline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <polyline> (<geometry>)
  ;; Optimised to use arrays not lists to avoid terrible (distance) consing
  ;; For speed set initial dimension to a size unlikely to need increasing
  ((points :accessor points
           :initform (make-array 1000 :adjustable t :fill-pointer 0)
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

(defun make-polyline (&key (points nil))
  "Constructor function."
  (let ((poly (make-instance '<polyline>)))
    (when points
      (loop for point across points
            do (append-point poly point)))
    poly))

(defmethod print-object ((object <polyline>) stream)
  "Make a human readable string describing the polyline."
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(BOUNDS: ~a POINTS: ~:{ (~,2f, ~,2f)~})"
          (bounds object)
          (map 'list #'(lambda (p) (list (x p) (y p)))
               (points object)))))

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
  (let ((poly (make-polyline)))
    (dotimes (i count)
      (append-point poly (random-point-in-rectangle rect)))
    poly))

(defun new-segment-far-enough-p (p poly sep)
  "Check that adding the segment from the end of poly to p won't break
   its separation of points/lines."
  (let ((q (last-point poly))
        (ok t))
    (dotimes (i (- (point-count poly) 1))
      (when (< (distance-point-line (aref (points poly) i) p q)
               sep)
        (setf ok nil)
        (return)))
    ok))

(defun make-random-polyline-in-rectangle-sep (rect count sep)
  "Create a polyline with the given number of points in the given bounds,
   with each point separated from the others and their lines by at least
   sep. This is to avoid the pen getting trapped by small gaps between
   points or between points and lines."
  (assert (> count 2))
  (let ((poly (make-polyline)))
    (append-point poly (random-point-in-rectangle rect))
    (loop while (< (point-count poly) count)
          do (let ((p (random-point-in-rectangle rect)))
               (when (>= (distance p poly) sep)
                 (when (or (< (point-count poly) 2)
                           (new-segment-far-enough-p p poly sep))
                   (append-point poly p)))))
    poly))

(defun make-polyline-from-points (points)
  "Create a polyline with the given points."
  (let ((poly (make-polyline)))
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
  (highest-leftmost-point-in-array (points poly)))

(defmethod area ((poly <polyline>))
  "Get the area of the POLYGON"
  ;;Cleanme!
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
            (ray-line (make-line :from p
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
  (make-polyline :points (vector (make-point :x (x rect) :y (y rect))
                                 (make-point :x (+ (x rect) (width rect))
                                             :y (y rect))
                                 (make-point :x (+ (x rect) (width rect))
                                             :y (+ (y rect) (height rect)))
                                 (make-point :x (x rect)
                                             :y (+ (y rect) (height rect)))
                                 (make-point :x (x rect) :y (y rect)))))

(defun poly-line-aref (poly index)
  "Read the nth line segment of the polyline as a line."
  (let ((pts (points poly)))
    (make-line :from (aref pts index)
               :to (aref pts (+ index 1)))))

(defmacro do-poly-lines ((poly sym &optional (index (gensym))) &body body)
  "Apply fun to each line in poly, or not if there is only 1 point."
  (with-gensyms (poly-points previous-point current-point i)
    `(let ((,poly-points (points ,poly)))
       (when (> (point-count ,poly) 1)
         (let ((,previous-point (first-point ,poly))
               (,current-point nil)
               (,sym nil)
               (,index 0))
           (dotimes (,i (- (point-count ,poly) 1))
             (setf ,current-point (aref ,poly-points (+ ,i 1)))
             (setf ,sym (make-line :from ,previous-point
                                   :to ,current-point))
             ,@body
             (incf ,index)
             (setf ,previous-point ,current-point)))))))

(defmethod intersects ((l <line>) (poly <polyline>))
  "Find whether the line intersects or is contained by the polyline."
  ;; Currently only intersects
  (let ((result nil))
    (do-poly-lines (poly l2)
      (when (intersects l l2)
        (setf result t)
        (return)))
    result))

(defmethod intersections ((l <line>) (poly <polyline>))
  "Get all intersections between l and poly, if any."
  (let ((result nil))
    (do-poly-lines (poly l2)
      (let ((intersection-t (intersects l l2)))
        (when intersection-t
          (push (line-at-t l intersection-t) result))))
    (reverse result)))

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
      (do-poly-lines (poly1 l1)
    (do-poly-lines (poly2 l2)
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
    (do-poly-lines (poly ll)
             (when (intersects l ll)
               (setf result t)
               (break)))
    result))

(defun convex-hull (the-points)
  "Get the convex hull of an array of points."
  (let* ((first-point (highest-leftmost-point-in-array the-points))
         (current-point first-point)
         (next-point nil)
         (hull (make-array 1 :adjustable t :fill-pointer 0)))
    (vector-push-extend first-point hull)
    (loop until (and (not (eq next-point nil))
                     (eq next-point first-point))
          do (setf next-point
                   (point-with-all-left current-point the-points))
          (vector-push-extend next-point hull)
          (setf current-point next-point))
    (make-polyline :points hull)))

;; Actually for polygons! We only want the sign for determining cw/ccw.
(defun polyline-signed-area (polyline)
  (let ((area 0.0)
        (ps (points polyline))
        (num-ps (length (points polyline))))
    (dotimes (i num-ps)
      (let ((p1 (aref ps i))
            (p2 (aref ps (mod (+ i 1) num-ps))))
      (incf area
          (* (- (x p2) (x p1))
             (+ (y p2) (y p1)))))    
    (/ area 2.0))))

(defun polyline-clockwise-p (polyline)
  (< 0.0 (polyline-signed-area polyline)))

(defun ensure-polyline-clockwise (polyline)
  "Destructively ensure that the polyline's segmants are in clockwise order."
  (unless (polyline-clockwise-p polyline)
    (setf (points polyline)
          (nreverse (points polyline))))
  polyline)

(defun loop-polyline (polyline)
  "Make a polyline a closed loop without changing its appearance."
  (append (points polyline)
          (reverse (points polyline))))

;; A naive brute-force algorithm

(defun divide-polyline-segments (polyline)
  "Divide a polyline into shorter line segments where it self-intersects."
  (let ((newpoints (list (from (first (points polyline))))))
    ;; For each polyline
    (do-poly-lines (polyline l1)
      (let ((intersections nil))
        ;; If it is intersected by any other lines
        (do-poly-lines (polyline l2)
          (unless (point= l1 l2)
            (let ((intersects-at (intersects l1 l2)))
              (when intersects-at
                ;; Add it to the points describing new line segments.
                (push intersections intersects-at)))))
        ;; Destructively order any new points by distance from the start of
        ;; the line segment.
        (sort ps
              (lambda (p1 p2) (< (distance p1 (from l1))
                                 (distance p2 (from l1)))))
        ;; Add any intersection points and the point at the end of this segment
        ;; to the new polyline points.
        (setq newpoints (append newpoints ps (list (to l1))))))
    newpoints))

(defun choose-random-polyline-line (poly)
  "Randomly choose a line in the polyline."
  (let ((start (random-number (- (point-count poly) 1))))
    (make-line :from (copy-point (aref (points poly) start))
               :to (copy-point (aref (points poly) (+ start 1))))))

(defun choose-random-polyline-point (poly)
  "Randomly choose a point in the polyline."
  (choose-one-of (points poly)))

(defun choose-random-polyline-points-subsequence (poly count)
  "Randomly choose a series of points of length count in the polyline."
  (let ((start (random-number (- (point-count poly) count))))
    (subseq (points poly) start (+ start count))))

(defun random-point-in-polyline (poly)
  "Try to find a point within the polyline outline, or nil if fails."
  (dotimes (i 10000)
    (let ((p (random-point-in-rectangle (bounds poly))))
      (when (contains poly p)
        (return p)))))
