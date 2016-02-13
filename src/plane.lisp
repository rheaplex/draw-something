;; plane.lisp - A plane (layer, level, plane) in the drawing.
;; Copyright (C) 2006, 2010, 2016 Rhea Myers rhea@myers.studio
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

(defconstant +min-planes+ 1)
(defconstant +max-planes+ 5)

(defconstant +min-figures+ 3)
(defconstant +max-figures+ 9)

(defclass plane ()
  ((figure-policy :accessor figure-policy
		  :initarg :figure-policy
		  :documentation "The function for generating figures.")
   (figures :accessor figures
	    :initform (make-vector 10)
	    :initarg :figures
	    :documentation "The figures of the plane.")
   (figure-count :accessor figure-count
		 :type integer
		 :initarg :figure-count
		 :documentation "The number of figures to make for the plane.")
   (pen :accessor plane-pen
	;;:type pen
	:initarg :pen
	:documentation "The pen properties for the plane."))
  (:documentation "A plane of the drawing."))

(defconstant +plane-pen-distance-minimum+ 0.1)
(defconstant +plane-pen-distance-maximum+ 5.0)
(defconstant +plane-pen-tolerance-minimum+ (/ +plane-pen-distance-minimum+ 2.0))
(defconstant +plane-pen-tolerance-maximum+ (/ +plane-pen-distance-maximum+ 2.0))

(defun make-plane-pen (plane-index num-planes)
  "Make a pen for the plane."
  (declare (ignore plane-index num-planes))
   #|(let ((plane-factor (* (/ 1.0 (- num-planes 1))
			 plane-index)))
    (make-instance 'pen
		   :distance (+ +plane-pen-distance-minimum+
				(* plane-factor
				   (- +plane-pen-distance-maximum+
				      +plane-pen-distance-minimum+)))
		   :step 1.0
		   :tolerance (+ +plane-pen-tolerance-minimum+
				 (* plane-factor
				    (- +plane-pen-tolerance-maximum+
				       +plane-pen-tolerance-minimum+))))))|#
  nil)

(defconstant +minimum-number-of-planes+ 1)
(defconstant +maximum-number-of-planes+ (length *figure-generation-method-list*))

(defun number-of-planes ()
  "Decide how many planes to have"
  (random-range +minimum-number-of-planes+
                +maximum-number-of-planes+))

(defun number-of-figures-for-plane (plane-index)
  "Randomly determine how many figures a plane should have."
  (declare (ignore plane-index))
  (random-range 2 10))

(defun plane-forms-bounds (the-plane)
  "Get the bounding rectangles for every form of every figure on the plane"
  (let ((results (make-vector 0)))
    (loop for figure across (figures the-plane)
       do (loop for form across (forms figure)
	     do (vector-push-extend (bounds form) results)))
    results))

(defun make-planes (the-drawing count)
  "Make the planes, ready to have skeletons for figures generated."
  (advisory-message "Making planes.~%")
  (loop for point-method in (figure-generation-methods count)
     for i from 0 below count
     do (advisory-message (format nil "Making plane ~d.~%" (+ i 1)))
     do (vector-push-extend 
	 (make-instance 'plane 
			:figure-count (number-of-figures-for-plane i)
			:figure-policy point-method
			:pen nil ;;(make-plane-pen i count)
            )
	 (planes the-drawing))))

(defun make-plane-skeletons (the-plane the-drawing)
  "Generate the skeletons for the figures of the plane."
  (advisory-message "Making plane skeleton(s).~%")
  (setf (figures the-plane)
	(funcall (figure-policy the-plane)
		 (composition-points the-drawing))))

(defun make-planes-skeletons (the-drawing)
  "Generate the skeletons for the figures of each plane."
  (advisory-message "Making planes skeletons.~%")
  (loop for l across (planes the-drawing)
     do (make-plane-skeletons l the-drawing)))

(defun draw-plane-figures (the-plane)
  "Draw around the skeletons of the figures of the plane."
  (advisory-message "Drawing plane figure(s).~%")
  (loop for fig across (figures the-plane)
	do (draw-figure fig))) ;; (pen l)

(defun draw-planes-figures (the-drawing)
  "Draw around the skeletons of the figures of each plane."
  (advisory-message "Drawing planes figures.~%")
  (loop for l across (planes the-drawing)
	do (draw-plane-figures l)))

#|(defmethod make-figure-for-plane ((figure-bounds rectangle) (plane integer))
  (let* ((form-width (/ (width figure-bounds) plane))
	 (form-height (/ (height figure-bounds) plane)))
    (make-figure figure-bounds form-width form-height)))

(defmethod make-figures ((the-drawing drawing))
  "Make the figures for the drawing."
  (let ((figure-count (random-range-inclusive +min-figures+ +max-figures+)))
    (advisory-message (format nil "Making ~a figures.~%" 
			      figure-count))
    (loop for i from 1 to figure-count
      do (advisory-message (format nil "Making figure ~a/~a.~%" i
				   figure-count))
      do (add-figure the-drawing
		     (make-figure-for-plane (bounds the-drawing) i)))))|#

(defun find-space-on-plane (the-drawing the-plane required-size-rect)
  "Find empty space on the plane of given size, or nil if fail"
  ;; Efficiency decreases with number of figures. Cells would be constant.
  ;; FIXME: Uses bounds rather than outline of actual figure
  ;; TODO: Try figure bounds first. No intersection? proceed
  ;;       Next try form bounds.
  ;;       Next try form skeletons.
  ;;       Finally try form outlines.
  ;;  (assert (contains (bounds d) required-size))
  (let ((candidate (make-instance 'rectangle :x 0 :y 0
                                  :width (width required-size-rect)
                                  :height (height required-size-rect)))
        (plane-rects (plane-forms-bounds the-plane))
        ;; The resulting rect must fit in within the drawing bounds
        (width-to-search (- (width (bounds the-drawing))
                            (width required-size-rect)))
        (height-to-search (- (height (bounds the-drawing))
                             (height required-size-rect)))
        (result nil))
    (block outside-the-loops
      ;; Use dotimesloop to ensure we don't find the top left space each time
      (dotimesloop (v 0 (random-range 0 height-to-search) height-to-search)
                   (setf (y candidate) v)
	(dotimesloop (h 0 (random-range 0 width-to-search) width-to-search)
                 (setf (x candidate) h)
                 (when (intersects-none candidate plane-rects)
                   (setf result candidate)
                   (return-from outside-the-loops)))))
    result))

(defun find-space-on-plane-range (the-drawing the-plane min-size-rect
                                  max-size-rect steps)
  "Find empty space on the plane larger than min-size up to max-size, or nil"
  (let ((width-step-size (/ (- (width max-size-rect) (width min-size-rect))
                            steps))
        (height-step-size (/ (- (height max-size-rect) (height min-size-rect))
                             steps))
        (result nil))
    (dotimes (step steps)
      (let* ((required-size (make-instance 'rectangle :x 0 :y 0
                                           :width (- (width max-size-rect)
                                                     (* width-step-size
                                                        step))
                                           :height (- (height max-size-rect)
                                                      (* height-step-size
                                                         step))))
             (candidate (find-space-on-plane the-drawing
                                             the-plane
                                             required-size)))
        (when candidate
          (setf result candidate)
          (return))))
    result))
