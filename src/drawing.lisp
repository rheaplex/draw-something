;; drawing.lisp - A drawing.
;; Copyright (C) 2006, 2010, 2016, 2021 Rhea Myers.
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

(defpackage #:draw-something.drawing
  (:use :cl)
  (:import-from #:draw-something.choosing
                #:choose-n-of
                #:choose-n-of-ordered
                #:choose-one-of
                #:random-number
                #:random-range)
  (:import-from #:draw-something.colour
                #:<colour>)
  (:import-from #:draw-something.geometry
                #:<point>
                #:<polyline>
                #:<rectangle>
                #:x
                #:y
                #:bounds
                #:append-point
                #:convex-hull
                #:distance
                #:first-point
                #:highest-leftmost-point
                #:include-point
                #:last-point
                #:make-polyline-from-points
                #:point-count
                #:points
                #:random-points-at-rectangle-corners
                #:random-points-in-rectangle
                #:random-points-on-rectangle)
  (:export #:<drawing>
           #:<pen-parameters>
           #:composition-points
           #:do-drawing-forms
           #:draw-planes-figures
           #:figures
           #:fill-colour
           #:forms
           #:ground
           #:make-composition-points
           #:make-planes
           #:make-planes-skeletons
           #:number-of-planes
           #:pen-distance))

(in-package #:draw-something.drawing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro dotimesloop ((var from start to) &rest body)
  "Loop from start below to, then from from below to e.g. for 0 5 10
var = 5, 6, 7, 8, 9, 0, 1, 2, 3, 4
Note that this is evaluated as two loops"
  `(progn (loop for ,var from ,start below ,to
                do (progn ,@body))
          (loop for ,var from ,from below ,start
                do (progn ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A classic computer graphics turtle.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <turtle-parameters> ()
  ((turn-step :accessor turn-step
              :initform 1.0
              :initarg :turn-step
              :documentation "How many radians the turtle turns at a time.")
   (move-step :accessor move-step
              :initform 1.0
              :initarg :move-step
              :documentation "How far the turtle moves each step."))
  (:documentation "A set of parameters for turtle operations to use."))

(defclass <turtle> ()
  ((location :accessor location
             :initform (make-instance '<point>)
             :initarg :location
             :documentation "The turtle's current location.")
   (direction :accessor direction
              :initform 0.0
              :initarg :direction
              :documentation "The heading, in radians anticlockwise."))
  (:documentation "A classic computer graphics turtle plus randomness ."))

(defun turn (the-turtle amount)
  "Turn the turtle by the given amount in degrees,"
  (setf (direction the-turtle)
        (+ (direction the-turtle) amount)))

(defun left (the-turtle amount)
  "Turn the turtle left by the given amount in degrees,"
  (turn the-turtle (- amount)))

(defun right (the-turtle amount)
  "Turn the turtle left by the given amount in degrees,"
  (turn the-turtle amount))

(defun next-point-x (the-turtle amount)
  "The x co-ordinate of the next point the turtle would move forward to."
  (+ (x (location the-turtle))
     (* amount (cos (direction the-turtle)))))

(defun next-point-y (the-turtle amount)
  "The y co-ordinate of the next point the turtle would move forward to."
  (+ (y (location the-turtle))
     (* amount (sin (direction the-turtle)))))

(defun next-point (the-turtle amount)
  "The next point the turtle would move forward to."
  (make-instance '<point>
                 :x (next-point-x the-turtle amount)
                 :y (next-point-y the-turtle amount)))

(defun forward (the-turtle amount)
  "Move the turtle forward the given distance at the turtle's current angle."
  (setf (location the-turtle)
        (next-point the-turtle amount)))

(defun backward (the-turtle amount)
  "Move the turtle backward the given distance at the turtle's current angle."
  (setf (location the-turtle)
        (next-point the-turtle (- amount))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A pen configuration for drawing a form with a turtle.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <pen-parameters> (<turtle-parameters>)
  ((distance           :accessor pen-distance
                       :initarg :distance
                       :documentation "How far from the skeleton to draw.")
   (distance-tolerance :accessor distance-tolerance
                       :initarg :distance-tolerance
                       :documentation "How far from the distance is tolerable.")
   (drift-probability  :accessor drift-probability
                       :initarg :drift-probability
                       :documentation "How likely it is the pen will wobble.")
   (drift-range        :accessor drift-range
                       :initarg :drift-range
                       :documentation "The +/- pen wobble range."))
  (:documentation "A set of parameters for a pen to use."))

;; Start with fixed values for tuning
;; Move to random ranges for production

(defparameter *plane-1-pen*
  (make-instance '<pen-parameters>
                 :move-step          4.0
                 :distance           20.0
                 :distance-tolerance 5.0
                 :turn-step          0.1
                 :drift-probability  0.0
                 :drift-range        0.1))

(defparameter *plane-2-pen*
  (make-instance '<pen-parameters>
                 :move-step          2.0
                 :distance           10.0
                 :distance-tolerance 3.3
                 :turn-step          0.1
                 :drift-probability  0.0
                 :drift-range        0.1))

(defparameter *plane-3-pen*
  (make-instance '<pen-parameters>
                 :move-step          2.0
                 :distance           7.0
                 :distance-tolerance 2.0
                 :turn-step          0.1
                 :drift-probability  0.0
                 :drift-range        0.1))

(defparameter *plane-4-pen*
  (make-instance '<pen-parameters>
                 :move-step          2.0
                 :distance           5.0
                 :distance-tolerance 2.0
                 :turn-step          0.1
                 :drift-probability  0.0
                 :drift-range        0.1))

(defparameter *plane-5-pen*
  (make-instance '<pen-parameters>
                 :move-step          2.0
                 :distance           3.0
                 :distance-tolerance 1.0
                 :turn-step          0.1
                 :drift-probability  0.0
                 :drift-range        0.1))

(defparameter *plane-6-pen*
  (make-instance '<pen-parameters>
                 :move-step          2.0
                 :distance           2.0
                 :distance-tolerance 1.0
                 :turn-step          0.1
                 :drift-probability  0.0
                 :drift-range        0.1))

(defparameter *plane-7-pen*
  (make-instance '<pen-parameters>
                 :move-step          1.0
                 :distance           1.0
                 :distance-tolerance 0.2
                 :turn-step          0.1
                 :drift-probability  0.0
                 :drift-range        0.1))

(defparameter *plane-pen-parameters* (vector *plane-1-pen* *plane-2-pen*
                                             *plane-3-pen* *plane-4-pen*
                                             *plane-5-pen* *plane-6-pen*
                                             *plane-7-pen*))


(defun next-pen-distance (skeleton-forms pen-params the-turtle)
  "How far the pen will be from the guide shape when it next moves forwards."
  (let ((dist most-positive-single-float)
        (p (next-point the-turtle (move-step pen-params) )))
    (loop for skel across skeleton-forms
          do (let ((new-dist (distance p skel)))
               (when (< new-dist dist)
                 (setf dist new-dist))))
    dist))

(defun next-pen-too-close (skeleton-forms pen-params the-turtle)
  "Will the pen move to be too close from the guide shape next time?"
  (< (random-number (distance-tolerance pen-params))
     (- (next-pen-distance skeleton-forms pen-params the-turtle)
        (pen-distance pen-params))))

(defun next-pen-too-far (skeleton-forms pen-params the-turtle)
  "Will the pen move to be too far from the guide shape next time?"
  (< (random-number (distance-tolerance pen-params))
     (- (pen-distance pen-params)
        (next-pen-distance skeleton-forms pen-params the-turtle))))

(defun ensure-next-pen-far-enough (skeleton-forms pen-params the-turtle)
  "If the pen would move too close next time, turn it left until it wouldn't."
  (loop while (next-pen-too-close skeleton-forms pen-params the-turtle)
        do (left the-turtle (random-number (turn-step pen-params)))))

(defun ensure-next-pen-close-enough (skeleton-forms pen-params the-turtle)
  "If the pen would move too far next time, turn it right until it wouldn't."
  (loop while (next-pen-too-far skeleton-forms pen-params the-turtle)
        do (right the-turtle (random-number (turn-step pen-params)))))

(defun drift-pen-direction (pen-params the-turtle)
  "Adjust the pen's direction to simulate human neurophysiological noise."
  (if (< (random-number 1.0) (drift-probability pen-params))
      (turn the-turtle
            (random-range (- (drift-range pen-params))
                          (drift-range pen-params)))))

(defun adjust-next-pen (skeleton-forms pen-params the-turtle)
  "Drift or correct the pen's heading around the shape."
  (drift-pen-direction pen-params the-turtle)
  (ensure-next-pen-far-enough skeleton-forms pen-params the-turtle)
  (ensure-next-pen-close-enough skeleton-forms pen-params the-turtle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The top-level drawing object.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +min-drawing-size+ 200.0)
(defconstant +max-drawing-size+ 600.0)

(defclass <drawing> ()
  ((bounds :accessor bounds
           :type <rectangle>
           :initarg :bounds
           :documentation "The dimensions of the drawing.")
   (planes :accessor planes
           :type vector
           :initarg :planes
           :initform (make-array 1 :adjustable t :fill-pointer 0)
           :documentation "The planes of the drawing.")
   (ground :accessor ground
           :type <colour>
           :initarg :ground
           :initform nil
           :documentation "The flat background colour of the drawing.")
   (composition-points :accessor composition-points
                       :type vector
                       :initarg :composition-points
                       :initform (make-array 1 :adjustable t :fill-pointer 0)
                       :documentation "The points for the composition"))
  (:documentation "A drawing in progress."))

(defmethod initialize-instance :after ((drawing <drawing>) &key)
  (log:info "Making drawing.")
  (log:info "Bounds: ~a ." (bounds drawing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A picture plane
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +min-planes+ 1)
(defconstant +max-planes+ 5)

(defconstant +min-figures+ 3)
(defconstant +max-figures+ 9)

(defclass <plane> ()
  ((figure-policy :accessor figure-policy
                  :initarg :figure-policy
                  :documentation "The function for generating figures.")
   (figures :accessor figures
            :initform (make-array 1 :adjustable t :fill-pointer 0)
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
  (make-instance '<pen>
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
;;FIXME: This should be (length *figure-generation-method-list*)
(defconstant +maximum-number-of-planes+ 4)

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
  (let ((results (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for figure across (figures the-plane)
          do (loop for form across (forms figure)
                   do (vector-push-extend (bounds form) results)))
    results))

(defun make-planes (the-drawing)
  "Make the planes, ready to have skeletons for figures generated."
  (log:info "Making planes.")
  (let ((count (number-of-planes)))
    (loop for point-method in (figure-generation-methods count)
          for i from 0 below count
          do (log:info "Making plane ~d ." (+ i 1))
          do (vector-push-extend
              (make-instance '<plane>
                             :figure-count (number-of-figures-for-plane i)
                             :figure-policy point-method
                             :pen nil ;;(make-plane-pen i count)
                             )
              (planes the-drawing)))))

(defun make-plane-skeletons (the-plane the-drawing)
  "Generate the skeletons for the figures of the plane."
  (log:info "Making plane skeleton(s).")
  (setf (figures the-plane)
        (funcall (figure-policy the-plane)
                 (composition-points the-drawing))))

(defun make-planes-skeletons (the-drawing)
  "Generate the skeletons for the figures of each plane."
  (log:info "Making planes skeletons.")
  (loop for l across (planes the-drawing)
        do (make-plane-skeletons l the-drawing)))

(defun draw-plane-figures (the-plane)
  "Draw around the skeletons of the figures of the plane."
  (log:info "Drawing plane figure(s).")
  (loop for fig across (figures the-plane)
        do (draw-figure fig))) ;; (pen l)

(defun draw-planes-figures (the-drawing)
  "Draw around the skeletons of the figures of each plane."
  (log:info "Drawing planes figures.")
  (loop for l across (planes the-drawing)
        do (draw-plane-figures l)))

#|(defmethod make-figure-for-plane ((figure-bounds <rectangle>) (plane integer))
(let* ((form-width (/ (width figure-bounds) plane))
(form-height (/ (height figure-bounds) plane)))
(make-figure figure-bounds form-width form-height)))

(defmethod make-figures ((the-drawing <drawing>))
"Make the figures for the drawing."
(let ((figure-count (random-range-inclusive +min-figures+ +max-figures+)))
(log:info "Making ~a figures." figure-count)
(loop for i from 1 to figure-count
do (log:info "Making figure ~a/~a." i figure-count)
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
  (let ((candidate (make-instance '<rectangle> :x 0 :y 0
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
      (let* ((required-size (make-instance '<rectangle> :x 0 :y 0
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A figure on a plane.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +min-forms+ 5)
(defconstant +max-forms+ 10)

(defclass <figure> ()
  ((forms :accessor forms
          :type vector
          :initform (make-array 1 :adjustable t :fill-pointer 0)
          :initarg :forms
          :documentation "The forms of the figure.")
   (bounds :accessor bounds
           :type rectangle
           :initarg :bounds
           :documentation "The bounds of the figure."))
  (:documentation "A figure drawn in the drawing."))

(defun make-figure-from-points (points)
  "Make a figure with a single polyline from the provided points."
  (log:info "Making figure.")
  (let ((fig (make-instance '<figure>)))
    (vector-push-extend (make-form-from-points points)
                        (forms fig))
    fig))

(defun draw-figure (fig)
  "Draw the forms of a figure."
  (loop for form across (forms fig)
        do (log:info "Drawing figure.")
        do (draw-form form (choose-one-of *plane-pen-parameters*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A form in a figure.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +min-form-points+ 1)
(defconstant +max-form-points+ 12)

(defconstant +form-step-limit+ 10000)

(defconstant +pen-width+ 1.0)

(defclass <form> ()
  ((skeleton :accessor skeleton
             :type vector
             :initarg :skeleton
             :initform (make-array 1 :adjustable t :fill-pointer 0)
             :documentation "The guide shapes for the outline.")
   (outline :accessor outline
            :type polyline
            :initform (make-instance '<polyline>)
            :documentation "The outlines for the skeleton. Will be outline_s_.")
   (bounds :accessor bounds
           :type rectangle
           :initform (make-instance '<rectangle>)
           :initarg :bounds
           :documentation "The bounds of the form.")
   (fill-colour :accessor fill-colour
                :type <colour>
                :initarg :colour
                :initform nil
                :documentation "The flat body colour of the form.")
   (stroke-colour :accessor stroke-colour
                  :type <colour>
                  :initarg :colour
                  :initform (make-instance '<colour>
                                           :hue 0.0
                                           :saturation 0.0
                                           :brightness 0.0)
                  :documentation "The outline colour of the form."))
  (:documentation "A form drawn in the drawing."))

;; Skeleton will ultimately be generated from a list of objects, kept separately
;; Forms will be able to have no fill or no outline independently

(defun form-first-point (the-form)
  "Get the first point in the outline of the form."
  (first-point (outline the-form)))

(defun form-point-count (the-form)
  "The number of points in the outline of the form."
  (point-count (outline the-form)))

(defun most-recent-point (the-form)
  "The most recent point added to the outline of the form."
  (last-point (outline the-form)))

(defun make-form-start-point (form-skeleton pen-params)
  "Get the point to start drawing at."
  (let ((start-point nil))
    (loop for skel across form-skeleton
          do (let* ((hp (highest-leftmost-point skel))
                    (candidate
                      (make-instance '<point>
                                     :x (x hp)
                                     :y (+ (y hp)
                                           (pen-distance pen-params)))))
               (when (or (not start-point)
                         (> (y candidate) (y start-point))
                         (and (= (y candidate) (y start-point))
                              (< (x candidate) (x start-point))))
                 (setf start-point candidate))))
    start-point))

(defun make-form-turtle (the-form pen-params)
  "Make the turtle to draw around the form."
  (make-instance '<turtle>
                 :location (make-form-start-point (skeleton the-form)
                                                  pen-params)
                 :direction (- (/ pi 2.0))))

(defun make-form-from-points (points)
  "Make a form, ready to be started."
  (log:info "Making form points.")
  (let* ((skel (make-polyline-from-points points))
         (the-form (make-instance '<form>
                                  :skeleton (vector skel)
                                  :bounds (bounds skel))))
    ;;(draw-form the-form) ;; Remove for codelets
    the-form))

(defun path-ready-to-close (the-form pen-params)
  (and (> (form-point-count the-form) 2) ;; Ignore very first point
       (< (distance (most-recent-point the-form)
                    (form-first-point the-form))
          (move-step pen-params))))

(defun path-timeout (the-form)
  "Make sure that a failure of the form algorithm hasn't resulted in a loop."
  (let ((has-timed-out (> (form-point-count the-form)
                          +form-step-limit+)))
    (when has-timed-out
      (log:debug "ERROR: FORM PATH TIMED OUT ============================"))
    has-timed-out))

(defun should-finish-form (the-form pen-params)
  "Decide whether the form should finish."
  (or (path-ready-to-close the-form pen-params)
      (path-timeout the-form)))

(defun draw-form (the-form pen-params)
  "Find the next point forward along the drawn outline of the shape."
  (let* ((form-bounds (bounds the-form))
         (the-outline (outline the-form))
         (the-turtle (make-form-turtle the-form pen-params)))
    (log:info "Drawing form.")
    (append-point the-outline (location the-turtle))
    (loop until (should-finish-form the-form pen-params)
          do (progn
               (adjust-next-pen (skeleton the-form) pen-params the-turtle)
               (forward the-turtle (move-step pen-params))
               (let ((new-location (location the-turtle)))
                 (append-point the-outline new-location)
                 (include-point form-bounds new-location))))
    (append-point the-outline (form-first-point the-form))))

(defmacro do-drawing-forms ((drawing form-variable-name) &body body)
  "Run code for each form of each figure of a drawing."
  (let ((plane-var (gensym))
        (figure-var (gensym)))
    `(loop for ,plane-var across (planes ,drawing)
           do (loop for ,figure-var across (figures ,plane-var)
                    do (loop for ,form-variable-name across (forms ,figure-var)
                             do (progn ,@body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generating the point population for the composition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-composition-points (the-drawing count)
  "Generate the points on the image plane that the composition will use."
  (log:info "Making ~d composition points." count)
  (let* ((b (bounds the-drawing))
         (corner-count (random-number 4))
         (interior-count (random-number (- count
                                           corner-count)))
         (edge-count (- count
                        interior-count
                        corner-count)))
    (setf (composition-points the-drawing)
          (concatenate 'vector
                       (random-points-at-rectangle-corners b corner-count)
                       (random-points-on-rectangle b edge-count)
                       (random-points-in-rectangle b interior-count)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convex Hull
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-hull-figures (the-points count low-count high-count)
  "Make count hull figures. They may overlap or touch. Todo: prevent this."
  (log:info "Making ~d hull figure(s) for plane." count)
  (map-into (make-array count)
            (lambda ()
              (make-figure-from-points
               (points (convex-hull
                        (choose-n-of (random-range low-count
                                                   (min high-count
                                                        (length the-points)))
                                     the-points)))))))

(defconstant +min-hulls-per-plane+ 1)
(defconstant +max-hulls-per-plane+ 4)
(defconstant +min-hull-points+ 3)
(defconstant +max-hull-points+ 12)

(defun make-hull-figures-for-plane (points)
  "The plane population policy using hulls. "
  (make-hull-figures points
                     (random-range +min-hulls-per-plane+
                                   +max-hulls-per-plane+)
                     +min-hull-points+
                     (min (length points) +max-hull-points+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polygons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-polygon-figures (points count low-count high-count)
  "Make count polygon figures. They may overlap or touch. Todo: prevent this."
  (log:info "Making ~d polygon figure(s) for plane." count)
  (let ((polygons (make-array count)))
    (map-into
     polygons
     (lambda ()
       (make-figure-from-points (choose-n-of (random-range low-count
                                                           high-count)
                                             points))))))

(defconstant +min-polygons-per-plane+ 1)
(defconstant +max-polygons-per-plane+ 5)
(defconstant +min-polygon-points+ 3)
(defconstant +max-polygon-points+ 12)

(defun make-polygon-figures-for-plane (points)
  "The plane population policy using polygons. "
  (make-polygon-figures points
                        (random-range +min-polygons-per-plane+
                                      +max-polygons-per-plane+)
                        +min-polygon-points+
                        (min (length points) +max-polygon-points+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-line-figure (points)
  "Make a line figure using two of the points."
  (let ((p1p2 (choose-n-of 2 points)))
    (make-instance '<figure>
                   :forms
                   (make-instance '<form>
                                  :contents
                                  (make-instance '<line>
                                                 :from (first p1p2)
                                                 :to (second p1p2))))))

(defun make-line-figures (points count)
  "Make count line figures. They may overlap or touch. Todo: ensure they don't."
  (log:info "Making ~d line figure(s) for plane." count)
  (let ((lines (make-array count)))
    (map-into lines
              (lambda () (make-figure-from-points (choose-n-of 2 points))))))

(defconstant +min-lines-per-plane+ 1)
(defconstant +max-lines-per-plane+ 8)

(defun make-line-figures-for-plane (points)
  "The plane population policy using liness. "
  (make-line-figures points
                     (random-range +min-lines-per-plane+
                                   (min (floor (/ (length points) 2.0))
                                        +max-lines-per-plane+))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Points
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-point-figure (point)
  "Make a point figure."
  (make-instance '<figure>
                 :forms (make-instance '<form>
                                       :contents point)))

(defun make-point-figures (points count)
  "Make count point figures."
  (log:info "Making ~d point figure(s) for plane." count)
  (let ((source-points (choose-n-of count points))
        (point-figures (make-array count :fill-pointer 0)))
    (loop for p across source-points
          do (vector-push-extend (make-figure-from-points (vector p))
                                 point-figures))
    point-figures))

(defconstant +min-points-per-plane+ 1)
(defconstant +max-points-per-plane+ 12)

(defun make-point-figures-for-plane (points)
  "The plane population policy using points. "
  (make-point-figures points
                      (random-range +min-points-per-plane+
                                    (min (length points)
                                         +max-points-per-plane+))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Figure generation method selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *figure-generation-method-list*
  '(make-hull-figures-for-plane
    make-polygon-figures-for-plane
    make-line-figures-for-plane
    make-point-figures-for-plane))

(defun figure-generation-methods (count)
  (choose-n-of-ordered count *figure-generation-method-list*))

