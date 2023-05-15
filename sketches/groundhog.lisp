;; groundhog.lisp - Finds free rectangular spaces on a plane.
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

;; Make sure you have draw-something and sketch in local-projects,
;; e.g. in ~/quicklisp/local-projects/ or ~/.roswell/local-projects/ .

;;FIXME: Rectangles must be within drawing bounds.
;;       This is made compelx by wrap-around
;;       and by the fact that when we decreate search rectangle size
;;       this will have to move the lower Y limit of the search down
;;       and the right X limit to the right.
;;FIXME: This code is complex - it would be simpler with yield()
;;       Or with a different search stragegy.

(ql:quickload '(:draw-something :sdl2 :sdl2kit :sketch))

(defpackage :groundhog
  (:use :cl :sketch :draw-something))

(in-package :groundhog)

(defparameter +pen-outline-distance+ 5.2)
(defparameter +pen-outline-distance-tolerance+ 0.7)
;; defconstant isn't happy here o_O
(defparameter *pen-params*
  (ds::make-pen-parameters
   :move-step          1.3 ;;1.0
   :distance           +pen-outline-distance+
   :distance-tolerance +pen-outline-distance-tolerance+
   :turn-step          0.01 ;;0.1
   :drift-probability  0.0
   :drift-range        0.0)) ;;0.1

;;TODO work this in to the drawing but not the ground
(defparameter *border-width* (+ +pen-outline-distance+
                                +pen-outline-distance-tolerance+))

;; 11in x 14in
(defparameter +page-width+ 960)
(defparameter +page-height+ 540)
(defparameter +drawing-width+ 900)
(defparameter +drawing-height+ 480)
(defparameter +drawing-x+ (/ (- +page-width+ +drawing-width+) 2.0))
(defparameter +drawing-y+ (/ (- +page-height+ +drawing-height+) 2.0))

(defparameter +forms-count+ 24)
(defparameter +form-size-min+ 100)
(defparameter +form-size-max+ 100)

(defparameter +layer-skeleton-colours+
  #("red" "orange" "green" "blue" "pink" "purple" "brown"))

(ds::random-init (get-universal-time))


 (defparameter +substrate-bounds+
   (ds::make-rectangle :x 0
                       :y 0
                       :width +page-width+
                       :height +page-height+))
(defparameter +min-bounds+
                     (ds::make-rectangle :x 0
                                         :y 0
                                         :width (/ +drawing-width+ 8)
                                         :height (/ +drawing-height+ 8)))
(defparameter +max-bounds+
  (ds::make-rectangle :x 0
                      :y 0
                      :width (/ +drawing-width+ 4)
                      :height (/ +drawing-height+ 4)))
(defparameter +drawing-bounds+
  (ds::make-rectangle :x +drawing-x+
                      :y +drawing-y+
                      :width +drawing-width+
                      :height +drawing-height+))

(defvar *drawing*
  (ds::make-drawing :substrate-bounds +substrate-bounds+
                    :bounds +drawing-bounds+
                    :colour-scheme-applier
                    (ds::make-colour-scheme-applier
                     :scheme (ds::default-colour-scheme)
                     :spec-list (ds::chooser-spec))))
(defvar *fs* nil)
(defvar *dtl-h* nil)
(defvar *dtl-v* nil)
(defvar *candidate* nil)
(defvar *num-bounds* 8)
(defvar *boundss* (make-array 0 :adjustable t :fill-pointer 0))

;; A "dotimesloop" iterator state.
;; It generates start .. 1 below to, then from .. 1 below start.
;; e.g. 10 15 20 gives 15, 16, 17, 18, 19, 10, 11, 12, 13, 14
;; Once done, the current value is nil.

(defstruct (dtl (:constructor make-dtl (from start to
                                        &aux (current start)
                                          (which-phase 1))))
  from
  start
  to
  which-phase
  current)

(defun dtl-finished-p (iter)
  (null (dtl-which-phase iter)))

;; This is not very efficient.

(defun dotimesloop-step (iter)
  ;; To handle cases where start equals from or to,
  ;; We keep track of which loop we are on (start..to,
  ;; followed by from..start).
  ;; >= catches where they started equal and we incfed them.
  (case (dtl-which-phase iter)
    (1 (when (>= (dtl-current iter) (dtl-to iter))
         (setf (dtl-which-phase iter) 2)
         (setf (dtl-current iter) (dtl-from iter))))
    (2  (when (>= (dtl-current iter) (dtl-start iter))
          (setf (dtl-current iter) nil)
          (setf (dtl-which-phase iter) nil))))
  ;; We wouldn't need this test at the start of the function,
  ;; But it's simpler to have the incf here, if less efficient.
  (unless (dtl-finished-p iter)
    (incf (dtl-current iter)))
  iter)

;; A find space state struct.

(defstruct (fs)
  in-rect
  min-rect
  max-rect
  current-rect
  steps
  width-step-size
  height-step-size)

(defun fs-finished-p (fss)
  (= (fs-steps fss) 0))

(defun find-space-in-rect ()
  (let ((candidate
          (ds::make-rectangle
           :x (dtl-current *dtl-h*)
           :y (dtl-current *dtl-v*)
           :width (ds::width (fs-current-rect *fs*))
           :height (ds::height (fs-current-rect *fs*)))))
    (setf *candidate* candidate)
    (if (ds::intersects-none candidate *boundss*)
        candidate
        nil)))

(defun shrink-fs-current-rect ()
  (decf (fs-steps *fs*))
  (when (> (fs-steps *fs*) 0)
    (decf (ds::width (fs-current-rect *fs*))
          (fs-width-step-size *fs*))
    (decf (ds::height (fs-current-rect *fs*))
          (fs-height-step-size *fs*))))

(defun fs-reset-dtl-h ()
  (let* ((left (ds::x *drawing*))
         (right (+ (ds::y *drawing*)
                   (ds::width *drawing*)))
         (h-start (ds::random-range left right)))
    (setf *dtl-h* (make-dtl left h-start right))))

(defun update-find-space-state ()
  (dotimesloop-step *dtl-h*)
  (when (dtl-finished-p *dtl-h*)
    (dotimesloop-step *dtl-v*)
    (unless (dtl-finished-p *dtl-v*)
      (shrink-fs-current-rect)
      (unless (fs-finished-p *fs*)
        (fs-reset-dtl-h))))
  nil)

;; Step from large to small size, searching for each
(defun step-find-space ()
  (let ((result (find-space-in-rect)))
    (if result
        result
        (update-find-space-state))))

(defun init-find-space (in-rect min-rect max-rect steps)
  (setf *candidate* nil)
  (setf *fs*
        (make-fs :in-rect in-rect
                 :min-rect min-rect
                 :max-rect max-rect
                 :current-rect max-rect
                 :steps steps
                 :width-step-size (/ (- (ds::width max-rect)
                                        (ds::width min-rect))
                                     steps)
                 :height-step-size (/ (- (ds::height max-rect)
                                         (ds::height min-rect))
                                      steps)))
  (let* ((bounds (ds::bounds *drawing*))
         (left (ds::x bounds))
         (right (+ left (ds::width bounds)))
         (h-start (ds::random-range left right))
         (bottom (ds::y bounds))
         (top (+ bottom (ds::height bounds)))
         (v-start (ds::random-range bottom top)))
    (setf *dtl-v* (make-dtl bottom v-start top))
    (setf *dtl-h* (make-dtl left h-start right))))

(defun reset-find-space-state ()
  (setf *candidate* nil)
  (setf *fs* nil))

(defun try-to-find-space ()
  (unless *fs*
    (init-find-space (ds::bounds *drawing*)
                     +min-bounds+
                     +max-bounds+
                     10))
  ;;(format t "--> ~a ~a ~a~%" *fs* *dtl-v* *dtl-h*)
  (let ((result (step-find-space)))
    (when result
      (vector-push-extend result *boundss*)
      (reset-find-space-state))))

(defsketch groundhog ((title "groundhog")
                      (width +page-width+)
                      (height +page-height+)
                      (y-axis :up))
  (rect +drawing-x+ +drawing-y+ +drawing-width+ +drawing-height+)
  (when (< (length *boundss*) *num-bounds*)
    (try-to-find-space))
  (loop for b across *boundss*
        do (rect (ds::x b) (ds::y b) (ds::width b) (ds::height b)))
  (when *candidate*
    (rect (ds::x *candidate*) (ds::y *candidate*)
          (ds::width *candidate*) (ds::height *candidate*))))

(defmethod kit.sdl2:mousebutton-event ((window groundhog) state ts b x y)
  (when (eq state :mousebuttondown)
    (reset-find-space-state)
    (setf *boundss* (make-array 1 :adjustable t :fill-pointer 0))))

#-sbcl (make-instance 'groundhog)
#+sbcl (sdl2:make-this-thread-main #'(lambda ()
                                       (make-instance 'groundhog)))
