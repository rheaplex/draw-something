;; groundhog-c.lisp - Finds free rectangular spaces on a plane.
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

(defparameter +num-bounds+ 8)

(defvar *search-state*)
(defvar *boundss*)

(defclass <growable-rectangle> (ds::<rectangle>)
  ((directions :initform '(left right up down)
               :accessor directions)
   (min :initarg :min
        :accessor min-rect)
   (max :initarg :max
        :accessor max-rect)
   (within :initarg :within
           :accessor within-rect)
   (avoid :initarg :avoid
          :accessor avoid-rects)))

(defmethod initialize-instance :after ((o <growable-rectangle>)
                                       &key
                                         initial-x
                                         initial-y)
  (setf (ds::x o) initial-x)
  (setf (ds::y o) initial-y)
  (setf (ds::width o) 1)
  (setf (ds::height o) 1))

(defmethod can-grow-rectangle-p ((rect <growable-rectangle>))
  (not (null (directions rect))))

(defmethod found-rectangle-p ((rect <growable-rectangle>))
  (and (>= (ds::width rect) (ds::width (min-rect rect)))
       (>= (ds::height rect) (ds::height (min-rect rect)))))

(defmethod search-finished-p ((rect <growable-rectangle>))
  (or (found-rectangle-p rect)
      (not (can-grow-rectangle-p rect))))

;;FIXME Break this up.
(defmethod grow-rectangle ((rect <growable-rectangle>))
  (let ((new-rect (ds::copy-rectangle rect))
        (which-way (ds::choose-one-of (directions rect))))
    (case which-way
      ;; Try to grow in direction.
      (up
       (incf (ds::height new-rect)))
      (down
       (incf (ds::height new-rect))
       (decf (ds::y new-rect)))
      (left
       (decf (ds::x new-rect))
       (incf (ds::width new-rect)))
      (right
       (incf (ds::width new-rect))))
    ;; If we collided or spilled over, stop growing in that direction.
    (when (or (ds::intersects-any new-rect (avoid-rects rect))
              (not (ds::contains (within-rect rect) new-rect)))
      (setf new-rect nil)
      (setf (directions rect)
            (remove which-way (directions rect))))
    (when new-rect
      (case which-way
        (up
         (setf (ds::height rect) (ds::height new-rect)))
        (down
         (setf (ds::height rect) (ds::height new-rect)
               (ds::y rect) (ds::y new-rect)))
        (left
         (setf (ds::x rect) (ds::x new-rect)
               (ds::width rect) (ds::width new-rect)))
        (right
         (setf (ds::width rect) (ds::width new-rect))))
      ;; If the rect has reached its maximum width,
      ;; don't grow any further to the left or right
      (when (= (ds::width rect) (ds::width (max-rect rect)))
        (setf (directions rect)
              (remove-if #'(lambda (x) (member x '(left right)))
                         (directions rect))))
      ;; If the rect has reached its maximum height,
      ;; don't grow any further at the top or bottom.
      (when (= (ds::height rect) (ds::height (max-rect rect)))
        (setf (directions rect)
              (remove-if #'(lambda (x) (member x '(up down)))
                         (directions rect))))))
  rect)

(defun init-search ()
  (let ((initial-rect (ds::random-rectangle-in-rectangle-size +drawing-bounds+
                                                              1
                                                              1)))
    (setf *search-state* (make-instance '<growable-rectangle>
                                        :min +min-bounds+
                                        :max +max-bounds+
                                        :within +drawing-bounds+
                                        :avoid *boundss*
                                        :initial-x (ds::x initial-rect)
                                        :initial-y (ds::y initial-rect)))))

(defun search-step ()
  (grow-rectangle *search-state*))

(defun init-groundhog ()
  (setf *boundss* (make-array 0 :adjustable t :fill-pointer 0)))

(init-groundhog)
(init-search)

(defsketch groundhog ((title "groundhog")
                      (width +page-width+)
                      (height +page-height+)
                      (y-axis :up))
  (rect +drawing-x+ +drawing-y+ +drawing-width+ +drawing-height+)
  (when (< (length *boundss*) +num-bounds+)
    (when (search-finished-p *search-state*)
      (when (found-rectangle-p *search-state*)
        (vector-push-extend (ds::copy-rectangle *search-state*) *boundss*))
      (init-search))
    (let ((rect (search-step)))
      (with-pen (make-pen :stroke +red+)
        (rect (ds::x rect)
              (ds::y rect)
              (ds::width rect)
              (ds::height rect)))))
  (loop for b across *boundss*
        do (rect (ds::x b) (ds::y b) (ds::width b) (ds::height b))))

  (defmethod kit.sdl2:mousebutton-event ((window groundhog) state ts b x y)
    (when (eq state :mousebuttondown)
      (init-groundhog)
      (init-search)))

  #-sbcl (make-instance 'groundhog)
  #+sbcl (sdl2:make-this-thread-main #'(lambda ()
                                         (make-instance 'groundhog)))
