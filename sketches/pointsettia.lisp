;; pointsettia.lisp - Places random points on a plane.
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

;; Make sure you have draw-something and sketch in local-projects,
;; e.g. in ~/quicklisp/local-projects/ or ~/.roswell/local-projects/ .

(ql:quickload '(:draw-something :sdl2 :sdl2kit :sketch))

(defpackage :pointsettia
  (:use :cl :sketch :draw-something))

(in-package :pointsettia)

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

(defparameter +point-count+ 64)
(defparameter +point-radius+ 2)

(defparameter +layer-skeleton-colours+
  #("red" "orange" "green" "blue" "pink" "purple" "brown"))

(ds::random-init (get-universal-time))

(defvar the-drawing
  (ds::make-drawing :substrate-bounds
                    (ds::make-rectangle :x 0
                                        :y 0
                                        :width +page-width+
                                        :height +page-height+)
                    :bounds
                    (ds::make-rectangle :x +drawing-x+
                                        :y +drawing-y+
                                        :width +drawing-width+
                                        :height +drawing-height+)
                    :colour-scheme-applier
                    (ds::make-colour-scheme-applier
                     :scheme (ds::default-colour-scheme)
                     :spec-list (ds::chooser-spec))))

(defsketch pointsettia ((title "pointsettia")
                        (width +page-width+)
                        (height +page-height+)
                        (y-axis :up))
  (when (< (length (ds::composition-points the-drawing)) +point-count+)
    (vector-push-extend (ds::random-point-in-rectangle
                         (ds::bounds the-drawing))
                        (ds::composition-points the-drawing)))
  (rect +drawing-x+ +drawing-y+ +drawing-width+ +drawing-height+)
  (loop for p across (ds::composition-points the-drawing)
        do (circle (ds::x p) (ds::y p) +point-radius+)))

(defmethod kit.sdl2:mousebutton-event ((window pointsettia) state ts b x y)
  (when (eq state :mousebuttondown)
    (setf (ds::composition-points the-drawing)
           (make-array 1 :adjustable t
                         :fill-pointer 0))))

#-sbcl (make-instance 'pointsettia)
#+sbcl (sdl2:make-this-thread-main #'(lambda ()
                                       (make-instance 'pointsettia)))


#|
;; Alternative, very unbundled solution to Skecth in macOS under sbcl
(ql:quickload '(:sdl2 :sdl2kit :sketch-examples :trivial-main-thread))
(kit.sdl2:with-start (:this-thread-p t)
  (sdl2-ttf:init)
  (setf sketch::*initialized* t)
  (sb-int:set-floating-point-modes :traps nil)
  (sdl2:gl-set-attr :multisamplesamples 4)
  (sdl2:gl-set-attr :context-profile-mask 1)
  (sdl2:gl-set-attr :context-major-version 3)
  (sdl2:gl-set-attr :context-minor-version 3)
  (make-instance 'sketch-examples:sinewave)
  ;;(make-instance 'kit.sdl2.test:cube-window :w 400 :h 400)
  )
|#
