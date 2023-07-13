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

(in-package :draw-something)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The top-level drawing object.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <drawing> ()
  ((substrate-bounds :accessor substrate-bounds
                     :type <rectangle>
                     :initarg :substrate-bounds
                     :documentation "The paper/screen/window dimensions.")
   (bounds :accessor bounds
           :type <rectangle>
           :initarg :bounds
           :documentation "The dimensions of the drawing.")
   (planes :accessor planes
           :type vector
           :initarg :planes
           :initform (make-array 1 :adjustable t :fill-pointer 0)
           :documentation "The planes of the drawing.")
   (colour-scheme-applier :accessor colour-scheme-applier
                          :initarg :colour-scheme-applier
                          :documentation "The colour for the drawing,")
   (ground :accessor ground
           :type <colour>
           :initarg :ground
           :initform (make-colour :hue 0 :saturation 0 :brightness 1)
           :documentation "The flat background colour of the drawing.")
   (minimum-separation :accessor min-sep
                       :initarg :min-sep
                       :documentation "The minimum point/line seperation required for drawing")
   (composition-points :accessor composition-points
                       :type vector
                       :initarg :composition-points
                       :initform (make-array 1 :adjustable t
                                               :fill-pointer 0)
                       :documentation "The points for the composition"))
  (:documentation "A drawing in progress."))

(defun make-drawing (&key substrate-bounds min-sep bounds colour-scheme-applier)
  (log-info "Making drawing: bounds: ~a ." bounds)
  (make-instance '<drawing>
                 :substrate-bounds substrate-bounds
                 :bounds bounds
                 :min-sep min-sep
                 :colour-scheme-applier colour-scheme-applier 
                 :ground
                 (choose-colour-for colour-scheme-applier 'background)))

(defun choose-colour (drawing symbol)
  (choose-colour-for (colour-scheme-applier drawing) symbol))

(defmacro do-drawing-forms ((drawing form-variable-name) &body body)
  "Run code for each form of each figure of a drawing."
  (let ((plane-var (gensym))
        (figure-var (gensym)))
    `(loop for ,plane-var across (planes ,drawing)
           do (loop for ,figure-var across (figures ,plane-var)
                    do (loop for ,form-variable-name
                               across (forms ,figure-var)
                             do (progn ,@body))))))
