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

(defclass <drawing> (<tagged>)
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
           :documentation "The flat background colour of the drawing.")
   (composition-points :accessor composition-points
                       :type vector
                       :initarg :composition-points
                       :initform (make-array 1 :adjustable t
                                               :fill-pointer 0)
                       :documentation "The points for the composition"))
  (:documentation "A drawing in progress."))

(defun make-drawing (&key substrate-bounds bounds colour-scheme-applier)
  (log-info "Making drawing.")
  (log-info "Bounds: ~a ." bounds)
  (make-instance '<drawing>
                 :substrate-bounds substrate-bounds
                 :bounds bounds
                 :colour-scheme-applier colour-scheme-applier 
                 :ground (choose-colour-for colour-scheme-applier
                                            'background)))

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


;; Do these all the way down, drawing to geometric..

;;(defgeneric recent-tags ((subject <tagged>)))
;;(defgeneric recent-colours ((subject <polychrome>) &key count))
#|(defgeneric intersects-tags (bounds tagged))
(defgeneric intersects-colours (bounds polychrome))

(defmethod recent-colours ((drawing <drawing>))
  "Get every colour used to fill or stroke a form recently."
  (let ((colours '()))
    (do-drawing-forms (drawing form)
      (when (fill-colour form)
        (push (fill-colour form) colours))
      (when (stroke-colour form)
        (push (stroke-colour form) colours)))
    (reverse colours)))

(defmethod recent-tags ((drawing <drawing>))
  "Get every tag applied recently."
  (let ((tags '()))
    (do-drawing-forms (drawing form)
      (when (fill-colour form)
        (push (fill-colour form) colours))
      (when (stroke-colour form)
        (push (stroke-colour form) colours)))
    (reverse tags)))
|#
