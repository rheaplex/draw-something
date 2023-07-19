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
   (ground :accessor ground
           :type <colour>
           :initform nil
           :initarg :ground
           :documentation "The flat background colour of the drawing.")
   (minimum-separation :accessor min-sep
                       :initarg :min-sep
                       :documentation "The minimum point/line seperation required for drawing")
   (points :accessor points
           :type vector
           :initarg :composition-points
           :initform (make-array 1 :adjustable t
                                   :fill-pointer 0)
           :documentation "The points for the drawing"))
  (:documentation "A drawing in progress."))

(defun make-drawing (&key substrate-bounds min-sep bounds)
  (make-instance '<drawing>
                 :substrate-bounds substrate-bounds
                 :bounds bounds
                 :min-sep min-sep))

(defmethod print-object ((object <drawing>) stream)
  "Make a human readable string describing the drawing."
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(SUBSTRATE-BOUNDS: ~a BOUNDS: ~a MIN-SEP: ~a GROUND: ~a PLANES:~%~a)"
          (substrate-bounds object)
          (bounds object)
          (min-sep object)
          (ground object)
          (planes object))))

(defmacro do-drawing-forms ((drawing form-variable-name) &body body)
  "Run code for each form of each figure of a drawing."
  (let ((plane-var (gensym))
        (figure-var (gensym)))
    `(loop for ,plane-var across (planes ,drawing)
           do (loop for ,figure-var across (figures ,plane-var)
                    do (loop for ,form-variable-name
                               across (forms ,figure-var)
                             do (progn ,@body))))))
