;;  drawing.lisp - A drawing.
;;  Copyright (C) 2006, 2010, 2021 Rhea Myers
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

(defconstant +min-drawing-size+ 200.0)
(defconstant +max-drawing-size+ 600.0)

(defclass <drawing> ()
  ((bounds :accessor bounds
           :type rectangle
           :initarg :bounds
           :documentation "The dimensions of the drawing.")
   (planes :accessor planes
           :type vector
           :initarg :planes
           :initform (make-vector 10)
           :documentation "The planes of the drawing.")
   (ground :accessor ground
           :type colour
           :initarg :ground
           :initform nil
           :documentation "The flat background colour of the drawing.")
   (composition-points :accessor composition-points
                       :type vector
                       :initarg :composition-points
                       :initform (make-vector 10)
                       :documentation "The points for the composition"))
  (:documentation "A drawing in progress."))

(defmethod initialize-instance :after ((drawing <drawing>) &key)
  (advisory-message "Making drawing.~%")
  (advisory-message (format nil
                            "Bounds: ~a .~%"
                            (rectangle-string (bounds drawing)))))
