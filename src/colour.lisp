;; colour.lisp - Colour handling.
;; Copyright (C) 2006, 2016, 2021 Rhea Myers.
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
;; Colour as hsl (and rgb conversion).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <colour> ()
  ((hue :accessor hue
        :initform 1.0
        :initarg :hue
        :documentation "The colour-wheel hue of the colour.")
   (saturation :accessor saturation
               :initform 1.0
               :initarg :saturation
               :documentation "The saturation of the colour.")
   (lightness :accessor lightness
               :initform 1.0
               :initarg :lightness
               :documentation "The lightness of the colour."))
  (:documentation "A colour"))

(defmethod print-object ((object <colour>) stream)
  "Make a human readable string describing the rectangle."
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(HUE: ~,2f SATURATION: ~,2f LIGHTNESS: ~,2f)"
          (hue object)
          (saturation object)
          (lightness object))))

(defun make-colour (&key hue saturation lightness)
  "Constuctor function."
  (make-instance '<colour> :hue hue
                           :saturation saturation
                           :lightness lightness))

;; https://stackoverflow.com/a/29316972

(defun hue-to-rgb (p q a)
  (if (< a 0.0)
      (incf a 1.0))
  (if (> a 1.0)
      (decf a 1.0))
  (cond
    ((< a (/ 1.0 6.0))
     (+ p (* (- q p) 6.0 a)))
    ((< a (/ 1.0 2.0))
     q)
    ((< a (/ 2.0 3.0))
     (+ p (- q p) (* (- (/ 2.0 3.0) a) 6.0)))
    (t
     p)))

(defun colour-to-rgb (col)
  "Convert the hue/saturation/lightness colour to RGB."
  (let ((h (/ (hue col) 360.0))
        (s (saturation col))
        (l (lightness col)))
    (cond
      ;; Grey
      ((zerop s)
       (values l l l))
      ;; Black
      ((zerop l)
       (values 0.0 0.0 0.0))
      (t
       (let* ((q (if (< l 0.5) (* l (1+ s)) (- (+ l s) (* l s))))
              (p (-  (* 2.0 l) q)))
         (values (hue-to-rgb p q (+ h (/ 1.0 3.0)))
                 (hue-to-rgb p q h)
                 (hue-to-rgb p q (- h (/ 1.0 3.0)))))))))

(defun colour-to-rgb-hex (col)
  "Convert the hue/saturation/lightness colour to a string #RRGGBB."
  (multiple-value-bind (r g b)
      (colour-to-rgb col)
    (format nil
            "#~2,'0X~2,'0X~2,'0X"
            (floor (* r 255))
            (floor (* g 255))
            (floor (* b 255)))))

(defun colour-to-rgb-vector (col)
  "Convert hsb to #(r g b)."
  (multiple-value-bind (r g b) (colour-to-rgb col)
    (vector r g b)))
