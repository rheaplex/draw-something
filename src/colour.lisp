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
;; Colour as hsb (and rgb conversion).
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
   (brightness :accessor brightness
               :initform 1.0
               :initarg :brightness
               :documentation "The brightness of the colour."))
  (:documentation "A colour"))

(defun hsb-to-rgb (col)
  "Convert the hue/saturation/brightness colour to RGB."
  (if (= (saturation col) 0)
      (values-list (list (brightness col) (brightness col) (brightness col)))
      (let* ((p (* (brightness col)
                   (- 1.0
                      (saturation col))))
             (q (* (brightness col)
                   (- 1.0
                      (* (saturation col) (hue col)))))
             (tt (* (brightness col)
                    (- 1.0
                       (* (saturation col)
                          (- 1.0 (hue col)))))))
        (case (floor (* (hue col) 5.0))
          ((0) (values-list (list (brightness col) tt p))) ;; Red
          ((1) (values-list (list q (brightness col) p))) ;; Yellow
          ((2) (values-list (list p (brightness col) tt))) ;; Green
          ((3) (values-list (list p q (brightness col)))) ;; Cyan
          ((4) (values-list (list tt p (brightness col)))) ;; Blue
          ((5) (values-list (list (brightness col) p q))))))) ;; Magenta

(defun hsb-to-rgb-hex (col)
  "Convert the hue/saturation/brightness colour to a string #RRGGBB."
  (multiple-value-bind (r g b)
      (hsb-to-rgb col)
    (format nil
            "#~2,'0X~2,'0X~2,'0X"
            (floor (* r 255))
            (floor (* g 255))
            (floor (* b 255)))))
