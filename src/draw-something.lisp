;;  draw-something.lisp -  The main lifecycle code for draw-something.
;;  Copyright (C) 2006, 2016, 2021 Rhea Myers
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

;; 11in x 14in
(defparameter +page-size+ '(1056 . 1344))
(defparameter +drawing-size+ '(900 . 900))
(defparameter +drawing-x+ (/ (- (car +page-size+) (car +drawing-size+)) 2.0))
(defparameter +drawing-y+ (/ (- (cdr +page-size+) (cdr +drawing-size+)) 2.0))

;; defconstant isn't happy here o_O
(defparameter *pen-params*
  (make-instance '<pen-parameters>
                 :move-step          1.3 ;;1.0
                 :distance           5.2
                 :distance-tolerance 0.7
                 :turn-step          0.01 ;;0.1
                 :drift-probability  0.0
                 :drift-range        0.0)) ;;0.1

(defparameter *border-width* (* 2 (pen-distance *pen-params*)))

(defun draw-something (pathspec randseed)
  "Make the drawing data structures and create the image."
  (advisory-message "Starting draw-something.~%")
  (random-init randseed)
  (let* ((drawing-bounds (make-instance '<rectangle>
                                        :x +drawing-x+
                                        :y +drawing-y+
                                        :width (car +drawing-size+)
                                        :height (cdr +drawing-size+)))
         (the-drawing (make-instance '<drawing> :bounds drawing-bounds)))
    (make-composition-points the-drawing (random-range 8 42))
    (make-planes the-drawing (number-of-planes))
    (planes the-drawing)
    (make-planes-skeletons the-drawing)
    (draw-planes-figures the-drawing)
    (colour-objects the-drawing *all-object-symbols*)
    (advisory-message "Finished drawing.~%")
    (write-svg +page-size+ the-drawing pathspec)
    (advisory-message "Finished draw-something.~%")))
