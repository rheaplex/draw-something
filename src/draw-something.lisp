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

(defconstant +drawing-size+ 600.0)
(defconstant +num-skeleton-points+ 12)

;; defconstant isn't happy here o_O
(defparameter *pen-params*
  (make-instance '<pen-parameters>
                 :move-step          2.0 ;;1.0
                 :distance           5.0
                 :distance-tolerance 1.5
                 :turn-step          0.05 ;;0.1
                 :drift-probability  0.0
                 :drift-range        0.0)) ;;0.1

(defparameter *border-width* (* 2 (pen-distance *pen-params*)))

(defun draw-something (pathspec randseed)
  "Make the drawing data structures and create the image."
  (advisory-message "Starting draw-something.~%")
  (random-init randseed)
  (let* ((drawing-bounds (make-instance '<rectangle>
                                               :x 0
                                               :y 0
                                               :width +drawing-size+
                                               :height +drawing-size+))
         (point-bounds (inset-rectangle drawing-bounds *border-width*))
         (skeleton-points (random-points-in-rectangle point-bounds
                                                      +num-skeleton-points+))
         (form (make-form-from-points skeleton-points)))
         ;;(the-drawing (make-instance '<drawing> :bounds drawing-bounds)))
         ;;(make-composition-points the-drawing 12) ;; (random-range 8 42))
    ;; (make-planes the-drawing number-of-planes))
    ;;(vector-push-extend
     ;;(make-instance '<plane>
    ;;              :figure-count 1
    ;;              :figure-policy point-method
    ;;              :pen ))
    ;;(planes the-drawing)
    ;;(make-planes-skeletons the-drawing)
    ;;(draw-planes-figures the-drawing)
    ;;(colour-objects the-drawing *all-object-symbols*)
    (draw-form form *pen-params*)
    (advisory-message "Finished drawing.~%")
    (let ((filepath (write-svg-form form drawing-bounds pathspec)))
      (advisory-message "Finished draw-something.~%")
      filepath)))

#|(defun draw-something (&optional (pathspec nil))
  "The main method that generates the drawing and writes it to file."
  (advisory-message "Starting draw-something.~%")
  (setf *random-state* (make-random-state t))
  ;;(format t "Random state: ~a.~%" (write-to-string *random-state*))
  (let ((the-drawing (generate-drawing)))
    (advisory-message "Finished drawing.~%")
    (let ((filepath (write-svg the-drawing pathspec)))
      (advisory-message "Finished draw-something.~%")
      filepath)))|#
