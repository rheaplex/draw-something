;; draw-something.lisp -  The main lifecycle code for draw-something.
;; Copyright (C) 2006, 2008, 2016, 2021 Rhea Myers.
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
;; Let's go!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +pen-outline-distance+ 5.2)
(defparameter +pen-outline-distance-tolerance+ 0.7)
(defparameter *pen-params*
  (make-pen-parameters  :move-step          1.3 ;;1.0
                        :distance           +pen-outline-distance+
                        :distance-tolerance +pen-outline-distance-tolerance+
                        :turn-step          0.01 ;;0.1
                        :drift-probability  0.0
                        :drift-range        0.0))  ;;0.1

;;TODO work this in to the drawing but not the ground
(defparameter *border-width* (+ +pen-outline-distance+
                                +pen-outline-distance-tolerance+))

;; 11in x 14in
(defparameter +page-size+ '(7680 . 4320))
(defparameter +drawing-width+ 7680)
(defparameter +drawing-height+ 4320)
(defparameter +drawing-x+ (/ (- (car +page-size+) +drawing-width+) 2.0))
(defparameter +drawing-y+ (/ (- (cdr +page-size+) +drawing-height+) 2.0))

(defparameter +planes-count+ 4)

(defparameter +planes-figures-max+ (vector 8 32 64 (random-range 1024 4096)))
(defparameter +planes-sizes-min+ #(2 3 5 16))
(defparameter +planes-sizes-max+ #(1 2 1 16))

(defun generate-filename ()
  "Make a unique filename for the drawing, based on the current date & time."
  (multiple-value-bind (seconds minutes hours date month year)
      (decode-universal-time (get-universal-time))
    (format nil
            "~a-~2,,,'0@A~2,,,'0@A~2,,,'0@A-~2,,,'0@A~2,,,'0@A~2,,,'0@A"
            "drawing" year month date hours minutes seconds)))

(defun draw-something (&key (randseed nil) (savedir nil) (filename nil))
  "Make the drawing data structures and create the image."
  (log-info "Starting draw-something.")
  (random-init (or randseed (get-universal-time)))
  (let ((drawing (make-drawing :bounds
                               (make-rectangle :x +drawing-x+
                                               :y +drawing-y+
                                               :width +drawing-width+
                                               :height +drawing-height+)
                               :substrate-bounds
                               (make-rectangle :x 0
                                               :y 0
                                               :width (car +page-size+)
                                               :height (cdr +page-size+))
                               :min-sep
                               (* +pen-outline-distance+ 3)))
        (colours (create-colours (+ (length *figure-generation-method-list*)
                                    1)
                                 20)))

    (log-info "Drawing created: ~a." drawing)
    (format t "Colour buckets: ~a~%" colours)
    (setf (ground drawing) (choose-colour-for colours 0))
    ;; Proceed plane by plane, figure by figure.
    (dotimes (i (length *figure-generation-method-list*))
      (log-info "~%Plane: ~d~%" i)
      (let ((plane (make-plane :pen-params *pen-params*)))
        (dotimes (j (aref +planes-figures-max+ i))
          (log-info "Trying to produce figure ~d of plane ~d" j i)
          ;; Give up on first failure.
          (unless (funcall (nth i *figure-generation-method-list*)
                           drawing
                           plane
                           (floor (min +drawing-width+ +drawing-height+)
                                  (aref +planes-sizes-min+ i))
                           (floor (min +drawing-width+ +drawing-height+)
                                  (aref +planes-sizes-max+ i)))
            (return)))
        (log-info "Colouring forms.")
        (do-plane-forms (plane form)
          (setf (fill-colour form)
                (choose-colour-for colours (+ i 1))))
        ;; Add here so its points don't count for searches when making it.
        (vector-push-extend plane (planes drawing))))
    (log-info "Finished drawing: ~a" drawing)
    (let ((filepath (write-drawing drawing
                                   (or savedir
                                       (make-pathname :directory
                                                      '(:relative "drawings")))
                                   (or filename
                                       (generate-filename)))))
      ;; Make sure this goes to stdout
      (format t "Wrote file to: ~a~%" filepath))
    (log-info "Finished draw-something.")
    drawing))
