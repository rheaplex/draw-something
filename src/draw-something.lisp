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

;; 11in x 14in
(defparameter +page-size+ '(7680 . 4320))
(defparameter +drawing-width+ 7680)
(defparameter +drawing-height+ 4320)
;; (defparameter +page-size+ '(1280 . 768))
;; (defparameter +drawing-width+ 1280)
;; (defparameter +drawing-height+ 768)
(defparameter +drawing-x+ (/ (- (car +page-size+) +drawing-width+) 2.0))
(defparameter +drawing-y+ (/ (- (cdr +page-size+) +drawing-height+) 2.0))

(defparameter +planes-count+ 4)

(defparameter +planes-figures-max+ (vector 4 4 16 16 16 16 16 16 16))
(defparameter +planes-sizes-min+ #(2 3 5 8 8 8 8 8 8))
(defparameter +planes-sizes-max+ #(1 2 1 4 4 4 4 4 4))

;; (defparameter +pen-step-max+ 12)
;; (defparameter +pen-step-min+ 2)
;; (defparameter +pen-distance-max+ 50)
;; (defparameter +pen-distance-min+ 10)
;; (defparameter +pen-tolerance-max+ 10)
;; (defparameter +pen-tolerance-min+ 4)
(defparameter +pen-unit-scale+ nil)
(defparameter +pen-step-max+ nil)
(defparameter +pen-step-min+ nil)
(defparameter +pen-distance-max+ nil)
(defparameter +pen-distance-min+ nil)
(defparameter +pen-tolerance-max+ nil)
(defparameter +pen-tolerance-min+ nil)

;;Todo work this in to the drawing but not the ground
(defparameter *border-width* nil)

(defun gen-params ()
  (setf +pen-unit-scale+
        (random-range 1 (floor (/ (min +drawing-width+
                                       +drawing-height+)
                                  512))))
  (setf +pen-step-max+ (* 12 +pen-unit-scale+))
  (setf +pen-step-min+ (* 2 +pen-unit-scale+))
  (setf +pen-distance-max+ (* 50 +pen-unit-scale+))
  (setf +pen-distance-min+ (* 10 +pen-unit-scale+))
  (setf +pen-tolerance-max+ (* 10 +pen-unit-scale+))
  (setf +pen-tolerance-min+ (* 4 +pen-unit-scale+))
  (setf *border-width* (+ +pen-distance-max+ +pen-tolerance-max+)))

(defun generate-filename ()
  "Make a unique filename for the drawing, based on the current date & time."
  (multiple-value-bind (seconds minutes hours date month year)
      (decode-universal-time (get-universal-time))
    (format nil
            "~a-~2,,,'0@A~2,,,'0@A~2,,,'0@A-~2,,,'0@A~2,,,'0@A~2,,,'0@A"
            "drawing" year month date hours minutes seconds)))

(defun draw-everything (drawing)
  ;; Proceed plane by plane, figure by figure.
  (dotimes (i (length *figure-generation-method-list*))
    (let ((plane (make-plane :pen-params
                             (generate-pen-parameters +pen-step-max+
                                                      +pen-step-min+
                                                      +pen-distance-max+
                                                      +pen-distance-min+
                                                      +pen-tolerance-max+
                                                      +pen-tolerance-min+
                                                      (length *figure-generation-method-list*)
                                                      i)))
          (points-for-plane (shuffle (drawing-points drawing)))
          (min-size (floor (min +drawing-width+ +drawing-height+)
                           (aref +planes-sizes-min+ i)))
          (max-size (floor (min +drawing-width+ +drawing-height+)
                           (aref +planes-sizes-max+ i))))
      (log-info "~%Plane: ~d Min-size: ~a Max-size: ~a Generator: ~a~%"
                i
                min-size
                max-size
                (nth i *figure-generation-method-list*))
      (dotimes (j (aref +planes-figures-max+ i))
        (log-info "Trying to produce figure ~d of plane ~d" j i)
        (let ((fig (funcall (nth i *figure-generation-method-list*)
                            drawing
                            plane
                            points-for-plane
                            min-size
                            max-size)))
          ;; Give up on first failure, later ones probably won't succeed.
          (unless fig
            (return))
          (draw-figure fig (pen-params plane))
          ;; Trying to draw may fail and leave the figure empty
          (when (> (form-count fig) 0)
            (vector-push-extend fig (figures plane)))))
      (vector-push-extend plane (planes drawing)))))

(defparameter +colours-per-plane+ 4)

(defun colour-everything (drawing)
  (log-info "~%Colouring drawing.~%")
  ;; +1 for ground, which I suppose we could turn into a plane?
  (let ((colours (create-colours (+ (length *figure-generation-method-list*)
                                    1)
                                 +colours-per-plane+)))
    (log-info "Colours: ~a" colours)
    (log-info "Colouring ground.")
    (setf (ground drawing) (choose-colour-for colours 0 +colours-per-plane+))
    (log-info "Colouring forms.")
    (dotimes (i (planes-count drawing))
      (log-info "Colouring plane ~d" i)
      (let ((plane (aref (planes drawing) i)))
        (do-plane-forms (plane form)
          (setf (fill-colour form)
                (choose-colour-for colours (+ i 1) +colours-per-plane+))
          (log-info "Colouring form - h: ~a s: ~a l: ~a"
                    (hue (fill-colour form))
                    (saturation (fill-colour form))
                    (lightness (fill-colour form))))))
    (log-info "Finished colouring forms."))
  (log-info "Finished colouring.~%"))

(defun draw-something (&key (randseed nil) (savedir nil) (filename nil))
  "Make the drawing data structures and create the image."
  (log-info "Starting draw-something.")
  (random-init (or randseed (get-universal-time)))
  (gen-params)
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
                               (* +pen-distance-max+ 1.5))))
    (log-info "Drawing created: ~a." drawing)
    (draw-everything drawing)
    (colour-everything drawing)
    (log-info "Finished drawing.")
    (let ((filepath (svg-write-drawing drawing
                                       (or savedir
                                           (make-pathname :directory
                                                          '(:relative "drawings")))
                                       (or filename
                                           (generate-filename)))))
      (log-info "Finished draw-something ~a." filepath)
      ;; Make sure Emacs is built with ImageMagick support,
      ;; otherwise this image won't scale!
      #+swank (when swank::*emacs-connection*
                (swank::eval-in-emacs
                 `(find-file-other-window ,(namestring (truename filepath)))))
      drawing)))
