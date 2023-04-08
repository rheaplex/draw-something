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

(defpackage #:draw-something
  (:use #:cl)
  (:import-from #:draw-something.choosing
                #:random-init
                #:random-range)
  (:import-from #:draw-something.colour
                #:make-colour-scheme-applier-fun)
  (:import-from #:draw-something.geometry
                #:<rectangle>)
  (:import-from #:draw-something.drawing
                #:<drawing>
                #:<pen-parameters>
                #:do-drawing-forms
                #:draw-planes-figures
                #:fill-colour
                #:make-composition-points
                #:make-planes
                #:make-planes-skeletons)
  (:import-from #:draw-something.pdf
                #:write-drawing
                #:write-and-show-drawing)
  (:export #:draw-something))

(in-package #:draw-something)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let's go!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +pen-outline-distance+ 5.2)
(defparameter +pen-outline-distance-tolerance+ 0.7)
;; defconstant isn't happy here o_O
(defparameter *pen-params*
  (make-instance '<pen-parameters>
                 :move-step          1.3 ;;1.0
                 :distance           +pen-outline-distance+
                 :distance-tolerance +pen-outline-distance-tolerance+
                 :turn-step          0.01 ;;0.1
                 :drift-probability  0.0
                 :drift-range        0.0)) ;;0.1

;;TODO work this in to the drawing but not the ground
(defparameter *border-width* (+ +pen-outline-distance+
                                +pen-outline-distance-tolerance+))

;; 11in x 14in
(defparameter +page-size+ '(1056 . 1344))
(defparameter +drawing-width+ 900)
(defparameter +drawing-height+ 900)
(defparameter +drawing-x+ (/ (- (car +page-size+) +drawing-width+) 2.0))
(defparameter +drawing-y+ (/ (- (cdr +page-size+) +drawing-height+) 2.0))

(defun generate-filename ()
  "Make a unique filename for the drawing, based on the current date & time."
  (multiple-value-bind (seconds minutes hours date month year)
      (decode-universal-time (get-universal-time))
    (format nil
            "~a-~2,,,'0@A~2,,,'0@A~2,,,'0@A-~2,,,'0@A~2,,,'0@A~2,,,'0@A"
            "drawing" year month date hours minutes seconds)))

(defun draw-something (randseed savedir filename)
  "Make the drawing data structures and create the image."
  (log:config :nopretty :notime :nofile)
  (log:info "Starting draw-something.")
  (random-init randseed)
  (let* ((choose-colour (make-colour-scheme-applier-fun))
         (drawing-bounds (make-instance '<rectangle>
                                        :x +drawing-x+
                                        :y +drawing-y+
                                        :width +drawing-width+
                                        :height +drawing-height+))
         (drawing (make-instance '<drawing>
                                 :bounds drawing-bounds
                                 :ground (funcall choose-colour 'background))))
    (make-composition-points drawing (random-range 8 42))
    (make-planes drawing)
    (make-planes-skeletons drawing)
    (draw-planes-figures drawing)
    (do-drawing-forms (drawing form)
      (setf (fill-colour form)
            (funcall choose-colour form)))
    (log:info "Finished drawing.")
    (write-and-show-drawing +page-size+
                            drawing
                            (or savedir
                                (make-pathname :directory
                                               '(:relative "drawings")))
                            (or filename
                                (generate-filename)))
    (log:info "Finished draw-something.")))
