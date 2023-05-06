;; gui-base.lisp - The base for a simple gui for draw-something.
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

(in-package :common-lisp-user)

(defpackage draw-something.gui-base
  (:nicknames :gui-base)
  (:use :common-lisp)
  (:export
   #:<gui>
   #:show-content
   #:create-rect-fill
   #:create-rect-stroke
   #:create-points
   #:create-polyline-stroke
   #:create-polyline-fill
   #:create-drawing
   #:show-colour
   #:show-colours
   #:show-colour-scheme
   #:show-drawing-composition-points
   #:show-plane-skeletons
   #:show-planes-skeletons
   #:show-plane
   #:show-drawing
   #:show-rendered-drawing))

(in-package :draw-something.gui-base)

(defclass <gui> ()
  ())
(defgeneric show-content (gui width height title fun))
(defgeneric create-rect-fill (gui rect colour))
(defgeneric create-rect-stroke (gui rect colour width))
(defgeneric create-points (gui points colour radius))
(defgeneric create-polyline-stroke (gui polyline colour width))
(defgeneric create-polyline-fill (gui polyline colour))

(defparameter +black+ (ds::make-colour :hue 0
                                       :saturation 0
                                       :brightness 0))

(defparameter +pen-outline-distance+ 5.2)
(defparameter +pen-outline-distance-tolerance+ 0.7)
;; defconstant isn't happy here o_O
(defparameter *pen-params*
  (ds::make-pen-parameters
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
(defparameter +page-width+ 960)
(defparameter +page-height+ 540)
(defparameter +drawing-width+ 900)
(defparameter +drawing-height+ 480)
(defparameter +drawing-x+ (/ (- +page-width+ +drawing-width+) 2.0))
(defparameter +drawing-y+ (/ (- +page-height+ +drawing-height+) 2.0))

(defparameter +planes-count+ 4)

(defparameter +colour-sample-size+ 256)
(defparameter +colours-window-width+ 2048)
(defparameter +colours-window-height+ 256)
(defparameter +point-radius+ 2)

(defparameter +layer-skeleton-colours+
  #("red" "orange" "green" "blue" "pink" "purple" "brown"))

(defun create-drawing (&key (randseed nil))
  "Make the drawing data structures and create the image."
  (ds::log-info "Starting draw-something.")
  (ds::random-init (or randseed (get-universal-time)))
  (ds::make-drawing :substrate-bounds (ds::make-rectangle :x 0
                                                          :y 0
                                                          :width +page-width+
                                                          :height +page-height+)
                    :bounds (ds::make-rectangle :x +drawing-x+
                                                :y +drawing-y+
                                                :width +drawing-width+
                                                :height +drawing-height+)
                    :colour-scheme-applier
                    (ds::make-colour-scheme-applier :scheme (ds::default-colour-scheme)
                                                    :spec-list (ds::chooser-spec))))

(defun show-colour (gui colour)
  "Display a sample of a single <colour>."
  (show-content gui
                +colour-sample-size+
                +colour-sample-size+
                (format nil "draw-something - colour ~a"
                        colour)
                (lambda (gui)
                  (create-rect-fill gui
                                    (ds::make-rectangle :x 0
                                                        :y 0
                                                        :width +colour-sample-size+
                                                        :height +colour-sample-size+)
                                    colour))))

(defun create-colours (gui colours)
  (let* ((num-colours (length colours))
         (bar-width (/ +colours-window-width+ num-colours)))
    (loop for i from 0 below (length colours)
          for colour in colours
          do (create-rect-fill gui
                               (ds::make-rectangle :x (* i bar-width)
                                                   :y 0
                                                   :width (* (+ i 1) bar-width)
                                                   :height +colours-window-height+)
                               colour))))

(defun show-colours (gui colours)
  "Show a palette/list of <colours>."
  (show-content gui
                +colours-window-width+
                +colours-window-height+
                (format nil "draw-something - colours ~a"
                        colours)
                (lambda (gui)
                  (create-colours gui colours))))

(defun show-colour-scheme (gui scheme)
  ;;FIXME batch or row the colours for each symbol/hue
  (show-colours gui (ds::all-colours scheme)))

(defun show-drawing-composition-points (gui drawing)
    (show-content gui
                  (ds::width (ds::substrate-bounds drawing))
                  (ds::height (ds::substrate-bounds drawing))
                  (format nil "draw-something - composition points ~a"
                          drawing)
                  (lambda (gui)
                    (create-points gui
                                   (ds::composition-points drawing)
                                   +black+
                                   +point-radius+))))

(defun show-polyline (gui polyline)
  (show-content gui
                (ds::width (ds::bounds polyline))
                (ds::height (ds::bounds polyline))
                (format nil "draw-something - polyline ~a"
                        polyline)
                (lambda (gui)
                  (create-points gui
                                 (ds::points polyline)
                                 +black+
                                 +point-radius+)
                  (create-polyline-stroke gui polyline +black+ 2))))

(defun create-figure-skeletons-lines (gui figure colour)
  (loop for form across (ds::forms figure)
        do (create-form-skeleton-lines gui form colour)))

(defun create-plane-skeletons-lines (gui plane colour)
  (loop for figure across (ds::figures plane)
        do (create-figure-skeletons-lines gui figure colour)))

(defun show-plane-skeletons (gui drawing plane-index)
  (let ((plane (aref (ds::planes drawing) plane-index))
        (colour (aref +layer-skeleton-colours+ plane-index)))
    (show-content gui
                  (ds::width (ds::substrate-bounds drawing))
                  (ds::height (ds::substrate-bounds drawing))
                  (format nil
                          "draw-something - planes skeletons for ~a ~a #~d"
                          plane drawing plane-index)
                  (lambda (gui)
                    (create-plane-skeletons-lines gui plane colour))))

  (defun show-planes-skeletons (gui drawing)
    (show-content gui
                  (ds::width (ds::substrate-bounds drawing))
                  (ds::height (ds::substrate-bounds drawing))
                  (format nil "draw-something - planes skeletons for ~a"
                          drawing)
                  (lambda (gui)
                    (loop for colour across +layer-skeleton-colours+
                          for plane across (ds::planes drawing)
                          do (create-plane-skeletons-lines gui
                                                           plane
                                                           colour))))))

;; Diagnostic renders

(defun create-form-skeleton-lines (gui form colour)
  (loop for polyline across (ds::skeleton form)
        do (create-polyline-stroke gui polyline colour 2)))

(defun create-form-fill (gui form)
  (let ((fill (ds::fill-colour form)))
    (when fill
      (create-polyline-fill gui (ds::outline form) fill))))

(defun create-figure-outlines-lines (gui figure colour)
  (loop for form across (ds::forms figure)
        do (create-polyline-stroke gui (ds::outline form) colour 2)))

(defun create-figure-fills-polygons (gui figure)
  (loop for form across (ds::forms figure)
        do (create-form-fill gui form)))

(defun create-plane-outlines-lines (gui plane colour)
  (loop for figure across (ds::figures plane)
        do (create-figure-outlines-lines gui figure colour)))

(defun create-plane-fills-polygons (gui plane)
  (loop for figure across (ds::figures plane)
        do (create-figure-fills-polygons gui figure)))

;; TODO show-planes-outlines

(defun create-plane (gui plane colour)
  (create-plane-fills-polygons gui plane)
  (create-plane-outlines-lines gui plane colour)
  (create-plane-skeletons-lines gui plane colour))

(defun show-plane (gui drawing plane-index)
  (show-content gui
                (ds::width (ds::substrate-bounds drawing))
                (ds::height (ds::substrate-bounds drawing))
                (format nil "draw-something - plane #~d ~a"
                        plane-index drawing)
                (lambda (gui)
                  (let ((colour (aref +layer-skeleton-colours+
                                      plane-index))
                        (plane (aref (ds::planes drawing) plane-index)))
                    (create-plane gui plane colour)))))

(defun show-drawing (gui drawing)
  (show-content gui
                (ds::width (ds::substrate-bounds drawing))
                (ds::height (ds::substrate-bounds drawing))
                (format nil "draw-something - drawing ~a"
                        drawing)
                (lambda (gui)
                  (loop for colour across +layer-skeleton-colours+
                        for plane across (ds::planes drawing)
                        do (create-plane gui plane colour))
                  (create-points gui
                                 (ds::composition-points drawing)
                                 +black+
                                 +point-radius+)
                  (create-rect-stroke gui
                                      (ds::bounds drawing)
                                      +black+
                                      1))))

;; Render to screen.

(defun render-form (gui form)
  (let ((fill (ds::fill-colour form))
        (stroke (ds::stroke-colour form)))
    (when fill
      (create-polyline-fill gui (ds::outline form) fill))
    (when stroke
      (create-polyline-stroke gui (ds::outline form) stroke))))

(defun render-plane (gui plane)
  (loop for figure across (ds::figures plane)
        do (loop for form across (ds::forms figure)
                 do (render-form gui form))))

(defun show-rendered-drawing (gui drawing)
  (show-content gui
                (ds::width (ds::substrate-bounds drawing))
                (ds::height (ds::substrate-bounds drawing))
                (format nil "draw-something - drawing ~a"
                        drawing)
                (lambda (gui)
                  (create-rect-fill gui
                                    (ds::bounds drawing)
                                    (ds::ground drawing))
                  (loop for plane across (ds::planes drawing)
                        do (render-plane gui plane)))))
