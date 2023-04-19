;; gui.lisp - A simple ltk gui for draw-something.
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

(defpackage #:draw-something.gui
  (:use :common-lisp)
  (:nicknames #:gui)
  (:import-from #:choosing
                #:random-init
                #:random-range)
  (:import-from #:colour
                #:hsb-to-rgb-hex
                #:make-colour-scheme-applier-fun)
  (:import-from #:geometry
                #:<point>
                #:<polyline>
                #:<rectangle>
                #:x
                #:y
                #:height
                #:width
                #:bounds
                #:append-point
                #:convex-hull
                #:distance
                #:first-point
                #:highest-leftmost-point
                #:include-point
                #:intersects-none
                #:last-point
                #:make-polyline-from-points
                #:point-count
                #:points
                #:random-points-at-rectangle-corners
                #:random-points-in-rectangle
                #:random-points-on-rectangle)
  (:import-from #:drawing
                #:<drawing>
                #:<pen-parameters>
                #:composition-points
                #:do-drawing-forms
                #:draw-planes-figures
                #:figures
                #:figure-generation-methods
                #:fill-colour
                #:forms
                #:ground
                #:make-composition-points
                #:make-planes
                #:make-planes-skeletons
                #:number-of-planes
                #:outline
                #:pen-distance
                #:planes
                #:skeleton)
  (:export #:make-drawing
           #:show-colour
           #:show-colour-scheme
           #:show-drawing
           #:show-drawing-composition-points
           #:show-plane
           #:show-plane-skeletons
           #:show-planes-skeletons))

(in-package #:draw-something.gui)

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

(defparameter +planes-count+ 4)

(defun make-drawing (&key (randseed nil))
  "Make the drawing data structures and create the image."
  (log:info "Starting draw-something.")
  (random-init (or randseed (get-universal-time)))
  (let ((choose-colour (make-colour-scheme-applier-fun))
        (drawing-bounds (make-instance '<rectangle>
                                       :x +drawing-x+
                                       :y +drawing-y+
                                       :width +drawing-width+
                                       :height +drawing-height+)))
    (make-instance '<drawing>
                   :bounds drawing-bounds
                   :ground (funcall choose-colour 'background))))

(defun show-colour (colour)
  "Display a sampel of a single <colour>."
  (ltk:with-ltk ()
    (ltk:wm-title ltk:*tk* (format nil "draw-something - colour ~a" colour))
    (let ((c (make-instance 'ltk:canvas
                            :width 256
                            :height 256
                            :background (hsb-to-rgb-hex colour))))
      (ltk:pack c :expand 1 :fill :both))))

(defparameter +colours-window-width+ 2048)
(defparameter +colours-window-height+ 256)

(defun show-colours (colours)
  "Show a palette/list of <colours>."
  (ltk:with-ltk ()
    (ltk:wm-title ltk:*tk* (format nil "draw-something - colours ~a" colours))
    (let* ((num-colours (length colours))
           (bar-width (/ +colours-window-width+ num-colours))
           (c (make-instance 'ltk:canvas
                             :width +colours-window-width+
                             :height +colours-window-height+)))
      (loop for i from 0 below (length colours)
            for colour in colours
            do (let ((rgb (hsb-to-rgb-hex colour)))
                 (ltk:itemconfigure c
                                    (ltk:create-rectangle c
                                                          (* i bar-width)
                                                          0
                                                          (* (+ i 1) bar-width)
                                                          +colours-window-height+)
                                    :fill rgb)))
      (ltk:pack c :expand 1 :fill :both))))

(defun show-colour-scheme (scheme)
  ;;FIXME batch or row the colours for each symbol/hue
  (show-colours (colour::all-colours scheme)))

(defparameter +point-radius+ 2)

(defun create-points (c points)
  (loop for point across points
        do (ltk:create-oval c
                            (- (x point) +point-radius+)
                            (- (y point) +point-radius+)
                            (+ (x point) +point-radius+)
                            (+ (y point) +point-radius+))))

(defun show-points (points width height)
  (ltk:with-ltk ()
    (ltk:wm-title ltk:*tk* (format nil "draw-something - points ~a" points))
    (let ((c (make-instance 'ltk:canvas :width width :height height)))
      (create-points c points)
      (ltk:pack c :expand 1 :fill :both))))

(defun show-drawing-composition-points (drawing)
  (show-points (composition-points drawing) 900 900))

(defparameter +layer-skeleton-colours+ #("red" "orange" "green" "blue" "pink" "purple" "brown"))

(defun polyline-to-line-coords (polyline)
  "Convert a sequence of <point>s to a flat list of xn yn... for ltk's (create-line)."
  ;; Reverse the co-ordinates so the order is the same as the original
  ;; Notice this means that we have to have X and Y the wrong way round below!
  (reverse (loop for point across (geometry:points polyline)
                 collect (y point)
                 collect (x point))))

(defun create-line-from-polyline (c polyline)
  (ltk:create-line c (polyline-to-line-coords polyline)))

(defun create-polygon-from-polyline (c polyline)
  (ltk:create-polygon c (polyline-to-line-coords polyline)))

(defun show-polyline (polyline width height)
  (ltk:with-ltk ()
    (ltk:wm-title ltk:*tk* (format nil "draw-something - polyline ~a" polyline))
    (let ((c (make-instance 'ltk:canvas :width width :height height)))
      (create-line-from-polyline c polyline)
      (ltk:pack c :expand 1 :fill :both))))

(defun create-form-skeleton-lines (c form colour)
  (loop for polyline across (drawing:skeleton form)
        do (ltk:itemconfigure c
                              (create-line-from-polyline c polyline)
                              :fill colour)))

(defun create-figure-skeletons-lines (c figure colour)
  (loop for form across (drawing:forms figure)
        do (create-form-skeleton-lines c form colour)))

(defun create-plane-skeletons-lines (c plane colour)
  (loop for figure across (drawing:figures plane)
        do (create-figure-skeletons-lines c figure colour)))

(defun show-plane-skeletons (drawing plane-index)
  (ltk:with-ltk ()
    (let ((c (make-instance 'ltk:canvas :width 900 :height 900))
          (plane (aref (drawing:planes drawing) plane-index))
          (colour (aref +layer-skeleton-colours+ plane-index)))
      (ltk:wm-title ltk:*tk* (format nil "draw-something - planes skeletons for ~a ~a #~d" plane drawing plane-index))
      (create-plane-skeletons-lines c plane colour)
      (ltk:pack c :expand 1 :fill :both))))

(defun show-planes-skeletons (drawing)
  (ltk:with-ltk ()
    (ltk:wm-title ltk:*tk* (format nil "draw-something - planes skeletons for ~a" drawing))
    (let ((c (make-instance 'ltk:canvas :width 900 :height 900)))
      (loop for colour across +layer-skeleton-colours+
            for plane across (drawing:planes drawing)
            do (loop for figure across (drawing:figures plane)
                     do (loop for form across (drawing:forms figure)
                              do (loop for polyline across (drawing:skeleton form)
                                       do (ltk:itemconfigure c
                                                             (ltk:create-line c (polyline-to-line-coords polyline))
                                                             :fill colour)))))
      (ltk:pack c :expand 1 :fill :both))))

(defun create-form-outline-line (c form colour)
  (ltk:itemconfigure c
                     (create-line-from-polyline c (drawing:outline form))
                     :fill colour))

(defun create-figure-outlines-lines (c figure colour)
  (loop for form across (drawing:forms figure)
        do (create-form-outline-line c form colour)))

(defun create-plane-outlines-lines (c plane colour)
  (loop for figure across (drawing:figures plane)
        do (create-figure-outlines-lines c figure colour)))

(defun create-form-fill-polygon (c form)
  (ltk:itemconfigure c
                     (create-polygon-from-polyline c (drawing:outline form))
                     :fill (colour:hsb-to-rgb-hex (drawing:fill-colour form))))

(defun create-figure-fills-polygons (c figure)
  (loop for form across (drawing:forms figure)
        do (create-form-fill-polygon c form)))

(defun create-plane-fills-polygons (c plane)
  (loop for figure across (drawing:figures plane)
        do (create-figure-fills-polygons c figure)))

;; TODO show-planes-outlines

(defun create-plane (c plane colour)
  (create-plane-fills-polygons c plane)
  (create-plane-outlines-lines c plane colour)
  (create-plane-skeletons-lines c plane colour))

(defun show-plane (drawing plane-index)
  (ltk:with-ltk ()
    (let ((c (make-instance 'ltk:canvas :width 900 :height 900))
          (colour (aref +layer-skeleton-colours+ plane-index))
          (plane (aref (drawing:planes drawing) plane-index)))
      (ltk:wm-title ltk:*tk* (format nil "draw-something - planes skeletons for ~a #~d ~a" drawing plane-index))
      (create-plane c plane colour)
      (ltk:pack c :expand 1 :fill :both))))
  
(defun show-drawing (drawing)
  (ltk:with-ltk ()
    (ltk:wm-title ltk:*tk* (format nil "draw-something - drawing ~a" drawing))
    (let ((c (make-instance 'ltk:canvas :width 900 :height 900)))
      (loop for colour across +layer-skeleton-colours+
            for plane across (drawing:planes drawing)
            do (create-plane c plane colour))
      (create-points c (composition-points drawing))
      (ltk:pack c :expand 1 :fill :both))))

#|

(defvar d (gui:make-drawing))

(gui:show-colour (drawing:ground d))

(drawing:make-composition-points d (choosing:random-range 8 42))
(gui:show-drawing-composition-points d)

(drawing:make-planes d (drawing:figure-generation-methods gui::+planes-count+))
(drawing:make-planes-skeletons d)
(gui:show-plane-skeletons d 1)
(gui:show-planes-skeletons d)

(drawing:draw-planes-figures d)

(gui:show-plane d 1)

(gui:show-drawing d)

(defvar colour-scheme (colour::default-colour-scheme))
(defvar choose-colour (colour::make-colour-scheme-applier-fun colour-scheme))
(gui:show-colour-scheme colour-scheme)


(drawing:do-drawing-forms (d form)
  (setf (drawing:fill-colour form)
  (funcall choose-colour form)))

(gui:show-drawing d)

|#

#|

(defvar d (gui:make-drawing))

(drawing:make-composition-points d (choosing:random-range 8 42))
(gui:show-drawing-composition-points d)

(drawing:make-planes d (drawing:figure-generation-methods gui::+planes-count+))
(drawing:make-planes-skeletons d)
(gui:show-planes-skeletons d)

(drawing:draw-planes-figures d)

(defvar colour-scheme (colour::default-colour-scheme))
(defvar choose-colour (colour::make-colour-scheme-applier-fun colour-scheme))
(gui:show-colour-scheme colour-scheme)

(drawing:do-drawing-forms (d form)
  (setf (drawing:fill-colour form)
  (funcall choose-colour form)))

(gui:show-drawing d)

|#
