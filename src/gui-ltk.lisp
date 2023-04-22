;; gui-ltk.lisp - A simple ltk gui for draw-something.
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

(defpackage #:draw-something.gui-ltk
  (:use :common-lisp :draw-something)
  (:nicknames #:ltk-gui)

(in-package #:draw-something.gui-ltk)

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
(defparameter +page-width+ 960)
(defparameter +page-height+ 540)
(defparameter +drawing-width+ 900)
(defparameter +drawing-height+ 480)
(defparameter +drawing-x+ (/ (- +page-width+ +drawing-width+) 2.0))
(defparameter +drawing-y+ (/ (- +page-height+ +drawing-height+) 2.0))

(defparameter +planes-count+ 4)

(defun make-drawing (&key (randseed nil))
  "Make the drawing data structures and create the image."
  (log-info "Starting draw-something.")
  (random-init (or randseed (get-universal-time)))
  (let ((choose-colour (make-colour-scheme-applier-fun (default-colour-scheme)))
        (drawing-bounds (make-instance '<rectangle>
                                       :x +drawing-x+
                                       :y +drawing-y+
                                       :width +drawing-width+
                                       :height +drawing-height+)))
    (make-instance '<drawing>
                   :bounds drawing-bounds
                   :ground (funcall choose-colour 'background))))



(defmacro show-with-canvas-bounds ((canvas-var-name width height title &rest title-format-args) &body body)
  `(ltk:with-ltk ()
     (ltk:wm-title ltk:*tk* (format nil ,title ,@title-format-args))
     (let ((,canvas-var-name (make-instance 'ltk:canvas
                                            :width ,width
                                            :height ,height)))
       ;;(ltk:move-all ,canvas-var-name +drawing-x+
       ;;              (- (- +drawing-height+ +drawing-y+)))
       (ltk:scale ,canvas-var-name 1 -1)
       ,@body
       (ltk:pack ,canvas-var-name :expand 1 :fill :both))))

(defmacro show-with-canvas ((canvas-var-name title &rest title-format-args) &body body)
  `(show-with-canvas-bounds (canvas-var-name +drawing-x+ +drawing-y+
                                             ,title ,@title-format-args)
     ,@body
     (let ((bounds (ltk:create-rectangle ,canvas-var-name
                                         +drawing-x+
                                         +drawing-y+
                                         (+ +drawing-x+ +drawing-width+)
                                         (+ +drawing-y+ +drawing-height+))))
       (ltk:itemconfigure ,canvas-var-name bounds :fill "")
       (ltk:itemconfigure ,canvas-var-name bounds :outline "magenta")
       (ltk:itemconfigure ,canvas-var-name bounds :width 2))
     (ltk:pack ,canvas-var-name :expand 1 :fill :both)))

(defun show-colour (colour)
  "Display a sample of a single <colour>."
  (show-with-canvas-bounds (c 256 256 "draw-something - colour ~a" colour)
    (ltk:itemconfigure c
                       (ltk:create-rectangle c 0 0 256 256)
                       :fill (hsb-to-rgb-hex colour))))

(defparameter +colours-window-width+ 2048)
(defparameter +colours-window-height+ 256)

(defun show-colours (colours)
  "Show a palette/list of <colours>."
  (show-with-canvas-bounds (c +colours-window-width+ +colours-window-height+
                              "draw-something - colours ~a" colours)
    (let* ((num-colours (length colours))
           (bar-width (/ +colours-window-width+ num-colours)))
      (loop for i from 0 below (length colours)
            for colour in colours
            do (let ((rgb (hsb-to-rgb-hex colour)))
                 (ltk:itemconfigure c
                                    (ltk:create-rectangle c
                                                          (* i bar-width)
                                                          0
                                                          (* (+ i 1) bar-width)
                                                          +colours-window-height+)
                                    :fill rgb))))))

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

(defun show-drawing-composition-points (drawing)
    (show-with-canvas (c "draw-something - composition points ~a" drawing)
      (create-points c (composition-points drawing))))

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

(defun show-polyline (polyline)
  (show-with-canvas (c "draw-something - polyline ~a" polyline)
    (create-points c (geometry:points polyline))
    (create-line-from-polyline c polyline)))

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
  (let ((plane (aref (drawing:planes drawing) plane-index))
        (colour (aref +layer-skeleton-colours+ plane-index)))
    (show-with-canvas (c "draw-something - planes skeletons for ~a ~a #~d"
                         plane drawing plane-index)
      (create-plane-skeletons-lines c plane colour))))

(defun show-planes-skeletons (drawing)
  (show-with-canvas (c "draw-something - planes skeletons for ~a"
                        drawing)
    (loop for colour across +layer-skeleton-colours+
          for plane across (drawing:planes drawing)
          do (loop for figure across (drawing:figures plane)
                   do (loop for form across (drawing:forms figure)
                            do (loop for polyline across (drawing:skeleton form)
                                     do (ltk:itemconfigure c
                                                           (ltk:create-line c (polyline-to-line-coords polyline))
                                                           :fill colour)))))))

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
  (show-with-canvas (c "draw-something - plane #~d ~a"
                       plane-index drawing)
    (let ((colour (aref +layer-skeleton-colours+ plane-index))
          (plane (aref (drawing:planes drawing) plane-index)))
      (create-plane c plane colour))))
  
(defun show-drawing (drawing)
  (show-with-canvas (c "draw-something - drawing ~a"
                       drawing)
    (loop for colour across +layer-skeleton-colours+
          for plane across (drawing:planes drawing)
          do (create-plane c plane colour))
    (create-points c (composition-points drawing))))

#|

(defvar d (ltk-gui:make-drawing))

(ltk-gui:show-colour (drawing:ground d))

(drawing:make-composition-points d (choosing:random-range 8 42))
(ltk-gui:show-drawing-composition-points d)

(drawing:make-planes d (drawing:figure-generation-methods ltk-gui::+planes-count+))
(drawing:make-planes-skeletons d)
(ltk-gui:show-plane-skeletons d 1)
(ltk-gui:show-planes-skeletons d)

(drawing:draw-planes-figures d)

(ltk-gui:show-plane d 1)

(ltk-gui:show-drawing d)

(defvar colour-scheme (colour::default-colour-scheme))
(defvar choose-colour (colour::make-colour-scheme-applier-fun colour-scheme))
(ltk-gui:show-colour-scheme colour-scheme)

(drawing:do-drawing-forms (d form)
  (setf (drawing:fill-colour form)
  (funcall choose-colour form)))

(ltk-gui:show-drawing d)

|#

#|

(load "src/load-ltk-gui")

(defvar d (ltk-gui:make-drawing))

(drawing:make-composition-points d (choosing:random-range 8 42))
(ltk-gui:show-drawing-composition-points d)

(drawing:make-planes d (drawing:figure-generation-methods ltk-gui::+planes-count+))
(drawing:make-planes-skeletons d)
(ltk-gui:show-planes-skeletons d)

(drawing:draw-planes-figures d)

(defvar colour-scheme (colour::default-colour-scheme))
(defvar choose-colour (colour::make-colour-scheme-applier-fun colour-scheme))
(ltk-gui:show-colour-scheme colour-scheme)

(drawing:do-drawing-forms (d form)
  (setf (drawing:fill-colour form)
  (funcall choose-colour form)))

(ltk-gui:show-drawing d)

|#

#|

(setf d (ltk-gui:make-drawing))

(drawing:make-composition-points d (choosing:random-range 32 128))
(drawing:make-planes d (drawing:figure-generation-methods ltk-gui::+planes-count+))
(drawing:make-planes-skeletons d)
(drawing:draw-planes-figures d)
(defvar colour-scheme (colour::default-colour-scheme))
(defvar choose-colour (colour::make-colour-scheme-applier-fun colour-scheme))
(drawing:do-drawing-forms (d form)
  (setf (drawing:fill-colour form)
  (funcall choose-colour form)))

(ltk-gui:show-drawing d)

|#
