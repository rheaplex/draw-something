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

(in-package :common-lisp-user)

(defpackage draw-something.gui-ltk
  (:nicknames :gui-ltk)
  (:use :common-lisp)
  (:import-from #:draw-something.gui-base
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
                #:show-rendered-drawing)
  (:export
   #:make-gui
   #:create-drawing
   #:show-colour
   #:show-colour-scheme
   #:show-drawing-composition-points
   #:show-plane-skeletons
   #:show-planes-skeletons
   #:show-plane
   #:show-drawing
   #:show-rendered-drawing))

(in-package :draw-something.gui-ltk)

(defclass <gui-ltk> (<gui>)
  ((canvas
    :accessor canvas)))

(defun make-gui ()
  (make-instance '<gui-ltk>))

(defmethod show-content ((gui <gui-ltk>) width height title draw-fun)
  (ltk:with-ltk ()
    (ltk:wm-title ltk:*tk* title)
    (setf (canvas gui) (make-instance 'ltk:canvas
                                      :width width
                                      :height height))
    ;;(ltk:move-all ,canvas-var-name +drawing-x+
    ;;              (- (- +drawing-height+ +drawing-y+)))
    (ltk:scale (canvas gui) 1 -1)
    (funcall draw-fun gui)
    (ltk:pack (canvas gui) :expand 1 :fill :both)))

(defmethod create-rect-fill ((gui <gui-ltk>) rect colour)
  "Display a sample of a single <colour>."
  (ltk:itemconfigure (canvas gui)
                     (ltk:create-rectangle (canvas gui)
                                           (ds::x rect)
                                           (ds::y rect)
                                           (+ (ds::x rect)
                                              (ds::width rect))
                                           (+ (ds::y rect)
                                              (ds::height rect)))
                     :fill (ds::hsb-to-rgb-hex colour)))


(defmethod create-rect-stroke ((gui <gui-ltk>) rect colour width)
  "Display a sample of a single <colour>."
  (let ((bounds (ltk:create-rectangle (canvas gui)
                                      (ds::x rect)
                                      (ds::y rect)
                                      (+ (ds::x rect)
                                         (ds::width rect))
                                      (+ (ds::y rect)
                                         (ds::height rect)))))
    (ltk:itemconfigure (canvas gui)
                       bounds
                       :fill "")
    (ltk:itemconfigure (canvas gui)
                       bounds
                       :outline (ds::hsb-to-rgb-hex colour))
    (ltk:itemconfigure (canvas gui)
                       bounds
                       :width width)))

(defmethod create-rect-fill ((gui <gui-ltk>) rect colour)
  (ltk:itemconfigure (canvas gui)
                     (ltk:create-rectangle (canvas gui)
                                           (ds::x rect)
                                           (ds::y rect)
                                           (ds::width rect)
                                           (ds::height rect))
                     :fill (ds::hsb-to-rgb-hex colour)))

(defmethod create-points ((gui <gui-ltk>) points colour radius)
  (loop for point across points
        do (ltk:create-oval (canvas gui)
                            (- (ds::x point) radius)
                            (- (ds::y point) radius)
                            (+ (ds::x point) radius)
                            (+ (ds::y point) radius))))

(defun %polyline-to-line-coords (polyline)
  "Convert a sequence of <point>s to a flat list of xn yn... ."
  ;; Reverse the co-ordinates so the order is the same as the original
  ;; Notice this means that we have to have X and Y the wrong way round below!
  (reverse (loop for point across (ds::points polyline)
                 collect (ds::y point)
                 collect (ds::x point))))

(defmethod create-polyline-stroke ((gui <gui-ltk>) polyline colour width)
  (let ((l (ltk:create-line (canvas gui)
                            (%polyline-to-line-coords polyline))))
    (when (and colour width)
      (ltk:itemconfigure (canvas gui) l :fill "")
      (ltk:itemconfigure (canvas gui) l :outline (ds::hsb-to-rgb-hex colour))
      (ltk:itemconfigure (canvas gui) l :width width))))

(defmethod create-polyline-fill ((gui <gui-ltk>) polyline colour)
  (let ((l (ltk:create-polygon (canvas gui)
                               (%polyline-to-line-coords polyline))))
    (when colour
      (ltk:itemconfigure (canvas gui) l :fill colour))))

#|

(defvar d (gui-ltk:make-drawing))

(gui-ltk:show-colour (drawing:ground d))

(drawing:make-composition-points d (choosing:random-range 8 42))
(gui-ltk:show-drawing-composition-points d)

(drawing:make-planes d (drawing:figure-generation-methods gui-ltk::+planes-count+))
(drawing:make-planes-skeletons d)
(gui-ltk:show-plane-skeletons d 1)
(gui-ltk:show-planes-skeletons d)

(drawing:draw-planes-figures d)

(gui-ltk:show-plane d 1)

(gui-ltk:show-drawing d)

(defvar colour-scheme (colour::default-colour-scheme))
(defvar choose-colour (colour::make-colour-scheme-applier-fun colour-scheme))
(gui-ltk:show-colour-scheme colour-scheme)

(drawing:do-drawing-forms (d form)
(setf (drawing:fill-colour form)
(funcall choose-colour form)))

(gui-ltk:show-drawing d)

|#

#|

(load "src/load-gui-ltk")

(defvar d (gui-ltk:create-drawing))

(drawing:make-composition-points d (choosing:random-range 8 42))
(gui-ltk:show-drawing-composition-points d)

(drawing:make-planes d (drawing:figure-generation-methods gui-ltk::+planes-count+))
(drawing:make-planes-skeletons d)
(gui-ltk:show-planes-skeletons d)

(drawing:draw-planes-figures d)

(defvar colour-scheme (colour::default-colour-scheme))
(defvar choose-colour (colour::make-colour-scheme-applier-fun colour-scheme))
(gui-ltk:show-colour-scheme colour-scheme)

(drawing:do-drawing-forms (d form)
(setf (drawing:fill-colour form)
(funcall choose-colour form)))

(gui-ltk:show-drawing d)

|#

#|

(setf d (gui-base:create-drawing))
(setf *gui* (gui-glut:make-gui))
(ds::make-composition-points d 36)
(gui-ltk:show-drawing-composition-points *gui* d)



(ds::make-planes d (ds::figure-generation-methods 4)) ; ; ; ;
(drawing:make-planes-skeletons d)
(drawing:draw-planes-figures d)
(defvar colour-scheme (colour::default-colour-scheme))
(defvar choose-colour (colour::make-colour-scheme-applier-fun colour-scheme))
(drawing:do-drawing-forms (d form)
(setf (drawing:fill-colour form)
(funcall choose-colour form)))

(gui-ltk:show-drawing d)

|#
