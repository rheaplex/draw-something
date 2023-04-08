;; gtk.lisp - A Gtk+ user interface for draw-something.
;; Copyright (C) 2010 Rhea Myers
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Allow the number of planes and (via...) the figure policies to be set

;; Allow the composition points to be retained between drawings

;; Animate drawing of paths, use :after methods on path generators
;; Might need to re-arrange form.lisp to support this
;; and/or add/remove wrappers dynamically

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :draw-something/gui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Just a dummy drawing initially
(defvar *drawing* 
  (make-instance 'draw-something::drawing
		 :bounds (make-instance 'draw-something::rectangle
					:width 400 :height 400)))
(defvar *window*)
(defvar *drawing-area*)
(defvar *draw-composition-points* t)
(defvar *draw-skeletons* t)
(defvar *draw-outlines* t)
(defvar *draw-colours* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod size-window-for-drawing (window (d draw-something::drawing))
  (gtk:within-main-loop
   (gtk:gtk-window-resize *window*
			  (ceiling (draw-something::width
				    (draw-something::bounds *drawing*)))
			  ;;FIXME: Actually get menu bar height
			  (+ 24
			     (ceiling (draw-something::height
				       (draw-something::bounds *drawing*)))))
   (gtk:widget-queue-draw *window*)))

(defmethod set-drawing ((d draw-something::drawing))
  "Set the drawing and reconfigure the window to fit it"
  (assert *window*)
  (setf *drawing* d)
  (size-window-for-drawing *window* d))

(defmethod make-new-drawing ()
  "Make a drawing and set the window to use it"
  (let ((drawing (draw-something::make-drawing)))
    (draw-something::make-composition-points drawing 20)
    (draw-something::make-planes drawing (draw-something::number-of-planes))
    (draw-something::make-planes-skeletons drawing)
    (draw-something::draw-planes-figures drawing)
    (draw-something::colour-objects drawing draw-something::all-object-symbols)
    (set-drawing drawing)))

(defmethod make-drawing-with-single-outline (width height skeleton-points
					     policy)
  "Make a drawing with a single plane/figure/form/skeleton/outline for testing"
  (let* ((drawing (make-instance 'draw-something::drawing
				 :bounds (make-instance 
					  'draw-something::rectangle
					  :x 0 :y 0 :height height :width width)
				 :composition-points skeleton-points))
	 (figure (draw-something::make-figure-from-points skeleton-points))
	 (plane (make-instance 'draw-something::plane
			       :figure-policy policy
			       :figures (vector figure)
			       :figure-count 1)))
    (setf (draw-something::planes drawing) (vector plane))
    (draw-something::draw-figure figure)
    (draw-something::colour-objects drawing draw-something::all-object-symbols)
    (set-drawing drawing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities for applying functions to forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Move these to the main package, they're useful

(defmethod apply-figure-forms ((f draw-something::figure) fun)
  "Apply fun to each form in the figure"
  (loop for form across (draw-something::forms f)
       do (funcall fun form)))

(defmethod apply-plane-forms ((p draw-something::plane) fun)
  "Apply fun to each form on the plane"
  (loop for f across (draw-something::figures p)
       do (apply-figure-forms f fun)))

(defmethod apply-drawing-forms ((d draw-something::drawing) fun)
  "Apply fun to each form in the drawing"
  (loop for p across (draw-something::planes d)
       do (apply-plane-forms p fun)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing the drawing in the drawing-area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod draw-background ()
  "Erase the drawing area, setting the background colour if colouring"
  (rectangle 0 0 (draw-something::width (draw-something::bounds *drawing*))
	     (draw-something::height (draw-something::bounds *drawing*)))
  (if *draw-colours*
      (multiple-value-bind (r g b) 
	  (draw-something::hsb-to-rgb (draw-something::ground *drawing*))
	(set-source-rgb r g b))
      (set-source-rgb 1 1 1))
  (fill-path))

(defmethod draw-composition-points ()
  "Draw all the drawing's composition points"
  (loop for point across (draw-something::composition-points *drawing*)
        do (progn
             (arc (draw-something::x point) (draw-something::y point) 
	                2 0 (* 2 pi))
             (set-line-width 1)
             (set-source-rgb 0 0 0)
             (stroke)
             (stroke-preserve)
             (set-source-rgb 1 1 1)
             (fill-path))))

(defmethod draw-form-skeletons ((f draw-something::form))
  "Draw the form's skeleton polylines (usually only one)"
  (set-line-width 1)
  (loop for bone across (draw-something::skeleton f)
        do (progn
             (draw-something::do-poly-lines (line bone)
               (move-to (draw-something::x (draw-something::from line))
	                      (draw-something::y (draw-something::from line)))
               (line-to (draw-something::x (draw-something::to line))
	                      (draw-something::y (draw-something::to line))))
             (set-source-rgb (draw-something::random-range 0.5 0.9)
		                         (draw-something::random-range 0.5 0.9)
		                         (draw-something::random-range 0.5 0.9))
             (stroke))))

(defmethod draw-skeletons ()
  "Draw the skeletons of every form in the drawing"
  (apply-drawing-forms *drawing* #'draw-form-skeletons))

(defmethod draw-form-outline ((f draw-something::form))
  "Draw the finished outline for the form"
  (set-line-width 1)
  (let ((outline (draw-something::points (draw-something::outline f))))
    (move-to (draw-something::x (aref outline 0))
	     (draw-something::y (aref outline 0)))
    (loop for i from 1 below (length outline)
	 do (line-to (draw-something::x (aref outline i))
		     (draw-something::y (aref outline i)))))
  (set-source-rgb 0 0 0)
  (stroke))

(defmethod draw-outlines ()
  "Draw the finished outlines for all the forms in the drawing"
  (apply-drawing-forms *drawing* #'draw-form-outline))

(defmethod draw-form-colour ((f draw-something::form))
  "Colour in the outline of the form"
  (let ((outline (draw-something::points (draw-something::outline f))))
    (move-to (draw-something::x (aref outline 0))
	     (draw-something::y (aref outline 0)))
    (loop for i from 1 below (length outline)
	 do (line-to (draw-something::x (aref outline i))
		     (draw-something::y (aref outline i)))))
  (multiple-value-bind (r g b) 
      (draw-something::hsb-to-rgb (draw-something::fill-colour f))
    (set-source-rgb r g b))
  (fill-path))

(defmethod draw-colours ()
  "Colour in the outlines of all the forms in the drawing"
  (apply-drawing-forms *drawing* #'draw-form-colour))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Gtk+ user interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-expose-event (widget)
  "Redraw the drawing in the drawing area"
  (with-gdk-context (ctx (widget-window widget))
    (with-context (ctx)
      (draw-background)
      (when *draw-colours*
	(draw-colours))
      (when *draw-composition-points*
	(draw-composition-points))
      (when *draw-skeletons*
	(draw-skeletons))
      (when *draw-outlines*
	(draw-outlines))
      nil)))

(defmacro connect-toggled-signal (menu-item global)
  "Set it so that toggling the menu item toggles the global variable"
  `(gobject:connect-signal ,menu-item "toggled"
			  (lambda (widget)
			    (setf ,global (gtk:check-menu-item-active widget))
			    (gtk:widget-queue-draw *window*))))

(defmethod make-menu ()
  "Make the menu for the window"
  (let ((menu (make-instance 'gtk::menu-item :label "Drawing"))
	(submenu (make-instance 'gtk:menu))
	(new-drawing (make-instance 'gtk:menu-item :label "New Drawing"))
	(separator (make-instance 'gtk:separator-menu-item))
	(composition (make-instance 'gtk:check-menu-item
				    :label "Composition Points"))
	(skeleton (make-instance 'gtk:check-menu-item :label "Skeleton"))
	(outline (make-instance 'gtk:check-menu-item :label "Outline"))
	(colour (make-instance 'gtk:check-menu-item :label "Colour")))
    (gobject:connect-signal new-drawing "activate"
			    (lambda (widget)
			      (declare (ignore widget))
			      (make-new-drawing)))
    (setf (gtk:check-menu-item-active composition) *draw-composition-points*)
    (setf (gtk:check-menu-item-active skeleton) *draw-skeletons*)
    (setf (gtk:check-menu-item-active outline) *draw-outlines*)
    (setf (gtk:check-menu-item-active colour) *draw-colours*)
    (connect-toggled-signal composition *draw-composition-points*)
    (connect-toggled-signal skeleton *draw-skeletons*)
    (connect-toggled-signal outline *draw-outlines*)
    (connect-toggled-signal colour *draw-colours*)
    (gtk:menu-shell-append submenu new-drawing)
    (gtk:menu-shell-append submenu separator)
    (gtk:menu-shell-append submenu composition)
    (gtk:menu-shell-append submenu skeleton)
    (gtk:menu-shell-append submenu outline)
    (gtk:menu-shell-append submenu colour)
    (setf (gtk:menu-item-submenu menu) submenu)
    menu))

(defmethod make-window ()
  "Make the main UI window, complete with drawing area and contextual menu"
  (gtk:within-main-loop
   (let ((window (make-instance 'gtk:gtk-window
				:type :toplevel
				:window-position :center
				:title "draw-something"
				:default-width 200
				:default-height 200))
	 (menu-bar (make-instance 'gtk:menu-bar))
	 (drawing-area (make-instance 'gtk:drawing-area))
	 (v-box (make-instance 'gtk:v-box)))
     (if *drawing*
	 (size-window-for-drawing *window* *drawing*))
     (gtk:container-add window v-box)
     (gtk:box-pack-start v-box menu-bar :expand nil)
     (gtk:widget-show menu-bar)
     (gtk:menu-shell-append menu-bar (make-menu))
     (gtk:box-pack-start v-box drawing-area)
     (gobject:connect-signal drawing-area
			     "expose-event"
			     (lambda (widget event)
			       (declare (ignore event))
			       (handle-expose-event widget)))
     (gtk:widget-show window)
     (setf *window* window)
     (setf *drawing-area* drawing-area))))

