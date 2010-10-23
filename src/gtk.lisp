(require :asdf)
(asdf:operate 'asdf:load-op :cl-gtk2-gtk)
(asdf:operate 'asdf:load-op :cl-cairo2)
(asdf:operate 'asdf:load-op :cl-gtk2-cairo)
(asdf:operate 'asdf:load-op :draw-something)

(defpackage #:ui
  (:shadowing-import-from #:cl-cairo2 #:scale)
  (:use :cl #:gtk #:cl-cairo2 #:cl-gtk2-cairo :draw-something))

(in-package #:ui)

(defvar *drawing*)
(defvar *window*)
(defvar *drawing-area*)
(defvar *drawing-functions* '())

(defun handle-expose-event (widget)
    (with-gdk-context (ctx (widget-window widget))
      (with-context (ctx)
	(dolist (fun *drawing-functions*)
	  (funcall fun))
        nil)))

(defmethod make-window ()
  (gtk:within-main-loop
   (setf *window* (make-instance 'gtk:gtk-window
				 :type :toplevel
				 :window-position :center
				 :title "draw-something"
				 :default-width 200
				 :default-height 200))
   (setf *drawing-area* (make-instance 'gtk:drawing-area))
   (gobject:connect-signal *drawing-area* 
			   "expose-event"
			   (lambda (widget event)
			     (declare (ignore event))
			     (handle-expose-event widget)))
   (gtk:container-add *window* *drawing-area*)
   (gtk:widget-show *window*)))

(defmethod set-drawing ((d draw-something::drawing))
  (setf *drawing* d)
  (gtk:within-main-loop
   (gtk:gtk-window-resize *window*
			  (ceiling (draw-something::width
				    (draw-something::bounds *drawing*)))
			  (ceiling (draw-something::height
				    (draw-something::bounds *drawing*)))))
  ;; erase the drawing area
)

(defmethod draw-background ()
  (rectangle 0 0 (draw-something::x (draw-something::bounds *drawing*))
	     (draw-something::y (draw-something::bounds *drawing*)))
  (set-source-rgb 1 1 1)
  (fill-path))

(defmethod draw-composition-points ()
  (draw-something::dovector 
   (point (draw-something::composition-points *drawing*))
   (arc (draw-something::x point) (draw-something::y point) 
	2 0 (* 2 pi))
  (set-source-rgb 1 0 0)
  (fill-path)))

(defmethod enable-draw-composition-points ()
  (if (not (find #'draw-composition-points *drawing-functions*))
      (push #'draw-composition-points *drawing-functions*)))

(defmethod disable-draw-composition-points ()
  (setf *drawing-functions* 
	(remove #'draw-composition-points *drawing-functions*)))

(defmethod draw-form-skeletons ((f draw-something::form))
  (set-line-width 1)
  (draw-something::dovector (bone (draw-something::skeleton f))
    (draw-something::do-poly-lines (line bone)
      (move-to (draw-something::x (draw-something::from line))
	       (draw-something::y (draw-something::from line)))
      (line-to (draw-something::x (draw-something::to line))
	       (draw-something::y (draw-something::to line))))
    (set-source-rgb (draw-something::random-range 0.5 0.9)
		    (draw-something::random-range 0.5 0.9)
		    (draw-something::random-range 0.5 0.9))
    (stroke)))

(defmethod draw-figure-skeletons ((f draw-something::figure))
  (loop for form across (draw-something::forms f)
       do (draw-form-skeletons form)))

(defmethod draw-plane-skeletons ((p draw-something::plane))
  (loop for f across (draw-something::figures p)
       do (draw-figure-skeletons f)))

(defmethod draw-skeletons ()
  (loop for p across (draw-something::planes *drawing*)
       do (draw-plane-skeletons p)))

(defmethod enable-draw-skeletons ()
  (if (not (find #'draw-skeletons *drawing-functions*))
      (push #'draw-skeletons *drawing-functions*)))

(defmethod disable-draw-skeletons ()
  (setf *drawing-functions* (remove #'draw-skeletons *drawing-functions*)))

(defmethod draw-form-outline ((f draw-something::form))
  (set-line-width 1)
  (draw-something::do-poly-lines (line  (draw-something::outline f))
    (move-to (draw-something::x (draw-something::from line))
	     (draw-something::y (draw-something::from line)))
    (line-to (draw-something::x (draw-something::to line))
	     (draw-something::y (draw-something::to line))))
    (set-source-rgb (draw-something::random-range 0.1 0.4)
		    (draw-something::random-range 0.1 0.4)
		    (draw-something::random-range 0.1 0.4))
  (stroke))

(defmethod draw-figure-outlines ((f draw-something::figure))
  (loop for form across (draw-something::forms f)
       do (draw-form-outline form)))

(defmethod draw-plane-outlines ((p draw-something::plane))
  (loop for f across (draw-something::figures p)
       do (draw-figure-outlines f)))

(defmethod draw-outlines ()
  (loop for p across (draw-something::planes *drawing*)
       do (draw-plane-outlines p)))

(defmethod enable-draw-outlines ()
  (if (not (find #'draw-outlines *drawing-functions*))
      (push #'draw-outlines *drawing-functions*)))

(defmethod disable-draw-outlines ()
  (setf *drawing-functions* (remove #'draw-outlines *drawing-functions*)))

(setf *drawing-functions* (list #'draw-background))

(setf *window* (make-window))
(set-drawing (draw-something::make-drawing))

(draw-something::make-composition-points *drawing* 20)
(ui::enable-draw-composition-points)

(draw-something::make-planes *drawing* (draw-something::number-of-planes))
(draw-something::make-planes-skeletons *drawing*)
(ui::enable-draw-skeletons)

(draw-something::draw-planes-figures *drawing*)
(ui::enable-draw-outlines)

;; Have a test function like:
;; drawing-with-single-outline (width height skeleton))

;; Animate drawing of paths, use :after methods on path generators
;; Might need to re-arrange form.lisp to support this
;; or add/remove wrappers dynamically

;; UI to enable/disable drawing of each element
;; Drawing system force corretc order of drawing