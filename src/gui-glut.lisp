(defpackage draw-something.gui-glut
  (:nicknames :gui-glut)
  (:use :common-lisp)
  (:import-from #:draw-something.gui-base
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
  (:import-from #:draw-something.gui-cl-vectors
                 #:<gui-cl-vectors>
                 #:aa-image
                 #:aa-image-width
                 #:aa-image-height
                 #:show-content
                 #:create-rect-fill
                 #:create-rect-stroke
                 #:create-points
                 #:create-polyline-stroke
                 #:create-polyline-fill)
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

(in-package :draw-something.gui-glut)

(defclass window (glut:window)
  ((gui :accessor gui :initarg :gui)
   (texture-id :accessor texture-id :initform #x0))
  (:default-initargs :mode '(:double :rgb)))

(defmethod glut:display ((win window))
  (gl:clear-color 255 255 255 255)
  (gl:clear :color-buffer-bit)
  (gl:load-identity)
  (gl:with-primitive :quads
    (gl:tex-coord 0 0) (gl:vertex (aa-image-width (gui win))
                                  (aa-image-height (gui win)))
    (gl:tex-coord 1 0) (gl:vertex 0
                                  (aa-image-height (gui win)))
    (gl:tex-coord 1 1) (gl:vertex 0
                                  0)
    (gl:tex-coord 0 1) (gl:vertex (aa-image-width (gui win))
                                  0))
  (glut:swap-buffers))

(defmethod glut:reshape ((win window) width height)
  (when (zerop height)
    (setq height 1))
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  ;; center the camera on the texture quad.
  (let* ((left (/ (- width (aa-image-width (gui win))) -2.0))
         (right (+ left width))
         (bottom (/ (- height (aa-image-height (gui win))) -2.0))
         (top (+ bottom height)))
    (gl:ortho left right bottom top -1 1))
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:enable :texture-2d)
  ;;FIXME: this ia the wrong place to keep making these.
  (setf (texture-id win) (first (gl:gen-textures 1)))
  (gl:bind-texture :texture-2d (texture-id win))
  ;; The texture isn't drawn without at least the next line.
  (gl:tex-parameter :texture-2d :texture-min-filter :linear)
  (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
  (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-border)
  (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-border)
  (gl:tex-image-2d  :texture-2d 0 :rgba
                    (aa-image-width (gui win)) (aa-image-height (gui win))
                    0 :rgba :unsigned-byte (aa-image (gui win))))

(defmethod glut:keyboard ((win window) key x y)
  (declare (ignore x y))
  (case key
    (#\escape (glut:destroy-current-window)))
  ;;(glut:post-redisplay)
  )

(defclass <gui-glut> (<gui-cl-vectors>)
  ())

(defun make-gui ()
  (make-instance '<gui-glut>))

(defmethod show-content ((gui <gui-glut>) width height title draw-fun)
  ;; This generates the image
  (call-next-method gui width height title draw-fun)
  ;; This displays it
  (trivial-main-thread:call-in-main-thread
   (lambda ()
     #+sbcl (sb-int:set-floating-point-modes :traps nil)
     (glut:display-window (make-instance 'window :gui gui
                                                 :width width
                                                 :height height)))))


"
(asdf:load-system 'draw-something/gui-glut)
(defvar d (draw-something.gui-base:create-drawing))
(defvar *gui* (gui-glut:make-gui))
(ds::make-composition-points d 256)
(gui-base:show-drawing *gui* d)
"
