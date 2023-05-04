

;; Set up state once, create and bind texture once.
;; tex-image-2d for each update.

(in-package :draw-something.gui)

(defparameter +width+ 512) ; must be power of two
(defparameter +height+ 512) ; must be power of two
(defparameter +window-width+ 800)
(defparameter +window-height+ 600)

(defvar *field* (make-array (* +width+ +height+ 4)
                            :element-type '(unsigned-byte 8)
                            ;; Alpha must be 0 .
                            :initial-element 0))

(loop for i below +width+ do
  (loop for j below +height+
        do (let ((pixel (* 4 (+ i (* +width+ j)))))
             (setf (aref *field* (+ 0 pixel)) (mod i 255)
                   (aref *field* (+ 1 pixel)) (mod (+ i j) 255)))))

(defclass window (glut:window)
  ((image :accessor image :initarg image)
   (texture-id :accessor texture-id :initform #x0))
  (:default-initargs :pos-x 100 :pos-y 100
                     :width +window-width+ :height +window-height+
                     :mode '(:double :rgb)))

(defmethod glut:display ((win window))
  (gl:clear-color 255 255 255 255)
  (gl:clear :color-buffer-bit)
  (gl:load-identity)
  (gl:with-primitive :quads
    (gl:tex-coord 0 0) (gl:vertex +width+ +height+)
    (gl:tex-coord 1 0) (gl:vertex 0       +height+)
    (gl:tex-coord 1 1) (gl:vertex 0       0)
    (gl:tex-coord 0 1) (gl:vertex +width+ 0))
  (glut:swap-buffers))

(defmethod glut:reshape ((win window) width height)
  (when (zerop height)
    (setq height 1))
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  ;; center the camera on the texture quad.
  (let* ((left (/ (- width +width+) -2.0))
         (right (+ left width))
         (bottom (/ (- height +height+) -2.0))
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
  (gl:tex-image-2d :texture-2d 0 :rgba +width+ +height+ 0
                   :rgba :unsigned-byte *field*))

(trivial-main-thread:call-in-main-thread
 (lambda ()
   (sb-int:set-floating-point-modes :traps nil)
    (glut:display-window (make-instance 'window))))


(defmacro show-with-canvas-bounds ((canvas-var-name width height title &rest title-format-args) &body body)
  ;; `(ltk:with-ltk ()
  ;;    (ltk:wm-title ltk:*tk* (format nil ,title ,@title-format-args))
  ;;    (let ((,canvas-var-name (make-instance 'ltk:canvas
  ;;                                           :width ,width
  ;;                                           :height ,height)))
  ;;      ;;(ltk:move-all ,canvas-var-name +drawing-x+
  ;;      ;;              (- (- +drawing-height+ +drawing-y+)))
  ;;      (ltk:scale ,canvas-var-name 1 -1)
  ;;      ,@body
  ;;      (ltk:pack ,canvas-var-name :expand 1 :fill :both)))
  )

(defmacro show-with-canvas ((canvas-var-name title &rest title-format-args) &body body)
  ;; `(show-with-canvas-bounds (canvas-var-name +drawing-x+ +drawing-y+
  ;;                                            ,title ,@title-format-args)
  ;;    ,@body
  ;;    (let ((bounds (ltk:create-rectangle ,canvas-var-names
  ;;                                        +drawing-x+
  ;;                                        +drawing-y+
  ;;                                        (+ +drawing-x+ +drawing-width+)
  ;;                                        (+ +drawing-y+ +drawing-height+))))
  ;;      (ltk:itemconfigure ,canvas-var-name bounds :fill "")
  ;;      (ltk:itemconfigure ,canvas-var-name bounds :outline "magenta")
  ;;      (ltk:itemconfigure ,canvas-var-name bounds :width 2))
  ;;    (ltk:pack ,canvas-var-name :expand 1 :fill :both))
  )

(defun create-rect (c x y width height colour)
  "Display a sample of a single <colour>."
  ;; (ltk:itemconfigure c
  ;;                    (ltk:create-rectangle c x y width height)
  ;;                    :fill (hsb-to-rgb-hex colour))
  )

(defun create-points (c points colour)
  ;; (loop for point across points
  ;;       do (ltk:create-oval c
  ;;                           (- (x point) +point-radius+)
  ;;                           (- (y point) +point-radius+)
  ;;                           (+ (x point) +point-radius+)
  ;;                           (+ (y point) +point-radius+)))
  )

(defun polyline-to-line-coords (polyline)
  "Convert a sequence of <point>s to a flat list of xn yn... ."
  ;; Reverse the co-ordinates so the order is the same as the original
  ;; Notice this means that we have to have X and Y the wrong way round below!
  ;; (reverse (loop for point across (points polyline)
  ;;                collect (y point)
  ;;                collect (x point)))
  )

(defun create-polyline-stroke (c polyline &optional colour)
  ;; (let (l (ltk:create-line c (polyline-to-line-coords polyline)))
  ;;   (when colour
  ;;     (ltk:itemconfigure c l colour)))
  )

(defun create-polyline-fill (c polyline &optional (colour))
;; (let (l (ltk:create-polygon c (polyline-to-line-coords polyline)))
;;    (when colour
;;      (ltk:itemconfigure c l colour)))
  )
