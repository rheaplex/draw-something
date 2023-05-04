(in-package :draw-something-gui)


;; (aa-misc:make-image width height #(255 255 255))

;; Re-used to draw each group of paths of the same colour.
(defvar *aa-state* (aa:make-state))

;;TODO: optimize to use aa:cells-sweep/rectangle
(defun rasterize (image state colour)
  (let ((put-pixel (aa-misc:image-put-pixel image colour))
        (put-span (aa-misc:image-put-span image colour)))
	(aa:cells-sweep state put-pixel put-span)))

(defun use-state (paths)
  (aa:reset-state *aa-state*)
  (if (listp paths)
      (vectors:update-state *aa-state* paths)
      (vectors:update-state *aa-state* (list paths)))
  *aa-state*)

;;FIXME: CREATE RECT FILL AND STROKE 

(defun create-colour (image x y width height colour)
  (rasterize image
             (use-state (paths:make-rectangle-path x y width height))
             (hsb-to-rgb-vector colour)))

(defun create-points (image points colour)
  (rasterize image
             (use-state (loop for point across points
                              collect
                              (path:make-circle-path (x point)
                                                     (y point)
                                                     +point-radius+)))
             (hsb-to-rgb-vector colour)))

;;FIXME: CREATE POLYLINE FILL AND STROKE

(defun create-polyline-stroke (image polyline colour line-width)
  (when colour
    (rasterize image
               (use-state (paths:stroke-path line-width
                                             (paths:make-simple-path
                                              (polyline-to-line-coords polyline)
                                              :open-polyline)))
               (hsb-to-rgb-vector colour))))

  (defun create-polyline-fill (image polyline colour)
    (when colour
      (rasterize image
                 (use-state (paths:make-simple-path (polyline-to-line-coords polyline)
                                                    :polygon)
                            (hsb-to-rgb-vector colour)))))
