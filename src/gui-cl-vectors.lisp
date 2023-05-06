;; gui-cl-vectors.lisp - A simple cl-vectros rendering layer for draw-something.
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

(defpackage draw-something.gui-cl-vectors
  (:nicknames :gui-cl-vectors)
  (:use :common-lisp)
  (:import-from #:draw-something.gui-base
                #:<gui>
                #:show-content
                #:create-rect-fill
                #:create-rect-stroke
                #:create-points
                #:create-polyline-stroke
                #:create-polyline-fill)
  (:export  #:<gui-cl-vectors>
            #:aa-image
            #:aa-image-width
            #:aa-image-height
            #:show-content
            #:create-rect-fill
            #:create-rect-stroke
            #:create-points
            #:create-polyline-stroke
            #:create-polyline-fill))

(in-package :draw-something.gui-cl-vectors)

(defclass <gui-cl-vectors> (<gui>)
  ((aa-state
    :reader aa-state
    :initform (aa:make-state))
   (aa-image
    :accessor aa-image)
   (aa-image-width
    :accessor aa-image-width)
   (aa-image-height
    :accessor aa-image-height))
  (:documentation "A mixin class that stores state for cl-vectors"))

;; FIXME: ignores opacity and alpha-function
(defun image-put-pixel (gui &optional (color #(0 0 0)) (opacity 1.0) (alpha-function :normalized))
  (let ((image (aa-image gui))
        (width (aa-image-width gui))
        ;;(height (aa-image-height gui))
        )
    (lambda (x y alpha)
      (declare (optimize speed (safety 0) (debug 0)))
      (let ((base (* 4 (+ x (* y width)))))
        (setf (aref image base)       (aref color 0))
        (setf (aref image (+ base 1)) (aref color 1))
        (setf (aref image (+ base 2)) (aref color 2))))))

;;TODO: optimize to use aa:cells-sweep/rectangle
(defun rasterize (gui colour)
  (let* ((state (aa-state gui))
         (put-pixel (image-put-pixel gui colour))
         ;;(put-span (image-put-span gui colour))
         )
    (aa:cells-sweep state put-pixel ;;;put-span
                    )))

(defun use-state (gui paths)
  (let ((state (aa-state gui)))
    (aa:state-reset state)
    (if (listp paths)
        (vectors:update-state state paths)
        (vectors:update-state state (list paths)))))

(defmethod show-content ((gui <gui-cl-vectors>) width height title fun)
  (declare (ignore title))
  (setf (aa-image-width gui)  width
        (aa-image-height gui) height
        (aa-image gui)        (make-array (* width height 4)
                                          :element-type '(unsigned-byte 8)
                                          :initial-element 255))
  (funcall fun gui))

(defmethod create-rect-fill ((gui <gui-cl-vectors>) rect colour)
  (use-state gui (paths:make-rectangle-path
                  (ds::x rect)
                  (ds::y rect)
                  (+ (ds::x rect) (ds::width rect))
                  (+ (ds::y rect) (ds::height rect))))
  (rasterize gui
             (ds::hsb-to-rgb-vector colour)))

(defmethod create-rect-stroke ((gui <gui-cl-vectors>) rect colour width)
  (when colour
    (use-state gui
               (paths:stroke-path (paths:make-rectangle-path
                                   (ds::x rect)
                                   (ds::y rect)
                                   (+ (ds::x rect) (ds::width rect))
                                   (+ (ds::y rect) (ds::height rect)))
                                  width))
    (rasterize gui
               (ds::hsb-to-rgb-vector colour))))

(defmethod create-points ((gui <gui-cl-vectors>) points colour radius)
  (use-state gui
             (loop for point across points
                   collect
                   (paths:make-circle-path (ds::x point)
                                           (ds::y point)
                                           radius)))
  (rasterize gui
             (ds::hsb-to-rgb-vector colour)))

(defun polyline-to-line-coords (polyline)
  "Convert a sequence of <point>s to a list of (x . y)."
  ;; Reverse the co-ordinates so the order is the same as the original
  (reverse (loop for point across (ds::points polyline)
                 collect (cons (ds::x point) (ds::y point)))))

(defmethod create-polyline-stroke ((gui <gui-cl-vectors>) polyline colour width)
  (when colour
    (use-state gui
               (paths:stroke-path (paths:make-simple-path
                                   (polyline-to-line-coords polyline)
                                   :open-polyline)
                                  width))
    (rasterize gui
               (ds::hsb-to-rgb-vector colour))))

(defmethod create-polyline-fill ((gui <gui-cl-vectors>) polyline colour)
  (when colour
    (use-state gui
               (paths:make-simple-path (polyline-to-line-coords polyline)
                                       :polygon))
    (rasterize gui
               (ds::hsb-to-rgb-vector colour))))
