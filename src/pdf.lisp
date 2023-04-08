;;  pdf.lisp - Writing PDF to streams.
;;  Copyright (C) 2023 Myers Studio Ltd.
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

(defpackage #:draw-something.pdf
  (:use #:cl)
  (:import-from #:pdf
                #:close-and-fill
                #:close-and-stroke
                #:fill-path
                #:line-to
                #:move-to
                #:rectangle
                #:register-page-reference
                #:set-line-width
                #:set-rgb-fill
                #:set-rgb-stroke
                #:with-document
                #:with-outline-level
                #:with-page
                #:with-saved-state
                #:write-document)
  (:import-from #:draw-something.colour
                #:hsb-to-rgb)
  (:import-from #:draw-something.geometry
                #:x
                #:y
                #:width
                #:height)
  (:import-from #:draw-something.drawing
                #:bounds
                #:figures
                #:fill-colour
                #:forms
                #:ground
                #:outline
                #:planes
                #:points)
  (:export
   #:write-pdf
   #:write-and-show-pdf))

(in-package #:draw-something.pdf)

(defvar *layer-index* 1)

(defun pdf-fill-rgb (col)
  (multiple-value-bind (r g b) (hsb-to-rgb col)
    (set-rgb-fill r g b)))

(defun pdf-stroke-rgb (col)
  "Write the stroke property."
  (multiple-value-bind (r g b) (hsb-to-rgb col)
    (set-rgb-stroke r g b)))

(defun pdf-draw-path (points)
  "Add a path to the page."
  (move-to (x (aref points 0))
           (y (aref points 0)))
  (do ((i 1 (+ i 1)))
      ((= i (length points)))
    (line-to (x (aref points i))
             (y (aref points i)))))

(defun pdf-rectfill (rect col)
  "Draw a rectangle with the given co-ordinates and dimensions."
  (with-saved-state
    (pdf-fill-rgb col)
    (rectangle (x rect) (y rect) (width rect) (height rect))
    (close-and-fill)))

(defun pdf-rectstroke (rect col)
  "Draw a rectangle with the given co-ordinates and dimensions."
  (with-saved-state
    (pdf-stroke-rgb col)
    (set-line-width 1)
    (rectangle (x rect) (y rect) (width rect) (height rect))
    (close-and-stroke)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pdf-form-skeleton (form)
  "Write the skeleton the drawing is made around."
  (with-saved-state
    (pdf-stroke-rgb (make-instance '<colour>
                                   :hue 0.7
                                   :saturation 0.4
                                   :brightness 0.9))
    (set-line-width 1)
    ;; This will break if we use other shapes in the skeleton.
    (pdf-draw-path (points (aref (skeleton form) 0)))
    (stroke)))

(defun pdf-form-fill (form)
  "Write the drawing fill."
  (if (fill-colour form)
      (with-saved-state
        (pdf-fill-rgb (fill-colour form))
        (pdf-draw-path (points (outline form)))
        (fill-path))))

(defun pdf-form-stroke (form)
  "Write the drawing outline."
  (if (stroke-colour form)
      (with-saved-state
        (pdf-stroke-rgb (stroke-colour form))
        (pdf-draw-path (points (outline form)))
        (stroke))))

(defun pdf-form (form)
  "Write the form."
  (pdf-form-fill form)
  ;;(pdf-form-skeleton form)
  ;;(pdf-form-stroke form)
  )

#|
(defun pdf-write-form (form drawing-bounds filespec)
"Write the form"
(log:info "Writing form to file ~a ." filespec)
(ensure-directories-exist *save-directory*)
;;(pdf-ground drawing pdf)
;;(pdf-frame drawing pdf)
(pdf-form form)
filespec))

(defun write-pdf-form (form drawing-bounds &optional (filespec nil))
"Write the form as an pdf file."
(log:info "Saving form as pdf.")
(pdf-write-form form
drawing-bounds
(if filespec filespec (generate-filename ".pdf"))))
|#

(defun pdf-figure (figure)
  "Write the figure for early multi-figure versions of draw-something."
  ;;(pdf-rgb 0.0 0.0 0.0 :to pdf)
  ;;(pdf-rectstroke (bounds fig) :to pdf)
  ;;(pdf-stroke :to pdf)
  (loop for fm across (forms figure)
        do (pdf-form fm)))

(defun pdf-ground (drawing)
  "Colour the drawing ground."
  (let ((ground-colour (ground drawing)))
    (if ground-colour
        (pdf-rectfill (bounds drawing) ground-colour))))

(defun pdf-frame (drawing)
  "Frame the drawing. Frame is bigger than PDF bounds but should be OK."
  (pdf-rectstroke (inset-rectangle (bounds drawing) -1)
                  (make-instance '<colour> :brightness 0.0)))

(defun pdf-write-drawing (page-size drawing filespec)
  "Write the drawing"
  (log:info "Writing drawing to file ~a ." filespec)
  (with-document ()
    (with-page (:bounds (vector 0 0 (car page-size) (cdr page-size)))
      (with-outline-level ((format nil "draw-something ~a" filespec)
                           (register-page-reference))
        (pdf-ground drawing)
        ;;(pdf-frame drawing)
        (loop for plane across (planes drawing)
              do (loop for fig across (figures plane)
                       do (pdf-figure fig)))))
    (write-document filespec))
  filespec)

(defun pdf-display-drawing (filepath)
  "Show the drawing to the user in the GUI."
  (let ((command
          #+(or macos macosx darwin) "/usr/bin/open"
          #-(or macos macosx darwin) "/usr/bin/xdg-open"))
    (uiop:run-program (list command (namestring filepath))))
  filepath)

(defun write-drawing (page-size drawing directory filename)
  "Write the drawing as an pdf file."
  (log:info "Saving drawing as pdf.")
  (ensure-directories-exist directory)
  (pdf-write-drawing page-size
                     drawing
                     (merge-pathnames
                      (make-pathname
                       :name filename
                       :type "pdf")
                      directory)))

(defun write-and-show-drawing (page-size drawing directory filename)
  "Write and display the drawing as an pdf file."
  (log:info "Viewing drawing as pdf.")
  (pdf-display-drawing (write-drawing page-size drawing directory filename)))
