;;  postscript.lisp - Writing PostScript to streams.
;;  Copyright (C) 2006, 2016 Rhea Myers rhea@myers.studio
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

(in-package "DRAW-SOMETHING")

(defvar *ps-stream* t)

(defun write-eps-header (width height &key (to *ps-stream*))
  "Write the standard raw PostScript header."
  (format to "%!PS-Adobe-3.0 EPSF-3.0~%")
  (format to "%%BoundingBox: 0 0 ~a ~a~%" width height)
  (format to "/L {lineto} bind def~%/M {moveto} bind def~%"))

(defun write-eps-footer (&key (to *ps-stream*))
  "Write the standard (but optional PostScript footer"
  (format to "%%EOF~%"))

(defun write-rgb (r g b &key (to *ps-stream*))
  "Set the PostScript RGB colour value."
  (format to "~F ~F ~F setrgbcolor~%" r g b))

(defun write-colour (col &key (to *ps-stream*))
  (multiple-value-bind (r g b) (hsb-to-rgb col)
    (write-rgb r g b :to to)))

(defun write-close-path (&key (to *ps-stream*))
  "Close the current PostScript path by drawing a line between its endpoints."
  (format to "closepath~%"))

(defun write-stroke (&key (to *ps-stream*))
  "Stroke the current PostScript path."
  (format to "stroke~%"))

(defun write-fill (&key (to *ps-stream*))
  "Fill the current PostScript path."
  (format to "fill~%"))

(defun write-new-path (&key (to *ps-stream*))
  "Start a new PostScript path."
  (format to "newpath~%"))

(defun write-moveto (x y &key (to *ps-stream*))
  "Move the PostScript pen to the given co-ordinates"
  (format to "~,3F ~,3F M " x y))

(defun write-lineto (x y &key (to *ps-stream*))
  "Draw a line with the PostScript pen to the given co-ordinates"
  (format to "~,3F ~,3F L " x y))

(defun write-subpath (points &key (to *ps-stream*))
  "Write a subpath of a PostScript path."
  (write-moveto (x (aref points 0))
                (y (aref points 0))
                :to to)
  (do ((i 1 (+ i 1)))
      ((= i (length points)))
    (write-lineto (x (aref points i))
                  (y (aref points i))
                  :to to)))

(defun write-rectfill (rect &key (to *ps-stream*))
  "Draw a rectangle with the given co-ordinates and dimensions."
  (format to "~F ~F ~F ~F rectfill~%" (x rect) (y rect) (width rect)
          (height rect)))


(defun write-rectstroke (rect &key (to *ps-stream*))
  "Draw a rectangle with the given co-ordinates and dimensions."
  (format to "~F ~F ~F ~F rectstroke~%" (x rect) (y rect) (width rect)
          (height rect)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun write-form-skeleton (the-form ps)
  "Write the skeleton the drawing is made around."
  (write-rgb 0.4 0.4 1.0 :to ps)
  (write-new-path :to ps)
  (write-subpath (points (skeleton the-form)) :to ps)
  (write-stroke :to ps))

(defun write-form-fill (the-form ps)
  "Write the drawing outline."
  (write-colour (fill-colour the-form) :to ps)
  (write-new-path :to ps)
  (write-subpath (points (outline the-form)) :to ps)
  (write-fill :to ps))

(defun write-form-stroke (the-form ps)
  "Write the drawing outline."
  (write-rgb 0.0 0.0 0.0 :to ps)
  ;;(write-rectstroke (bounds the-form) :to ps)
  (write-new-path :to ps)
  (write-subpath (points (outline the-form)) :to ps)
  (write-stroke :to ps))

(defun write-form (the-form ps)
  "Write the form."
  (write-form-fill the-form ps)
  ;;(write-figure-skeleton fig ps)
  ;;(write-form-stroke f ps)
  )

(defun write-figure (fig ps)
  "Write the figure for early multi-figure versions of draw-something."
  ;;(write-rgb 0.0 0.0 0.0 :to ps)
  ;;(write-rectstroke (bounds fig) :to ps)
  ;;(write-stroke :to ps)
  (loop for fm across (forms fig)
       do (write-form fm ps)))

(defun write-ground (the-drawing ps)
  "Colour the drawing ground."
  (write-colour (ground the-drawing) :to ps)
  (write-rectfill (bounds the-drawing) :to ps))

(defun write-frame (the-drawing ps)
  "Frame the drawing. Frame is bigger than PS bounds but should be OK."
  (write-rectstroke (inset-rectangle (bounds the-drawing) -1)
                    :to ps))

(defun eps-write-drawing (name the-drawing)
  "Write the drawing"
  (advisory-message (format nil "Writing drawing to file ~a .~%" name))
  (ensure-directories-exist save-directory)
  (with-open-file (ps name :direction :output
                      :if-exists :supersede)
    (write-eps-header (width (bounds the-drawing))
                      (height (bounds the-drawing))
                      :to ps)
    (write-ground the-drawing ps)
    ;;(write-frame the-drawing ps)
    (loop for plane across (planes the-drawing)
          do (loop for fig across (figures plane)
                   do (write-figure fig ps)))
    (write-eps-footer :to ps)
    (pathname ps)))

(defun eps-display-drawing (filepath)
  "Show the drawing to the user in the GUI."
  (let ((command
         #+(or macos macosx darwin) "/usr/bin/open"
         #-(or macos macosx darwin) "/usr/bin/gv"))
    #+sbcl (sb-ext:run-program command (list filepath) :wait nil)
    #+openmcl (ccl::os-command (format nil "~a ~a" command filepath)))
  filepath)

(defun write-and-show-eps (the-drawing)
  "Write and display the drawing as an eps file."
  (advisory-message "Saving and viewing drawing as eps.~%")
  (eps-display-drawing (eps-write-drawing (generate-filename)
                                          the-drawing)))
