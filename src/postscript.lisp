;; postscript.lisp - Writing PostScript to streams.
;; Copyright (C) 2006  Rhea Myers rhea@myers.studio
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

(in-package :draw-something)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing the drawing as an EPS file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *ps-stream* t)

(defmethod write-eps-header (x y width height &key (to *ps-stream*))
  "Write the standard EPS header."
  (format to "%!PS-Adobe-3.1 EPSF-3.0~%%ADO_DSC_Encoding: UTF8~%")
  (format to "%%Creator: (draw-something)")
  (format to "%%BoundingBox: ~d ~d ~d ~d~%" x y width height)
 ;;(format to "%%HiResBoundingBox: ~f ~f ~f ~f~%" x y width height)
 ;; (format to "%%CropBox: ~f ~f ~f ~f~%" x y width height)
  (format to "%%LanguageLevel: 3~%%Pages: 1~%")
  (format to "%%EndComments~%%%BeginProlog~%")
  (format to "/L {lineto} bind def~%/M {moveto} bind def~%")
  (format to "%%EndProlog~%"))

(defmethod write-eps-footer (&key (to *ps-stream*))
  "Write the standard (but optional) PostScript footer"
  (format to "%%EOF~%"))

(defmethod write-rgb (r g b &key (to *ps-stream*))
  "Set the PostScript RGB colour value."
  (format to "~F ~F ~F setrgbcolor~%" r g b))

(defmethod write-colour ((col <colour>) &key (to *ps-stream*))
  (multiple-value-bind (r g b) (colour-to-rgb col)
    (write-rgb r g b :to to)))

(defmethod write-close-path (&key (to *ps-stream*))
  "Close the current PostScript path by drawing a line between its endpoints."
  (format to "~%closepath~%"))

(defmethod write-stroke (&key (to *ps-stream*))
  "Stroke the current PostScript path."
  (format to "~%stroke~%"))

(defmethod write-fill (&key (to *ps-stream*))
  "Fill the current PostScript path."
  (format to "~%fill~%"))

(defmethod write-new-path (&key (to *ps-stream*))
  "Start a new PostScript path."
  (format to "newpath~%"))

(defmethod write-moveto (x y &key (to *ps-stream*))
  "Move the PostScript pen to the given co-ordinates"
  (format to "~,3F ~,3F M " x y))

(defmethod write-lineto (x y &key (to *ps-stream*))
  "Draw a line with the PostScript pen to the given co-ordinates"
  (format to "~,3F ~,3F L " x y))

(defmethod write-subpath (points &key (to *ps-stream*))
  "Write a subpath of a PostScript path."
  (write-moveto (x (aref points 0))
                (y (aref points 0))
                :to to)
  (do ((i 1 (+ i 1)))
      ((= i (length points)))
    (write-lineto (x (aref points i))
                  (y (aref points i))
                  :to to)))

(defmethod write-rectfill ((rect <rectangle>) &key (to *ps-stream*))
  "Draw a rectangle with the given co-ordinates and dimensions."
  (format to "~F ~F ~F ~F rectfill~%" (x rect) (y rect) (width rect)
          (height rect)))


(defmethod write-rectstroke ((rect <rectangle>) &key (to *ps-stream*))
  "Draw a rectangle with the given co-ordinates and dimensions."
  (format to "~F ~F ~F ~F rectstroke~%" (x rect) (y rect) (width rect)
          (height rect)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod write-form-skeleton ((f <form>) ps)
  "Write the skeleton the drawing is made around."
  (format ps "% START: skeleton~%")
  (loop for skel across (skeleton f)
        do (format ps "% ")
           (write-subpath (points skel) :to ps)
           (format ps "~%"))
  (format ps "% END: skeleton~%"))

(defmethod write-form-fill ((f <form>) ps)
  "Write the drawing outline."
  (format ps "% START: fill~%")
  (write-colour (fill-colour f) :to ps)
  (write-new-path :to ps)
  (write-subpath (points (outline f)) :to ps)
  (write-fill :to ps)
  (format ps "% END: fill~%"))

(defmethod write-form-stroke ((f <form>) ps)
  "Write the drawing outline."
  (format ps "% START: stroke~%")
  ;;(write-rgb 0.0 0.0 0.0 :to ps)
  (write-colour (stroke-colour f) :to ps)
  ;;(write-rectstroke (bounds f) :to ps)
  (write-new-path :to ps)
  (write-subpath (points (outline f)) :to ps)
  (write-stroke :to ps)
  (format ps "% END: stroke~%"))

(defmethod write-form ((f <form>) ps)
  "Write the form."
  (format ps "% START: form~%")
  (write-form-skeleton f ps)
  (when (fill-colour f)
    (write-form-fill f ps))
  (when (stroke-colour f)
    (write-form-stroke f ps))
  (format ps "% END: form~%"))

(defmethod write-figure ((fig <figure>) ps)
  "Write the figure for early multi-figure versions of draw-something."
  (log-info "Writing figure - bounds: ~a." (bounds fig))
  ;;(write-rgb 0.0 0.0 0.0 :to ps)
  ;;(write-rectstroke (bounds fig) :to ps)
  ;;(write-stroke :to ps)
  (format ps "% START: figure~%")
  (loop for fm across (forms fig)
        do (write-form fm ps))
  (when (composition-bounds fig)
    (write-colour (fill-colour (aref (forms fig) 0)) :to ps)
    (write-rectstroke (composition-bounds fig) :to ps))
  (format ps "% END: figure~%"))

(defmethod write-ground ((the-drawing <drawing>) ps)
  "Colour the drawing ground."
  (format ps "% START: ground~%")
  (write-colour (ground the-drawing) :to ps)
  (write-rectfill (bounds the-drawing) :to ps)
  (format ps "% END: ground~%"))

(defmethod write-frame ((the-drawing <drawing>) ps)
  "Frame the drawing. Frame is bigger than PS bounds but should be OK."
  (write-rectstroke (inset-rectangle (bounds the-drawing) -1)
                    :to ps))

(defmethod write-drawing ((the-drawing <drawing>) directory filename)
  "Write the drawing"
  ;;(log-info "Writing drawing to file ~a" filename)
  (ensure-directories-exist directory)
  (let ((filepath (merge-pathnames (make-pathname
                                    :name filename
                                    :type "eps")
                                   directory)))
  (with-open-file (ps filepath
                      :direction :output
                      :if-exists :supersede)
    (write-eps-header (x (bounds the-drawing))
                      (y (bounds the-drawing))
                      (+ (x (bounds the-drawing))
                         (width (bounds the-drawing)))
                      (+ (y (bounds the-drawing))
                         (height (bounds the-drawing)))
                      :to ps)
    (format ps
            "% SUBSTRATE SIZE: ~d ~d ~d ~d~%"
            (x (substrate-bounds the-drawing))
            (y (substrate-bounds the-drawing))
            (+ (x (substrate-bounds the-drawing))
               (width (substrate-bounds the-drawing)))
            (+ (y (substrate-bounds the-drawing))
               (height (substrate-bounds the-drawing))))
    (write-ground the-drawing ps)
    ;;(write-frame the-drawing ps)
    (loop for plane across (planes the-drawing)
          do (log-info "WRITING PLANE - ~d FIGURES." (length (figures plane)))
             (format ps "% START: plane~%")
             (loop for fig across (figures plane)
                   do (write-figure fig ps))
             (format ps "% END: plane~%"))
    (write-eps-footer :to ps)
    (pathname ps))))
