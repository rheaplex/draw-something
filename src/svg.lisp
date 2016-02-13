;;  svg.lisp - Writing SVG to streams.
;;  Copyright (C) 2007, 2010, 2016 Rob Myers rob@robmyers.org
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

(defvar *svg-stream* t)

(defun svg-header (width height &key (to *svg-stream*))
  "Write the start of the SVG file."
  (format to "<?xml version=\"1.0\" standalone=\"no\"?>~%")
  (format to "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"~%")
  (format to "  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">~%")
  (format to "<svg width=\"~dpx\" height=\"~dpx\" viewBox=\"0 0 ~d ~d\"~%"
          width height width height)
  (format to "xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">~%"))

(defun svg-footer (&key (to *svg-stream*))
  "Write the end of the svg file."
  (format to "</svg>~%"))

(defun svg-rgb (col)
  (multiple-value-bind (r g b) (hsb-to-rgb col)
    (format nil "#~2,'0X~2,'0X~2,'0X" (round (* r 255)) 
	    (round (* g 255)) (round (* b 255)))))

(defun svg-path-tag-start (&key (to *svg-stream*))
  "Write the start of the path tag."
  (format to "<path"))

(defun svg-path-tag-end (&key (to *svg-stream*))
  "Write the start of the path tag."
  (format to " />~%"))

(defun svg-path-d-start (&key (to *svg-stream*))
  "Write the start of the path d."
  (format to " d=\""))

(defun svg-path-d-end (&key (to *svg-stream*))
  "Write the start of the path d."
  (format to "\""))

(defun svg-fill (col &key (to *svg-stream*))
  "Write the fill property."
  (format to " fill=\"~a\" " (svg-rgb col)))

(defun svg-stroke (col &key (to *svg-stream*))
  "Write the stroke property."
  (format to " stroke=\"~a\" " (svg-rgb col)))

(defun svg-close-path (&key (to *svg-stream*))
  "Close the current PostScript path by drawing a line between its endpoints."
  (format to " z"))

(defun svg-moveto (x y &key (to *svg-stream*))
  "Move the PostScript pen to the given co-ordinates"
  (format to " M ~,3F ~,3F" x y))

(defun svg-lineto (x y &key (to *svg-stream*))
  "Draw a line with the PostScript pen to the given co-ordinates"
  (format to " L ~,3F ~,3F" x y))

(defun svg-subpath (points &key (to *svg-stream*))
  "Write a subpath of a PostScript path."
  (svg-moveto (x (aref points 0))
                (y (aref points 0))
                :to to)
  (do ((i 1 (+ i 1)))
      ((= i (length points)))
    (svg-lineto (x (aref points i))
                  (y (aref points i))
                  :to to)))

(defun svg-rectfill (rect col &key (to *svg-stream*))
  "Draw a rectangle with the given co-ordinates and dimensions."
  (format to
          "<rect x=\"~F\" y=\"~F\" width=\"~F\" height=\"~F\" fill=\"~a\" />~%"
          (x rect) (y rect) (width rect) (height rect) (svg-rgb col)))

(defun svg-rectstroke (rect col &key (to *svg-stream*))
  "Draw a rectangle with the given co-ordinates and dimensions."
  (format to
   "<rect x=\"~F\" y=\"~F\" width=\"~F\" height=\"~F\" stroke=\"~a\" />~%"
   (x rect) (y rect) (width rect) (height rect) (svg-rgb col)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun svg-form-skeleton (the-form ps)
  "Write the skeleton the drawing is made around."
  (svg-path-tag-start :to ps)
  (svg-stroke (make-instance 'colour
                                   :hue 0.3
                                   :saturation 0.6
                                   :brightness 0.6)
                    :to ps)
  (svg-path-d-start :to ps)
  (svg-subpath (points (skeleton the-form)) :to ps)
  (svg-path-d-end :to ps)
  (svg-path-tag-end :to ps))

(defun svg-form-fill (the-form ps)
  "Write the drawing outline."
  (svg-path-tag-start :to ps)
  (svg-fill (fill-colour the-form)
                  :to ps)
  (svg-path-d-start :to ps)
  (svg-subpath (points (outline the-form)) :to ps)
  (svg-path-d-end :to ps)
  (svg-path-tag-end :to ps))

(defun svg-form-stroke (the-form ps)
  "Write the drawing outline."
 (svg-path-tag-start :to ps)
  (svg-stroke (make-instance 'colour
                                   :hue 0.0
                                   :saturation 0.0
                                   :brightness 0.0)
                    :to ps)
  (svg-path-d-start :to ps)
  (svg-subpath (points (outline the-form)) :to ps)
  (svg-path-d-end :to ps)
  (svg-path-tag-end :to ps))

(defun svg-form (the-form ps)
  "Write the form."
  (svg-form-fill the-form ps)
  ;;(svg-figure-skeleton fig ps)
  ;;(svg-form-stroke the-form ps)
  )

(defun svg-figure (fig ps)
  "Write the figure for early multi-figure versions of draw-something."
  ;;(svg-rgb 0.0 0.0 0.0 :to ps)
  ;;(svg-rectstroke (bounds fig) :to ps)
  ;;(svg-stroke :to ps)
  (loop for fm across (forms fig)
       do (svg-form fm ps)))

(defun svg-ground (the-drawing ps)
  "Colour the drawing ground."
  (svg-rectfill (bounds the-drawing) (ground the-drawing)
                :to ps))

(defun svg-frame (the-drawing ps)
  "Frame the drawing. Frame is bigger than PS bounds but should be OK."
  (svg-rectstroke (inset-rectangle (bounds the-drawing) -1)
                    (make-instance 'colour :brightness 0.0)
                    :to ps))

(defun svg-write-drawing (name the-drawing)
  "Write the drawing"
  (advisory-message (format nil "Writing drawing to file ~a .~%" name))
  (ensure-directories-exist save-directory)
  (with-open-file (ps name :direction :output
                      :if-exists :supersede)
    (svg-header (width (bounds the-drawing))
                (height (bounds the-drawing))
                      :to ps)
    (svg-ground the-drawing ps)
    ;;(svg-frame the-drawing ps)
    (loop for plane across (planes the-drawing)
       do (loop for fig across (figures plane)
                   do (svg-figure fig ps)))
    (svg-footer :to ps)
    (pathname ps)))

(defun svg-display-drawing (filepath)
  "Show the drawing to the user in the GUI."
  (let ((command
         #+(or macos macosx darwin) "/usr/bin/open"
         #-(or macos macosx darwin) "/usr/bin/iceweasel"))
    #+sbcl (sb-ext:run-program command (list filepath) :wait nil)
    #+openmcl (ccl::os-command (format nil "~a ~a" command filepath)))
  filepath)

(defun write-svg (the-drawing &optional (filespec nil))
  "Write the drawing as an svg file."
  (advisory-message "Saving drawing as svg.~%")
  (svg-write-drawing (if filespec filespec (generate-filename ".svg"))
                     the-drawing))

(defun write-and-show-svg (the-drawing &optional (filespec nil))
  "Write and display the drawing as an svg file."
  (advisory-message "Viewing drawing as svg.~%")
  (svg-display-drawing (write-svg the-drawing filespec)))
