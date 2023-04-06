;;  svg.lisp - Writing SVG to streams.
;;  Copyright (C) 2007, 2010, 2016, 2021 Rhea Myers
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

(defvar *layer-index* 1)

(defun to-svg-y (ps-y svg-height)
  "Flip our y-up values to y-down."
  (- svg-height ps-y))

(defun flip-points (points svg-height)
  "Flip the y of a copy of each of the points."
  (map 'vector (lambda (p)
                 (make-instance '<point>
                                :x (point-x p)
                                :y (to-svg-y (point-y p) svg-height)))))

(defun svg-header (width height &key (to *svg-stream*))
  "Write the start of the SVG file."
  (format to "<?xml version=\"1.0\" standalone=\"no\"?>~%")
  (format to "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"~%")
  (format to "  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">~%")
  ;; Note that we use millimetres
  (format to "<svg width=\"~dmm\" height=\"~dmm\"~%  viewBox=\"0 0 ~d ~d\"~%"
          width height width height)
  ;; For previewing
  (format to "  viewport-fill=\"white\"~%")
  (format to "  xmlns=\"http://www.w3.org/2000/svg\"~%  version=\"1.1\"~%")
  ;; For the layer information
  (format
   to
   "  xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\">~%"))

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
  (if col
      (format to " fill=\"~a\"" (svg-rgb col))
    (format to " fill=\"none\"")))

(defun svg-stroke (col &key (to *svg-stream*))
  "Write the stroke property."
  (if col
      (format to " stroke=\"~a\"" (svg-rgb col))
    (format to " stroke=\"none\"")))

(defun svg-close-path (&key (to *svg-stream*))
  "Close the current PostScript path by drawing a line between its endpoints."
  (format to " z"))

(defun svg-moveto (x y y-height &key (to *svg-stream*))
  "Move the PostScript pen to the given co-ordinates"
  (format to " M ~,3F ~,3F" x (to-svg-y y y-height)))

(defun svg-lineto (x y y-height &key (to *svg-stream*))
  "Draw a line with the PostScript pen to the given co-ordinates"
  (format to " ~,3F ~,3F" x (to-svg-y y y-height)))

(defun svg-subpath (points y-height &key (to *svg-stream*))
  "Write a subpath of a PostScript path."
  (svg-moveto (x (aref points 0))
              (y (aref points 0))
              y-height
              :to to)
  (do ((i 1 (+ i 1)))
      ((= i (length points)))
    (svg-lineto (x (aref points i))
                (y (aref points i))
                y-height
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

(defun svg-layer-start (kind &key (to *svg-stream*))
  "Start an Inkscape layer for Axidraw."
  (format to
          "<g inkscape:groupmode=\"layer\" inkscape:label=\"~d-~a\">~%"
          *layer-index*
          kind)
  (incf *layer-index*))

(defun svg-layer-end (&key (to *svg-stream*))
  "End an Inkscape layer."
  (format to "</g>~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing writing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun svg-form-skeleton (form y-height svg)
  "Write the skeleton the drawing is made around."
  (svg-layer-start "skeleton" :to svg)
  (svg-path-tag-start :to svg)
  (svg-fill nil :to svg)
  (svg-stroke (make-instance '<colour>
                             :hue 0.7
                             :saturation 0.4
                             :brightness 0.9)
              :to svg)
  (svg-path-d-start :to svg)
  ;; This will break if we use other shapes in the skeleton.
  (svg-subpath (points (aref (skeleton form) 0)) y-height :to svg)
  (svg-path-d-end :to svg)
  (svg-path-tag-end :to svg)
  (svg-layer-end :to svg))

(defun svg-form-fill (form y-height svg)
  "Write the drawing fill."
  (if (fill-colour form)
      (progn
        (svg-path-tag-start :to svg)
        (svg-stroke nil :to svg)
        (svg-fill (fill-colour form) :to svg)
        (svg-path-d-start :to svg)
        (svg-subpath (points (outline form)) y-height :to svg)
        (svg-path-d-end :to svg)
        (svg-path-tag-end :to svg))))

(defun svg-form-stroke (form y-height svg)
  "Write the drawing outline."
  (if (stroke-colour form)
      (progn
        (svg-layer-start "outline" :to svg)
        (svg-path-tag-start :to svg)
        (svg-fill nil :to svg)
        (svg-stroke (stroke-colour form) :to svg)
        (svg-path-d-start :to svg)
        (svg-subpath (points (outline form)) y-height :to svg)
        (svg-path-d-end :to svg)
        (svg-path-tag-end :to svg)
        (svg-layer-end :to svg))))

(defun svg-form (form y-height svg)
  "Write the form."
  (svg-form-fill form y-height svg)
  ;;(svg-form-skeleton form y-height svg)
  ;;(svg-form-stroke form y-height svg)
  )

(defun svg-write-form (form drawing-bounds filespec)
  "Write the form"
  (advisory-message (format nil "Writing form to file ~a .~%" filespec))
  (ensure-directories-exist *save-directory*)
  (with-open-file (svg filespec :direction :output
                      :if-exists :supersede)
                  (svg-header (width drawing-bounds)
                              (height drawing-bounds)
                              :to svg)
                  ;;(svg-ground drawing svg)
                  ;;(svg-frame drawing svg)
                  (svg-form form (height drawing-bounds) svg)
                  (svg-footer :to svg)
                  (pathname svg)))


(defun write-svg-form (form drawing-bounds &optional (filespec nil))
  "Write the form as an svg file."
  (advisory-message "Saving form as svg.~%")
  (svg-write-form form
                  drawing-bounds
                  (if filespec filespec (generate-filename ".svg"))))

(defun svg-figure (figure y-height svg)
  "Write the figure for early multi-figure versions of draw-something."
  ;;(svg-rgb 0.0 0.0 0.0 :to svg)
  ;;(svg-rectstroke (bounds fig) :to svg)
  ;;(svg-stroke :to svg)
  (loop for fm across (forms figure)
       do (svg-form fm y-height svg)))

(defun svg-ground (drawing svg)
  "Colour the drawing ground."
  (let ((ground-colour (ground drawing)))
    (if ground-colour
        (svg-rectfill (bounds drawing) ground-colour :to svg))))

(defun svg-frame (drawing svg)
  "Frame the drawing. Frame is bigger than SVG bounds but should be OK."
  (svg-rectstroke (inset-rectangle (bounds drawing) -1)
                    (make-instance '<colour> :brightness 0.0)
                    :to svg))

(defun svg-write-drawing (page-size drawing filespec)
  "Write the drawing"
  (let ((page-width (car page-size))
        (page-height (cdr page-size)))
    (advisory-message (format nil "Writing drawing to file ~a .~%" filespec))
    (ensure-directories-exist *save-directory*)
    (with-open-file (svg filespec :direction :output
                         :if-exists :supersede)
      (svg-header page-width page-height :to svg)
      (svg-ground drawing svg)
      ;;(svg-frame drawing svg)
      (loop for plane across (planes drawing)
            do (loop for fig across (figures plane)
                     do (svg-figure fig page-height svg)))
      (svg-footer :to svg)
      (pathname svg))))

(defun svg-display-drawing (filepath)
  "Show the drawing to the user in the GUI."
  (let ((command
         #+(or macos macosx darwin) "/usr/bin/open"
         #-(or macos macosx darwin) "/usr/bin/display"))
    #+sbcl (sb-ext:run-program command (list filepath) :wait nil)
    #+openmcl (ccl::os-command (format nil "~a ~a" command filepath)))
  filepath)

(defun write-svg (page-size drawing &optional (filespec nil))
  "Write the drawing as an svg file."
  (advisory-message "Saving drawing as svg.~%")
  (svg-write-drawing page-size drawing (if filespec filespec (generate-filename ".svg"))))

(defun write-and-show-svg (page-size drawing &optional (filespec nil))
  "Write and display the drawing as an svg file."
  (advisory-message "Viewing drawing as svg.~%")
  (svg-display-drawing (write-svg page-size drawing filespec)))
