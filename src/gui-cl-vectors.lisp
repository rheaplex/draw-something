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

(in-package #:draw-something)

;; See rasterize.lisp

#|

(defvar d (ltk-gui:make-drawing))

(ltk-gui:show-colour (drawing:ground d))

(drawing:make-composition-points d (choosing:random-range 8 42))
(ltk-gui:show-drawing-composition-points d)

(drawing:make-planes d (drawing:figure-generation-methods ltk-gui::+planes-count+))
(drawing:make-planes-skeletons d)
(ltk-gui:show-plane-skeletons d 1)
(ltk-gui:show-planes-skeletons d)

(drawing:draw-planes-figures d)

(ltk-gui:show-plane d 1)

(ltk-gui:show-drawing d)

(defvar colour-scheme (colour::default-colour-scheme))
(defvar choose-colour (colour::make-colour-scheme-applier-fun colour-scheme))
(ltk-gui:show-colour-scheme colour-scheme)

(drawing:do-drawing-forms (d form)
  (setf (drawing:fill-colour form)
  (funcall choose-colour form)))

(ltk-gui:show-drawing d)

|#

#|

(load "src/load-ltk-gui")

(defvar d (ltk-gui:make-drawing))

(drawing:make-composition-points d (choosing:random-range 8 42))
(ltk-gui:show-drawing-composition-points d)

(drawing:make-planes d (drawing:figure-generation-methods ltk-gui::+planes-count+))
(drawing:make-planes-skeletons d)
(ltk-gui:show-planes-skeletons d)

(drawing:draw-planes-figures d)

(defvar colour-scheme (colour::default-colour-scheme))
(defvar choose-colour (colour::make-colour-scheme-applier-fun colour-scheme))
(ltk-gui:show-colour-scheme colour-scheme)

(drawing:do-drawing-forms (d form)
  (setf (drawing:fill-colour form)
  (funcall choose-colour form)))

(ltk-gui:show-drawing d)

|#

#|

(setf d (ltk-gui:make-drawing))

(drawing:make-composition-points d (choosing:random-range 32 128))
(drawing:make-planes d (drawing:figure-generation-methods ltk-gui::+planes-count+))
(drawing:make-planes-skeletons d)
(drawing:draw-planes-figures d)
(defvar colour-scheme (colour::default-colour-scheme))
(defvar choose-colour (colour::make-colour-scheme-applier-fun colour-scheme))
(drawing:do-drawing-forms (d form)
  (setf (drawing:fill-colour form)
  (funcall choose-colour form)))

(ltk-gui:show-drawing d)

|#
