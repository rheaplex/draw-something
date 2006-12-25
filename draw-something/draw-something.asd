;;  draw-something.asd - The main package for draw-something
;;  Copyright (C) 2006  Rob Myers rob@robmyers.org
;;
;;  This program is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program; if not, write to the Free Software
;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(require :asdf)

(in-package #:asdf)

(asdf:defsystem #:draw-something
  :serial t
  :components
  ((:file "package")
   (:file "utilities")
   (:file "geometry")
   (:file "point")
   (:file "line")
   (:file "circle")
   (:file "arc")
   (:file "rectangle")
   (:file "polyline")
;;   (:file "cell-matrix")
   (:file "colour")
   (:file "colouring-new")
   (:file "turtle")
   (:file "form")
   (:file "figure")
   (:file "codelet")
   (:file "drawing")
   (:file "postscript")
   (:file "draw-something")))
