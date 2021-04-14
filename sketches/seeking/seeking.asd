;; seeking.asd - Turtle graphics point seeking.
;; Copyright (C) 2016 Rhea Myers
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

(asdf:defsystem #:seeking
  :description "Simple seeking behaviour testing."
  :author "Rhea Myers"
  :license "GNU GPL v3, or later at your option"
  :depends-on (#:cl-cffi-gtk)
  :serial t
  :components ((:file "package")
               (:file "seeking")))
