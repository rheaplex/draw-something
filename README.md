draw-something
==============

By Rhea Myers

An artistic image generator inspired by Harold Cohen's AARON.

Installation
============

See INSTALL.

Usage
=====

Creating Drawings
-----------------

./draw-something

./draw-something drawing-filename.svg

./draw-something /path/to/directory/another-drawing.svg

./draw-something && inkscape `ls -tr *.svg | tail -1`

Running Tests
-------------

./run-tests
