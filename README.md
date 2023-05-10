draw-something
==============

By Rhea Myers

An artistic image generator inspired by Harold Cohen's AARON.

What is the minimal behaviour a program needs to display to be considered
an analogue to human creativity?

Can we better understand our own artmaking by encoding it?

Can we recognise inhuman aesthetics?

Installation
============

You will need a Common Lisp environment.

The scripts included use sbcl via Roswell:

https://roswell.github.io/

The basic draw-something system has been written to have no dependencies.

Simply link this directory to your Common Lisp system's local-projects/
directory. This will be ~/.roswell/local-projects for Roswell.

Dependencies for the GUI, for testing, and for the smaller programs in
sketches/ can be installed using quicklisp, which Roswell should include:

https://www.quicklisp.org/beta/

Some dependencies may need newer versions than are included in Quicklisp.
These should be cloned from their git repositories.

They may also need modifying to support some architectures:

At the time of writing, on macOS under m2 Apple Silicon, the required
libraries are for the sketches/ directory are:

    cl-sdl2
    cl-sdl2-image
    cl-sdl-ttf
    sdlkit
    sketch

Usage
=====

Creating Drawings
-----------------

    ./draw-something

Sketches
---------

The folder sketches/ contains workbenches/sketches for exploring and
visualizing behaviuours for the larger draw-something system.

You'll need sketch installed:

    ros install vydd/sketch
    ros groundhog.lisp

Clicking on the sketch window usually restarts the process.
