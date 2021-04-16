#    Groundhog - A program that finds rectangular spaces within bounds
#    Copyright (C) 2010 Rhea Myers
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.


import random


class Point:
      def __init__(self, x, y):
            self.x = x
            self.y = y

      def __str__(self):
            return "%s, %s" % (self.x, self.y)


class Bounds:
      """Horizontal and vertical bounds of an area."""

      def __init__(self, width, height):
            self.width = width
            self.height = height

      def __str__(self):
            return "%s, %s" % (self.width, self.height)


class BoundsRange:
      """Minimum and maximum horizontal and vertical bounds of an area."""

      def __init__(self, min_bounds, max_bounds):
            self.min = min_bounds
            self.max = max_bounds

      def __str__(self):
            return "(%s)..(%s)" % (self.min, self.max)


class Rectangle:
      """A Rectangle has x and y co-ordinates, and width and height dimensions.
         Its bounds are exclusive; point 20, 20 is outside rectange 0,0,20,20"""

      def __init__(self, x, y, width, height):
            self.x = x
            self.y = y
            self.width = width
            self.height = height

      def __str__(self):
            return "%s, %s, %s x %s" % (self.x, self.y, self.width, self.height)

      def contains_coords(self, x, y):
            """Check whether x and y lie within the rectangle's bounds"""
            return \
                x >= self.x and \
                x < self.x + self.width and \
                y >= self.y and \
                y < self.y + self.height

      def contains_rectangle(self, x, y, width, height):
            """Check whether other rectangle is within the rectangle's bounds"""
            other_right = x + width
            other_top = y + height
            top = self.y + self.height
            right = self.x + self.width
            return \
                x >= self.x and \
                x < right and \
                other_right >= self.x and \
                other_right <= right and \
                y >= self.y and y \
                < top and \
                other_top >= self.y and \
                other_top <= top

      def random_point_in_bounds(self, x_inset=0, y_inset=0,
                                 w_inset=0, h_inset=0):
            """Create a random point within the rectangle's bounds"""
            return Point(random.randrange(self.x + x_inset,
                                          self.x + self.width - (w_inset + 1)),
                         random.randrange(self.y + x_inset,
                                          self.y + self.height - (h_inset + 1)))


class CellMatrix:
      """A matrix of cells to use for tracking form creation"""

      def __init__(self, width, height):
            self.bounds = Rectangle(0, 0, width, height)
            self.cell_rows = [[None for column in range(width)]
                              for row in range(height)]

      def __str__(self):
            # The cell matrix is stored upside down, represent it right way up
            representation = ""
            for row in self.cell_rows:
                  row_representation = ""
                  for cell in row:
                        row_representation += "%s" % (cell)
                  representation = row_representation + "\n" + representation
            return representation

      def mark_cell(self, x, y, value):
            """Mark the cell with a value"""
            assert self.bounds.contains_coords(x, y), \
                "%s, %s not in %s, %s" % (x, y, self.bounds.width,
                                          self.bounds.height)
            self.cell_rows[y][x] = value
            return value

      def roughen(self, count):
            """Mark some cells as unusable to roughen the surface"""
            for i in range(count):
                  p = self.bounds.random_point_in_bounds()
                  self.mark_cell(p.x, p.y, True)

      def mark_rectangle(self, rect, value):
            """Set the cells in rectangle to value"""
            for row in range(rect.y, rect.y + rect.height):
                  for column in range(rect.x, rect.x + rect.width):
                        self.mark_cell(column, row, value)

      def cell(self, x, y):
            """Get the cell's value"""
            assert self.bounds.contains_coords(x, y), \
                "%s, %s not in %s, %s" % (x, y, self.bounds.width,
                                          self.bounds.height)
            return self.cell_rows[y][x]

      def is_marked(self, x, y):
            """Determine whether the cell is marked with a value"""
            return self.cell(x, y)


class SpaceFinder:
      """An object to find space within a drawing/cell matrix.
         Once initialized, the same object can be used to find many spaces.
         Trying to find space may fail if there is no suitable space,
         or if the object's brute-force search fails."""

      def __init__(self, cell_matrix, bounds_range):
            #FIXME: We grow height/width equally at present,
            #  so height/width are same
            cheat_min = min(bounds_range.min.width, bounds_range.min.height)
            cheat_max = min(bounds_range.max.width, bounds_range.max.height)
            self.cell_matrix = cell_matrix
            self.min_width = cheat_min #min_width
            self.min_height = cheat_min #min_height
            self.max_width = cheat_max #max_width
            self.max_height = cheat_max #max_height

      def start_search(self):
            """Initialize this run"""
            self.origin = None
            self.bounds = None
            # Arbitrary re-try value for brute-force search for unmarked cells
            tries = 1000
            # Try to find an un-marked cell
            while tries > 0:
                  bounds = self.cell_matrix.bounds
                  self.origin = bounds.random_point_in_bounds(0, 0,
                                                              self.min_width,
                                                              self.min_height)
                  # Found an un-marked cell?
                  if not self.cell_matrix.is_marked(self.origin.x,
                                                    self.origin.y):
                        # Set up the state to start searching there
                        self.bounds = Rectangle(self.origin.x, self.origin.y,
                                                1, 1)
                        break
                  tries = tries - 1
            # We succeeded if we found a starting point within the try limit
            return tries > 0

      def new_top_border_contains_marked_cells(self, new_width, new_height):
            """Check proposed top border for marked cells"""
            does = False
            # Check only new border pixels from grown rectangle
            # Check proposed new top of bounding rectangle for collisions
            for x in range(self.bounds.x, self.bounds.x + new_width):
                  if self.cell_matrix.is_marked(x, self.bounds.y +
                                                # -1: rect are bounds exclusive
                                                new_height - 1):
                        does = True
                        break
            return does

      def new_right_border_contains_marked_cells(self, new_width, new_height):
            """Check proposed right border for marked cells"""
            does = False
            # -1 So we don't test the furthest pixel twice
            for y in range(self.bounds.y, self.bounds.y + new_height - 1):
                  # -1: rect bounds exclude
                  if self.cell_matrix.is_marked(self.bounds.x + new_width - 1,
                                                y):
                              does = True
                              break
            # We succeeded if there were no marked cells indicating collisions
            return does

      def new_border_contains_marked_cells(self, new_width, new_height):
            """Check to see whether the proposed  bounds include marked cells"""
            does = self.new_top_border_contains_marked_cells(new_width,
                                                             new_height)
            if not does:
                  does = self.new_right_border_contains_marked_cells(new_width,
                                                                     new_height)
            return does

      def try_to_grow_bounds(self):
            """Try to increase the bounds of the found space"""
            # Grow one row/column at a time to make checks easier
            new_width = self.bounds.width + 1
            new_height = self.bounds.height + 1
            can_continue = True
            # Fail if the new bounds are outside the cell matrix bounds
            if not self.cell_matrix.bounds.contains_rectangle(self.bounds.x,
                                                              self.bounds.y,
                                                              new_width,
                                                              new_height):
                  can_continue = False
            # Fail if the new bounds edges contain cells from another figure
            elif self.new_border_contains_marked_cells(new_width, new_height):
                  can_continue = False
            # Otherwise succeed
            else:
                  self.bounds.width = new_width
                  self.bounds.height = new_height
            # We succeeded if new bounds are in drawing bounds and don't collide
            return can_continue

      def bounds_smaller_than_minimum_width_or_height(self):
            """Make sure the current bounds aren't smaller than requested"""
            return self.bounds.height < self.min_height or \
                self.bounds.width < self.min_width

      def bounds_not_larger_than_maximum_width_or_height(self):
            """Make sure the current bounds aren't larger than requested"""
            return self.bounds.height < self.max_height and \
                self.bounds.width < self.max_width

      def grow_bounds(self):
            """Grow the bounds as large as they can go in the given position"""
            found = True
            # Grow until we reach maximum size
            while self.bounds_not_larger_than_maximum_width_or_height():
                  can_continue = self.try_to_grow_bounds()
                  # If we hit another figure, stop
                  if not can_continue:
                        # Fail if we hit another figure & aren't big enough
                        if self.bounds_smaller_than_minimum_width_or_height():
                              found = False
                        break
            # We succeeded if bounds have grown to fit required size range
            return found

      def try_to_find_space(self):
            """Keep trying to find space in the requested size range"""
            # Arbitrary re-try value for brute-force search for empty space
            tries = 1000
            found = False
            while tries > 0:
                  found = self.start_search() and self.grow_bounds()
                  if found:
                        break
                  tries = tries - 1
            result = None
            if found:
                  result = self.bounds
            return result


class RGBColour:
      """A colour expressed in terms of abstract red, green and blue quantities"""

      def __init__(self, r, g, b):
            self.r = r
            self.g = g
            self.b = b


class Figure():
      """A figure in the drawing"""

      def __init__(self):
            self.colour = RGBColour(random.random(),
                                    random.random(),
                                    random.random())


class RectangleFigure(Figure):
      """A rectangular figure in the drawing"""

      def __init__(self, rect):
            Figure.__init__(self)
            self.geometry = rect

      def mark_cells(self, cells):
            cells.mark_rectangle(self.geometry, self)


class ComparativeSizes:
      """Make various size ranges based on an original size.
         Ranges are inclusive."""

      def __init__(self, width, height):
            self.bounds = Bounds(width, height)
            self.small_lower = 5
            self.small_upper = 15
            self.medium_lower = 16
            self.medium_upper = 26
            self.large_lower = 27
            self.large_upper = 37

      def random_value(self, base, lower, upper):
            """Return a value that is low..upper% of base"""
            return int((base / 100.0) * random.uniform(lower, upper))

      def random_range(self, base, lower, upper):
            """Return a random range with min..max in lower..upper% of base"""
            a = self.random_value(base, lower, upper)
            b = self.random_value(base, lower, upper)
            if a > b:
                  a, b = b, a
            return (a, b)

      def make_bounds_range(self, lower, upper):
            """Make a bounds range within the given % range"""
            # If this leads to aspect ratio stretching, change
            min_width, max_width = self.random_range(self.bounds.width,
                                                     lower,
                                                     upper)
            min_height, max_height = self.random_range(self.bounds.height,
                                                       lower,
                                                       upper)
            return BoundsRange(Bounds(min_width, min_height),
                               Bounds(max_width, max_height))

      def small_bounds_range(self):
            """Return a small bounds range relative to the total bounds"""
            return self.make_bounds_range(self.small_lower, self.small_upper)

      def medium_bounds_range(self):
            """Return a medium bounds range relative to the total bounds"""
            return self.make_bounds_range(self.medium_lower, self.medium_upper)

      def large_bounds_range(self):
            """Return a large bounds range relative to the total bounds"""
            return self.make_bounds_range(self.large_lower, self.large_upper)


class Drawing:
      def __init__(self, width, height):
            self.bounds = Rectangle(0, 0, width, height)
            self.cells = CellMatrix(width, height)
            self.figures = []
            self.sizes = ComparativeSizes(self.bounds.width,
                                          self.bounds.height)
            self.small_finder = SpaceFinder(self.cells,
                                            self.sizes.small_bounds_range())
            self.medium_finder = SpaceFinder(self.cells,
                                            self.sizes.medium_bounds_range())
            self.large_finder = SpaceFinder(self.cells,
                                            self.sizes.large_bounds_range())
            self.is_finished = False

      def add_new_figure(self):
            assert self.is_finished is not True
            # Dumb random choice of figure size rather than expanding tokens
            size = random.randrange(3)
            if size is 2:
                  print "Finding space for large figure"
                  result = self.large_finder.try_to_find_space()
            elif size is 1:
                  print "Finding space for medium figure"
                  result = self.medium_finder.try_to_find_space()
            else:
                  print "Finding space for small figure"
                  result = self.small_finder.try_to_find_space()
            print result
            if result:
                  print "Found space for figure"
                  new_figure = RectangleFigure(result)
                  self.add_figure(new_figure)
            else:
                  print "Couldn't find space for figure."
                  print "Finishing drawing."
                  self.is_finished = True
            return self.is_finished

      def add_figure(self, figure):
            """Add the figure to the drawing"""
            self.figures.append(figure)
            figure.mark_cells(self.cells)


def make_drawing(width, height):
      """Make a drawing"""
      print "Creating drawing data structure."
      drawing = Drawing(width, height)
      print "Created drawing data structure."
      drawing.cells.roughen(10)
      print "Adding figures to drawing."
      for i in range(0, 20):
            drawing_finished = drawing.add_new_figure()
            if drawing_finished:
                  break
      print "Added figures to drawing."
      return drawing


import gtk


class GroundhogApp(gtk.Window):
      def __init__(self):
            super(GroundhogApp, self).__init__()
            width = 800
            height = 600
            self.drawing = make_drawing(width, height)
            self.set_title("Groundhog")
            self.resize(width, height)
            self.set_position(gtk.WIN_POS_CENTER)
            self.connect("destroy", gtk.main_quit)
            darea = gtk.DrawingArea()
            darea.connect("expose-event", self.expose)
            self.add(darea)
            self.show_all()

      def draw(self, widget):
            cr = widget.window.cairo_create()

            for figure in self.drawing.figures:
                  cr.set_source_rgb(figure.colour.r,
                                    figure.colour.g,
                                    figure.colour.b)
                  r = figure.geometry
                  cr.rectangle(r.x, r.y, r.width, r.height)
                  cr.fill()

      def expose(self, widget, event):
            self.draw(widget)


GroundhogApp()
gtk.main()


# TODO:
# Concepts of "some" and "enough"
# Object to generate tokens
# Object to expand tokens
