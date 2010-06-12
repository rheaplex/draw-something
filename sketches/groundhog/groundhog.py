#    Groundhog - A program that finds rectangular spaces within bounds
#    Copyright (C) 2010  Rob myers <rhea@myers.studio>
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


class Rectangle:
      """A Rectangle has x and y co-orfinates, and width and height dimensions.
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
            return x >= self.x and x < self.x + self.width and \
                y >= self.y and y < self.y + self.height

      def contains_rectangle(self, x, y, width, height):
            """Check whether other rectangle lies within the rectangle's bounds"""
            other_right = x + width
            other_top = y + height
            top = self.y + self.height
            right = self.x + self.width
            return \
                x >= self.x and \
                x < right and \
                other_right >= self.x and \
                other_right < right and \
                y >= self.y and y \
                < top and \
                other_top >= self.y and \
                other_top < top

      def random_point_in_bounds(self, x_inset=0, y_inset=0, w_inset=0,h_inset=0):
            """Create a random point within the rectangle's bounds"""
            return Point(random.randrange(self.x + x_inset, 
                                          self.x + self.width - (w_inset + 1)),
                         random.randrange(self.y + x_inset, 
                                          self.y + self.height - (h_inset + 1)))


class CellMatrix:
      """A matrix of cells to use for tracking form creation"""

      def __init__(self, width, height):
            self.bounds = Rectangle(0, 0, width, height)
            self.cell_rows = [[0 for column in range(width)]
                              for row in range(height)]

      def __str__(self):
            # The cell matrix is stored upside down, represent it right way up"""
            representation = ""
            for row in self.cell_rows:
                  row_representation = ""
                  for cell in row:
                        row_representation += " %s " % (cell)
                  representation = row_representation + "\n" + representation
            return representation
      
      def mark_cell(self, x, y, value):
            """Mark the cell with a value"""
            assert self.bounds.contains_coords(x, y), \
                "%s, %s not in %s, %s" % (x, y, self.bounds.width,
                                          self.bounds.height)
            self.cell_rows[y][x] = value
            return value

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

      def __init__(self, cell_matrix, min_width, min_height, max_width,
                   max_height):
            self.cell_matrix = cell_matrix
            self.min_width = min_width
            self.min_height = min_height
            self.max_width = max_width
            self.max_height = max_height

      def start_search(self):
            """Initialize this run"""
            self.origin = None
            self.bounds = None
            # Arbitrary re-try value for brute-force search for unmarked cells
            tries = 1000
            # Try to find an un-marked cell
            while True:
                  bounds = self.cell_matrix.bounds
                  self.origin = bounds.random_point_in_bounds(0, 0, 
                                                              self.min_width,
                                                              self.min_height)
                  # Found an un-marked cell? 
                  if not self.cell_matrix.is_marked(self.origin.x, self.origin.y):
                        # Set up the state to start searching there
                        self.bounds = Rectangle(self.origin.x,
                                                self.origin.y,
                                                1, 1)
                        break
                  # Make sure we haven't run out of tries
                  tries = tries - 1
                  if tries <= 0:
                        break
            # We succeeded if we found a starting point before we ran out of tries
            return tries > 0

      def new_border_contains_marked_cells(self, new_width, new_height):
            """Check to see whether the proposed  bounds include marked cells"""
            it_does = False            
            # Check only new border pixels from grown rectangle
            # Check the proposed new top of the bounding rectangle for collisions
            for x in range(self.bounds.x, self.bounds.x + new_width):
                  if self.cell_matrix.is_marked(x, self.bounds.y + new_height):
                        it_does = True
                        break
            # If there were no collisions there, Check the proposed new right also
            if not it_does:
                  # -1 So we don't test the furthest pixel twice
                  for y in range(self.bounds.y, self.bounds.y + new_height - 1):
                        if self.cell_matrix.is_marked(self.bounds.x + new_width,
                                                      y):
                              it_does = True
                              break
            # We succeeded if there were no marked cells indicating collisions
            return it_does

      def try_to_grow_bounds(self):
            """Try to increase the bounds of the found space"""
            # Grow one row/column at a time to make bounds/collision checks easier
            new_width = self.bounds.width + 1
            new_height = self.bounds.height + 1
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
                  can_continue = True
            # We succeeded if the new bounds are in the drawing and don't collide
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


def test_space_finder():
      """Test space finding"""
      cells = CellMatrix(50, 40)
      finder = SpaceFinder(cells, 5, 5, 20, 20)
      for i in range(0, 20):
            found = finder.try_to_find_space()
            if found:
                  print finder.bounds
                  # Mark the cells so they won't be chosen again
                  cells.mark_rectangle(finder.bounds, chr(ord('a') + i))
            else:
                  print "Failed"
                  break
      print cells

test_space_finder()


#FIXME:
# Top and right rows aren't used


#TODO:
# Roughen the surface
# Object to generate tokens
# Object to expand tokens
# Object to generate "some"
# Object to generate small/medium/large dimensions
# Drawing object to keep track of the rectangles and save them
