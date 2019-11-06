# Input Data format:

The triangles are represented by a matrix. Each odd line is shifted down
by 1/2.  0 represents nothing, 7 represents a full triangle.


[[7,7,7], [7,0,0]] translates to:

 |\    |\
 | \   | \
 | /|\ | /
 |/ | \|/
 |\ | /
 | \|/
 | /
 |/

Numbers between 1 and 6 represent partial triangles

1: |
2:  \
3: |\
4:  /
5: |/
6: >

# Display

For each beam of a triangle, some lines are always visible.
The trick is to display the lines that are visible sometimes.

To do that, we consider each summit of a triangle:

  N
  |\E
  |/
  S

For each summit, there are 6 outgoing beams:

  NW N NE
    \|/
    /|\
  SW S SE

Two of them belong to the current triangle, four of them belong to
the neighbours.  So we have to compute the extra lines for each beam
combination. There are six beams, either here or not here, so that gives
2⁶ = 64 possibilities. Let's hope we can factor things out.
For each cross point, there are 24 lines that can be there or not, depending
on the outgoing beams.

Once we know how to compute this, we can draw all the triangles:
 - each triangle is responsible for drawing its own beams
 - each triangle is responsible for drawing its own north summit
 - for the south summit, the triangle only draws it if it has no triangle below
 - for the east summit, the triangle only draws it if it has no triangle to the right, below
