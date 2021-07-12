# The Blind Hen submission

We solved problems using three approaches:

## Simulated Annealing with a selection of neighbor configuration generators

This is guided by a penalty score that describes how bad a figure is from
satisfying the rules, and on an order-of-magnitude less, its dislike score.

Our neighbor generators included:
- Randomly moving a single vertex a single move
- Moving a single vertex N moves according to a mini-simulated annealing
- Reflection / rotation / translation of the whole figure
- Local edge-and-integer-point preserving transformations, e.g. rotation
  around articulation points, sub-graph reflection over vertical lines, etc.

## A DFS that places figure vertices on hole vertices.

We filter out impossible configurations early by looking at local edge lengths
between other placed vertices.

We then run Simulated Annealing on the remaining unplaced figure vertices,
using a conservative neighbor generation that only moves single vertices.

This was built in a mad scramble during the last 6 hours of the contest.
  
## GUI tool solving by hand with computed assistance

The GUI tool allows moving vertices by hand, individually or by selecting
multiple. It also allows running the simulated annealing for N steps at a time.

The GUI tool was indispensable for understanding the behaviour of our automatic solvers,
and also made it more fun to interact with the problems.