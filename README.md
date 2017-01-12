
# Analysis Framework
For Automated Software Analysis at ITU 2016.

## Code Organization
`Anal.hs` contains the `Analysis` type and the `Optimization` type. These types
serve as the entry points for new analyses and optimizations. There are a few
different examples of analyses and optimizations in the `Anal.*` modules.
The supporting modules are

- `Data.Cmm.AST` which represents the abstract syntax of the C-- language.
- `Data.Cmm.Parser` which parses C-- programs.
- `Data.CFG` which represents the control-flow graph of a C-- program.
- `Data.Lat` which contains the typeclass for Lattices
- `Text.Pretty` which contains a simple typeclass for pretty-printing

## Exam notes
- `UnitLat` is superflous, and we might as well have used `Lat ()`

## TODO:
- Done!

## DONE:
- Allow for backwards analyses ✓
- Implement a form of backwards analyses ✓
- Create some proper testing ✓
- Find a way to make a source-to-source mapping from the result of the analyses
    - I.e. actually use the result of the analyses to perform an optimization
      on the source code (AST) ✓
    - Use the result of an analysis to modify control-flow graph ✓
- Generate new source from the control-flow graph ✓
- Implement constrant-propagation ✓
- Afterwards, dead-code elimination ✓
- Abstract concrete analysis away, to get general framework ✓
- Present program points, and the result of analyses in a better way ✓
    - To check if its correct (verification) ✓
- Make more, advanced examples ✓
- Make a simple parser ✓
- Graphviz! ✓
- The `While` statement is not working. Consider maybe implementing confluence
  points explicitly in the control-flow graph. Should also simplify the code
  alot in `compute`. ✓
- Sequence multiple analyses into a "big" analysis ✓
- Write report and presentation ✓
