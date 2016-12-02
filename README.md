
# Analysis Framework
For Automated Software Analysis at ITU 2016.

## TODO:
- Afterwards, dead-code elimination
- Find a way to make a source-to-source mapping from the result of the analyses
    - I.e. actually use the result of the analyses to perform an optimization
      on the source code (AST)
    - Use the result of an analysis to modify control-flow graph
    - Generate new source from the control-flow graph
- Create some proper testing
    - halway done

## DONE:
- Implement constrant-propagation ✓
- Abstract concrete analysis away, to get general framework ✓
- Present program points, and the result of analyses in a better way ✓
    - To check if its correct (verification) ✓
- Make more, advanced examples ✓
- Make a simple parser ✓
- Graphviz! ✓
- The `While` statement is not working. Consider maybe implementing confluence
  points explicitly in the control-flow graph. Should also simplify the code
  alot in `compute`. ✓