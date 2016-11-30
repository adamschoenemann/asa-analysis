
# Analysis Framework
For Automated Software Analysis at ITU 2016.

## TODO:
- Abstract concrete analysis away, to get general framework
    - halfway done
- Afterwards, implement constant-propagation and dead-code elimination
- Find a way to make a source-to-source mapping from the result of the analyses
    - I.e. actually use the result of the analyses to perform an optimization
      on the source code (AST)
- Create some proper testing
    - halway done

## DONE:
- Present program points, and the result of analyses in a better way ✓
    - To check if its correct (verification) ✓
- Make more, advanced examples ✓
- Make a simple parser ✓
- Graphviz! ✓
- The `While` statement is not working. Consider maybe implementing confluence
  points explicitly in the control-flow graph. Should also simplify the code
  alot in `compute`. ✓