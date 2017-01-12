
# Analysis Framework
For Automated Software Analysis at ITU 2016.

## Building and running
To build, you should use `stack build`.
To test, you can use `stack test` or `stack repl --test` and then `Main.main`
to run all the tests.

It is a library, so you cannot really run it :) But to explore the code
interactively, yo ucan use `stack repl --test` and now you should
have loaded all the code into scope, along with the test classes.

You can now access the test-programs as strings with `in1, in2 ... in14`.
Parse them with `parseCmm` or `unsafeParse`.
Print them after parsing with `ppr` or `putPrettyLn`
There are some analyses available as `available, constProp, livenessAnal`.
You can run an analysis on a program with
`analyzeProgram :: Lat a => Analysis a -> Program -> Map ID a`.
You can run a single optimization with `runOpt :: Optimization -> Program -> Program`.
There are some optimizations available as `constPropOpt, deadCodeOpt`.
Liveness and Available do not have optimizations yet.
You can "fully" optimize a program with `optimizeProg :: [Optimization] -> Program -> Program`.



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
