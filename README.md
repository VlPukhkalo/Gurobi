# Gurobi Optimization in Wolfram Mathematica 
## About project
This package allows you to use the [Gurobi Optimizer](https://www.gurobi.com/products/gurobi-optimizer/) in Wolfram Mathematica. It helps to find solutions of Linear and Non-Linear programming problems, as well as Multi-Objective Optimization problems.
## Usage
### Basic use
The main function is `GurobiOptimization [c, m, b, l, dom, directory]` with:
- `c` the vector of objective function coefficients
- `m` the constraint matrix
- `b` the constraint constants
- `l` the bounds of variables
- `dom` takes the elements of x to be in the domain, either Reals or Integers
- `directory` the project directory

You can also add multiple or non-linear objective function. Note that this changes the structure of the input (refer to [examples](https://github.com/VlPukhkalo/Gurobi/tree/main/Examples)).

In addition, the package includes the following functions:
- `createLPfile [c, m, b, l, dom, directory]` will create a model .lp file in the specified directory
- `getSolution[LPfileName]` will solve the model from the `LPfileName` file and write the result to the .sol file
- `importResults[directory, solName]` will return the found solution imported from `solName` file 
- `DeleteLP` `DeleteSOL` `ClearGurobiDirectory` will clear the current or specified directory from unnecessary files
- `GetLastLogging[directory]` will return information about the last run from the [gurobi.log](https://www.gurobi.com/documentation/9.1/refman/logging.html)

### Options
Below are some options to help you manage the optimizer. 

To control model [parameters](https://www.gurobi.com/documentation/9.1/refman/parameter_descriptions.html), use the following:
- `ResultFileOpt`
- `MethodOpt`
- `TimeLimitOpt`
- `MIPGapOpt`
- `BestObjStopOpt`
- `BestBdStopOpt`
- `FeasRelaxBigMOpt`
- `IterationLimitOpt`
- `MIPFocusOpt`
- `NonConvexOpt`

Also option `SetFullStringOpt` allows you to enter a string that will be exactly passed to the optimizer.

If you are solving Multi-Objective Optimization problem, you can use the `MultiObjOpt` with nested `PriorityOpt`, `WeightOpt`, `AbsTolOpt`, `RelTolOpt`.

For models with nonlinear constraints and use `QuadrConsOpt`.
If model contains simple general constraints (MIN, MAX, OR, AND, ABS) or Indicator constraints, use `IndicatorConsOpt` and `GenConsOpt`. 
More information about it [here](https://www.gurobi.com/documentation/9.1/refman/lp_format.html). 
 
You can also use the following:
- `missionOpt` sets an optimization goal, either Minimize (default) or Maximize
- `lpNameopt` changes the name of the .lp file where the model will be written
- `getOFValue` when the value is set to True, the program  return, in addition to the vector of variables, the value of the objective function

## Related links
[This](https://github.com/ahrvoje/numerics/blob/master/strtod/StringToDouble.wl) tool was used to processing Hungarian number notation. 

For more information refer to [examples](https://github.com/VlPukhkalo/Gurobi/tree/main/Examples) and [documentation](https://www.gurobi.com/documentation/9.1/). 
