# multIntTestFunc
The package provides implementations of test functions for multivariate numerical integration that can be used to test multivariate integration routines.
The package covers five different integration domains (unit hypercube, unit ball, unit sphere, standard simplex and R^n).
For each domain several functions with different properties (smooth, non-differentiable, ...) are available. The functions are available in all dimensions n >= 1.
For each function the exact value of the integral is known and implemented to allow testing the accuracy of multivariate integration routines.
Details on the test functions and the value of their integral can be found in <a href="documentation_test_functions.pdf">documentation_test_functions.pdf</a>.

The key feature of the package is that it provides a unified object oriented interface 

- implemented functions are available for any dimension n>= 1. This facilitates the test of high dimensional integration routines and allows to gauge the behaviour for changing number of dimensions n,
- unified, intuitive and object oriented interface
- a number of functions and integration domains

## Example
To define a test function, the keyword `new` is used together with the type of test function and the relevant parameters.
For example, the class `Rn_Gauss` implements the function

h<sub>&theta;</sub>(x) = &theta;<sub>o</sub> x + &theta;<sub>1</sub>x

```r
require(multIntTestFunc)
n <- as.integer(3)
testFunc <- new("Rn_Gauss",dim=n)

```

