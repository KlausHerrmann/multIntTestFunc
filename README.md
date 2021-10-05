# multIntTestFunc
The package provides implementations of test functions for multivariate numerical integration that can be used to test multivariate integration routines.
The package covers five different integration domains (unit hypercube, unit ball, unit sphere, standard simplex and R^n).
For each domain several functions with different properties (smooth, non-differentiable, ...) are available. The functions are available in all dimensions n >= 1.
For each function the exact value of the integral is known and implemented to allow testing the accuracy of multivariate integration routines.
Details on the available test functions and the value of their integral can be found in <a href="documentation_test_functions.pdf">documentation_test_functions.pdf</a>.

The key features of the package are
- a unified, intuitive and object oriented interface;
- that the implemented functions are available for any dimension n>= 1. This facilitates the test of high dimensional integration routines and allows to gauge the behaviour when the number of dimensions n changes;
- that the package provides a number of different functions and integration domains.

## Code example
To define a test function, the keyword `new` is used together with the type of test function and the relevant parameters.
For example, the class `Rn_Gauss` implements the function

f(x<sub>1</sub>,...,x<sub>n</sub>) = exp(- &sum;<sub>k=1</sub><sup>n</sup>x<sub>k</sub><sup>2</sup>),

for which the value of the integral is known to be
 
&int;<sub>-&infin;</sub><sup>&infin;</sup> ... &int;<sub>-&infin;</sub><sup>&infin;</sup> f(x<sub>1</sub>,...,x<sub>n</sub>) dx<sub>1</sub> ... dx<sub>n</sub> = (&pi;)<sup>n/2</sup>.

In R the function we can define the function, evaluate it for a given set of points and retrieve the theoretical value of the integral as folows: 
```r
require(multIntTestFunc)
n <- as.integer(3)
testFunc <- new("Rn_Gauss",dim=n)
testPoints <- matrix(c(0,0,0, 1,0,0, 0,1,0, 0,0,1),4,3,byrow = TRUE)
eval <- evaluate(f,testPoints) #evaluate the function f
print(eval)
print(exactIntegral(f))
```

