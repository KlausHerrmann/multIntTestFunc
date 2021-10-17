# multIntTestFunc
The package provides implementations of test functions for multivariate numerical integration that can be used to test multivariate integration routines.
The package covers six different integration domains (unit hypercube, unit ball, unit sphere, standard simplex, non-negative real numbers and R^n).
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

In R we can define the function, evaluate it for a given set of points and retrieve the theoretical value of the integral as follows: 
```r
require(multIntTestFunc)
n <- as.integer(3)
f <- new("Rn_Gauss",dim=n)
x <- matrix(c(0,0,0, 1,0,0, 0,1,0, 0,0,1),4,3,byrow = TRUE)
eval <- evaluate(f,x) #evaluate f(x) for x=(0,0,0), x=(1,0,0), x=(0,1,0) and x=(0,0,1)
print(eval)
print(exactIntegral(f))
```

## Installation
The stable version of the package is available on CRAN at <a href="https://cran.r-project.org/web/packages/multIntTestFunc/index.html">https://cran.r-project.org/web/packages/multIntTestFunc/index.html</a> and can be installed as usual.

To install the latest version from GitHub either clone the repository with git or download the ZIP file.
In both cases stick with the `main` branch.
In R navigate with `setwd` to the sub-folder `multIntTestFunc` and run
```r
#setwd("...YOUR PATH TO multIntTestFunc...") 
devtools::document()
devtools::build()
devtools::install()
```

To get the latest development version follow the same steps but clone from branch `development`.

## Contribute
To add a function two steps are necessary:

1. Describe the function in the reference manual `documentation_test_functions.tex` including possible sources in the bib file. It is also necessary to update the tables at the end of the file.
1. Create a R file for the function in the R directory following the naming convention `DOMAION_NAME.R`. The implementation has to implement all inherited methods (with Roxygen comments). For the `getReferences` method an identifier for the section in `documentation_test_functions.tex` has to be included as the first element of the vector. 
