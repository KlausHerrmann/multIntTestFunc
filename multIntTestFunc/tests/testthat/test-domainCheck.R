
library(testthat)

#simple tests in 3D for the domain check functions
test_that("test domainCheck", {
    expect_true(checkClosedUnitCube(matrix(c(0.5,0.5,0.5),1,3)))
    expect_true(checkClosedUnitBall(matrix(c(0.0,0.0,0.0),1,3)))
    expect_true(checkUnitSphere(matrix(c(1.0,1.0,1.0)/sqrt(3),1,3),eps=0.001))
    expect_true(checkStandardSimplex(matrix(c(0.5,0.2,0.3),1,3)))
    expect_true(checkRn(matrix(c(0.0,0.0,0.0),1,3)))
    expect_true(checkPos(matrix(c(5.0,5.0,5.0),1,3)))

})
