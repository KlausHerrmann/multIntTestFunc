
library(testthat)

#simple tests in 3D for the domain check functions
test_that("test domainCheck", {
    expect_true(checkClosedUnitCube(c(0.5,0.5,0.5)))
    expect_true(checkClosedUnitBall(c(0,0,0)))
    expect_true(checkUnitSphere(c(1,1,1)/sqrt(3),eps=0.0001))
    expect_true(checkStandardSimplex(c(0.5,0.3,0.2)))
    expect_true(checkRn(c(0,0,0)))
})
