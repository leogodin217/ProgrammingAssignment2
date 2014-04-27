source("cachematrix.R")


context("Matrix inverse calculations")

 test_that("Correct matrix inverse is calculated", {
     test_matrix <- matrix(rnorm(1:25), nrow=5) # a 5x5 matrix
     cache_matrix <- makeCacheMatrix(test_matrix)
     inverse_matrix <- solve(test_matrix) # Inverse matrix
     #the inverse of the inverse is the original matrix
     expect_that(cacheSolve(cache_matrix), equals(inverse_matrix)) # Invers of the inverse is the original matrix
   })
