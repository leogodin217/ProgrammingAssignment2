source("cachematrix.R")

context("Cashed Matrix")

test_that("Cashed Matrix has getter and setter", {
    test_matrix <- matrix(rnorm(1:25), nrow=5) # A 5x5 matrix
    cached_matrix <- makeCacheMatrix(test_matrix)

    # We should return four functions
    expect_that(length(cached_matrix), equals(4))

    # Test get function function
    expect_that(cached_matrix$get(), equals(test_matrix))

    # Test set function
    new_matrix <- matrix(rnorm(1:16), nrow=4) # A 4x4 matrix
    cached_matrix$set(new_matrix)
    expect_that(cached_matrix$get(), equals(new_matrix))
    })

test_that("Cashed matrix has getter and setter for inverse", {
    test_matrix <- matrix(rnorm(1:25), nrow=5) # A 5x5 matrix
    cached_matrix <- makeCacheMatrix(test_matrix)

    # Test that the inverse matrix is calculated correctly
    inverse_matrix = solve(test_matrix)
    cached_matrix$set_inverse(solve(test_matrix)) # set it to be the inverse
    expect_that(cached_matrix$get_inverse(), equals(inverse_matrix))
    })

context("makeCacheMatrix caching")

    test_that("No inverse is set on first run of makeCacheMatrix", {
        # get_inverse() should return NULL
         test_matrix <- matrix(rnorm(1:25), nrow=5) # A 5x5 matrix
        cached_matrix <- makeCacheMatrix(test_matrix)
        expect_that(cached_matrix$get_inverse(), equals(NULL))
        })

    test_that("Inverse is set to null after set()", {
        # Create a new cached matrix and set the inverse
        test_matrix <- matrix(rnorm(1:25), nrow=5) # A 5x5 matrix
        cached_matrix <- makeCacheMatrix(test_matrix)
        inverse_matrix <- solve(test_matrix)
        cached_matrix$set_inverse(inverse_matrix)

        # Assign a new matrix to cached_matrix
        new_matrix <- matrix(rnorm(1:16), nrow=4) # A 4x4 matrix
        cached_matrix$set(new_matrix)
        expect_that(cached_matrix$get_inverse(), equals(NULL))

        })
