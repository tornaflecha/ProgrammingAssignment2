## This is the solution to the Programming Assignment 2: Lexical Scoping (Coursera R course)

## The first function (makeCacheMatrix) creates a list with this elements:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

#For this assignment, assume that the matrix supplied is always invertible
#Computing the inverse of a square matrix can be done with the (solve) function in R. 

#For example, if X is a square invertible matrix, then solve(X) returns its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse <<- inverse
        getinverse <- function() inverse
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# The second one (cacheSolve), return a matrix that is the inverse of matrix function argument input (x). 
# It first checks if the inverse has already been computed before.
# 1. If the answer is yes, advise you that the result has been returned from cache, show the result and skips the matrix inverse computation.
# 2. If the answer is no, it calculates the matrix inverse and return the result.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
                message("The result was into cached data") 
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}


# For test:

# 1. I create a 2x2 matrix: m <- matrix(c(1:4),2,2)

#> m
#       [,1] [,2]
#[1,]    1    3
#[2,]    2    4

# 2. I call makeCacheMatrix function with the m matrix input argument and I save
#    the result within minverse variable

# minverse <- makeCacheMatrix(m)


# 3. the first time if i call to the cacheSolve(minverse), 
#    the inverse matrix is computed (don´t show the advice message) and show the result

#> cacheSolve(minverse)
        #[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5


# 4. If I call the function a second time, it firt shows the cache advice and after show the result:

#> cacheSolve(minverse)
# The result was into cached data
#       [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

