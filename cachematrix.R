## This pair of functions will be used to get the inverse of a matrix.
## The first time, the inverse is calculated with the R function Solve
## and it's stored, and then, when we need to inverse the matrix again,
## we get the previously stored inverse matrix.

## We first have to run makeCacheMatrix with the matrix to inverse
## as a parameter. This will save the matrix and will create a list
## with the needed functions for cacheSolve execution.
## Then we can run cacheSolve with the list created with makeCacheMatrix
## as input parmeter.


## The function makeCacheMatrix stores in a list four functions:
## store and recover a matrix, and store and recover its inverse.

makeCacheMatrix <- function(input_matrix = matrix()) {
    ## When calling makeCacheMatrix,we first initizlize its inverse as NULL
    inv_matrix <- NULL
    ## We then create the function to store the input matrix.
    ## We initialize the matrix and its inverse at the parent's level.
        set_input_matrix <- function(y) {
        input_matrix <<- y
        inv_matrix <<- NULL
    }
    ## We define the three other functions.
    get_input_matrix <- function() input_matrix
    set_inv_matrix <<- function(inverse) inv_matrix <<- inverse
    get_inv_matrix <<- function() inv_matrix
    ## We create a list with the four functions.
    list(set_input_matrix = set_input_matrix ,
         get_input_matrix = get_input_matrix,
         set_inv_matrix   = set_inv_matrix,
         get_inv_matrix   = get_inv_matrix)
}


## cacheSolve takes a matrix as an input. If its inverse is cached, the
## function gets the cached inverse matrix by calling makeCacheMatrix.
## Otherwise, it applies the R function Solve to the input matrix
## to obtain its inverse, and it stores it by calling makeCacheMatrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## First we call makeCacheMatrix to get the inverse matrix,if it exists
    ## The input parameter of cacheSolve is the list previously created
    ## with makeCacheMatrix. The parameter we use to call makeCacheMatrix
    ## is the function we want to apply as a subset of the list.
    inv_matrix <- x$get_inv_matrix()
    if(!is.null(inv_matrix)) {
        ## There is a cached inverse matrix
        message("getting cached inverse matrix")
        ## We exit from cacheSolve
        return(inv_matrix)
    }
    ## There is no cached inverse matrix: we use the R function Solve.
    input_matrix <- x$get_input_matrix()
    message("calculating cached inverse matrix with Solve")
    inv_matrix <- solve(input_matrix)
    ## We store the inv_matrix by subsetting the list created
    ## with makeCacheMatrix to use the set_inv_matrix function.
    x$set_inv_matrix(inv_matrix)
    inv_matrix
}