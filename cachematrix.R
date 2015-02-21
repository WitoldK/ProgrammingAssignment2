## Coursera, Programming R - Assignment 2: caching mechanism for inverting large matrices. 
#  Mechanism constructed using a combination of two functions:
#  - makeCacheMatrix() for "registering" matrices
#  - cacheSolve() for retrieving cached (if available), inverted matrix.

#’ makeCacheMatrix(): Part of caching mechanism for inverting matrices. 
#’ Takes (invertible) matrix as an input and extends the object with additional functions
#’ (set/get and setsolve/getsolve) used by cacheSolve().
#’ Note set() functions can be used for consecutive modifications to the original matrix. 
#’ By re-setting s value to NULL it effectively clears the cache thus avoiding incorrect, stale, values being returned.
#'
#' @param x An invertible matrix. There is no validation that the matrix is actually invertible.
#' @return Returns a list of functions created.
#' @examples
#' x <- makeCacheMatrix(matrix(1:4,2,2))
#' > cacheSolve(x)
#'      [,1] [,2]
#' [1,]   -2  1.5
#' [2,]    1 -0.5
#' # when executed second time data gets retrieved from cache
#' > cacheSolve(x)
#' getting cached data
#'      [,1] [,2]
#' [1,]   -2  1.5
#' [2,]    1 -0.5

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y # update the original x 
                s <<- NULL # to make sure cache gets cleared whenever object is modified 
        }
        get <- function() x # simply retrieve the matrix 
        setsolve <- function(solve) s <<- solve # update s with the value of inverted matrix
        getsolve <- function() s # retrieve inverted matrix
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


#’ cacheSolve(): Part of caching mechanism for inverting matrices. 
#’ Takes an object created using makeCacheMatrix() and returns inverted matrix 
#’ either retrieved from cache or computed using solve().
#'
#' @param x An object created using makeCacheMatrix()
#' @return Inverted matrix.
#' @examples
#' x <- makeCacheMatrix(matrix(1:4,2,2))
#' > cacheSolve(x)
#'      [,1] [,2]
#' [1,]   -2  1.5
#' [2,]    1 -0.5
#' # when executed second time data gets retrieved from cache
#' > cacheSolve(x)
#' getting cached data
#'      [,1] [,2]
#' [1,]   -2  1.5
#' [2,]    1 -0.5

cacheSolve <- function(x, ...) {  
        s <- x$getsolve() # try to retrieve inverted matrix
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get() # retrieve the base matrix
        s <- solve(data, ...) # compute inverted matrix using solve()
        x$setsolve(s) # push the computed value for future use
        s # return inverted matrix

}
