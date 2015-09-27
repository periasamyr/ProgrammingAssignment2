## The below two functions are used to cache the inverse of an invertible matrix
## and re-use the result from the cache if the matrix does not change between two
## execution of the inverse computation. 

## Please note that it is assumed that matrix being supplied is invertible

## First function builds a list of functions to :
## 1. set the value of the matrix 
## 2. get the value of the matrix
## 3. set the inverse of the matrix using solve() function
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                                   ## Initiatlize inverse matrix variable
    set <- function (y = matrix()) {
        x <<- y                                 ## Function to caches value of matrix in global environment
        m <<- NULL                              ## Reset inverse matrix variable
    }
    get <- function() x                         ## Function to get the matrix
    setinverse <- function(solve) m <<- solve   ## Function to cache the inverse of matrix
    getinverse <- function() m                  ## Function to get computed inverse matrix
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)               ## Build a vector of required functions
}

## Second function computes the inverse of the matrix using the vector
## of functions created using the above first function. It first checks if inverse has already 
## been computed and if so, it get the inverse from cache using 'getinverse' function
## If inverse has not been computed earlier, it computes it using 'solve'function and 
## caches the result using 'setinverse'function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()                         ## Get the inverse matrix variable
    if (!is.null(m)) {                          ## If it contains earlier computed inverse
        message("getting cached matrix")        ## print the cached matrix
        return(m)
    }
    data <- x$get()                             ## If inverse has not been computed, get the matrix
    m <- solve(data, ...)                       ## Compute inverse of it using solve function
    x$setinverse(m)                             ## Cache the computed inverse and 
    m                                           ## print the computed matrix
}
