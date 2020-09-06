## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function create a special matrix object, and create a list with its main elements.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set<- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i<<-inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function
## This function evaluates if the inverse of a matrix exists, if so, get the inverse from the cache, which is a faster calculation mode,
## and if there is no exist an inverse, it then calculate the inverse of that matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)){
        message("Calculating inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinv(i)
    i
}
