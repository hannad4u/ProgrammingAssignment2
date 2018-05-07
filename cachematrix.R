## this assignment for R proraming course

##this function is for calculate inverse for cache matrix
makeCacheMatrix <- function(x = matrix()) {
    matInv <- NULL
    set <- function(y) {
        x <<- y
        matInv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) matInv <<- inverse
    getInverse <- function() matInv
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}

##this function is for solving from argument cached from previuse function
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if( !is.null(m) ) {
        message("Getting cached inv matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data) %*% data
    x$setInverse(m)
    m
}




