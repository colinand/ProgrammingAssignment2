
## Creates a matrix that is used to get and store inverted cached matrix 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y  ## assigns y to x variable in the parent environment
        inv <<- NULL
        
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)     ##Creates list containing funtions to 
                                    ## 1) Set the matrix, 2) get the matric, 3) set inverse matrix, 4) get inverse matrix
}


## The following function returns the inverse of the matrix. It first checks if
## the inverse exists. If so, it gets the result, says "getting cached data" and skips the
## computation. If not, it computes the inverse, sets the value in the cache with the
## setinverse function.
## The solve function computes the matrix if necessary


cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")  ##Message returned if inverse is cached
        return(inv)
        
    }
    data <- x$get()
    inv <- solve(data, ...)  ##assigns the inverse matrix solution to inv
    x$setinverse(inv)
    inv
}
