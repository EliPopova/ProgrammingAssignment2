
makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set = function(y) {
        
        x <<- y
        inv <<- NULL
    }
    
    get = function() x
    setinv = function(inverse) inv <<- inverse
    getinv = function() inv
    list(set=set, get=get, 
         setinv=setinv,
         getinv=getinv)
}


## This function checks if the matrix inverse has already been calculated
## If so, it gets the inversion from the chache. If no, it gets the 
## inversion calculated in the function above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv = x$getinv()
    
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    data = x$get()
    inv = solve(data, ...)
    
    x$setinv(inv)
    
    return(inv)
}
