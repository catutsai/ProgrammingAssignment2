## makeCacheMatrix is to create a matrix with set/get/setinv/getinv.

makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
    get <- function() x
    setinv <- function(inv = matrix()) invx <<- inv
    getinv <- function() invx
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve is to solve inverse of sqaure matrix.
## IF NOT A SQUARE MATRIX: 錯誤在solve.default(data)

cacheSolve <- function(x, ...) {
    invx <- x$getinv()  
    if(!is.null(invx)) {
        message("getting cached inverted matrix")
        return(invx)
    }
    data <- x$get()
    invx <- solve(data)
    x$setinv(invx)
    invx
}
