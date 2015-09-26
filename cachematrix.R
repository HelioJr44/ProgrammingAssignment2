## makeCacheMatrix returns a list with 4 functions: set for setting a matrix; get for getting the matrix saved; setinv for calculating the inverse matrix; getinv for getting the saved inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    set <- function(y) {
        x <<- y
        inv_matrix <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inv_matrix <<- inv
    getinv <- function() inv_matrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve returns a matrix that is the inverse of 'x'. If the inverse have already been calculated, it is not necessary to calculate again, then the value is got from the function "getinv()". Else, the inverse is calculated with the function solve() and saved with the funtion "setinv()"  

cacheSolve <- function(x, ...) {
    inv_matrix <- x$getinv()
    if(!is.null(inv_matrix)) {
        message("getting cached data")
        return(inv_matrix)
    }
    data <- x$get()
    inv_matrix <- solve(data, ...)
    x$setinv(inv_matrix)
    inv_matrix
}