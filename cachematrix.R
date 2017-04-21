## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        # initialize the inverse matrix
        inv <- NULL
        
        # set the value of matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # get the value of matrix
        get <- function() x
        
        # set the value of inverse
        setinv <- function(initial_inv) inv <<- initial_inv
        
        # set the value of inverse
        getinv <- function() inv
        
        # return all above function
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}
        
## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinv function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # get the mattix
        data <- x$get()
        
        # solve the inverse
        inv <- solve(data, ...)
        
        # cache the inverse of the matrix
        x$setinv(inv)
        
        # return inverse
        inv
        
}
