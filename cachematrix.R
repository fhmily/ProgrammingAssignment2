## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# function makeCacheMatrix will create a closure with
# methods for storing and retriving cached inversed Matrix
makeCacheMatrix <- function(x = matrix()) {
    # closure variable for caching inversed Matrix
    inversedM <- NULL
    
    # method for setting original Matrix
    set <- function(y) {
        x <<- y
        inversedM <<- NULL
    }
    
    # method for getting original Matrix
    get <- function() x
    
    # method for getting inversed Matrix
    getInversedM <- function() inversedM
    
    # method for setting inversed Matrix
    setInversedM <- function(y) {
        inversedM <<- y
    }
    
    # return methods to complete the closure
    list(
        get = get,
        set = set,
        getInversedM = getInversedM,
        setInversedM = setInversedM
    )
    
}


## Write a short comment describing this function

# function cacheSolve accept makeCacheMatrix returned value
# to retrieve or calculate inversed Matrix from it
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    iM <- x$getInversedM()
    
    # check if inversed matrix has already been created
    if(!is.null(iM)){
        # cached value returned
        return(iM)
    } else {
        # calculate the reversed Matrix and set cache
        iM <- solve(x$get())
        x$setInversedM(iM)
        
        # return the value
        iM
    }
}
