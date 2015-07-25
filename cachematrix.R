## method to create a list of operations that will handle the input matrix
## compute its inverse, store and return it when needed
makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the inverted matrix as NULL
        theInverse <- NULL
        
        ##The set function where we can set the input matrix
        set <- function(inputMatrix) {
                x <<- inputMatrix
                theInverse <<- NULL
        }
        ## The get function to get the input matrix
        get <- function() x
        
        ## Setting the inverted matrix
        setInverse <- function(inv) theInverse <<- inv
        
        ##Getting the inverted matrix
        getInverse <- function() theInverse
        
        ## List of possible oprations
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## method to compute and return the inverse of the matrix from cache
## or freshly computed for a new.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' through the getInverse
        theInverse <- x$getInverse()
        
        ## Null check to verify if inverse has already been computed and 
        ## stored or not. If computed return the same and do not compute again.
        if(!is.null(theInverse)) {
                message("getting cached data")
                return(theInverse)
        }
        ## get method to get the original matrix data
        data <- x$get()
        ## The inverse computing step
        theInverse <- solve(data)
        
        ## Setting the computed inverse
        x$setInverse(theInverse)
        
        ##The computed inverse to be returned
        theInverse
}
