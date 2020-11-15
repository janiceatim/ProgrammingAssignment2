# Below are two R functions (makeCacheMatrix and cacheSolve) for the Peer Reviewed Lexical Scoping Task.
# Matrix inversion is usually costly consumption, as such catching the inverse of a matrix is beneficial

# makeCacheMatrix function creates a special "matrix"

makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL
       set <- function(y) {
               x <<- y
               inv <<- NULL
       }
       get <- function() x
       setInverse <- function(Inverse) inv <<- Inverse
       getInverse <- function() inv
       list(set = set, 
            get = get, 
            setInverse = setInverse, 
            getInverse = getInverse)
       }

# cacheSolve function computes the inverse of the above
# If the inverse is calculated, the computation is skipped

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
               message("getting cached data")
               return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
        }
