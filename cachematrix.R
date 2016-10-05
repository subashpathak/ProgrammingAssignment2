

makeCacheMatrix <- function(x = matrix()) {
inv<-null
set<-function(y) {
        x<<-y
        inv<<-null }
          
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)        
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. 
 #If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.          
                                                                        
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(inv)
        inv       
}
