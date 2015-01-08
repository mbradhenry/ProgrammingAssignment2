# By utilizing functions 'makeCacheMatrix' and 'cacheSolve' defined below, we can 
# store a matrix and its inverse in cache so that the inverse need not be 
# recomputed when it is needed more than once in a program. This can save 
# considerable time in the case that the matrix is large or the inverse is 
# invoked often in a program.

# The 'makeCacheMatrix' returns a list consisting of four functions ('set', 'get', 
# 'setinverse', 'getinverse'), which may be used to cache a matrix and its 
# inverse so that both can be accessed often without requiring additional
# computation. 

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        
        # The 'set' function places 'x' into memory and sets 'm' to NULL. The object 
        # 'm' is the cached value given back by 'getinverse' and so it must be set
        # to NULL since the matrix held by 'get' has been changed by 'set'.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # The 'get' function returns the matrix 'x' that is held in cache.
        get <- function() x
        
        # The 'setinverse' function is called by the cacheSolve function and 
        # places into memory the inverse of the matrix held by 'get' as the 
        # object 'm'. The object 'm' exists in the global environment.
        setinverse <- function(inverse) m <<- inverse
        
        # The 'get' function returns the inverse matrix 'm' that is held in cache.
        getinverse <- function() m
        
        # makeCacheMatrix returns a list of the four functions defined above.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# The function 'cacheSolve' takes as input a list that is the output of the 
# function 'makeCacheMatrix'. If the inverse of the matrix held in cache by 
# 'makeCacheMatrix' has previously been computed, then 'cacheSolve' simply returns
# the cached inverse. Otherwise, 'cacheSolve' computes the inverse of the matrix
# held in cache and then places the inverse in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # cacheSolve tests whether the inverse of the matrix 'x' has previously 
        # been computed. If so, cacheSolve returned the cached version of the 
        # inverse of 'x'.
        m <- x$getinverse()
        if(!is.null(m)) {
                message("Getting the cached inverse.")
                return(m)
        }
        
        # Since the inverse of 'x' is not currently cached, cacheSolve must 
        # compute the inverse of 'x' and place the result in cache. It does so 
        # by getting 'x' from memory using 'get' and then employs 'solve' to 
        # compute the inverse of 'x'. Finally, it places the inverse in cache
        # using the 'setinverse' function.
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
