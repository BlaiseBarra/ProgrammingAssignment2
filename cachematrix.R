
## The end goal is to create a cache function to cache the inverse of a matrix. The Cache process is useful
## for storing results of processes that are "costly" with regards to processing power in order to save time
## from reprocessing the same thing over and over again. 
## The first step is to create the function makeCacheMatrix. This function retrieves the matrix, sets the inverse 
## value, and defines set, get, setinverse, and getinverse. 



makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

##The second function calculates the inverse of the matrix using the makeCacheMatrix function. It first checks if the inverse has already been calculated and cached.
## If it is cached, it returns the cached value and does not recalculate the inverse. If it is not cached, it will
## compute the matrix inverse. The bottom three functions below the hash line can be used as a test. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}####################################################################################################

## The first line defines "R" which is a simple 2 by 2 matrix. the second line defines "mymatrix" as the result
## of makecachematrix of the matrix "R". The final line returns the inverse. Comparing the final result to the 
## original value of "R" demonstrates that the inverse has completed successfully. running cacheSolve(myMatrix
## again will demonstrate that the function is now pulling the cached value and is not recalculating. The message 
## "getting cached data" appears now that the inverse is pulling from the cached results

R = matrix(c(1,2,3,4),nrow=2,ncol=2)

myMatrix <- makeCacheMatrix(R)

cacheSolve(myMatrix)