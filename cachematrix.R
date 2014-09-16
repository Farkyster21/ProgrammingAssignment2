##Assignment is to practice caching data types in order to save run time
#makeCacheMatrix: Function creates a special "matrix" object that can cache it's inverse
#cacheSolve: Function computes the inverse of special matrix, returned by above function
##If the inverse is already calculated( and the matrix hasn't changed), then the
##cacheSolve should retrieve the inverse from the cache


## The function: makeCacheMatrix: creates a special "Matrix", which is really a list containing
## functions that can access the matrix.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      ##Setting value of matrix
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      ##Gets the value of the matrix
      get <- function() x
      
      ##Sets the value of the Inverse
      setInverse <- function(solve) m <<- solve
      
      ##Gets the value of the Inverse
      getInverse <- function() m
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## The function: CacheSolve, calculates the inverse of the 
## special "Matrix" created by "makeCacheMatrix". But, it checks
## to see if the inverse has already been calculated. If it has, it
## gets inverse from the cache and skips the computation. Otherwise, it will
## calculate the inverse matrix of the data and set the value of the inverse in the cache
## With the setInverse function

cacheSolve <- function(x, ...) {
        ##Pulls in initial inverse matrix from makeCacheMatrix
        m <- x$getInverse()
        
        ##Returns Inverse if computation has already been calculated previously, for same matrix
        if(!is.null(m)){
              message("getting cached data")
              return(m)
        }
        
        ##Creates a new inverse matrix, if the matrix wasn't the same as before.
        data <- x$get()
        m <- solve(data,...)
        
        ##Sets the new value of the inverse matrix
        x$setInverse(m)
        
        ## Return a matrix that is the inverse of 'x'
        m
        
}
