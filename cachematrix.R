## This script contains 2 functions: makeCacheMatrix and cacheSolve

## The cacheSolve function creates a special "matrix",
## which is a list containing the following a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) i <<- inverse
        
        getinverse <- function() i
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse ) 
        
}

## The cacheSolve function returns the inverse of the
## special "matrix" created with makeCacheMatrix function
## and calculates the inverse or return the cached inverse
## if it is already available

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        i<-x$getinverse()
        
        if(!is.null(i)){
                
                message("getting cached data")
                return(i)
        }

        i<-solve(x$get())
        
        x$setinverse(i)
        
        i

}
