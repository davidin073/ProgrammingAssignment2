## Matrix inversion is usually a costly computation and their may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
	 s <- NULL
	 #set the value of the vector
     set <- function(y) 
	 {
                x <<- y
                s <<- NULL
     }
	 #get the value of the vector
     get <- function() x
	 #set the value of the mean
     setsolve <- function(solve) s <<- solve
	 #get the value of the mean
     getsolve <- function() s
     list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
		#get the value of the mean
		s <- x$getsolve()
        if(!is.null(s)) 
		{
                message("Getting cached data")
				#If exists return the inverse of the special "matrix"
                return(s)
        }
		#get the value of the vector
        data <- x$get()
		#calculate the inverse of the special "matrix"
        s <- solve(data, ...)
		#set the value of the mean
        x$setsolve(s)
        s
}
