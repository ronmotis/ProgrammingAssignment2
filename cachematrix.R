##These functions are for the Coursera Data Science: R Programming 
## Week 3 Assignment; GitHub user: Ronmotis

##This function creates a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL                             ## init. matrix inverse 
		set <- function(y) {                    ## define the set function to assign new 
		x <<- y                             	## value of matrix in parent env.
        inv <<- NULL                        	        ## if a new matrix, reset inv to NULL
			}
 
 get <- function() x                     		                ## get fucntion - return value of the matrix arg.
   
		setinverse <- function(inverse) inv <<- inverse         ## assigns value of inv in parent env.
		getinverse <- function() inv                            ## gets the value of inv where called
		list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)    ## need this in order to refer 
                                                                                                ## to the functions with the $ operator


}


## This function computes the inverse of the special "matrix" returned by make CacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
             inv <- x$getinverse()
     if(!is.null(inv)) {
         message("getting the inverse")
         return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinverse(inv)
     inv
  }
}
