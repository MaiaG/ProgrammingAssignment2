#Matrix inversion is usually a costly computation and there 
#may be some benefit to caching the inverse of a matrix rather 
#than computing it repeatedly. Here are written functions 
#that cache the inverse of a matrix.

#The first function, makeVector creates a special "vector", 
#which is really a list containing a function to:
#a)  set the value of the vector
#b)	get the value of the vector
#c)	set the value of the mean
#d)	 the value of the mean

#makeCacheMatrix: This function creates a special "matrix" object 
#that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#The following function calculates the inverse of the special "matrix" 
#returned by makeCacheMatrix above.However, it first checks to see 
#if the inverse has already been calculated. If so, it gets the inverse 
#from the cache and skips the computation. Otherwise, it calculates the 
#inverse of the data and sets the value of the inverse in the cache via 
#the setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)    #Return a matrix that is the inverse of 'x'
  inv
}




