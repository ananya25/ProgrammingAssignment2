## These Functions below are useful for caching the inverse of matrix. 
## Since Matrix Inversion is a time consuming process, it is beneficial to 
## cache the inverse of a matrix where repeated computations are required.

## This function - makeCacheMatrix() creates a special matrix which can store/cache its inverse
## This function returns a list containing four functions (which would be used in the second function - cacheSolve() )
## The list to be returned has four functions which are
## 1. setmatrix() - Set the value of matrix 
## 2. getmatrix() - Get the value of matrix  
## 3. setinverse() - Set the value of Inverse of the matrix 
## 4. getinverse() - Get the value of Inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
## Assigns Null value to cache	
 cache   <-  NULL
 
 ## Sets the matrix and flushes the cache
 setmatrix  <-  function(y) { 
 	x  <<-  y 
 	cache  <<-  NULL
 	}
 	
 ## Returns the stored  matrix	
 getmatrix <-  function() x
 
 ## Set the Inverse of the matrix
 setinverse  <-  function(inverse) {
 	cache  <<-  inverse
 	}
 ## Gets the inverse of the Matrix
 getinverse  <-  function() cache	
 
 ## Returns the list of four functions
 list(setmatrix = setmatrix,  getmatrix = getmatrix,  setinverse = setinverse,  getinverse = getinverse)
 
}


## This function  - cacheSolve() calculates the inverse of the matrix created by makeCacheMatrix()
## This function checks to see if there is some value in the cache where inverse of the matrix might be stored. 
## If a value is stored in the cache its returns that else computes the inverse of the matrix

cacheSolve <- function(x, ...) {

## get the cached value of inverse	
        cache <- x$getinverse()
## Check if there is value stored in cache , return if yes, else compute the inverse
       if (!is.null(cache)) { message ("getting cached data")
       return(cache)
       }
 ## Computes the Inverse      
       data <- x$getmatrix()
       cache <- solve(data)
       x$setinverse(cache)
       
       return (cache)
}

