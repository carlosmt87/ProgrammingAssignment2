## These functions take advantage of R Lexical Scoping rules
## to implement caching functionality for an inversable matrix object


#makeCacheMatrix can be use to transform a normal matrix into one
#that is able to cache its inverse matrix
makeCacheMatrix <- function(x = matrix()) {

	matrixInverse <- NULL
    message("Cacheable matrix created...")
    
    setData <- function(y)
    {
        x <<- y
        #If the matrix changes, it's inverse value must be
        #reset since the cached version is not valid anymore
        matrixInverse <<- NULL
        message("The matrix data has been succesfully set...")
      
    }
    
    getData <- function() x
    
    setInverse <- function(inverse) matrixInverse <<- inverse
    
    getInverse <- function() matrixInverse
    
    
    #Return a list of functions to manipulate the special matrix
    list(setData=setData, 
         getData = getData, 
         setInverse = setInverse, 
         getInverse=getInverse)
}


## This function returns the inverse matrix of a cacheable matrix object.
## If the inverse matrix is located in the cache, it will return this value.
## Otherwise, the value must be calculated

cacheSolve <- function(x, ...) {
      inverse <- x$getInverse()
  
  #If the inverse value exists, the cache version must be returned
  if(!is.null(inverse))
  {
      message("Getting the inverse matrix from cache...")
      
  }
  #If the inverse value doesn't exist, let's calculate it!
  else
  {
    message("Calculating the inverse matrix and caching the result...")
    matrix <- x$getData()
    inverse <- solve(matrix)
    x$setInverse(inverse)
    
  }
  
  inverse 
		
		
}
