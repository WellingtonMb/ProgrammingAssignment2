
# This assignment has functions that cache the inverse of a matrix.
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# If the inverse has already been calculated (and the matrix has not changed)
# then the cachesolve should retrieve the inverse from the cache.


# This assignment has functions that cache the inverse of a matrix.
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# If the inverse has already been calculated (and the matrix has not changed)
# then the cachesolve should retrieve the inverse from the cache.


makeCacheMatrix <- function(x = matrix()) { 
  
  matrix_Inverse <- NULL                     
  set <- function(y) 
  {                      
    x <<- y
    matrix_Inverse <<- NULL              
  }
  
  get <- function() x                           
  
  setinverse <- function(solve){
    matrix_Inverse <<- solve
  } 
  
  getinverse <- function() matrix_Inverse        
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) { 
  
  matrix_Inverse <- x$getinverse()
  if(!is.null(matrix_Inverse)) {                 
    message("getting cached data - Inverse of the matrix")
    return(matrix_Inverse)
  }
  
  data <- x$get()                              
  matrix_Inverse <- solve(data, ...)
  x$setinverse(matrix_Inverse)
  matrix_Inverse
}


