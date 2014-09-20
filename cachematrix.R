##Function  makeCacheMatrix : This function creates a special "matrix" 
##object that can cache its inverse.
makeCacheMatrix <- function( mat = matrix() ) 
{ 
  inv <- NULL 
  
  set <- function( matrix ) 
  { 
    mat <<- matrix 
    inv <<- NULL 
  } 
  
  get <- function() 
  { 
    mat 
  } 
  
  setInv <- function(inverse) 
  { 
    inv <<- inverse 
  } 
  
  
  getInv <- function()  
  { 
    inv
  } 
  
  list(set = set, get = get, 
       setInv = setInv, 
       getInv = getInv) 
} 

######################################## 

##Function cacheSolve : Computes the inverse of the special "matrix" 
##returned by  makeCacheMatrix  above. If the inverse has already been 
##calculated (and the matrix has not changed), then the  cachesolve  should retrieve the inverse from the cache. 
cacheSolve <- function(x, ...) 
{ 
  m <- x$getInv() 
  
  if( !is.null(m) ) 
  { 
    message("getting cached Inv matrix data") 
    return(m) 
  } 
  matdata <- x$get() 
  
  m <- solve(matdata) %*% matdata 
  
  x$setInv(m) 
  
  m 
}
