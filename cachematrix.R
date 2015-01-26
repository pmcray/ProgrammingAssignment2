## makeCacheMatrix and cachesolve are a pair of functions that have the support the 
## creation of and access to the cache of the inverse of a matrix. Inverting a matrix is 
## a common computational task. Matrices can be very large and calculating the inverse
## is a potentially computationally intensive process. It can therefore makes sense to 
## cache the inverse of a (typically large) matrix if it is used repeatedly in a set of 
## calculations. 

## The makeCacheMatrix function takes an ordinary numeric matrix and returns an special 
## matrix object in the form of a list, the inverse of which is calculated, cached and 
## accessed by the cacheSolve function. 

makeCacheMatrix <- function(x = matrix()) {

## The input to the makeCacheMatrix function is coerced to be a matrix.   
  
  m <- NULL
  
## Sets the value of a variable m in the function environment to be NULL.
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
## Defines the set function of the CacheMatrix object. This takes an argument y and sets
## the values of two variables in the parent environment: x to y, the argument to the set 
## function, and m to NULL.

  get <- function() x
  
## Defines the get function of the CacheMatrix object. This returns the value of x in the
## function environment.

  setinverse <- function(inverse) m <<- inverse
  
## Defines the setinverse function of the CacheMatrix object. This takes the value of the 
## inverse and sets the value of the variable m in the parent environment to be equal to
## the inverse.

  getinverse <- function() m

## Defines the getinverse function of the the CacheMatrix object. This returns the value of
## m, which is either NULL or the value of the inverse if setinverse function has been
## invoked.
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
  
## Constructs the CacheMatrix object, which is a list. This list is return by the function.
## The object has four methods corresponding to the four functions defined in the function
## createCacheMatrix function set, get, setinverse and getinverse.
}

## The cacheSolve function takes a CacheMatrix object produced by the makeCacheMatrix 
## function as input. It returns the inverse of the matrix wrapped by the cacheMatrix 
## object. If the CacheMatrix object has not been passed to the function before, it 
## calculates the inverse, stores it in the cache and returns it and also stores the 
## CacheMatrix object. If the CacheMatrix object has been passed to the function before, it 
## checks to see if the CacheMatrix object has changed. If it has changed, it calculates 
## the inverse, returns it and stores it in the cache, and stores the CacheMatrix object. 
## If the CacheMatrix object has not changed, the function retrieves the inverse from the 
## cache and returns it.

cacheSolve <- function(x, ...) {

## The first input to the cacheSolve function is a CacheMatrix object. 
  
  inverse <- x$getinverse()
  
## The getinverse() method of the CacheMatrix object is invoked to return the inverse, 
## which may be NULL if it has not been calculated yet.  
  
  if((!is.null(inverse)) && (identical(x, last_x))) {
    
## If the inverse is not NULL and the CacheMatrix object has not changed since it was last
## passed, the cached value of the inverse is stil valid.
    
    message("getting cached data")
    return(inverse)
    
## The cached value of the inverse is returned.      
    
  }

  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  
## If the inverse is NULL (has not yet been calculated) or the CacheMatrix object has 
## changed since it was last passed, the get method of the CacheMatrix object is invoked
## to obtain the original matrix. This is then passed to the solve function. The 
## setinverse method of the CacheMatrix object is then invoked to set the value of the 
## inverse associated with the CacheMatrix object.
  
  last_x <<- x

## As this is either the first time that the CacheMatrix object has been passed to the
## CacheSolve function or the CacheMatrix object has changed, the CacheMatrix object is 
## stored to a variable in the parent environment for later comparison as required.
  
  inverse

## Returns the value of the inverse is returned.    
}