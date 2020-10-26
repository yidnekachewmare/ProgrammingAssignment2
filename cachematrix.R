## We know that in most cases computing the inverse of a matrix is time taking and difficult when its dimensions 
## are getting larger and larger. So to cope with these difficulties we will deal with some pair of functions that 
## help to compute and cache the inverse of a matrix. 

## In this first function we create a special 'matrix' object that can cache the inverse of this special matrix. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- solve(x)
  getinverse <- function () inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## In this second function we compute the inverse of the special matrix returned by makeCacheMatrix function given 
## above and if the inverse is already computed this second function retrieves it from the cache.   

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <-x$get()
  inv <-solve(data, ...)
  x$setinverse(inv)
  inv
}
