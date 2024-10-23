
# Function to create a cache 'matrix' object 
makeCacheMatrix <- function(x = matrix()) 
{

  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL # Reset the cached inverse when the matrix changes
    
  }  
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list( set = set, get = get,setInverse = setInverse, getInverse = getInverse)
  
}


# Function to compute the inverse of the matrix

cacheSolve <- function(x, ...) 
{
  inv <- x$getInverse()   # Try to get the cached inverse
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  
  # If the inverse is not cached, compute it
  data <- x$get()
  inv <- solve(data,...)
  x$setInverse(inv)
  # Return the inverse
  inv
}

#Usage Example
mat <- matrix(c(1,2,3,4),nrow = 2,ncol = 2) 
inv1 <- cacheSove(mat)

# First call: computes and caches the inverse
cacheSolve(cachedMatrix)

# Second call: retrieves the cached inverse
cacheSolve(cachedMatrix)
