## Matrix inversion is usually a costly computation and there 
## may be some benefit to caching the inverse of a matrix
## rather than computing it repeatedly


## makeCacheMatrix: Constructor of our own Matrix object, 
## I holds a referece to the inverse of the matrix
## When we set a new matrix, the inverse is reset to null

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


## cacheSolve: function that returns the inverse of our Matrix 
## when there is nothing cached it will execute the calculations and cache the result in our Matrix object
## when there is a cached inverse in our Matrix object it will return it

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
  }
  else{
    message("calculating inverse and caching it")
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
  }
  return(inv)
}

#How to test
# 1. Create a square matrix

# x = rbind(c(1, -1/4), c(-1/4, 1))

# 2. Constructor of our matrix object that will cache the inverse

# m = makeCacheMatrix(x)

# 3. Print our matrix 

# m$get()

# 4. Invoke function that will return the inverse matrix. 
#    Notice in the logs that it calculates it since there is nothing cached

# cacheSolve(m)

# 5. Invoke function that will return the inverse matrix.
#    Notice in the logs that it doesn't calculate it but it returns the cached matrix

# cacheSolve(m)

