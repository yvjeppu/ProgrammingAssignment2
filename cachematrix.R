## 1. Source the file
## 2. Make two matrices
##    mat <- matrix(c(1,2,3,1,3,4,1,12,1),ncol=3,nrow=3,byrow=TRUE)
##    mat1 <- matrix(c(1,2,31,1,3,4,1,12,1),ncol=3,nrow=3,byrow=TRUE) 
## 3. Make the environment for cache
##    lis <- makeCacheMatrix()  
## 4. cacheSolve(mat, lis)  this will give the inverse first time
## 5. cacheSolve(mat, lis)  this will retrive from cache
## 6. cacheSolve(mat1, lis) this will recompute and store new matrix 
##    and inverese

cacheSolve <- function(mat, x, ...) {
  ##to find the inverse of matrix
  inv <- x$getinverse()
  if(!is.null(inv) && identical(x$get(),mat)) { ### check if the matrix is equal
    print("getting cached data")
    return(inv)
  }
  x$set(mat)
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}

makeCacheMatrix <- function() {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
    
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  lis<-list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  return(lis)
}
