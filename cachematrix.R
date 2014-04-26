## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) ##create matrix cache
{
  i <- NULL
  set <- function(y) ## sets the matrix
    {
    x <<- y  ##copies y to x
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse ##set inverse of matrix
  getinverse <- function() i  ## get inverse of matrix
  list(set= set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) ## return list with 4 values
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...)  ##calculate inverse of a matrix
{ 
  i <- x$getinverse() ##inverse of input matrix x
                                 if (!is.null(i)) ##cache created or not{
                                   message("getting cached data")
                                   return(i) ## return cache content
                                 }
                                 data <- x$get() ##get cache content
                                 i <- solve(data, ...)  
                                 x$setinverse(i) ##set the value
                                 i ## returns output
                                 
        ## Return a matrix that is the inverse of 'x'
}
