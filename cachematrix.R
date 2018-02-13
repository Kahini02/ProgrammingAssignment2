## Here we are writing a function for cache matrix funtion,which can store the original matrix and get the inverse once the function is run. 

makeCacheMatrix <- function(x = matrix()) 
{
  invMat <-NULL
  set <- function(y)    ## Set Value
  {
    x <<- y
    invMat <- NULL
  }
  get <- function()x        ##Get value of x
  setinv <- function(inverse)invMat <<- inverse
  getinv <- function()invMat  ##Inverse value
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Inverse of Special matrix created through the above function.

cacheSolve <- function(x, ...) 
{
  invMat <- x$getinv() ## Gets inverse value if calculated previously
  if(!is.null(invMat))
  {
    message("Getting cached data")
    return(invMat)         
  }              
    data <- x$get()   
    invMat <- solve(data)
    x$setinv(invMat)
    invMat         ## Return a matrix that is the inverse of 'x'
}
