## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function caches the input matrix and has the getter and setter methods to fetch & store data from memory whenever required
makeCacheMatrix <- function(x = matrix()) {
      #Initializing the Inverse Matrix as NULL
      inv <- NULL
      #Initializing the input Matrix suitable to function from the Parent Environment 
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      #Fetching the cached Matrix from the function
      get <- function() x
      #Calculating the inverse of a Matrix
      setinverse <- function(inverse) m <<- inverse
      #Fetching the cached inverse of the Matrix from the function
      getinverse <- function() m
      #Naming the Function Calls to fetch them to the Console
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
#This function calculates the inverse of the matrixif not already cached into the memory
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      #Get the inverse of a Matrix already stored in cache
      m <- x$getinverse()
      #If cached data exists we return the same data
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      #No cached data exists we do the following
      #Fetch the Matrix to calculate inverse
      data <- x$get()
      #Compute the inverse of a Matrix
      m <- solve(data, ...)
      #Cache the inverse into the memory
      x$setinverse(m)
      #Return the value of inverse 
      m
      
}