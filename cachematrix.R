# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
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


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}


makeCacheMatrix <- function(x = matrix()){    
  m <- NULL
  set <- function(y){
    x <<- y  
    m <<- NULL #store matrix in cache 
  }
  get <- function() x #get matrix
  setInverse <- function(solve) m<<- solve #set inverse matrix
  getInverse <- function() m #get inverse matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  ## create list of functions
}

## cacheSolve take a custom matrix type created by the makeCacheMatrix function
## and calculates the inverse matrix of it
## but first it checks to see if the calculation has been done before
## if it has been done before it recalls the data from the cache. If it has not been done 
## before it calculates the inverse matrix then store it in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()                 
  if(!is.null(m)){                   
    message("getting cached data")    
    return(m)                        
  }
  data <- x$get()                      
  m <- solve(data, ...)              
  x$setInverse(m)                    
}
