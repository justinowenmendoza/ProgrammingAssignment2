##Use makeCacheMatrix function to generate and 
#to create matrix and will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  z <- NULL 
  
  ##Provide the value that can be seen in the matrix
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  
  ##Set its functions
  get <- function()x
  
  ##Assign various conditions to set and generate the inverse matrix
  setInv <- function(inverse) {z <<- inverse}
  getInv <- function(){z}
  list (set = set, get = get, 
        setInv = setInv, 
        getInv = getInv)
}

##use cacheSolve to calculate for the findings

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  z <- x$getInv()
  if(!is.null(z)) {
    message("generating the results")
    return(z)
  }
  ##Processing the inverse matrix
  data <- x$get()
  z <- solve(data,...)
  x$setInv(z)
  z
}

