## The makeCacheMatrix and cacheSolve functions work in tandem to store and
## recall a calculated inverse matrix.  This storage reduces processing time
## for routinely used matrix calculations

# Function stores a specified matrix and calculated inverse in Parent Environment
# storage allows for easy recall and eliminate constant processing with solve()
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #Initialize m Variable
  
  #In parent environment, store X and clear m from history
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Get values from parent environment then calculate/store means
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  
  #Create a named list to store function values
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# Specialized solve function to return stored inverse matrix from makeCacheMatrix
cacheSolve <- function(x, ...) {
  m <- x$getInverse() #Attempt to retrieve Stored Inverse
  
  #If Value is stored, return the Inverse Matrix from storage
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  
  #If Not stored, retrieve Input and Calculate Inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

message("Test Functions")
m1<-matrix(c(0.5, -0.25, -1.0, .75), nrow =2, ncol=2)
m1
Test_Matrix1<-makeCacheMatrix(m1)


