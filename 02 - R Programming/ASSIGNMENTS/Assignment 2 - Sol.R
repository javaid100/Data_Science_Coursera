# =======================================================================================
# ============= A pair of functions that cache the mean of a vector =====================
# =======================================================================================
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

# This function computes the mean of the special "vector" returned by makeVector above.
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

# ==========================================================================================
# =============== A pair of functions that cache the inverse of a matrix ===================
# ==========================================================================================
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}

# ====================================================================================
# ==================================== Results =======================================
# ====================================================================================
pvector <- makeVector(c(2,3,4,5,6))
pvector$get()
pvector$getmean()
cachemean(pvector)
pvector$getmean()

pmatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
pmatrix$get()
pmatrix$getInverse()
cacheSolve(pmatrix)
pmatrix$getInverse()
