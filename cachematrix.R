## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      s <- function(y){
        x <<- y
        i <<- NULL
      }
      g <- function(){x}
      sInv <- function(inverse) { i <<- inverse}
      gInv <- function() {i}
      list(set = s, get = g, setInverse = sInv, getInverse = gInv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$gInv()
        if(!is.null(i)){
          message("get")
          return(i)
        }
        mat <- x$g()
        i <- solve(mat, ...)
        x$sInv(i)
        i
}
