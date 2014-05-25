## The function makeCacheMatrix takes a martix as input and calls the function
## cacheSolve to compute the inverse of that matrix.
##
makeCacheMatrix <- function(x=matrix()) {
## Function receives a matrix, if empty then an empty matrix is returned.
## If a valid matix is received the function cacheSolve is called to compute the inverse matrix.
##
  if (is.na(x[1,1])) {
    cat("Empty matix received","\n")
    return(x)
  }
  inv_mat <- cacheSolve(x)
  cat("The inverse matrix is :","\n")
  print(inv_mat)
  i_matc <<- x
}
## 
cacheSolve <- function(imat) {
## Compute the inverse of the matrix passed to this function (solve function). If the
## incoming matrix is the same as in the previous call, retrieve the inverse from cache.
## If this is a new matrix the inverse will be stored in cache.
##
  if (identical(imat,i_matc) ) {
    cat("Retrieving inverse matrix from cache...\n")
    return(o_matc)
    }
  else {
    cat("Calculating inverse...","\n")
    o_matc <<- solve(imat)
    i_matc <<- imat
    return(o_matc)
  }
}
