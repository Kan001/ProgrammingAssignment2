## The function makeCacheMatrix takes a matix as input (or default) and calls 
## the function cacheSolve to compute the inverse of that matrix.
## To save computing cycles cacheSolve will cache matrices identical to the preceding call
##
makeCacheMatrix <- function(x=matrix()) {
  if (is.na(x[1,1])) {
    cat("Empty matix received","\n")
    return(x)
  }
  inv_mat <- cacheSolve(x)
  o_matc <<- inv_mat  
  cat("The inverse matrix is :","\n")
  print(o_matc)
  i_matc <<- x
}
## 
cacheSolve <- function(imat) {
## Return a matrix that is the inverse of 'x'
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
