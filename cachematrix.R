## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix function expects square and inversible matrix, although it is not enforced. 
## This sets, gets matrix and its inverse.
## This returns a list with all 4 functions in it. 
makeCacheMatrix <- function(x = matrix()) {
  matInv<-NULL
  setMatrix<-function(newMat) {
    x<<-newMat ## This is to assign value to x which is outside the function.
    matInv<<-NULL
  }
  getMatrix<-function() x
  getInverse<-function(){
    matInv
  }
  setInverse<-function(Inv){
    matInv<<-Inv
  }
  list(setMatrix=setMatrix,getMatrix=getMatrix,getInverse=getInverse,setInverse=setInverse)
}


## Write a short comment describing this function
## For the list returned by above function makeCacheMatrix, cacheSolve will check if the Inverse of the matrix is already calculated or not.
## If it is calculated, it simply returns that. If not it calculates inverse, set the inverse then prints it as well.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matInv<-x$getInverse()
  if(!is.null(matInv)){
    message("This is cached inverse of Matrix")
    return(matInv)
  }
  oriMat<-x$getMatrix()
  matInv<-solve(oriMat)
  x$setInverse(matInv)
  matInv
}
