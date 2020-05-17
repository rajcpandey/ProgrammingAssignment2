## 
## cachematrix.R contains two functions makeCacheMatrix and cacheSolve. Sole purpose of makeCacheMatrix 
## is to supper assign variables and define functions which eventually returned as element/component of 
## a list object. cacheSolve is the main function which call for makeCacheMatrix and check if the matrix
## inverse has been performed already and return inverse accordingly.

## makeCacheMatrix - Takes input of a matrix object, creates a local variable m and assign it with NULL. 
## There are 4 functions defined within the scope of this master function and each of these in-scope 
## functions eventually assigned to a list object as an element of class matrix . These in-scope functions
## are available for use on the prompt by creating an object from the function makeCacheMatrix. Example is 
## given below:
## mat <- makeCacheMatrix(matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 3))
## mat$get() or any other function defined within the scope of makeCacheMatrix.
## In-scope function setInv does the trick and supper assign the first calculated inverse of the matrix to
## variable m which become available for recalling the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  #return list object with functions set, get, setInv, getInv as element of the list object
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve is the main caller function once the matrix object is ready for use. 

## x$getInv() assign m with the value which was passed to create the matrix. Next statement checks for the empltiness
## of the variable and if not then recall the cached inverse from super assigned variable m otherwise calculate inverse
## of the matrix using solve function. 

cacheSolve <- function(x, ...){
  #check if passed parameter is a matrix
     m <- x$getInv()
      #inverse matrix
      if(!is.null(m)){
        print("get inverse")
        m
      }
      else{
        m <- solve(x$get())
        x$setInv(m)
        m
      }
}
