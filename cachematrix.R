# Assigment 2: R function to cache the inverse of a matrix 
# To run:
#         1. M<-makeCacheMatrix(matrix(sample.int(10, 4*4, TRUE), 4, 4))
#         2. Run I<-cacheSolve(M) twice. The cache matrix will be used the second time.
#         3. P<-makeCacheMatrix(I) creates a new list with the inverse matrix
#         4. cacheSolve(P) returns the original matrix to check the result is right
#         5. M$get() returns the original matrix to compare with the previous one.
#

# This function creates a list containing a function 
# to contain the value of the matrix, to get the valu of the matrix
# to set the value fo the inverse matrix, and to get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  #Matrix initialization to NULL
  m<-NULL
  #Definition of function set matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  #Definition of function get
  get<-function() x
  
  #Definition of function setmatrix to set 
  #the value of the inverse matrix
  setmatrix<-function(solve) m<<- solve
  
  #Definition of function getmatrix to get 
  #the value of the inverse
  getmatrix<-function() m
  
  #Creates a list with set, get, setmatrix, and 
  #getmatrix functions
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}



## This function calculates the inverse of the matrix
## if this is not cached. Otherwise, returns the cached value 
cacheSolve <- function(x, ...) {
  
  #Assigns the matrix to m 
  m<-x$getmatrix()
  if(!is.null(m)){
    #The cached matrix is used
    #if it has been calculated before and 
    #it is in the x list
    message("Using cached matrix")
    return(m)
  }
  
  #If the inverse matrix is not cached the inverse is calculated
  m<-solve(x$get(), ...)
  # The inverse is assigned to the x list
  x$setmatrix(m)
  m
}
