## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function defines 4 functions(set,get,setmatrix and getmatrix)
makeCacheMatrix <- function(x = matrix()) {
# setting the Matrix to NULL as a placeholder for a future value
  m<-NULL
  #defines a function to set the Matrix, x, to a new Matrix, y, 
  # and resets the inverse Matrix, m, to NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  #returns the matrix, x
  get<-function() x
  #sets the inverse matrix that is obtained from the solve to m
  setmatrix<-function(solve) m<<- solve
  #returns the inverse matrix(m)
  getmatrix<-function() m
  #returns the 'special vector' containing all of the functions
  #just defined.
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## Write a short comment describing this function
# This function returns m if the inverse matrix variable(m) is not null
# if it is null then 
# verse matrix variable(m) is not null then finds the inverse matrix for the matrix x and set that
# in m
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #assign to m in cacheinverse Matrix, the value from 
  # getmatrix(m) that is NULL.
  m<-x$getmatrix()
  #If the inverse matrix stored under the parameters 
  #is not NULL, return it.
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  #If the mean is not stored then:
  #assign to matrix the original Matrix(x)
  matrix<-x$get()
  #Calculate the inverse matrix by calling solve on original matrix(x)
  m<-solve(matrix, ...)
  #set the inverse matrix obtained under the parameters
  #of original matrix(x)
  x$setmatrix(m)
  #Return Inverse matrix
  m 
}
