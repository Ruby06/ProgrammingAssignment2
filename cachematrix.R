## Put comments here that give an overall description of what your
## functions do
## two main functions: 
## makeCacheMatrix: creates a special matrix object 
## that can cache its inverse 
## cacheSolve: computes the inverse of the matrix returned  
## by makeCacheMatrix. Retrieves the inverse from the cache  
## if it has been aleady calculated and the original matrix has not changed 


## Write a short comment describing this function
## Create a special "matrix", which is a list containing 
## a function to 
## set the value of the matrix 
## get the value of the matrix 
## set the value of the inverse matrix 
## get the value of the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
inver <- NULL 
   set <- function(y) { 
     x <<- y 
     inver <<- NULL 
   } 
   get <- function() x 
   setinver <- function(inverse) inver <<- inverse 
   getinver <- function() inver 
   list(set = set, get = get, 
        setinver = setinver, 
        getinver = getinver) 
}


## Write a short comment describing this function
## This function calculates the inverse of the matrix wrapper created with the makeCacheMatrix function. However, 
## it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getinver() 
   if(!is.null(inver)) { 
     message("getting cached data") 
     return(inver) 
   } 
   data <- x$get() 
   inver <- solve(data, ...) 
   x$setinver(inver) 
   inver 
}
