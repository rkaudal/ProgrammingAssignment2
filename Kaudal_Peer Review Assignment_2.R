## Put comments here that give an overall description of what your
## functions do
## The goal of this exercise is to write a pair of functions, 
## 1. makeCacheMatrix
## 2. cacheSolve which cache the 
## inverse of the matrix.

## Write a short comment describing this function
## makeCacheMatrix is a function which creates a special "matrix" object that 
## can cache its inverse for the input (which is the invertible square matrix)


makeCacheMatrix <- function(x = matrix()) {
       i <- NULL
       set <- function(y){
                x<<-y
                i<<-NULL
       }
       get <-function()x
       setinverse<- function(inverse) i<<- inverse
       getinverse<- function()i
       list(set=set, get=get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve is a function which computes the inverse of the special matrix 
## returned by the makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cacheSolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     i<-x$getinverse()
     if(!is.null(i)){
       message("getting cached data")
       return(i)
     }
     data<- x$get()
     i<- solve(data,...)
     x$setinverse(i)
     i
}
my_matrix <- makeCacheMatrix(matrix(3:6,2,2))
my_matrix$get()
my_matrix$getinverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getinverse()

        
