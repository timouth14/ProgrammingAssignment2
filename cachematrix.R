## Create a matrix initialized as null
##set will restore no value for the function in case it was used before
##

## Store solve in cache to have inverse of the matrix

makeCacheMatrix <- function(x = matrix) {
    my_matrix <-NULL
    
     set <-function (y){ 
    y <<- x 
    my_matrix <<- NULL
     }
    
     get <- function () x
     
     setinverse<- function(inverse) my_matrix <<- inverse
     getinverse<- function() my_matrix
     
     list(set= set, get = get, setinverse= setinverse, getinverse= getinverse )
    
  
    

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
 
      my_matrix <- x$getinverse()
    if(!is.null(my_matrix)) {
      message("getting cached data")
      return(my_matrix)
    }
    data <- x$get()
    my_matrix <- solve(data, ...)
    x$setinverse(my_matrix)
    my_matrix
        ## Return a matrix that is the inverse of 'x'
}
