## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


##  makeCacheMatrix creates a special "Matrix", which is really a list containing a function to

## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of matrix
## 4. get the inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
       inverse<-NULL
set<- function(y){
       x<<-y
       inverse<<-NULL
}
       get<- function() x
       setinverse<- function(inverse) inverse<<- inverse
       getinverse<- function() inverse
       list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
          inverse<-x$getinverse()
if(!is.null(inverse)){
          message("getting cached data")
          return(inverse)
}
          data<- x$get()
          inverse<-solve(data)
          x$setinverse(inverse)
          inverse         ## Return a matrix that is the inverse of 'x'
}
