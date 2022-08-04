
## The functions below take advantage of the scoping rules of 
## the R language and observe how they can be manipulated to preserve 
## state inside of an R object. Specifically, they aid in calculating 
## (and storing) the value of the inverse of a given matrix such that 
## it does not have to be calculated every time it is retrieved, 
## saving time and increasing efficiency. 

## makeCacheMatrix below is a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix 
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      inv<-NULL
      set<- function(y) {
            x<<- y
            inv<-NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv<<- inverse
      getinverse <- function() inv
      list( set= set, get = get, setinverse = setinverse, 
            getinverse = getinverse)
}

## cacheSolve below checks to see if the inverse of the matrix 
## created with makeCacheMatrix above has already been calculated. 
## If it has, it gets the inverse from the cache. Otherwise, it calculates 
## the inverse of the data and sets the value of the inverse 
## in the cache via the setinverse function

cacheSolve <- function(x, ...) {
      inv<- x$getinverse()
      if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
      } 
      data<- x$get()
      inv<-solve(data, ...)
      x$setinverse(inv)
      inv
}

