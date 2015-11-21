## Efficient "Matrix Inversion" function.
## Result is stored in cache, so if the function is called more than once
## for the same matrix, the previously saved value is returned.


## Function makeCacheMatrix constructs a list object. Let's call it a
## special "matrix" object. This object is not a simple two dimensional array.
## It is actually a list containing four functions. They are used to :
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  ## create a matrix object x
  
  ## define the cache m
  m <- NULL
  
  
  ## Definition of 1st member of the list object that makeCacheMatrix constructs
  ## :set the value of the matrix
  set <- function(y) {
    x <<- y         ## assign the input matrix y to the variable x in the
    ## parent environment (environment of makeCacheMatrix function)  
    m <<- NULL      ## re-initialize m in the parent environment to null
  }
  
  ## Definition of 2nd member of the list object that makeCacheMatrix constructs
  ##  :get the value of the matrix
  
  get <- function() {
    x               ## return the value of variable x in the parent environment
  }
  
  ## Definition of 3rd member of the list object that makeCacheMatrix constructs
  ##  :set the value of the inverse matrix
  
  setinverse <- function(inverse) {
    m <<- inverse   ## set the cache m equal
		    ## to the inverse of the matrix x
  }
  
  ## Definition of 4th member of the list object that makeCacheMatrix constructs
  ##  :get the value of the inverse matrix
  
  getinverse <- function() {
    m               ## returns the cached inverse matrix
  }
  
  ## list object that makeCacheMatrix constructs
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}





## Caching the inverse of a matrix
## The following function calculates the inverse of the special "matrix" created
## with function makeCacheMatrix. However, it first checks to see if the inverse
## has already been calculated and stored in the cache (in the "matrix" object).
## If so, it 'get's the inverse from there and skips the computation.
## Otherwise, it calculates the matrix inverse and stores it in the cache
## via the 'setinverse' function.


cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  ## x is a special "matrix" object. As mentioned earlier this object is not
  ## just a two dimensional array, but a list object.
  
  ## Initially, m gets the value already stored in x$getinverse()
  ## If it is not null, it is returned as the output of cacheSolve function
  
  mm <- x$getinverse()
  if(!is.null(mm)) {
    message("getting cached data")
    return(mm)
  }
  
  ## Otherwise, a local variable named "data" is filled with the
  ## two dimensional array whose inverse we want to compute
  data <- x$get()
  
  ## and then the inversed matrix is calculated ...
  mm <- solve(data, ...)
  
  ## ...and stored
  x$setinverse(mm)
  
  mm
}


## In the above code there are two major pitfalls.
## Suppose A is an invertible 3x3 matrix 3x3, i.e
##          A<-matrix(c(9,2,3,4,5,6,7,8,9), ncol=3)
## Then let X be a "special" matrix (list object)
##          X<-makeCacheMatrix(A)
## If we calculate its inverse by calling :
##        cacheSolve(X)
## the inverse matrix of A is stored in list object X
## We can retrieve it by :
##        X$getinverse()
## The first pitfall is that if we set a new matrix in X
## with the exact same values that matrix A had:
##        B<-A
##        X$set()<-B
## then calling cacheSolve should have returned the inverse from the cache.
## Instead, it calculates it from the beginning.
## The code doesn't recognise that that the matrix to be inverted hasn't actually changed
##
## The second pitfall is that if after defining a "special" matrix object X
##          X<-makeCacheMatrix()
## we can set the contents of reverse matrix, even if we haven't even
## declare which is the matrix we want to invert.
##          X$setinverse(A)
## So, after we define the matrix we want to invert:
##          X$set(A)
## cacheSolve function doesn't calculate the correct result because
## the cache is already occupied with "garbage".
## There should be a way to prevent the usage of X$setinverse()
## outside of cacheSolve function!