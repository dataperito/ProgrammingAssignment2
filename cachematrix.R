## Instruction with example:
## Open the file "cachematrix.R";
## Compile the "makeCacheMatrix" and "cacheSolve" in the R (>);
## > m <- makeCacheMatrix(matrix(c(1, 3, 5, 7), c(2, 2)))
## > matrix(c(1, 3, 5, 7), c(2, 2))
## > cacheSolve(m)
## ------------------------------------
makeCacheMatrix <- function(x = matrix()) { 
n <- NULL
set <- function(y) { 
x <<- y 
n <<- NULL
} 
get <- function() x 
setinverse <- function(inv) n <<- inv 
getinverse <- function() n 
list(
set = set,
get = get, 
setinverse = setinverse,
getinverse = getinverse 
     )
 }


cacheSolve <- function(x, ...) { 
     n <- x$getinverse() 
     if(!is.null(n)) { 
         message("getting cached data") 
         return(n) 
     } 
     m <- x$get() 
     n <- solve(m, ...) 
     x$setinverse(n) 
     n 
 }
