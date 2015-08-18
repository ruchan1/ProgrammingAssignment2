## makeCacheMatrix creates a list containing a function to
## 1. set value of the matrix
## 2. get value of the matrix
## 3. set value of the inverse matrix
## 4. get value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y){ #set value of the matrix
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x #get value of the matrix
        setinverse <- function(inverse) inv <<- inverse #set value of thg inverse matrix
        getinverse <- function() inv #get value of the inverse matrix
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) #create the list
}

## cacheSolve function calculates the inverse of a matrix. It first checks if
## the inverse has already been calculated. If so, it gets the result from the cache
## and skips the calculation. 
## Otherwise, it calculates the inverse, and sets the value in the cache via 
## setinverse function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
       
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

### SAMPLE RUN
# > x = rbind(c(3, -1), c(-3, 2))
# > mat = makeCacheMatrix(x)
# > mat$get()
#      [,1] [,2]
# [1,]    3   -1
# [2,]   -3    2
#
# no cache value in the first run
# > cacheSolve(mat)
#          [,1]      [,2]
# [1,] 0.6666667 0.3333333
# [2,] 1.0000000 1.0000000
#
# 2nd run, retrieving value from cache
# > cacheSolve(mat)
# getting cached data
#           [,1]      [,2]
# [1,] 0.6666667 0.3333333
# [2,] 1.0000000 1.0000000
