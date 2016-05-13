## The following function 'makeCacheMatrix' is basically used to cache the inverse
## of a matrix, since calculating inverse of a matrix can take a huge time.

## set -> set the value of the matrix 'x'
## get -> get the value of the matrix 'x'
## getInverse -> get the precomputed inverse of the set matrix
## setInverse -> set the inverse of a matrix for future use

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve calculates the inverse of a matrix as
## - check if the inverse has already been computed
## - if yes
##      - return the cached value
## - else
##      - calculate the inverse
##      - set the inverse in the cache
##      - return the computed value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        inverse <- x$getInverse()
        
        if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
        inverse
}

## Sample Run

## > x = matrix(1:4, 2, 2)
## > mat = makeCacheMatrix(x)
## > mat$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## First Run
## > cacheSolve(mat)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Second Run
## > cacheSolve(mat)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## >