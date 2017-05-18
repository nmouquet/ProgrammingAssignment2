## Coursera Programming Assignment 2
# May 18th, 2017, Nicolas Mouquet, Montpellier
# 
# Calculate the inverse of a matrix in two steps : 
#      - Creates a special "matrix" object that can cache its inverse (makeCacheMatrix)
#      - Computes the inverse of the special "matrix" returned by makeCacheMatrix
#        If the inverse has already been calculated (and the matrix has not changed), 
#        then the cachesolve should retrieve the inverse from the cache
##


##Function makeCacheMatrix
#This function create a list containing functions to
#       -set the value of the matrix (x) 
#       -get the value of the matrix
#       -set the value of the inverse of the matrix (inv_mat)
#       -get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv_mat <<- solve
        getinverse <- function() inv_mat
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##Function makeCacheMatrix
#Computes the inverse of the "matrix" returned by makeCacheMatrix
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        inv_mat <- x$getinverse()
        if(!is.null(inv_mat)) {
                message("getting cached data")
                return(inv_mat)
        }
        data <- x$get()
        inv_mat <- solve(data)
        x$setinverse(inv_mat)
        inv_mat
}

##Testing the function 
        #generate a random 5*5 matrix 
                mattest <- matrix(runif(5^2),5) 
                mattest
        #compute the inverse 
                cacheSolve(makeCacheMatrix(mattest)) 
        #compute the inverse of the result to check if we find the initial matrix back 
                matinvtest <- cacheSolve(makeCacheMatrix(mattest))
                cacheSolve(makeCacheMatrix(matinvtest))






