# To run, first create a list for caching a matrix, using 'makeCacheMatrix' fucntion and passing it 
# invertible matrix.(remember the matrix should be invertible).
# Then, to compute and store the matrix, use the 'cacheSolve' function. In the first run,
# it will compute the matrix, and store it in the cacheMatrix list. It will also print 'Solving matrix'.
# In successuve runs, it will use the matrix already calculated in show the message 'Using Cache'.
# If you change the matrix, then the cache will be removed and the process will start again.

# This function will give us a list with can store a matrix and its inverse. We can get as well
# as set the matrix and the inverse using the functions in the list.
makeCacheMatrix <- function(matrix = matrix()) {
    inverse <- NULL
    getMatrix <- function() matrix
    setMatrix <- function(y) {
        matrix <<- y
        inverse <<- NULL
    }
    setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
    getInverse <- function() inverse
    list(getMatrix = getMatrix, setMatrix = setMatrix, setInverse = setInverse, getInverse = getInverse)
}

# This function takes the list we get from the above function and check if the inverse of the matrix is
# present. If the inverse is present, it fetches it and returns it without computing again. If not,
# it will compute the inverse, store it in the list, and return it.
cacheSolve <- function(cachedMatrix, ...) {
    inverse <- cachedMatrix$getInverse()
    if(!is.null(inverse)) {
        print('Using Cache')
        return(inverse)
    }
    print('Solving matrix')
    matrix <- cachedMatrix$getMatrix()
    inverse <- solve(matrix, ...)
    cachedMatrix$setInverse(inverse)
    inverse
}