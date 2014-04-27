## makeCacheMatrix: This function takes an invertible matrix as its input 
## parameter, and creates a list of functions: setMatrix(), getMatrix(),
## setInverseMatrix(), and getInverseMatrix().
## 
makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the inverse matrix as null.
        m.inverse.matrix <- NULL
        ## Define the setMatrix function
        setMatrix <- function(y) {
                ## Cache the input and cache the null inverse matrix.
                x <<- y
                m.inverse.matrix <<- NULL
        }
        ## Define the getMatrix function, which returns the matrix input.
        getMatrix <- function() x
        ## Define the setInverseMatrix function, which caches inverse.
        setInverseMatrix <- function(solve) m.inverse.matrix <<- solve
        ## Define the getInverseMatrix function, which returns the inverse of 
        ## the matrix input.
        getInverseMatrix <- function() m.inverse.matrix
        ## Create the output list of functions.
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## cacheSolve: This function takes as its input a list object created by the  
## makeCacheMatrix function, and returns the inverse of makeCacheMatrix's input
## matrix.  
##
## If a cached version of the inverse already exists, it returns that cached 
## version.
##
## Otherwise, it computes the inverse using the solve() function, caches that 
## inverse using makeCacheMatrix's setInverseMatrix function.
##
## If a non-invertible matrix is submitted as the input, the error is not 
## handled.
## 
cacheSolve <- function(x, ...) {
        ## Set a local inverse equal to the inverse matrix cached in the 
        ## makeCacheMatrix function.
        m.inverse.matrix <- x$getInverseMatrix()
        ## If the local inverse is not null, return the cached value.
        if(!is.null(m.inverse.matrix)) {
                return(m.inverse.matrix)
        }
        ## Otherwise, get the input's matrix, solve for its inverse, and use 
        ## input object to cache that inverse.
        data <- x$getMatrix()
        m.inverse.matrix <- solve(data)
        x$setInverseMatrix(m.inverse.matrix)
}
