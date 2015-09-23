##  The following two functions store a matrix and cache its inverse.

##  The function 'makeCacheMatrix' stores a list of functions:
##      setMatrix, getMatrix, setInverse & getInverse.
##  The input to makeCacheMatrix is a matrix 'x'.

makeCacheMatrix <- function(x = matrix()) {
        ##  The value of 'Inverse_Matrix' is initialized as Null.
        Inverse_Matrix <- NULL
        
        ##  The function 'setMatrix' is only used in the event that the existing stored 
        ##  matrix 'x' is to be replaced with a newly input matrix 'y'.
        ##  When the existing matrix is replaced, the 'Inverse_Matrix' value is reset to NULL as
        ##  any existing cached 'Inverse_Matrix' value is no longer valid for the new matrix.
        setMatrix <- function(y) {
                x <<- y 
                Inverse_Matrix <<- NULL
        }
        
        ## The 'getMatrix' function returns the currently stored matrix 'x'.
        getMatrix <- function() x
        
        ## The 'setInverse' function caches the value of the inverse of a matrix as 'Inverse_Matrix'.
        setInverse <- function(Inverted) Inverse_Matrix <<- Inverted
        
        ## The getInverse function returns the cached value 'Inverse_matrix'.
        getInverse <- function() Inverse_Matrix 
        
        ## The four functions are stored in 'makeCacheMatrix' by the list() function.
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The input of the function 'cacheSolve' is the object where makeCacheMatrix
## is stored (ie 'a' where 'a <- makeCacheMatrix(x)').
## The function 'cacheSolve' calculates the inverse of a matrix passed by
## 'makeCacheMatrix', or if the inverse has already been calculated and the
##  matrix has not changed, retrieves the inverse cached by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Assign the value of 'getInverse' stored in makeCacheMatrix
        ## to 'Inverse_Matrix'
        Inverse_Matrix <- x$getInverse()
        
        ## Assess the value of 'Inverse_Matrix'.  If it IS NOT equal to NULL,
        ## a valid cached inverse exists for the current matrix.  Print a
        ## message and return the value of 'Inverse_Matrix' and stop.
        ## If 'Inverse_Matrix' IS equal to NULL, exit the if loop.
        if(!is.null(Inverse_Matrix)) {
                message("Retrieving cached matrix... ")
                return(Inverse_Matrix)
        }
        
        ## THE FOLLOWING LINES ARE EXECUTED ONLY IF 'Inverse_Matrix' IS EQUAL
        ## TO NULL
        ## Retrieve matrix cached by 'makeCacheMatrix', assign it to 'data'.
        data <- x$getMatrix()
        
        ## Use solve() to calculate the inverse matrix and assign it to 
        ## 'Inverse_Matrix'.
        Inverse_Matrix <- solve(data, ...)
        
        ## Set the inverse matrix cached in 'makeCacheMatrix' to be equal to
        ## 'Inverse_Matrix'.
        x$setInverse(Inverse_Matrix)
        
        ## Print a message, return the value of the new 'Inverse_Matrix',
        ##and stop.
        message("Calculating new inverse...")
        Inverse_Matrix
}
