## This code uses scoping rules to cache values while producing the 
## inverse of an invertable matrix. This process is closely based off of the 
## mean vector demo.

## As with the demo, this function sets & gets the matrix, then sets & gets the 
## value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    nvrs <- NULL
    set <- function(y){
        x <<- y
        nvrs <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) nvrs <<- inverse
    getinverse <- function() nvrs
    list(set = set, 
         get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function checks to see if the inverse has already been set, and 
## retrieves it if so (with a message). If not, it runs a solve function to 
## create the inverse and sets it.

cacheSolve <- function(x, ...) {
        nvrs <- x$getinverse()
        if(!is.null(nvrs)) {
            message("one sec, I'm fetching your cached inverse data!")
            return(nvrs)
        }
        data <- x$get()
        nvrs <- solve(data,...)
        x$setinverse(nvrs)
        nvrs
}

