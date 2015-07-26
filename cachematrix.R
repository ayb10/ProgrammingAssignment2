## These functions allow you to cache the results of computations -- in this case
## the results of the solve() -- function so that it can be used again later.

## This creates an object that contains a vector, several functions, and a
## list naming and storing the functions. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL       ## Set m to null in the local environment.
        set <- function(y)  {  ##changes the vector stored in the main function
                x <<- y
                m <<- NULL  ## sets m to null in the containing environment
        }
        get <- function() x  ## This function returns matrix x that was set earlier in the function.
        setinverse <- function(solve) m <<- solve  ## stores the value of m
        getinverse <- function() m  ## returns the value of m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## this names and stores these functions to be called later.
}


## This function calculates the inverse of a matrix. But first it checks whether
## it has already been cached.  

cacheSolve <- function(x, ...) {
        m <- x$getinverse() ## Retrieves the value assigned to the m variable
        if(!is.null(m))  {  ## checks whether m is NOT null, if NOT null it 
                            ## returnsthe m variable.
                message("getting cached data")
                return(m)
        }
        data <- x$get()  ## If m is null, gets the matrix stored in the cache matrix function. 
                m <- solve(data, ...)  ## calcultaes the inverse of the matrix.
                x$setinverse(m) ## stores the inverse in object m created in 1ist function.
}
