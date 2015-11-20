## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This method sets the matrix inverse and when required it get 
## the matrix inverse to/from cache
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y  
                m <<- NULL
        }
        get <- function() x
        setmatinv <- function(matinv) m<<- matinv
        getmatinv <- function() m
        list(set = set, get = get,
             setmatinv = setmatinv,
             getmatinv = getmatinv) 
}


## Write a short comment describing this function
## This method check whether inverse of matrix exists in cache
## If it is in cache it retreives and return from cache
## If it does not exists it calculates and set inverse in cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatinv()
        if(!is.null(m)){ 
                message("getting cached data")    
                return(m)                         
        }
        data <- x$get()                    
        m <- solve(data, ...)               
        x$setmatinv(m)     
}

##These are the test cases of above methods
matx <- matrix(runif(6,2,50),2,2)
matxinvcache <- makeCacheMatrix(test)
matxinv <- cacheSolve(matxinvcache)
matxinv <- cacheSolve(matxinvcache)
