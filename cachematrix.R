## These functions can cache the inverse of a matrix so we haven't got to

## compute it repeteadly 

## Creates a matrix object that can cache its reverse.


makeCacheMatrix <- function(x = matrix()){
        
        invrs<<- NULL
        
        set <- function(y){
                
                x<<-y
                
                invrs<<-NULL
        }
        
        get<- function()x
        
        setInv <- function(Inv)invrs<<-Inv

        
        getInv <- function()invrs
        
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
        
}

## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated, 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function (x, ...){
        
        invrs<- x$getInv()
        
        if(!is.null(invrs)){
                
                message("getting cached data")
                
                return(invrs)
        }
        
        data <- x$get()
        
        invrs<- solve(data, ...)
        
        x$setInv(invrs)
        
        invrs
}

