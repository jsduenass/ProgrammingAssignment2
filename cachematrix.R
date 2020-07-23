#--------------------------------------
#           Solution
#--------------------------------------

## These functions are ment to cached the Inverse of a given matrix, 
## taking for granted that the input is a proper square invertible matrix
## no error handling has been taken into account  


## creates a cached matrix Object: a colletion of functions from which 
## the matrix and inverse values can be acces or set up 

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    set <- function(value) {
        x <<- value
        m <<- NULL
    }
    get <- function() x
    
    setInverse <- function(Inv_value) Inv <<- Inv_value
    getInverse <- function() Inv
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## takes as input a cached matrix Object,
## gets the value of the cached inverse, if the value has been calculated  
## it gets returned, if it hasn't been calculated the inverse is solved 
## and it's value stored inside the cached matrix Object

cacheSolve <- function(x, ...) {
    Inv <- x$getInverse()
    if(!is.null(Inv)) {
        message("inverse has alredy been calculated not need to do it again")
        message("here you go")
        return(Inv)
    }
    data <- x$get()
    message("first time calculating the inverse")
    message("it might take a while")
    
    Inv<- solve(data, ...)
    x$setInverse(Inv)
    message("here you go")
    
    Inv
}


#--------------------------------------
#           Example
#--------------------------------------

#M<- matrix(c(1,2,3,4),2)

#M_Obj <- makeCacheMatrix(M)

#ans<-cacheSolve(M_Obj)
#ans2<-cacheSolve(M_Obj)
