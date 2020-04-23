
#Creates the matrix used to make the inverse which has a list in which the value of the matrix is set, 
#the value of the matrix is got, the inverted matrix is set and the inverted matrix is got
makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL
    set <- function(y) {
        x <<- y #Assign a value to x in an environment that is different from the current environment. 
        inverted <<- NULL
    }
    get <- function() x
    setinverted <- function(inverse) inverted <<- inverse
    getinverted <- function() x
    list(set = set, get = get, setinverted = setinverted, getinverted = getinverted)
} 

#cacheSolve <- function(){} #Computes the inverse of the special matrix returned by makeCacheMatrix above
#If the inverse has already been calculated the cachesolve should retrieve the inverse from the cache
#Firslty it checks if the inverse has already been calculated, and if not it calculates it
cacheSolve <- function(x, ...){
    inverted <- x$getinverted()
    if(!is.null(inverted)) {
        message ("getting cached data")
        return(inverted)
    }
    data <- x$get()
    inverted <- solve(data, ...)
    x$setinverted(inverted)
    inverted #Print the inverted matrix
}
