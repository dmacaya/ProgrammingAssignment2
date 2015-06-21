## The two functions serve for interactively calculating and storing the 
## inverse of a matrix. The first one creates a list that contains 4 functions
## to view and manage an object containing a matrix and its inverse and,
## indirectly, this first function associates them. The second one calculates
## the inverse of the matrix and store it using the first function, or if the 
## inverse had been already calculated just give it (avoiding calculating it 
## again), through calling the first function.

## This function creates a list of four different functions. The second one
## (get) retrieves the matrix associated to the object that calls the function.
## The first one (set) changes the matrix associated to such object and set the 
## object that will store the inverse matrix ('i') to null. The fourth
## one (getinverse) gives the value of the inverse of the previous matrix if it
## has been previously calculated and stored (as object 'i'). The third one
## (setinverse) changes this last object. Additionally, the functions initially
## set object 'i' the value null. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function (y) {
                x <<- y
                i <<- NULL
        }
        get <- function () x
        setinverse <- function (solve) i <<- solve
        getinverse <- function () i
        list (set = set, get = get, 
              setinverse = setinverse, 
              getinverse = getinverse)

}

## This function retrieves the value of the inverse of the matrix stored 
## by makeCacheMatrix. First, it checks if the inverse has been already 
## calculated and stored in the list created by makeCacheMatrix. If so, it just
## returns the stored value. Else, it calculates the inverse, stores it in the 
## respective makeCacheMatrix list, and finally invokes it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
                message ("getting catched data")
                return (i)
        }
        data <- x$get()
        i <- solve (data, ...)
        x$setinverse (i)
        i
}