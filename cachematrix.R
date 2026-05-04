## This function creates a custom object that bundles a matrix together with its 
##own private storage space (a cache).

##Its job can be summarized in three main points:
        
##Data Encapsulation: It stores two pieces of information: the matrix itself and 
##a placeholder for its inverse. These aren't stored in the main workspace, but 
##inside a "hidden" environment unique to that specific matrix.

#State Management: It provides a controlled way to change the matrix. If you use 
##the set function to update the data, it is smart enough to automatically
##"clear" the old inverse, ensuring you never accidentally use an outdated 
##calculation for a new matrix.

##Interface Creation: It returns a list of functions (get, set, getInverse, 
##setInverse) that act as the only "tools" allowed to interact with that hidden 
##data.

## This defines the function. It takes one argument, x, which defaults to an empty matrix if nothing is provided.
makeCacheMatrix <- function(x = matrix()) {
        # 'inv' will store the cached inverse, initialized to NULL
        inv <- NULL
        
        # 1. Set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL # If the matrix changes, clear the cache
        }
        
        # 2. Get the value of the matrix
        get <- function() x
        
        # 3. Set the value of the inverse
        setInverse <- function(inverse) inv <<- inverse
        
        # 4. Get the value of the inverse
        getInverse <- function() inv
        
        # Return a list of the internal functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


##While makeCacheMatrix sets up the "storage unit," cacheSolve is the worker 
##that actually does the heavy lifting. Its job is to manage the logic: 
##"Do I need to calculate this, or do I already have it?"

##This what it does:
        
##1. The Check
##It starts by calling x$getInverse(). It’s essentially asking the matrix object,
##"Do you already have a saved version of your inverse?"

##2. The Shortcut (If Cache Exists)
##If the result is not NULL, it means the inverse has already been calculated. 
##cacheSolve skips all the math, prints a message saying "getting cached data", 
##and immediately returns the stored matrix. This is the "cost-saving" step.

##3. The Work (If Cache is Empty)
##If the result is NULL, it means this is either a brand-new matrix or the 
##matrix has changed since the last calculation. The function then:
        
##Uses x$get() to grab the original matrix data.

##Uses the solve() function in R to calculate the inverse.

##Uses x$setInverse() to push that result back into the cache for future use.

##4. The Return
##Finally, it returns the newly calculated inverse to the user.
cacheSolve <- function(x, ...) {
        # Check if the inverse is already cached
        inv <- x$getInverse()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # If not cached, get the matrix, calculate the inverse, and cache it
        data <- x$get()
        inv <- solve(data, ...) # solve(data) calculates the inverse in R
        x$setInverse(inv)
        
        inv
}

        
       