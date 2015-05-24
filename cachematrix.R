## These functions will cache any square, invertible matrix
##      and return them if already inverted
##      otherwise the matrix will be inverted and cached

## makeCacheMatrix inputs a square matrix
##      returns a list w/ functions to get/set matrix and
##      set/get the inverse matrix; list has inputs for cacheSolve

makeCacheMatrix <- function(x = numeric()) {
        
        inv <- NULL
        
        ## set matrix
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## get matrix
        get = function() x
        
        ## set inverse
        setInv = function(inv) inv <<- inv
        
        ## get inverse
        getInv = function() inv
        
        ## list results
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

## cacheSolve inputs a matrix
##      returns inverse of matrix 'x' computed by makeCacheMatrix

cacheSolve <- function(x, ...) {
 
        inv = x$getInv()
        
        ## if inverse matrix exists
        if(!is.null(inv)) {
                ## get inverse matrix from cache
                message("getting cached data")
                return(inv)
        }
        ## else, compute inverse matrix w/ solve fn
        data = x$get()
        inv = solve(data, ...)
        
        ## set value of inverse matrix
        x$setInv(inv)
        
        ## return inv
        inv
}
