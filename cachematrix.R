##Date:6/20/2015
###Function: makeCacheMatrix
####Description: creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {      
        inver <- NULL 
        set <- function(y) { 
                x <<- y 
                inver <<- NULL 
        } 
        get <- function() return(x) 
        seti <- function(mean) inver <<- mean 
        geti <- function() return(inver) 
        return(list(set = set, get = get, 
                    seti = seti, 
                    geti = geti)) 

}





##Date:6/20/2015
###Function: cacheSolve
####Description:  computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#####If the inverse has already been calculated (and the matrix has not changed), 
######then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$geti() 
        if(!is.null(inver)) { 
                message("Getting cached data") 
                return(inver) 
        } 
        dat <- x$get() 
        inver <- solve(dat, ...) 
        x$seti(inver) 
        return(inver)       
}



#a <- makeCacheMatrix( matrix(c(1:4), nrow = 2, ncol = 2) )
#summary(a)
#a$get()
#cacheSolve(a)
#cacheSolve(a) #get cached value after the 2nd time
