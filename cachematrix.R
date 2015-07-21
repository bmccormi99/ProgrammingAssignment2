## Contents:  makeCacheMatrix() and cacheSolve()
## RPROG-030 Week 3, Programming Assignment 2
## 21-JUL-2015

## Usage - after the source() functions are loaded.
## the console example that follow can be used to demostrate the functions.
## make the matrix and store it as d, then use the special mymat$set() function to pass a matrix into cache
## the first time you use cacheSolve it will display the inverse matrix
## the second time if will be display "getting cached matrix", then the inverse matrix. 

## Console Example - the following can be used to test.  
## The lines below with one set of "##" can be executed in R, after removing the "##".  
## ## (2 sets of ## are comments)
## source("cacheMatrix.R")
## ## create a simple 3x3 matrix
## localmat = matrix(c(1,2,5,4,3,4,2,1,2),3,3)
## mymat <- makeCacheMatrix()
## mymat$set(localmat)
## ## solve the inverse the first time, no cache
## cacheSolve(mymat)
## #solve the inverse the second time, no cache message is displayed
## cacheSolve(mymat)
## ## end of console example

## Function:  makeCacheMatrix   
## This function creates a special "matrix" object that can cache its inverse. 
##	Creates the environment functions for creating a matrix, and caching it
## 
makeCacheMatrix <- function(x = matrix()) {
        ## initialize
	  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## create the special matrix with the cache functions set/get
        get <- function() x
        setmat <- function(solve) m <<- solve
        getmat <- function() m
        list(set = set, get = get,
             setmat = setmat,
             getmat = getmat)
}



## Function:  cacheSolve 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix(). 
## If the inverse has already been cached, then cacheSolve should retrieve the inverse matrix from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  ## determine if the matrix (x) is null or not (!null = in cache)
	  m <- x$get()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        ## get the matrix, solve the inverse, and return it (m)		
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmat(m)
        m
}



