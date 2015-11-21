## Put comments here that give an overall description of what your
## functions do

## This function takes a mtrix as input and creates a "list" object  
##  which provides four methods to interact with the matrix:
##		- get : returns the matrix
##		- set : sets the matrix to a new one
##		- getInverse : returns the inverse of the matrix
##						if it has been computed yet
##		- setInverse : sets the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set<-function(y){
		x <<-y
		inv<<-NULL
	}
	get<-function() x
	getInverse<-function() inv
	setInverse<-function(i){ 
		message("Setting a new inverse matrix...")
		inv <<-i
	}
	list(get=get,set=set,
		getInverse=getInverse,setInverse=setInverse)
}


## This function computes and returns the inverse of a matrix
##	created with the 'makeCacheMatrix' function. It caches
##	the value, thus if you need to retrieve it several times,
##	the inverse matrix is computed only once.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(is.null(x$getInverse())){
        	x$setInverse(solve(x$get(),...))
        }
        x$getInverse()
}
