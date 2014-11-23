## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## cacheSolve function uses the solve built in function to return the inverse
## matrix of x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	## Calculate the inverse of x usin solve(x) function
	inverse_x <- solve (x)

	##retuns the inverse of x
	inverse_x
}
