## Put comments here that give an overall description of what your
## functions do

## Function to create an identity matrix composed of 1's.
## The matrix crated here will be compared to the result of the
## multiplication betwen the matrix and its inverse

createIdentity <- function (m){

	## Create an empty matrix with the same size of one passed 
	## as parameter

	n_m <- matrix (,NCOL(m), NROW(m))

	## Fill the matrix in a loop.

	for (i in 1:NCOL(m))
	{
		for (j in 1:NROW(m))
		{

	## If the element is in the main diagonal, it is 1. Else is 0 

			if (i == j)
			{
				n_m[i,j] <- 1
			}

			else
			{
				n_m[i,j] <- 0
			}
		}
	}
	
	## Retun the matrix

	n_m
}

## Create an object containing a matrix and its inverse. Encapsulate the 
## object in a list containig the functions for get and set the matrix and
## its inverse

makeCacheMatrix <- function(x = matrix()) {

	## Declare variables 'mat' for the matrix and 'inv' for its inverse
	## 'mat' has the value of the matrix 'x' and 'inv' receives NULL

	mat <- x
	inv <- NULL

	## Set function sets the information of 'mat' changing its value

	set <- function(y) {
		mat <<- y
	}

	## Get function returns the matrix 'mat'

	get <- function() mat

	## setInverse sets the value of 'inv' changing its value

	setInverse <- function (inverse) 
	{
		inv <<- inverse
	}

	## getInverse returns the matrix 'inv'

	getInverse <- function () inv

	## Encapsulates the function in a list and returs it

	list (set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse
	)
}


## cacheSolve function receives an object, 'x' containig a matrix its inverse.
## If the inverse has not been calculated, or the matrix has changed,
## calculates the inverse and sets it to the object.

cacheSolve <- function(x, ...) {
	
	## Takes the inverse matrix from the objact 'x'

	m_inv <- x$getInverse ()

	## Tests if the inverse is NULL

	if (!is.null (m_inv))
	{

	## If it is not NULL, tests changes in the matrix by mutiplying
	## it by its inverse and comparing it to a identity matrix	

		inverseTest <- x$getInverse () %*% x$get()
		identity <- createIdentity (x$get())

	## If there is no changes in the matrix, return the inverse.
	## Else, continues the function 

		if (all.equal(inverseTest, identity) == TRUE)
		{
			print ("Inverse already calculated")
			return (m_inv)
		}
	}

	## Gets the matrix and calculates its inverse using the function 'solve'
	
	m <- x$get()

	m_inv <- solve (m)

	## Sets the inverse matrix.

	x$setInverse (m_inv)
}
