## Put comments here that give an overall description of what your
## functions do

escalona_linha <- function (e, m, e1){
	e <- e - m*e1

	e
}


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	x_inv <- matrix (,NROW(x), NCOL(x))

	for (i in 1:NROW(x))
	{
		for (j in 1:NCOL(x))
		{
			if (i == j)
			{
				x_inv[i,j] <- 1
			}
			else
			{
				x_inv[i,j] <- 0
			}
		}
	}

	for (i in 2:NROW(x))
	{
		l <- lapply (x_inv[i,], escalona_linha, x[i,i-1]/x[i-1,i-1], x[i-1,i-1])
	
		for (j in 1:NCOL(x))
		{
			x_inv[i,j] <- l[j]
		}
	}
}
