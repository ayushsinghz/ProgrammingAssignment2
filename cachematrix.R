makeCacheMatrix <- function(x_matrix = numeric()) {
        m_inverse <- NULL
        set <- function(y_matrix) {
                x_matrix <<- y_matrix
                m_inverse <<- NULL
        }

        get <- function() x_matrix
        setinverse <- function(inverse)	m_inverse <<- solve(inverse)
        getinverse <- function() m_inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

cacheSolve <- function(x_matrix, ...) {
        m_inverse <- x_matrix$getinverse()
        if(length(m_inverse)!=0) {
                message("getting cached data")
                return(m_inverse)
        }
        data <- x_matrix$get()
	m_inverse <- solve(data)
        x_matrix$setinverse(m_inverse)
        m_inverse
}

cacheSolve(makeCacheMatrix(m))
