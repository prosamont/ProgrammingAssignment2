## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## La fonction makeCacheMatrix met en cache une matrice en paramètre
## et son inverse.

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)


}


## Write a short comment describing this function
## La fonction cacheSolve prend en paramètre le matrice de retour de la 
## fonction makeCacheMatrix. Et vérifier si l inverse de la matrice est en cache
## si oui, la valeur de l’inverse est récupérer en cache
## si non, l’inverse est calculée

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inverse <- x$getinverse()
        if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse

}
