makeCacheMatrix <- function(x = matrix()) {
  ## @x: matriz que puede ser invertida
  
  
  inv = NULL
  set = function(y) {
    
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, 
       get=get, 
       setinv=setinv, 
       getinv=getinv)
  ## una lista que 
  ##              1. establece la matriz
  ##              2. obtiene la matriz
  ##              3. establece la inversa
  ##              4. obtiene la inversa
}

cacheSolve <- function(x, ...) {
  ## matriz inversa de 'x
  
  inv = x$getinv()
  if (!is.null(inv)){
    
    message("getting c. data")
    return(inv)
  }
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  
  x$setinv(inv)
  
  return(inv)
}

