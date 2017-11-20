## The program gets a matrix and looks  for stored(cached) value of its. 
## inverse. If the stored(cached) value exists it prints it. Otherwise
## it computes the inverse and stores(caches) the value for later use.
 

## This function get a matrix and computes its inverse. Then stores(caches) the 
## inverse for later use.  

makeCacheMatrix <- function(x = matrix()) {
              m <- NULL
              set <- function(y){
                x <<- y
                m <<- NULL
             }
              
          get <- function()                  #stores the matrix whose inverse is to be computed
                    x 
                 
          setinverse <- function(solve)     #computes the inverse and stores(caches) the value
                   m <<- solve(x)
                 
          getinverse <- function()          
                    m
                  
          
          list = list(set = set,
                  get = get,
                  setinverse = setinverse,
                  getinverse = getinverse )
}




## This function looks for an inverse of a matrix if stored(cached) 
##  if the value is not available, it calculates and stores it.  

cacheSolve <- function(x, ...) {
          m <- x$getinverse()                    #calls from the list created in makeCacheMatrix
          if(!is.null(m)){                       #check if inverse has been calculated
            message("getting cached data")
            return(m)                            #return inverse from cache if available
          }
          data1 <- x$get()
          m <- solve(data1, ...)                 #computes inverse if not stored
          x$setinverse(m)                        #caches(stores) the newly computed inverse
          return(m)                             #return newly computed inverse
        
}

