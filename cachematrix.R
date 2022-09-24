## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix consists of set,get,setinv,getinv
## library(MASS) is used to calculate inverse for squared as well as non-squared matrices

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL                         ## initializing inverse as NULL
  set<-function(y){
                    x<<-y
                    inv<<-NULL
                    }
  get<-function()x                  ## function to get matrix x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
                      inver<-ginv(x)
                      inver%*%x      ## function to obtain inverse of the matrix
                     }
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
}


## Write a short comment describing this function
## This is used to get the cache data

cacheSolve <- function(x, ...)  ## gets Cache data
  {
  inv<-x$getinv()
  if(!is.null(inv)){                 ## Checking if inverse is NULL
                    message("The output is cached data!")
                    return(inv)        ## returns inverse value
  }
  data<-x$get()
  inv<-solve(data,...)            ## Calculates the inverse value
  x$setinv(inv)
  inv   ## Return a matrix that is the inverse of 'x'
}
