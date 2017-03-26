## Put comments here that give an overall description of what your
## functions do



## Write a short comment describing this function

## Writing a function in order to make a cache list Matrixobject,
## incorporates functions to set /get value of  matrix  and also 
## set and get value of the inverse of Matrix. 

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse)inv<<-inverse
        getinverse<-function()inv
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

## in this function, inverse cache is resolved from the cached Matrix 
## as generated in function above. First check if inverse if already calculated, 
## else computation of the invere with setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
        if(!is.null(inv)){
                message("getting cached data.")
                return(inv)
                }
        data<-x$get()
        inv<-solve(data)
        x$setinv(inv)
        inv
        }
