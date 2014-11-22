## The functions below are used to create a special object that stores a matrix and caches it's inverse.  
## To save computing time the second function first checks the cache before computing the matrix inversion.

## Creates a special matrix object and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        m<- NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
        setsolve<-function(solve) m<<-solve
        getsolve<-function()m
        
        list(get=get,
             setsolve=setsolve,
             getsolve=getsolve)
}


## Computes the inverse of the matrix returned by makeCacheMatrix or retrieves the inverse from the cache if already computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setsolve(m)
}
