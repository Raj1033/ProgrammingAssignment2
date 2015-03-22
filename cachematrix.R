## Cache the inverse of a matrix 
## 

## Function to create a special vector

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	setmat<-function(y){
		x<<-y
		inv<<-NULL
		}
	getmat<-function(){
		x
		}
	setinv<-function(inverse){
		inv<<-inverse
		}
	getinv<-function(){
		inv
		}
	matrix(c(setmat=setmat,getmat=getmat,setinv=setinv,getinv=getinv),2,2)
}


## Function to calculate the inverse of a matrix and
## if already calculated, retrieve it from cache

cacheSolve <- function(p, ...) {
        m <- p$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- p$getmat()
        m <- solve(data)
        x$setinv(m)
        m
}
