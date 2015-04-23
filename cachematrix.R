
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setsolve<-function(solve) q <<-solve
        getsolve<-function(q)
        list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}
cacheSolve <- function(x, ...) {
          q<-x$getsolve()
          if (!is.null(q)){
                  message("getting cached data")
                  return(m)
          }
          
          data<-x$get()
          q<-solve(data,...)
          x$setsolve(q)
          q
}
