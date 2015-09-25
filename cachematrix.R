##I must assume that the matrix supplied is always invertible (so, by definition, it is always a square one)

##This programme will create an special matrix which will be stored with his inverse (cache memory), therefore,
##it won't be necessary to recompute the inverse everytime we need it.


##makeCacheMatrix will store the matrix and its inverse (a closure)

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL 
        set<-function(y){ ##stores the matrix given by the user
                x<<-y ##avoids lexical scoping by setting the value of x in another environments
                m<<-NULL ##cleans inverse value everytime we change the matrix stored
        }
        
        get<-function()x ##return the matrix (doesn't requires any argument)
        
        setinverse<-function(solve) m<<-solve ##stores the inverse of the matrix (can be done manually or using cacheSolve)
        
        getinverse<-function() m ##return the inverse of the matrix
        
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse) ##it makes possible to access to the 4 functions
}



##cacheSolve will compute and store the inverse of the matrix if necessary
##cacheSolve function must be applied over 'special' matrices (ones created using makeCacheMatrix)

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) { ##it checks if the inverse has already been calculated
                message("getting cached data") 
                return(m) ##in that case, it returns the inverse previously stored and a message warning about it
        }
        data <- x$get()
        m <- solve(data, ...) ##computes the inverse of the 'special' matrix
        x$setinverse(m) ##stores the inverse in the list provided by makeCacheMatrix
        m
        ## Return a matrix that is the inverse of 'x'
}
