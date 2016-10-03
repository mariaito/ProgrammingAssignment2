## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { #This function creates a special "matrix" object that can cache its inverse
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {#This function computes the inverse of the special "matrix" returned by makeCacheMatrix, if the inverse has not been calculated. Otherwise, it retrieves the inverse from the cache.
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}


#validation
	##matrices definition
	#> one_matrix<-rbind(c(7,2,1),c(0,3,-1),c(-3,4,-2))
	#> two_matrix<-rbind(c(1,2,3),c(0,1,4),c(5,6,0))
	#> three_matrix<-rbind(c(1,3,1),c(1,1,2),c(2,3,4))

	##calling the first function (defined a, b, and c, respectively)
	#> a<-makeCacheMatrix(one_matrix)
	#> b<-makeCacheMatrix(two_matrix)
	#> c<-makeCacheMatrix(three_matrix)
	
	##calling the second function
	#> cacheSolve(a)
	#     [,1] [,2] [,3]
	#[1,]   -2    8   -5
	#[2,]    3  -11    7
	#[3,]    9  -34   21

	##comparing to the solve() output, in order to check if the inverse matrix is correct
	#> solve(one_matrix)
	#     [,1] [,2] [,3]
	#[1,]   -2    8   -5
	#[2,]    3  -11    7
	#[3,]    9  -34   21

	##checking if the "cache" message prints
	#> cacheSolve(a)
	#getting cached data
	#     [,1] [,2] [,3]
	#[1,]   -2    8   -5
	#[2,]    3  -11    7
	#[3,]    9  -34   21

	##same procedure, for matrices two and three
	#> cacheSolve(b)
	#     [,1] [,2] [,3]
	#[1,]  -24   18    5
	#[2,]   20  -15   -4
	#[3,]   -5    4    1
	
	#> solve(two_matrix)
	#     [,1] [,2] [,3]
	#[1,]  -24   18    5
	#[2,]   20  -15   -4
	#[3,]   -5    4    1
	
	#> cacheSolve(b)
	#getting cached data
	#     [,1] [,2] [,3]
	#[1,]  -24   18    5
	#[2,]   20  -15   -4
	#[3,]   -5    4    1
	
	#> cacheSolve(c)
	#     [,1] [,2] [,3]
	#[1,]    2    9   -5
	#[2,]    0   -2    1
	#[3,]   -1   -3    2
	
	#> solve(three_matrix)
	#     [,1] [,2] [,3]
	#[1,]    2    9   -5
	#[2,]    0   -2    1
	#[3,]   -1   -3    2
	
	#> cacheSolve(c)
	#getting cached data
	#     [,1] [,2] [,3]
	#[1,]    2    9   -5
	#[2,]    0   -2    1
	#[3,]   -1   -3    2
