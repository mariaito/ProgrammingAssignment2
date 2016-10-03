## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#The following function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) { 
    m <- NULL #assigns NULL to m
    set <- function(y) {
        x <<- y #assigns the input to the function to x, in the parent environment
        m <<- NULL #assigns NULL to m, in the parent environment
    }
    get <- function() x #returns x, from the parent environment
    setinv <- function(inv) m <<- inv #assigns inv to m, in the parent environment
    getinv <- function() m #getter for m
    list(set = set, get = get, #creates a list, and names all its elements
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {#This function computes the inverse of the special "matrix" returned by makeCacheMatrix, if the inverse has not been calculated. Otherwise, it retrieves the inverse from the cache.
    m <- x$getinv() #assigns getinv, from x in the former function, to m
    if(!is.null(m)) { #if m is not NULL, it means that the inverse has been calculated before. Therefore, the message getting cached data
        message("getting cached data")
        return(m)
    }
    data <- x$get() #receives x, from the previous function
    m <- solve(data, ...)#calculates the inverse matrix with the data variable, defined in the previous line
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
