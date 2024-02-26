## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

#makes a list of functions that operate on a matrix object

#matrix is initially set to be empty 

makeCacheMatrix<-function(x=matrix()) {

  inv<-NULL

  #to set the value of the matrix

  set<-function(y) {

    x<<-y

    inv<<-NULL

  }

  #to retrieve the value of the matrix

  get<-function() x

  #to set the inverse of the matrix

  setInverse<-function(inverse) inv<<-inverse

  #to retrieve the inverse of matrix

  getInverse<-function() inv

  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}

#create a cache matrix object

cache_matrix<-makeCacheMatrix()

#set the matrix

cache_matrix$set(matrix(c(1,2,3,4),nrow = 2))



#get the matrix 

print(cache_matrix$get())

# compute and set inverse

cache_matrix$setInverse(solve(cache_matrix$get()))

#get inverse

print(cache_matrix$getInverse())


#the cachesolve function takes special matrix object created by makeCacheMatrix 

#as input

cacheSolve<-function(x,...) {

  #check if inverse is already cached using the getinverse method 

  inverse<-x$getInverse()

  if(!is.null(inverse)){

    message("Inverse retrieved from cache")

    return(inverse)

  }

  #if not cached, compute inverse

  mat<-x$get()

  inv<-solve(mat,...)

  #cache the inverse 

  x$setInverse(inv)

  return(inv)
}


cache_matrix<-makeCacheMatrix()

cache_matrix$set(matrix(c(1,2,3,4), nrow = 2))

print(cache_matrix$get())

cache_matrix$setInverse(solve(cache_matrix$get()))

print(cache_matrix$getInverse())
