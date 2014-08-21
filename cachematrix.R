#The first function takes a matrix and creates a cachable list of elements that can be accesed with the second one.
#The first time the second function, cacheSolve, is used, it calculates the inverse of the matrix. In subsequent times,
# it only returns the already calculated inverse matrix, without recalculating it, to save time.
#The code is practically identical to that the instructors' one. I only added a new message.

makeCacheMatrix <- function(x = matrix()) {  	#the only argument is a matrix. 
                                                  #The function will really return a list that points to subfunctions
                                                  #inside it
  
  inv <- NULL	#inv is the variable that carries the interesting result, but also
                                                  #is a flag, reseted to NULL each time a NEW matrix is created
  
  set <- function(y) {	#all of this "set" subfunction...
    
    x <<- y	#...is included for coherence with the original code of the instructors, but
                                                  #can be ignored...
    
    inv <<- NULL	#...because it's never accessed from the cacheSolve function
    
  }
  
  get <- function() x	#this just returns the value of the original matrix
  
  setInverse <- function(inverse) inv <<- inverse #here the two ocurrences of the word 'inverse' could be substituted for
                                                  #anything (i.e. "Conan").'inverse' is only the name of the argument to the
                                                  #function, that superassigns this argument to inv
  
  
  getInverse <- function() inv				
  
  list(set = set, get = get,	#finally returns a list so that each 'method' or 'subfunction' can be 
                                                  #accessed via x$whatever from the function cacheSolve
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {                  #this is the second function, the one that calculates each new matrix only once.
  inv <- x$getInverse()                           #goes to makeCacheMatrix and gets the value of getInverse
  if(!is.null(inv)) {	#if "inv" is different from NULL, that is, if it's not the first time we've used the cacheSolve function...
    message("getting cached data")	#prints a message
    return(inv)	#returns the result that we already had calculated, without recalculating it
  }	#and if it's the first time, if inv is NULL, we end the if and simply continue...
  message("calculating the inverse")	#prints a message
  data <- x$get()	#if it's the first time, we have to calculate the inverse matrix. First we get the matrix from the list
  inv <- solve(data, ...)	#here is where the actual calculus is done
  x$setInverse(inv)	#this ends up assigning the result to the "inv" variable;inv is not NULL anymore, till we use makeMatrix again
  inv	#returns the final result
}
