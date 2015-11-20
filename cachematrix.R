## This is the second programming assignment in R Programming Language

## The below functions are to create a matrix, compute its inverse, 
## store it in cache and display the value.
## If the inverse is already calculated and stored, only display the value.

## Function to create a matrix and store its value in a variable 
## outside the scope of this function.
## This function also returns a list of 4 other functions used 
## to get and set values of matrix and to get and set values of inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
        ## Initializing local variable m to null
        m<-NULL
        ## Assigning the passed martix to x
        set<-function(y){
                x<<-y
                ## Assigning null to variable m outside the scope of this function.
                m<<-NULL 
        }
        ## Returns the matrix x
        get<-function() x
        ## Assigns the inverted matrix to matrix variable outside the scope of this function
        setmatrix<-function(solve) m<<- solve
        ## Returns the matrix stored in m
        getmatrix<-function() m
        ## Returns the list of function inside function makeCacheMatrix
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

## The function cacheSolve checks to see if the inverted matrix is already present.
## If it is not null, then display the inverted matrix.
## If null, then compute and display the inverted matrix.

cacheSolve <- function(x=matrix(), ...) {
        ## Gettingthe inverted matrix to check for null
        m<-x$getmatrix()
        ## If matrix is present, display and exit function
        if(!is.null(m)){ 
                message("getting cached data")
                return(m)
        }
        ## If matrix is not present,
        ## Retrieve the input matrix
        mat<-x$get()
        ## Compute inverse of matrix and assigns to local variable m
        m<-solve(mat, ...)
        ## Set Inverted matrix to original matrix variable(outside the scope of this function)
        x$setmatrix(m)
        ## returns and prints the inverted matrix.
        ## m is the local matrix here and x$getmatrix() 
        ## is the one stored outside the scope of this function. Both are same.
        m 
}