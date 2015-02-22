## Functions makeCacheMatrix and cacheSolve created as part of Programming Assignment 2 for R Programming course in Coursera


##makeCacheMatrix is a funtion that takes a Invertible matrix as an input and returns 
##                 a list with 4 elements each of which inturn is function
## First element list  : setmatrix is a function which can be used to create a matrix directly
## Second element of list : getmattix function is used to get the values in the matrix and display it
## Thrid element of list : calinverse function is used to calculate the inverse of a given matrix 
## Fourth element of list : getinverse function is used to get the inverse of the given matric

makeCacheMatrix  <- function(mat = matrix()) {
        
        mat_inverse<- NULL
        mat_setvalues <- function(matvalue){
                mat<<-matvalue
                mat_inverse <<- NULL
        }
        mat_getvalues<- function() mat
        mat_calinverse <- function(calinverse) mat_inverse <<- calinverse
        mat_getinverse <- function () mat_inverse
        list(setmatrix=mat_setvalues,getmatrix=mat_getvalues,calinverse=mat_calinverse,getinverse=mat_getinverse)
        
}

## cacheSolve matrix retunrs the inverse of a matrix that has  been provided.
## If inverse of the given matrix is already available in Cache,the cacheSolve retrives the inverse from the cache else it calucates the inverse


cacheSolve <- function(mat,...) {
        
        mat_value <- mat$getinverse()
        
        if(is.null(mat_value)) {
                mat_value <- solve(mat$getmatrix()) ## solve function in R caluclates the inverse of a matrix
                mat$calinverse(mat_value)
                message ("Inverse of the matrix has been Calculated")
        }
        else {
                message("Inverse of the matrix has been retrived from Cache")
        }
        
        return(mat_value)
}
