# Partimos de una nueva función

makeCacheMatrix <- function(x= matrix()){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function(){x}
        setInverse <- function(inverse){inv <<- inverse}   ## SetInverse es como se llama la función para la inversa 
        getInverse <- function(){inv}                      
        list(set=set,get=get, setInverse= setInverse, getInverse= getInverse) # Una lista para las entradas de la función
}


## Funcion CacheSolve
cacheSolve <- function(x, ...){
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")    ## Un mensaje cuando la inversa es Nula 
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)       ## Esto ayuda para sacar la inversa de la matriz.
        x$setInverse(inv)
        inv
}
