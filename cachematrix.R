## 编写可以缓存逆矩阵的函数对

## 此函数用于创建可缓存逆矩阵的特殊“矩阵”对象

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) n <<- solve
        getsolve <- function() n
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)  
}


## 此函数用于计算上述makeCacheMatrix返回的特殊“矩阵”的逆矩阵。
##如果已经计算逆矩阵（且尚未更改矩阵），那么cachesolve将检索缓存中的逆矩阵

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        n <- x$getsolve()
        if(!is.null(n)) {
                message("getting cached data")
                return(n)
        }
        data <- x$get()
        n <- solve(data, ...)
        x$setsolve(n)
        n
}
#检验
## > x = rbind(c(1, 2), c(2, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##      [,1] [,2]
##[1,]    1    2
##[2,]    2    1
##逆矩阵没缓存之前
##> cacheSolve(m)
##           [,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333
##缓存之后
##> cacheSolve(m)
##getting cached data
##           [,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333
