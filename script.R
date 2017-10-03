set.seed(42)
n <- 2000
knapsack_objects <-
    data.frame(w = sample(1:4000, size = n, replace = TRUE),
               v = runif(n = n, 0, 10000))
m <- nrow(knapsack_objects)

knapsack_brute_force <- function(x, W) {
    stopifnot(is.data.frame(x), length(W) == 1, is.numeric(W))
    #rep.int(2, n)
    n <- nrow(x)
    A <- c()
    best_v <- 0
    for (i in 1:2 ^ n) {
        j <- n
        temp_w <- 0
        temp_v <- 0
        while (A[j] != 0, j > 0) {
            A[j] <- 0
            j <- j - 1
            A[j] <- 1
            for (k in 1:n) {
                if (A[k] == 1) {
                    temp_w = temp_w + x[k, 1]
                    temp_v = temp_v = x[k, 2]
                    
                    
                    if (temp_v > best_v, temp_w <= W) {
                        best_v <- temp_v
                    }
                }
            }
        }
    }
    return(best_v, A)
}