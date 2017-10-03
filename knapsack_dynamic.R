knapsack_dynamic <- function(x, W) {
    stopifnot(is.data.frame(x),
              #is.numeric(x),
              x > 0,
              length(W) == 1,
              is.numeric(W))
    
    # initiate variables
    n <- nrow(x)
    K <- matrix(rep.int(0, (n + 1) * (W + 1)), ncol = W + 1, nrow = n + 1)
    # save the initial order of the elements
    element_no <- order(x[, 1])
    # sort weights and values in ascending order
    wt <- x[order(x[, 1]), 1]
    val <- x[order(x[, 1]), 2]

        for (i in 1:(n + 1)) {
        # loop through each item
        K[i, 1] <- 0
        
        # w = 123
        # i = 3
        
        for (w in 1:(W + 1)) {
            if (i == 1 || w == 1) {
                K[i, w] <- 0
            }
            else if (wt[i - 1] < w - 1) {
                K[i, w] <- max(val[i - 1] + K[i - 1, w - wt[i - 1]],  K[i - 1, w])
            }
            else if (wt[i - 1] == w - 1) {
                K[i, w] <- max(val[i - 1] + 0,  K[i - 1, w])
            }
            else{
                K[i, w] <- K[i - 1, w]
            }
            
        }
    }
    
    return(round(K[n+1,W+1],0))
    #return(K)
}
