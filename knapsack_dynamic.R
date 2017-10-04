knapsack_dynamic <- function(x, W) {
    
    stopifnot(is.data.frame(x),
              apply(x, c(1,2), is.numeric))
    
    stopifnot(x > 0,
              length(W) == 1,
              is.numeric(W))
    
    # initiate variables
    n <- nrow(x)
    K <-
        matrix(rep.int(0, (n + 1) * (W + 1)), ncol = W + 1, nrow = n + 1)
    # save the initial order of the elements
    element_order <- order(x[, 1])
    # sort weights and values in ascending order
    wt <- x[order(x[, 1]), 1]
    val <- x[order(x[, 1]), 2]
    elements <- c()
    
    for (i in 1:(n + 1)) {
        # This solution produces matrix with [1,] and [,1] filled with 0s.
        # Hence, the row number is n+1 and column number is W+1
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
    
    
    # Trace back which elements were included
    # Initiate the variables
    j <- n + 1
    k <- W + 1
    i <- 1
    
    while (j >= 2 && k >= 1) {
        if (K[j, k] > K[j - 1, k]) {
            elements[i] <- element_order[j - 1]
            i <- i + 1
            k <- k - wt[j - 1]
        }
        # move one row up in the result matrix
        j <- j - 1
    }
    
    # Sort chosen items in ascending order
    items <- elements[order(elements)]
    result <-
        list(value = round(K[n + 1, W + 1], 0), elements = items)
    return(result)
}
