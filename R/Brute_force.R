powerset <- function(items) {
    len = length(items)
    l = vector(mode = "list", length = 2 ^ len)
    l[[1]] = numeric()
    counter = 1L
    for (x in 1L:length(items)) {
        for (subset in 1L:counter) {
            counter = counter + 1L
            l[[counter]] = c(l[[subset]], items[x])
        }
    }
    return(l)
}

brute_force_knapsack <- function(x, W, parallel = FALSE) {
    
    stopifnot(is.data.frame(x),
              apply(x, c(1, 2), is.numeric),
              is.numeric(W))
    
    stopifnot(x > 0,
              length(W) == 1,
              W > 0,
              is.logical(parallel)))

if(parallel = FALSE){
    # initiate variables
    n <- nrow(x)
    best_v <- 0
    chosen_items <- c()
    items <- 1:n
    sets <- powerset(items)
    
    # loop through all possible sets (ommit empty set)
    for (i in 2:length(sets)) {
        c_sets <- unlist(sets[i])
        set_w <- 0
        set_v <- 0
        j <- 1
        # loop through the elements in the set
        while (j <= length(c_sets) && set_w <= W) {
            row <- c_sets[j]
            set_w <- set_w + x[row, 1]
            set_v <- set_v + x[row, 2]
            j <- j + 1
        }
        
        # compare the value of this set to the previous best value
        if (set_v > best_v && set_w <= W) {
            best_v <- round(set_v, 0)
            chosen_items <- c_sets
        }
    }
    result <- list(value = round(best_v, 2), elements = chosen_items)
    return(result)
} else{
    
    
}
}
