#' @title Brute Force
#' @name brute_force_knapsack
#' @param x A data frame with numeric positive values.
#' @param W A positive numeric scalar.
#' @param parallel Set to TRUE computation to be run on all cores.
#' @return A list with optimal value, weight and selected objects.
#' @description Gives you the optimal solution with the Knapsack problem computed by brute force.
#' Further information about the knapsack problem \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#' @export



brute_force_knapsack <- function(x, W, parallel = FALSE) {
    stopifnot(is.data.frame(x),
              apply(x, c(1, 2), is.numeric),
              is.numeric(W))
    
    stopifnot(x > 0,
              length(W) == 1,
              W > 0,
              is.logical(parallel))
    
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
    
    if (parallel == FALSE) {
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
        
        cores <- parallel::detectCores()
      
        
        selection <- unlist(mclapply(1:nrow(x), FUN = function(y) {
                combn(rownames(x), y, paste, collapse = " ")
            }, mc.cores = cores
        ))
        
        sum_w <- unlist(mclapply(1:nrow(x), FUN = function(y) {
                (combn(x[, "w"], y, sum))
            }, mc.cores = cores
            ))
        
        sum_v <- unlist(mclapply(1:nrow(x), FUN = function(y) {
                (combn(x[, "v"], y, sum))
            }, mc.cores = cores
        ))
        
        max_value <- max(sum_v[which(sum_w < W)])
        max_weight <- max(sum_w[which(sum_w < W)])
        elements <- selection[which(sum_v == max_value & sum_w <= W)]
        
        return(list(
            value = round(max_value, 0),
            weight = max_weight,
            element = elements
        ))
        
    }
}

brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500, parallel = TRUE)
