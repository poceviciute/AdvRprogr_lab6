#' @title Greedy Heuristic
#' @name greedy_knapsack
#' @param x A data frame with numeric positive values.
#' @param W A positive numeric scalar.
#' @return A list approximate of optimal value, weight and selected objects.
#' @description Gives you an approxiamtion of the optimal solution with the Knapsack problem computed by greedy heuristic
#' Further information about the knapsack problem \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#' @references \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#' @export

# Greedy Heuristic --------------------------------------------------------

greedy_knapsack <- function(x, W){
    stopifnot(is.data.frame(x),
              apply(x, c(1, 2), is.numeric),
              is.numeric(W))
    
    stopifnot(x > 0,
              length(W) == 1,
              W > 0)
    
    x <- x %>%
        mutate(element = seq_along((x[[1]])),
               density = v/w) %>% 
        arrange(desc(density))
    
    # Start values
    sum_weight <- 0
    sum_value <- 0
    weight_check <- 0
    selected_elements <- vector()
    i <- 1
    
    while (weight_check <= W) {
        sum_weight <- sum_weight + x[i, "w"]
        sum_value <- sum_value + x[i, "v"]
        
        selected_elements[i] <- x[i, "element"]
        
        weight_check <- sum_weight + x[i + 1, "w"]
        i <- i + 1
    }
    
    return(list(value = round(sum_value, 0),
                #sum_weight = sum_weight,
                elements = selected_elements))
}

greedy_knapsack(x = knapsack_objects[1:800, ], W = 3500)
#system.time(greedy_knapsack(x = knapsack_objects[1:1200, ], W = 2000))

