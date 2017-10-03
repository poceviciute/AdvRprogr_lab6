
# Data --------------------------------------------------------------------

set.seed(42)
n <- 2000
knapsack_objects <-
    data.frame(
        w=sample(1:4000, size = n, replace = TRUE),
        v=runif(n = n, 0, 10000)
    )


# Greedy Heuristic --------------------------------------------------------
library(dplyr)

greedy_knapsack <- function(x = knapsack_objects, w){
    stopifnot(is.numeric(w))
    
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
    
    while (weight_check <= w) {
        sum_weight <- sum_weight + x[i, "w"]
        sum_value <- sum_value + x[i, "v"]
        
        selected_elements[i] <- x[i, "element"]
        
        weight_check <- sum_weight + x[i + 1, "w"]
        i <- i + 1
    }
    
    return(list(value = round(sum_value, 0),
                sum_weight = sum_weight,
                selected_elements = selected_elements))
}

# system.time(greedy_heuristic(x = knapsack_objects[1:800, ], w = 3500))
# system.time(greedy_heuristic(x = knapsack_objects[1:1200, ], w = 2000))

