## ----data, warning = FALSE, comment= NA, message=FALSE, eval = FALSE-----
#  library(dplyr)
#  library(parallel)
#  
#  set.seed(42)
#  n <- 2000
#  knapsack_objects <-
#      data.frame(
#          w=sample(1:4000, size = n, replace = TRUE),
#          v=runif(n = n, 0, 10000)
#      )

## ---- brute_force, echo = FALSE, eval = FALSE----------------------------
#  brute_force_knapsack <- function(x, W, parallel = FALSE) {
#      stopifnot(is.data.frame(x),
#                apply(x, c(1, 2), is.numeric),
#                is.numeric(W))
#  
#      stopifnot(x > 0,
#                length(W) == 1,
#                W > 0,
#                is.logical(parallel))
#  
#      powerset <- function(items) {
#          len = length(items)
#          l = vector(mode = "list", length = 2 ^ len)
#          l[[1]] = numeric()
#          counter = 1L
#          for (x in 1L:length(items)) {
#              for (subset in 1L:counter) {
#                  counter = counter + 1L
#                  l[[counter]] = c(l[[subset]], items[x])
#              }
#          }
#          return(l)
#      }
#  
#      if (parallel == FALSE) {
#          # initiate variables
#          n <- nrow(x)
#          best_v <- 0
#          chosen_items <- c()
#          items <- 1:n
#          sets <- powerset(items)
#  
#          # loop through all possible sets (ommit empty set)
#          for (i in 2:length(sets)) {
#              c_sets <- unlist(sets[i])
#              set_w <- 0
#              set_v <- 0
#              j <- 1
#              # loop through the elements in the set
#              while (j <= length(c_sets) && set_w <= W) {
#                  row <- c_sets[j]
#                  set_w <- set_w + x[row, 1]
#                  set_v <- set_v + x[row, 2]
#                  j <- j + 1
#              }
#  
#              # compare the value of this set to the previous best value
#              if (set_v > best_v && set_w <= W) {
#                  best_v <- round(set_v, 0)
#                  chosen_items <- c_sets
#              }
#          }
#          result <- list(value = round(best_v, 2), elements = chosen_items)
#          return(result)
#  
#      } else{
#  
#          cores <- parallel::detectCores()
#  
#  
#          selection <- unlist(mclapply(1:nrow(x), FUN = function(y) {
#                  combn(rownames(x), y, paste, collapse = " ")
#              }, mc.cores = cores
#          ))
#  
#          sum_w <- unlist(mclapply(1:nrow(x), FUN = function(y) {
#                  (combn(x[, "W"], y, sum))
#              }, mc.cores = cores
#              ))
#  
#          sum_v <- unlist(mclapply(1:nrow(x), FUN = function(y) {
#                  (combn(x[, "v"], y, sum))
#              }, mc.cores = cores
#          ))
#  
#          max_value <- max(sum_v[which(sum_w < W)])
#          max_weight <- max(sum_w[which(sum_w < W)])
#          elements <- selection[which(sum_v == max_value & sum_w <= W)]
#  
#          return(list(
#              value = round(max_value, 0),
#              weight = max_weight,
#              element = elements
#          ))
#  
#      }
#  }
#  

## ---- eval = FALSE-------------------------------------------------------
#  single_core <- system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500))
#  all_core <- system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500, parallel = TRUE))
#  
#  print(list(single_core = single_core,
#             all_core = all_core))

## ---- echo = FALSE, eval = FALSE-----------------------------------------
#  knapsack_dynamic <- function(x, W) {
#  
#      stopifnot(is.data.frame(x),
#                apply(x, c(1, 2), is.numeric),
#                is.numeric(W))
#  
#      stopifnot(x > 0,
#                length(W) == 1,
#                W > 0)
#  
#      # initiate variables
#      n <- nrow(x)
#      K <-
#          matrix(rep.int(0, (n + 1) * (W + 1)), ncol = W + 1, nrow = n + 1)
#      # save the initial order of the elements
#      element_order <- order(x[, 1])
#      # sort weights and values in ascending order
#      wt <- x[order(x[, 1]), 1]
#      val <- x[order(x[, 1]), 2]
#      elements <- c()
#  
#      for (i in 1:(n + 1)) {
#          # This solution produces matrix with [1,] and [,1] filled with 0s.
#          # Hence, the row number is n+1 and column number is W+1
#          for (w in 1:(W + 1)) {
#              if (i == 1 || w == 1) {
#                  K[i, w] <- 0
#              }
#              else if (wt[i - 1] < w - 1) {
#                  K[i, w] <- max(val[i - 1] + K[i - 1, w - wt[i - 1]],  K[i - 1, w])
#              }
#              else if (wt[i - 1] == w - 1) {
#                  K[i, w] <- max(val[i - 1] + 0,  K[i - 1, w])
#              }
#              else{
#                  K[i, w] <- K[i - 1, w]
#              }
#  
#          }
#      }
#  
#  
#      # Trace back which elements were included
#      # Initiate the variables
#      j <- n + 1
#      k <- W + 1
#      i <- 1
#  
#      while (j >= 2 && k >= 1) {
#          if (K[j, k] > K[j - 1, k]) {
#              elements[i] <- element_order[j - 1]
#              i <- i + 1
#              k <- k - wt[j - 1]
#          }
#          # move one row up in the result matrix
#          j <- j - 1
#      }
#  
#      # Sort chosen items in ascending order
#      items <- elements[order(elements)]
#      result <-
#          list(value = round(K[n + 1, W + 1], 0), elements = items)
#      return(result)
#  }

## ---- eval = FALSE-------------------------------------------------------
#  dynamic <- system.time(knapsack_dynamic(x = knapsack_objects[1:18,], W = 3500))
#  

## ---- greedy_knapsack, echo = FALSE, eval = FALSE------------------------
#  
#  greedy_knapsack <- function(x, W){
#      stopifnot(is.data.frame(x),
#                apply(x, c(1, 2), is.numeric),
#                is.numeric(W))
#  
#      stopifnot(x > 0,
#                length(W) == 1,
#                W > 0)
#  
#      x <- x %>%
#          mutate(element = seq_along((x[[1]])),
#                 density = v/w) %>%
#          arrange(desc(density))
#  
#      # Start values
#      sum_weight <- 0
#      sum_value <- 0
#      weight_check <- 0
#      selected_elements <- vector()
#      i <- 1
#  
#      while (weight_check <= W) {
#          sum_weight <- sum_weight + x[i, "w"]
#          sum_value <- sum_value + x[i, "v"]
#  
#          selected_elements[i] <- x[i, "element"]
#  
#          weight_check <- sum_weight + x[i + 1, "w"]
#          i <- i + 1
#      }
#  
#      return(list(value = round(sum_value, 0),
#                  #sum_weight = sum_weight,
#                  elements = selected_elements))
#  }

## ---- gh_time, eval = FALSE----------------------------------------------
#  n_gh <- 1000000
#  
#  # Option 1
#  system.time(greedy_knapsack(x = knapsack_objects[1:1000, ], W = 2000))
#  
#  # Option 2
#  start <- Sys.time()
#  greedy_knapsack(x = knapsack_objects[1:1000, ], W = 2000)
#  end <- Sys.time()
#  gh_time <- end-start
#  gh_time
#  
#  
#  
#  answer_gh <- gh_time * (n_gh/1000)
#  
#  print(answer_gh)

