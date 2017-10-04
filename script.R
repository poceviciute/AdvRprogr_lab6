set.seed(42)
m <- 2000
knapsack_objects <-
    data.frame(w = sample(1:4000, size = m, replace = TRUE),
               v = runif(n = m, 0, 10000))
#m <- nrow(knapsack_objects)
x<- knapsack_objects[1:8,]


W_max <- 3500
W <- 3500

brute_force_knapsack(knapsack_objects[1:12,],5000)
knapsack_dynamic(knapsack_objects[1:12,],-5000)


x <- data.frame(w = c(1,3,4,5,2),
                      v = c(1,4,5,7,4))

example2 <- data.frame(w = c(60,30,100,200,2,-2),
                       v = c(15,45,55,75,45,400))
brute_force_knapsack(example2,50)
knapsack_dynamic(example2,50)

example1 <- data.frame(w = c(60,30,100,200,2,"-2"),
                       v = c(15,45,55,75,45,400))
brute_force_knapsack(example1,50)
knapsack_dynamic(example1,50)

example3 <- data.frame(w = c(101,302,403,504,20),
                       v = c(1500,450,55,7511,4500))



x <- data.frame(w = c(101,302,403,504,20),
                v = c(1500,450,55,7511,4500))
W <- 200

brute_force_knapsack(example3,500)
knapsack_dynamic(example3,500)
result[6,201]
result2 <- result[,100:201]
View(result2)
