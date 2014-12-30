library("ggplot2")

TSP_matrix <- function(filename){
  csv <- read.csv(filename, header=TRUE,sep=" ")
  node <- csv[["NODE"]]
  tsp_matrix <- matrix(nrow=2, ncol=length(node))
  for(index in node){
    tsp_matrix[1,index] <- csv[["COORD1"]][index]
    tsp_matrix[2,index] <- csv[["COORD2"]][index]
  }
  tsp_matrix
}

euc_2d <- function(c1,c2){
  sqrt((c1[1]-c2[1])^2+(c1[2]-c2[2])^2)
}

cost <- function(permutation, cities){
  distance <- 0
  for (index in seq_along(permutation)){
    c1 <- permutation[index]
    c2 <- if (index == length(permutation)) {permutation[1]} else {permutation[index+1]}
    distance <- distance + euc_2d(cities[,c1], cities[,c2])
  }
  distance
}

random_permutation <- function(cities){
  perm <- sample(seq_along(cities[1,]))
}

initialise_pheromone_matrix <- function(num_cities, naive_score){
  v <- num_cities/naive_score
  return(matrix(v, ncol=num_cities, nrow=num_cities))
}

calculate_choices <- function(cities, last_city, exclude, pheromone, c_heur, c_hist){
  choices <- list()
  skip_times <- 0
  for(index in seq_along(cities[1, ])){
    coord <- cities[, index]
    if (is.element(index, exclude)) {skip_times <- skip_times+1; next}
    prob <- list()
    prob$city <- index
    prob$history <- pheromone[last_city, index]^c_hist
    prob$distance <- euc_2d(cities[,last_city], coord)
    prob$heuristic <- (1.0/prob$distance)^c_heur
    prob$prob <- prob$history * prob$heuristic
    choices[[index-skip_times]] <- prob
  }
  choices
}

select_next_city <- function(choices){
  sum <- sum(unlist(lapply(choices, "[[", "prob")))
  if(sum==0) return(choices[[sample(1:length(choices),1)]]$city)
  v <- runif(1, 0.0, 1.0)
  for(choice in choices){
    v <- v - choice$prob/sum
    if (v <= 0.0) return(choice$city)
  }
  return(tail(choices, n=1))
}

stepwise_const <- function(cities, phero, c_heur, c_hist){
  perm <- sample(1:length(cities[1, ]),1)
  index <- 2
  while(length(perm)!=length(cities[1, ])) {
    choices <- calculate_choices(cities, perm[length(perm)], perm, phero, c_heur, c_hist)
    next_city <- select_next_city(choices)
    perm[index] <- next_city
    index <- index + 1
  }
  perm
}

decay_pheromone <- function(pheromone, decay_factor){
  apply(pheromone, c(1,2), function(P){(1.0-decay_factor)*P}) 
}

update_pheromone <- function(pheromone, solutions){
  mat <- pheromone
  for(other in solutions){
    for(index in seq_along(other$vector)){
      x <- other$vector[index]
      y <- if(index == length(other$vector)) {other$vector[1]} else {other$vector[index+1]}
      mat[x, y] <- pheromone[x, y] + (1.0/other$cost)
      mat[y, x] <- pheromone[x, y] + (1.0/other$cost)
    }
  }
  mat
}

search <- function(cities, max_it,num_ants, decay_factor, c_heur, c_hist){
  best <- list()
  best$vector <- random_permutation(cities)
  best$cost <- cost(best$vector, cities)
  pheromone <- initialise_pheromone_matrix(length(cities[1, ]), cities)
  best_vec <- vector(length=max_it)
  for(iter in 1:max_it){
    solutions <- list()
    for(ant in 1:num_ants){
      candidate <- list()
      candidate$vector <- stepwise_const(cities, pheromone, c_heur, c_hist)
      candidate$cost <- cost(candidate$vector, cities)
      best <- if(candidate$cost < best$cost) candidate else best
      solutions[[ant]] <- candidate
    }
    pheromone <- decay_pheromone(pheromone, decay_factor)
    pheromone <- update_pheromone(pheromone, solutions)
    cat('\niteration no.', iter, 'best:', best$cost)
    best_vec[iter] <- best$cost
  }
  return(list(cost_vector=best_vec, best=best))
}

#test function to chceck components while writing
test <- function(){
  cat('\ntest of load data:\n')
  berlin52 <- TSP_matrix("berlin52.txt")
  cat('class of berlin52 is', class(berlin52), '\n')
  print(berlin52)
  cat('\ntest of euc_2d\n')
  dist <- euc_2d(c(0, 0), c(3,4))
  cat('distance is ', dist)
  test_cities <- matrix(c(0,0, 1,0, 3,0, 3,2, 3,4), nrow=2)
  print(test_cities)
  cat('\ntest of random_permutation')
  test_permutation <- random_permutation(test_cities)
  cat('\npermutation vec is', test_permutation)
  cat('\ntest of cost')
  test_cost <- cost(test_permutation, test_cities)
  cat('\nvalue of cost is', test_cost)
  cat('\ntest of initialise_pheromone_matrix:\n')
  num_cities <- length(test_cities[1, ])
  test_pher_matrix <- initialise_pheromone_matrix(num_cities, test_cost)
  cat('\npher matrix:\n')
  print(test_pher_matrix)
  cat('\ntest of calculate_choices')
  perm <- sample(1:num_cities, 1)
  test_choices <- calculate_choices(test_cities, perm, perm, test_pher_matrix, 2, 0.1)
  cat('\ntest_choices:\n')
  print(test_choices)
  print(unlist(lapply(test_choices, "[[", "prob")))
  cat('\ntest select_next_city:\n')
  test_next <- select_next_city(test_choices)
  print(test_next)
  cat('\ntest stepwise_const:\n')
  test_step <- stepwise_const(test_cities, test_pher_matrix, 2.5, 0.1)
  print(test_step)
  cat('\ntest of decay pheromone\n')
  test_new_pher_mat <- decay_pheromone(test_pher_matrix, 0.5)
  print(test_new_pher_mat)
  cat('\ntest update pheromone\n')
  test_solution <- list()
  test_solution$vector <- test_step
  test_solution$cost <- cost(test_solution$vector, test_cities)
  test_solutions <- list(test_solution, test_solution)
  updated_pher_mat <- update_pheromone(test_new_pher_mat, test_solutions)
  cat('pher mat:\n')
  print(updated_pher_mat)
  cat('\nqplot()')
  print(qplot(x=1:5, y=1:5))
}

run <- function(){
  berlin52 <- TSP_matrix("berlin52.txt")
  max_it = 5
  num_ants = 30#ength(berlin52[1,])
  decay_factor = 0.6
  c_heur = 2.5
  c_hist = 1.0
  ret <- search(berlin52, max_it, num_ants, decay_factor, c_heur, c_hist)
  cat('\nHurra! Gotowe! najlepsze rozwiazanie:\n')
  print(ret$best)
  print(ret$cost_vector)
  print(qplot(x=seq_along(ret$cost_vector),y=ret$cost_vector, xlab="iteration", ylab="best result", geom="line"))
}

#test()

run()