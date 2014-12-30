
TSP_matrix <- function(filename, precision=0){
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
  perm <- sample(1:length(cities),1)
  index <- 2
  while(length(perm)!=length(cities)) {
    choices <- calculate_choices(cities, perm[length(perm)], perm, phero, c_heur, c_hist)
    next_city <- select_next_city(choices)
    perm[index] <- next_city
  }
  perm
}

decay_pheromone <- function(pheromone, decay_factor){
  apply(pheromone, c(1,2), function(P){(1.0-decay_factor)*P}) 
}

update_pheromone <- function(pheromone, solutions){
  update <- function(other){
    
  }
  lapply(solutions, update)
}

search <- function(cities, max_it,num_ants, decay_factor, c_heur, c_hist){
  
}

#test function to chceck components while writing
test <- function(){
  cat('\ntest of load data:\n')
  berlin52 <- TSP_matrix("berlin52.txt", 0)
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
}

test()