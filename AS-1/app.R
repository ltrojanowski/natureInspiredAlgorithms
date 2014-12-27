

euc_2d <- function(c1,c2){
  sqrt((c1[1]-c2[1])^2+(c1[2]-c2[2])^2)
}

cost <- function(permutation, cities){
  distance <- 0
  for (index in seq_along(permutation)){
    c2 <- if (index == length(permutation)) {permutation[1]} else {permutation[index+1]}
    distance <- distance + euc_2d(cities[c1], cities[c2])
  }
  distance
}

random_permutation <- function(cities){
  perm <- sample(cities) #or seq_along(cities)
}

initialise_pheromone_matrix <- function(num_cities, naive_score){
  v <- num_cities/naive_score
  return(matrix(v, ncol=num_cities, nrow=num_cities))
}

calculate_choices <- function(cities, last_city, exclude, pheromone, c_heur, c_hist){
  choices <- list()
  for(index in seq_along(cities)){
    coord <- cities[index]
    if (is.element(index, exclude)) next
    prob$history <- pheromone[last_city, index]^c_hist
    
  }
}