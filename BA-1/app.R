objective_function <- function(vec) { #matrix stores points in columns
  #cat('\nis objective_function argument a matrix',is.matrix(mat))
  basin_function <- function(vec){
    if(all(vec == 0)) {
      return(0)
    } else{
      return(sum(exp(-2.0/vec^2)+sin(vec*pi*2)))
    }
  }
  #return(apply(mat, 2, basin_function))
  return(basin_function(vec))
}

objective_function_wrapper <- function(x_vec, y_vec) {
  vec <-rbind(x_vec,y_vec)
  return(apply(vec,2, objective_function))
}

random_in_bounds <- function(minmax){
  return(minmax[1]+((minmax[2]-minmax[1])*runif(1, 0.0, 1.0)))
}

random_vector <- function(minmax){
  apply(minmax, 2, random_in_bounds)
}

create_random_bee <- function(search_space){
  return(list(vector=random_vector(search_space)))
}

evaluate_bee <-function(bee){
  bee$fitness <- objective_function(bee$vector)
  bee
}

sort_bees <- function(bees){
  costs <- sapply(bees, "[[", "fitness")
  sorted_indecis <- order(costs)
  sorted_bees <- bees[sorted_indecis]
}

create_neigh_bee <- function(site, patch_size, search_space){
  vec <- vector(length=length(site))
  for(index in seq_along(site)){
    v <- site[index]
    v <- if(runif(1,0.0, 1.0) < 0.5) {v+runif(1, 0.0, 1.0)*patch_size} else {v-runif(1,0.0,1.0)*patch_size}
    v <- if(v < search_space[1, index]) {search_space[1, index]} else {v}
    v <- if(v > search_space[2, index]) {search_space[2, index]} else {v}
    vec[index] <- v
  }
  bee <-list()
  bee$vector <- vec
  return(bee)
}

search_neigh <- function(parent, neigh_size, patch_size, search_space){
  fill_bee <- function(unused, par_vec, pat_siz, sear_sp){
    create_neigh_bee(par_vec, pat_siz, sear_sp)
  }
  neigh <- lapply(1:neigh_size, fill_bee, parent$vector, patch_size, search_space)
  #neigh <- replicate(neigh_size, create_neigh_bee(parent$vecotr, patch_size, search_space), simplify=FALSE)
  neigh <- lapply(neigh, evaluate_bee)
  sorted_bees <- sort_bees(neigh)
  return(sorted_bees[[1]])
}


create_scout_bees <- function(search_space, num_scouts){
  #gen_bee <- function(unused, sear_spac){
  #  create_random_bee(sear_spac)
  #}
  #bees <- lapply(1:num_scouts, gen_bee, search_space)
  bees <- replicate(num_scouts, create_random_bee(search_space), simplify=FALSE)
}

search <- function(max_gens, search_space, num_bees, num_sites, elite_sites,
                   patch_size, e_bees, o_bees){
  best <- NULL
  pop <- replicate(num_bees, create_random_bee(search_space), simplify=FALSE)
  solutions <- list()
  best_vec <- vector(length=max_gens)
  for(iteration in 1:max_gens){
    pop <- lapply(pop, evaluate_bee)
    pop <- sort_bees(pop)
    best <- if (is.null(best)||pop[[1]]$fitness < best$fitness) pop[[1]] else best
    solutions[[iteration]] <- best
    best_vec[iteration] <- best$fitness
    next_gen <- list()
    for(index in seq_along(pop)){
      parent <- pop[[index]]
      neigh_size <- if(index<elite_sites) {e_bees} else {o_bees}
      next_gen[[index]] <- search_neigh(parent, neigh_size, patch_size, search_space)
    }
    scouts <- create_scout_bees(search_space, (num_bees-num_sites))
    pop <- c(next_gen, scouts)
    patch_size <- patch_size * 0.92
    cat('\nit no.', iteration, 'patch size:', patch_size, 'best fitness:', best$fitness)
  }
  ret <- list(best=best, best_vec=best_vec, solutions=solutions)
}

#test function to test elements alongside writing
test <- function(){
  # test of random vector
  cat('\ntest of random_vector')
  test_matrix <- matrix(1:20, ncol = 10, nrow = 2)
  rand_vec <- random_vector(test_matrix)
  cat('random_vector output:\n', rand_vec)
  cat('\ntest of create_random_bee\n')
  test_bee <- list()
  test_bee <- create_random_bee(test_matrix)
  print(test_bee)
  cat('\ntest create_neigh_bee\n')
  neigh_bee <- create_neigh_bee(rand_vec, 1, test_matrix)
  print(neigh_bee)
  cat('\ntest search_neigh')
  test_search_space <- matrix(c(-5,5), ncol=2, nrow=2)
  test_parent <- create_random_bee(test_search_space)
  best_bee_in_neigh <- search_neigh(test_parent, 10, 0.5, test_search_space)
  cat('\nbest bee in neigh:\n')
  print(best_bee_in_neigh)
  cat('\ntest create_scout_bees:\n')
  test_scout_bees <- create_scout_bees(test_search_space, 5)
  print(test_scout_bees)
}

run <- function(){
  search_space <- matrix(c(-5, 5), ncol=2, nrow=2)
  max_gens <- 50
  num_bees <- 45
  num_sites <- 3
  elite_sites <- 1
  patch_size <- 3.0
  e_bees <- 7
  o_bees <- 2
  best <- search(max_gens, search_space, num_bees, num_sites, elite_sites,
                 patch_size, e_bees, o_bees)
  cat('\nhurra! gotowe! najlepszy wynik to:\n')
  #print(best$solutions)
  df <- data.frame(x = 1:max_gens, y=best$best_vec)
  p1 <- ggplot(df, aes(x=x, y=y))+geom_line(size=1)
  print(p1)
  print(best$best)
}


#test()
run()