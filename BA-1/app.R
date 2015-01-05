# bees algorithm

library('reshape2')
library('ggplot2')

objective_function <- function(vec) { #matrix stores points in columns
  #cat('\nis objective_function argument a matrix',is.matrix(mat))
  basin_function <- function(vec){
    if(all(vec == 0)) {
      return(0)
    } else{
      return(sum(exp(-2.0/vec^2)+sin(vec*pi*2)))
    }
  }
  #return(sum(vec^2))
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

unpack_bees <- function(list, arg) {
  unpack <- function(li){
    
  }
  lapply(list, unpack)
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
  return(sorted_bees)# unoptimal only for graphics[[1]])
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
  pop_list <- list()
  neigh_ret <- list()
  for(iteration in 1:max_gens){
    pop <- lapply(pop, evaluate_bee)
    #print(pop[[1]])
    pop <- sort_bees(pop)
    if (is.null(best)||pop[[1]]$fitness <= best$fitness) {best <- pop[[1]]}# else {best}
    solutions[[iteration]] <- best
    best_vec[iteration] <- best$fitness
    next_gen <- list()
    neigh_list <- list()
    for(index in 1:num_sites){
      parent <- pop[[index]]
      neigh_size <- if(index<=elite_sites) {e_bees} else {o_bees}
      neigh_list[[index]] <- search_neigh(parent, neigh_size, patch_size, search_space)
      #next_gen[[index]] <- neigh_list[[1]]
      next_gen <- c(neigh_list[[index]],next_gen)
    }
    scouts <- create_scout_bees(search_space, (num_bees-num_sites))
    if(iteration == 1 || iteration == max_gens){
      if(iteration == 1) {
        pop_list[[1]] <- pop
        neigh_ret[[1]] <- neigh_list
      } else {
        pop_list[[2]] <- pop
        neigh_ret[[2]] <- neigh_list
      }
    }
    #print(next_gen)
    pop <- c(next_gen, scouts)
    patch_size <- patch_size * 0.98
    cat('\nit no.', iteration, 'patch size:', patch_size, 'best fitness:', best$fitness)
  }
  ret <- list(best=best, best_vec=best_vec, solutions=solutions, pop_list=pop_list, neigh_list=neigh_ret)
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
  #wisualisation
  x <- y <-seq(from=-5, to =5,  by=0.1)
  z <- outer(x,y, objective_function_wrapper)
  surf3d <- melt(z)
  names(surf3d) <- c("x", "y", "z")
  surf3d$x <- rep(x, ncol(z) ); surf3d$y <- rep(y, each=nrow(z))
  p1 <- ggplot(data=surf3d, aes(x=x, y=y, z=z))
  p1 <- p1 + geom_tile(aes(fill=z))+scale_fill_gradient(low="black", high="white")+stat_contour()
  print(p1)
  
  search_space <- matrix(c(-5, 5), ncol=2, nrow=2)
  max_gens <- 300
  num_bees <- 40
  num_sites <- 3
  elite_sites <- 1
  patch_size <- 4.0
  e_bees <- 7
  o_bees <- 2
  best <- search(max_gens, search_space, num_bees, num_sites, elite_sites,
                 patch_size, e_bees, o_bees)
  #cat('\nhurra! gotowe! najlepszy wynik to:\n')
  #print(best$solutions)
  dfplot <- data.frame(x = 1:max_gens, y=best$best_vec)
  plot <- ggplot(dfplot, aes(x=x, y=y))+geom_line(size=1)
  
  beginning_pop <- do.call(cbind, lapply(best$pop_list[[1]], '[[', "vector"))
  ending_pop <- do.call(cbind, lapply(best$pop_list[[2]], '[[', "vector"))
  beginning_neigh <- matrix(nrow=2, unlist(lapply(best$neigh_list[[1]], lapply, '[[', 1)))
  ending_neigh <- matrix(nrow=2, unlist(lapply(best$neigh_list[[2]], lapply, '[[', 1)))
  df2 <- data.frame(x=beginning_pop[1,], y=beginning_pop[2,], z=2)
  df3 <-data.frame(x=ending_pop[1, ], y=ending_pop[2, ], z=2)
  df4 <- data.frame(x=beginning_neigh[1, ], y=beginning_neigh[2, ], z=2)
  #print(ending_neigh)
  df5 <- data.frame(x=ending_neigh[1, ], y=ending_neigh[2, ], z=2)
  print(p1 + #geom_point(data = df, size=3, color = "magenta")+
          geom_point(data = df2, size=3, color = "green")+
          geom_point(data = df3, size=3, color = "red")+
          geom_point(data = df4, size=3, color = "blue")+
          geom_point(data = df5, size=3, color = "magenta"))
  print(plot)
  print(best$best)
}


#test()
run()