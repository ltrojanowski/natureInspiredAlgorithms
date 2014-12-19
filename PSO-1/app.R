#PSO - Particle Swarm Optamisation
#library('ggplot2')


objective_function <- function(matrix) { #matrix stores points in columns
  basin_function <- function(vec){
    if(vec == 0) {
      return(0)
    } else{
      exp(-2.0/vec^2)
    }
    sum(sapply(vec, basin_function))
  }
  return(apply(matrix, 2, basin_function))
}

random_in_bounds <- function(minmax){
  return(minmax[1]+((minmax[2]-minmax[1])*runif(1, 0.0, 1.0)))
}

random_vector <- function(minmax){
  apply(minmax, 2, random_in_bounds)
}

create_particle <- function(search_space, vel_space){
  particle <-list(position=NA, cost=NA, b_position=NA, b_cost=NA, velocity=NA)
  particle$position = random_vector(search_space)
  particle$cost = objective_function(particle$position)
  particle$b_position = particle$position
  particle$b_cost = particle$cost
  particle$velocity = random_vector(vel_space)
  return(particle)
}

get_global_best <- function(population, current_best=NULL){
  
}
#function to test all component while writing
test <-function(){
  cat('\nplot objective function')
  vec <- seq(from=-5, to=5, by = 0.1)
  plot(vec, objective_function(vec), geom='line')
  cat('\ntest of random_vector')
  test_matrix <- matrix(1:20, ncol = 10, nrow = 2)
  rand_vec <- random_vector(test_matrix)
  cat('\nrand_vec:', rand_vec)
  cat('\nlength:', length(rand_vec))
  cat('\ntest create_particle')
  test_matrix <-matrix(1:10, ncol = 5, nrow = 2)
  ex_particle <- create_particle(test_matrix, test_matrix)
  cat('\nex_particle:\n')
  print(ex_particle)
}

test();