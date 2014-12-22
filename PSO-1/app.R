#PSO - Particle Swarm Optamisation
#library('ggplot2')
library('random')

objective_function <- function(mat) { #matrix stores points in columns
  cat('\nis objective_function argument a matrix',is.matrix(mat))
  basin_function <- function(vec){
    if(all(vec == 0)) {
      return(0)
    } else{
      return(sum(exp(-2.0/vec^2)))
    }
    #sum(sapply(vec, basin_function))
  }
  return(apply(mat, 2, basin_function))
}

objective_function_wrapper <- function(x_vec, y_vec) {
  vec <-rbind(x_vec,y_vec)
  return(objective_function(vec))
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
  particle$cost = objective_function(as.matrix(particle$position))
  particle$b_position = particle$position
  particle$b_cost = particle$cost
  particle$velocity = random_vector(vel_space)
  return(particle)
}

get_global_best <- function(population, current_best=NULL){
  costs <- sapply(population, "[[", "cost")
  sorted_indecis <- order(costs)
  sorted_pop <- population[sorted_indecis]
  best <- sorted_pop[[1]]
  if (is.null(current_best) || best$cost <= current_best$cost){
    current_best$position <- best$position
    current_best$cost <- best$cost
  }
  return(current_best)
}

update_velocity <- function(particle, gbest, max_v, c1, c2){
  v1 <- c1 * runif(length(particle$velocity), 0.0, 1.0) * particle$b_position - particle$position
  v2 <- c2 * runif(length(particle$velocity), 0.0, 1.0) * gbest$position - particle$position
  particle$velocity <- particle$velocity + v1 + v2
  particle$velocity <- ifelse(particle$velocity > max_v, max_v, particle$velocity)
  particle$velocity <- ifelse(particle$velocity < -max_v, -max_v, particle$velocity)
  return(particle)
}

update_position <- function(part, bounds){
  part$position <- part$position + part$velocity
  part <- ifelse(part$position > bounds[2, ], bounds[2, ]-abs(part$position-bounds[2, ]); part$velocity <- -1*part$velocity; part, part)
  part <- ifelse(part$position < bounds[1, ], bounds[1, ]+abs(part$position-bounds[1, ]); part$velocity <- -1*part$velocity; part, part)
}
#function to test all components while writing
test <-function(){
  vec<-seq(from=-2, to=1.9, by=0.1)
  cat('\nlength', length(vec))
  mat<-rbind(vec,vec)
  cat('\nmatrix: \n', mat)
  cat('\n is matrix: ', is.matrix(mat))
  cat('\ndim of matrix: ', dim(mat))
  objective_function(mat)
  #plot(vec, objective_function(mat))
  x <- y <-seq(from=-1, to =1,  by=0.1)
  cat('\nlength of x:', length(x))
  z <- outer(x,y, objective_function_wrapper)
  cat('\nz matrix:', z)
  cat('\ndim of z:', dim(z))
  contour(z)
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
  cat('\ntest of get_best_particle')
  two_dim_test_matrix <- matrix(1:4,ncol = 2, nrow = 2)
  test_population <- list()
  for (i in 1:5){
    test_population[[i]] <- create_particle(two_dim_test_matrix, two_dim_test_matrix)
  }
  cat('\n test population:\n')
  print(test_population)
  cat('test get_global_best:\n')
  test_best_particle <- get_global_best(test_population)
  print(test_best_particle)
  cat('\ntest update_velocity')
  test_changed_velocity <- update_velocity(test_population[[1]], test_best_particle, 2, 2, 2)
  cat('\noryginal particle:\n')
  print(test_population[[1]])
  cat('\nchanged particle:\n')
  print(test_changed_velocity)
}

test();