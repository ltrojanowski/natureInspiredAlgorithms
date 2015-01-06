# genetic algorithm

library("random")
library("ggplot2")
library("reshape2")

onemax <- function(bitstring){
  sum(bitstring)
}

random_bitstring <- function(num_bits)
{
  rand_bitstring <- sample(c(0,1), num_bits, replace = TRUE, prob = NULL)
}

binary_tournament <- function(pop){
  indexes <- sample(1:ncol(pop), 2, replace = FALSE)
  #comment out the next two lines after testing
  #cat('\nbinary vec one: ', pop[,indexes[1]])
  #cat('\nbinary vec two: ', pop[,indexes[2]])
  if (onemax(pop[,indexes[1]])>=onemax(pop[,indexes[2]])) return(pop[,indexes[1]]) else return(pop[,indexes[2]])
}

point_mutation <- function(bitstring, rate=1.0/length(bitstring))
{
  bitstring_size <- length(bitstring)
  #rate <- 1.0/bitstring_size
  randvec <- runif(bitstring_size, 0.0, 1.0)
  bitstring <- ifelse(randvec < rate, ifelse(bitstring == 1, 0, 1), bitstring)
}

crossover <- function(parent1, parent2, rate)
{
  if (runif(1, 0.0, 1.0) > rate)
  {
    toReturn <- parent1
  } else {
    crossoverPoint = sample(2:length(parent1)-1, 1)
    toReturn <- c(c(parent1[1:crossoverPoint]),c(parent2[(crossoverPoint+1):length(parent2)]))   
  }
  toReturn
}

reproduce <- function(selected, pop_size, p_cross, p_mutation){
  children <- matrix(nrow = nrow(selected), ncol = pop_size)#nrow = nrow(selected), ncol = pop_size)
  indexes <- rep(1:ncol(selected), length=pop_size)
  counter <- 1
  for (index in indexes)
  {
    p1 <- selected[,index]
    if (index+1 > ncol(selected)) {
      p2 <- selected[,1]
    } else if(index %% 2 == 1){
      p2 <- selected[,index+1]
    } else {
      p2 <- selected[,index-1]
    }
    #if (is.na(p2) == TRUE)
    #  p2 <- selected[,1]
    child <- crossover(p1, p2, p_cross)
    child <- point_mutation(child, p_mutation)
    children[, counter] <- child
    counter <- counter + 1
  }
  return(children)
}


search <- function(max_genes, num_bits, pop_size, p_crossover, p_mutation){
  #initialization population
  population <- matrix(ncol=pop_size, nrow=num_bits)
  for (index in 1:pop_size){
    population[,index] <- random_bitstring(num_bits)
  }
  #compute fitness
  fitness <- vector(length=pop_size)
  for (index in 1:pop_size){
    fitness[index] <- onemax(population[,index])
  }
  #
  #sorted_indices <- vector(length=pop_size)
  sorted_indices <- unname(unlist(sort(fitness, index.return = TRUE)['ix']))
  best <- fitness[sorted_indices[1]]
  best_vector <- vector(length=max_genes)
  best_bitstring <- population[sorted_indices[1]]
  #cat('\ninitial best: ', best)
  selected <- matrix(ncol=pop_size, nrow=num_bits)
  for(gen in 1:max_genes){
    for(index in 1:pop_size){
      selected[,index] <- binary_tournament(population)
    }
    children <- reproduce(selected, pop_size, p_crossover, p_mutation)
    for (index in 1:pop_size){
      fitness[index] <- onemax(children[,index])
    }
    sorted_indices <- unname(unlist(sort(fitness, index.return = TRUE)['ix']))
    if (fitness[sorted_indices[1]] >= best){
      best <- fitness[sorted_indices[1]]
      #print(best)
      population <- children
      #cat('\ngen: ', gen, 'best: ', best, 'bitstring: ', population[,sorted_indices[1]])
    }
    #else {
    #  cat('\nbest: ', best, 'bitstring: ', selected[,1])
    #}
    best_vector[gen] <- best;
    #if (best == num_bits) break;
  }
  return(list(best=best, best_vector=best_vector))
}

test <- function() {
  # test of onemax expected result 10
  print('test of onemax, expected result vector of 10')
  test1 <- onemax(c(1,1,1,1,1,1,1,1,1,1))
  cat('value: ', test1)
  cat('\nclass: ', class(test1))
  # test of random_bitstring
  print('test of random_bitstring, expected vector of 10 random values ')
  test2 <- random_bitstring(10)
  cat('value: ', test2)
  cat('\nclass: ', class(test2))
  # test of binary_tournament
  #generate example matrix
  cat('\ntest of binary_tournament')
  number <- 20
  examplematrix <- matrix(nrow=10, ncol = number)
  for(index in 1:number) {
    examplematrix[,index] <- random_bitstring(10);
  }
  selected <- binary_tournament(examplematrix)
  cat('\nvalue: ', selected)
  cat('\nclass:', class(selected))
  #test of pointmutation
  cat('\ntest of point_mutation expect different in one prob one point of all ones')
  mutated_bitstring <- point_mutation(c(1,1,1,1,1,1,1,1,1,1))
  cat('\nvalue: ', mutated_bitstring)
  cat('\nclass: ', class(mutated_bitstring))
  #test crossover
  cat('\ntest of crossover')
  crossed_bitstring <- crossover(c(1,1,1,1,1,1,1,1,1,1), c(0,0,0,0,0,0,0,0,0,0), 1)
  cat('\nvalue: ', crossed_bitstring)
  cat('\nclass: ', class(crossed_bitstring))
  #test reproduce
  cat('\ntest of reproduce')
  test_pop<-matrix(ncol=15, nrow=10)
  for (i in 1:ncol(test_pop)){
    test_pop[,i] <- random_bitstring(nrow(test_pop))
  }
  cat('\n test population matrix:\n', test_pop)
  cat('\n class of test population:', class(test_pop), 'dim:', dim(test_pop))
  test_child_pop <- reproduce(test_pop, ncol(test_pop), 0.98, 0.1)
  cat('\nclass of test_child_pop', class(test_child_pop))
  cat("\n test child pop:\n", test_child_pop)
  cat('\n dimensions of test child pop: ', dim(test_child_pop))
}


run <- function(){
  #problem configuration
  num_bits <- 64
  # algorithm configuration 1
  max_genes <- 200
  pop_size <- 100
  p_crossover <- 0.0
  p_mutation <- 1.0#(1.0/num_bits)/16
  #execute the algorithm
  num_of_exp <- 8
  mean_runs <- matrix(ncol=num_of_exp, nrow=max_genes)
  for(wrap in 1:num_of_exp){
    p_cross <- p_crossover #* (0.95)^(wrap-1)
    p_mut <- p_mutation * (0.5)^(wrap-1)
    runs <-100
    mean_run <- matrix(ncol=runs, nrow=max_genes)
    mean_best <- c(length = runs)
    for(run in 1:runs){
      solution <- search(max_genes, num_bits, pop_size, p_cross, p_mut)
      mean_run[,run] <- solution$best_vector
      mean_best[run] <- solution$best
      #  cat('\n Run No. ', run, '| Solution: ', solution$best)
    }
    cat('\n')
    df <- data.frame(x=seq_along(mean_run[,1]),y=mean_run)
    df2 <- melt(data= df,id.vars = "x")
    p1 <- ggplot(data = df2, aes(x=x, y = value, colour = variable, alpha))+geom_line(size=1)+theme(legend.position="none")
    p1 <- p1 + xlab("iteracja algorytmu") + ylab("najlepszy wynik")
    print(p1)
    mean_run <- apply(mean_run, 1, mean)
    mean_best <- mean(mean_best)
    cat('\nexp',wrap ,'\n Mean solution', mean_best)
    p2 <- qplot(x = seq_along(mean_run), y = mean_run, xlab="iteracja algorytmu", ylab="uœredniony wynik", geom="line")+geom_line(size=1)
    print(p2)
    mean_runs[ , wrap] <- mean_run
  }
  df3 <- data.frame(x=seq_along(mean_runs[, 1]), y=mean_runs)
  df4 <- melt(data= df3, id.vars = "x")
  p3 <- ggplot(data = df4, aes(x=x, y = value, color=variable))+
    geom_line(size=1)
    
  p3 <- p3 + xlab("iteracja algorytmu") + ylab("uœredniony wynik")
  print(p3)
}

run();