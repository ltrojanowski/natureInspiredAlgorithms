library("random")
library("foreach")

testfunction <- function()
{
  teststring <- "010101010101"
}

onemax <- function(bitstring){
  bitstring_split <- strsplit(bitstring, "")[[1]]
  sum <- 0
  for(bit in bitstring_split){
    if (bit == "1")
    {
      sum <- sum + 1
    }
  }
  return(sum)
}

random_bitstring <- function(num_bits)
{
  random_bitstring <- randomNumbers(num_bits, min = 0, max = 1, col = num_bits)  #not correct, need to define size as vec and join.
  return(paste(random_bitstring, collapse = ''))
}

point_mutation <- function(bitstring)
{
  bitstring_size <- size(bitstring_split <- strsplit(bitstring,"")[[1]])
  rate <- 1.0/bitstring_size
  randvec <- runif(bitstring_size, 0.0, 1.0)
  index <- 1
  for(i in seq_along(randvec)) {
    if (randvec[[i]] == 1)
    {
      if (bitsring_split[[i]] == "1")
        bitstring_split[[i]] = "0"
      else
        bitstring_split[[i]] = "1"
    }
  }
  bitstring <- paste(bitstring_split, collapse="")
  return(bitstring)
}

crossover <- function(parent1, parent2, rate)
{
  if (runif(1, 0.0, 1.0) >= rate)
  {
    return(parent1)
  } else {
    crossoverPoint = sample(1:length(parent1), 1)
    toReturn <- paste(parent1[1:crossoverPoint],parent2[crossoverPoint+1:length(parent2)],sep="")
    return toReturn    
  }
}

reproduce <- function(selected, pop_size, p_cross, p_mutation){
  children <- vector()
  lasindex <- length(selected)
  for (index in seq_along(selected))
  {
    p1 <- selected[index]
    (index %% 2 == 1) ? (p2 <- selected[index+1]) : (p2 <- selected[index - 1])
    if (index == length(selected) && index %% 2 == 1)
      p2 <- selected[index-1]
    child <- crossover(p1, p2, p_cross)
    child <- point_mutation(child, p_mutation)
    append(children, child)
  }
  return children
}

