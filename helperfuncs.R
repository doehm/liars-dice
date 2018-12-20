
# set dice value
set.dice.value <- function(note, max.val, prev.val = 0){
  good.val <- FALSE
  while(!good.val){
    val <- readline(note) %>% as.numeric()
    if(val > 0 & val <= max.val & !is.na(val) & (val > prev.val)){
      good.val <- TRUE
    }else{
      cat("please select a value between 1 and", max.val, "\n")
    }
  }
  return(val)
}



# roll table
roll.table.fn <- function(rolls){
  rt <- table(unlist(rolls))
  roll.table <- rep(0, 6)
  names(roll.table) <- 1:6
  roll.table[names(rt)] <- rt
  return(roll.table)
}


# generate games states
generate.game.states <- function(nplayers, ndice, bin.size = 20){
  
  # create the basic data frame with total dice and player dice count
  total.dice <- nplayers*ndice
  states <- t(combn(rep(0:ndice, nplayers), nplayers)) %>% as.data.frame() %>% distinct()
  colnames(states) <- paste0("p", 1:nplayers)
  states$total <- rowSums(states)
  states <- states %>% dplyr::select(total, p1) %>% dplyr::arrange(total, p1) %>% dplyr::filter(total > 0) %>% distinct
  
  # add in the probability bucket state
  game.states <- data.frame()
  for(k in 1:nrow(states)){
    total <- states$total[k]
    p1 <- states$p1[k]
    for(j in 0:bin.size){
      game.states <- rbind(game.states, c(total, p1, j))
    }
  }
  colnames(game.states) <- c("total", "p1", "prob_cat")
  return(game.states)
}




# generate reward matrix
generate.reward.matrix <- function(game.states){
  reward <- matrix(0, nrow = nrow(game.states), ncol = nrow(game.states))
  
  for(i in 1:nrow(reward)){
    
    # which state are we in
    total <- game.states$total[i]
    p1 <- game.states$p1[i]
    
    # small penalty for losing a die
    reward[i, p1 - game.states$p1 == 1 & total - game.states$total == 1 & game.states$p1 != game.states$total] <- -1
    
    # small reward for others losing a die
    reward[i, p1 == game.states$p1 & total - game.states$total == 1 & game.states$p1 != game.states$total & p1 > 0] <- 1
    
    # fail states
    if(p1 == 1){
      reward[i, which(total - game.states$total == 1 & game.states$p1 == 0)] <- -10
    }
    
    # win states
    if(total - p1 == 1){
      reward[i, game.states$total == p1 & game.states$p1 == p1] <- 10
    }
  }
  return(reward)
}




# call probability that there is at least the bid quantity on the table
calc.prob <- function(x, bin.size = 20) {
  if(x[3] <= x[4]){
    return(1*bin.size)
  }else{
    n <- x[1]-x[2]
    k <- seq(min(x[3]-x[4], n), n, 1)
    return(floor(sum(choose(n, k)*(1/6)^k*(5/6)^(n-k))*bin.size))
  }
}



# update Q matrix
update.Q <- function(play, Q.mat, reward, alpha = 0.1, discount = 0.9){
  for(k in 2:nrow(play)){
    curr.state <- play$y.state[k]
    prev.state <- play$y.state[k-1]
    action <- play$y.action[k]
    
    # Q update
    Q.mat[prev.state, action] <- (1-alpha)*Q.mat[prev.state, action] +
      alpha*(reward[[action]][prev.state, curr.state] + discount*max(Q.mat[curr.state,]))
  }
  return(Q.mat)
}

