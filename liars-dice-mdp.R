



# play a round of liars dice
liars.dice.round <- function(players, control, player.dice.count, agents, game.states, reward, Q.mat, a = 1, verbose = 1){
  
  # set array for recording results
  y.ctrl = c(); y.state = c(); y.action = c()
  
  # roll the dice for each player
  if(verbose > 0) cat("\n\n")
  rolls <- lapply(1:players, function(x) sort(sample(1:6, player.dice.count[[x]], replace = TRUE)))
  if(verbose > 1) lapply(rolls, function(x) cat("dice: ", x, "\n"))
  total.dice <- sum(unlist(player.dice.count))
  
  # set penalty
  penalty <- sapply(1:players, function(x) 0, simplify = FALSE)
  
  # print dice blocks
  if(verbose > 0) Dice(rolls[[1]])
  
  # set up roll table
  roll.table <- roll.table.fn(rolls)
  
  # initial bid
  if(verbose > 0) cat("place first bid\nPlayer", control, "has control\n")
  if(control == a){
    
    dice.value <- set.dice.value("dice value: ", 6)
    dice.quantity <- set.dice.value("quantity; ", sum(roll.table))
    
  }else{
    
    # agent plays
    p1.state <- which(game.states$total == total.dice & game.states$p1 == player.dice.count[[1]] & game.states$prob_cat == total.dice)
    pars <- list(dice = rolls[[control]], total.dice = total.dice, dice.value = NULL, dice.quantity = 0, p1.state = p1.state)
    agent.action <- agents[[control]](pars = pars, Q.mat = Q.mat)
    dice.value <- agent.action$dice.value
    dice.quantity <- agent.action$dice.quantity
    
  }
  
  
  # calculate probability cat and determine the game state
  # action set to raise because you can't call without an initial bid
  # this could be a 3rd action (initial bid) but it's not really necessary
  player.dice.qty <- table(rolls[[1]])[as.character(dice.value)]
  player.dice.qty <- ifelse(is.na(player.dice.qty), 0, player.dice.qty) %>% unname
  prob.cat <- calc.prob(c(total.dice, player.dice.count[[1]], dice.quantity, player.dice.qty))
  p1.state <- which(game.states$total == total.dice & game.states$p1 == player.dice.count[[1]] & game.states$prob_cat == prob.cat)
  p1.action <- "raise"
  
  # storing states for Q iteration
  y.ctrl = c(); y.state = c(); y.action = c()
  
  # moving control to the next player
  # storing the previous player since if the next player calls the previous player could lose a die
  prev <- control
  control <- control %% players + 1
  if(verbose > 0) cat("dice value ", dice.value, "; dice quantity ", dice.quantity, "\n")
  
  
  # loop through each player and continue until there is a winner and loser
  called <- FALSE
  while(!called){
    
    # check if the player with control is still in the game - if not skip
    if(player.dice.count[[control]] > 0){
      if(control == a){
        
        action <- readline("raise or call (r/c)? ")
        
      }else{
        
        # the agent makes a decision
        pars <- list(dice = rolls[[control]], total.dice = total.dice, dice.value = dice.value, dice.quantity = dice.quantity, p1.state = p1.state)
        agent.action <- agents[[control]](pars = pars, Q.mat = Q.mat)
        action <- agent.action$action
        
      }
      
      
      # storing states for reward iteration
      if(control == 1 & !is.null(agent.action$action)){
        player.dice.qty <- table(rolls[[1]])[as.character(dice.value)]
        player.dice.qty <- ifelse(is.na(player.dice.qty), 0, player.dice.qty) %>% unname
        
        p1.action <- agent.action$action
        prob.cat <- calc.prob(c(total.dice, player.dice.count[[1]], dice.quantity, player.dice.qty))
        p1.state <- which(game.states$total == total.dice & game.states$p1 == player.dice.count[[1]] & game.states$prob_cat == prob.cat)
      }
      
      
      # called
      if(action %in% c("call", "c")){
        
        if(verbose > 0) {
          cat("player", control, "called\nRoll table\n")
          print(roll.table)
        }
        
        # dice are reavealed
        
        # check if the quantity of dice value is less or more than the total in the pool
        # if more control loses otherwise control-1 win
        if(dice.quantity > roll.table[dice.value]){
          
          penalty[[prev]] <- penalty[[prev]] - 1
          if(verbose > 0) cat("player", prev, "lost a die\n")
          
        }else{
          
          penalty[[control]] <- penalty[[control]] - 1
          if(verbose > 0) cat("player", control, "lost a die\n")
          
        }
        
        # for Q iteration
        y.ctrl <- c(y.ctrl, control); y.state <- c(y.state, p1.state); y.action <- c(y.action, p1.action)
        
        # if called use the penalty array to change states
        prob.cat <- calc.prob(c(total.dice, player.dice.count[[1]], dice.quantity, player.dice.qty))
        p1.state <- which(game.states$total == total.dice-1 & game.states$p1 == player.dice.count[[1]]+penalty[[1]] & game.states$prob_cat == prob.cat)
        
        # break the loop
        called <- TRUE
        
      }else{
        
        if(verbose > 0) cat("player", control, "raised\n")
        
        if(control == a){
          
          # player sets next dice value
          dice.value <- set.dice.value("dice value: ", 6)
          dice.quantity <- set.dice.value("quantity; ", sum(roll.table))
          
        }else{
          
          dice.value <- agent.action$dice.value
          dice.quantity <- agent.action$dice.quantity
        }
        
        # p1 state after the raise
        prob.cat <- calc.prob(c(total.dice, player.dice.count[[1]], dice.quantity, player.dice.qty))
        p1.state <- which(game.states$total == total.dice & game.states$p1 == player.dice.count[[1]] & game.states$prob_cat == prob.cat)
        if(verbose > 0) cat("dice value", dice.value, "; dice quantity", dice.quantity, "\n")
      }
      
      # store info for Q update
      y.ctrl <- c(y.ctrl, control); y.state <- c(y.state, p1.state); y.action <- c(y.action, p1.action)
      
      # set the control player to now be the previous player
      prev <- control
    }
    
    # next player has control
    control <- control %% players + 1
  }
  
  # play results and return
  play <- data.frame(y.ctrl, y.state, y.action)
  return(list(penalty = penalty, play = play))
}








# play a full game of liars dice
play.liars.dice <- function(players = 4, num.dice = 6, auto = FALSE, verbose = 1, agents, Q.mat = NULL, train = FALSE, print.trans = FALSE){
  
  # begin!
  if(verbose > 0) liars.dice.title()
  
  # setting the number of dice each player has
  ndice <- sapply(rep(num.dice, players), function(x) x, simplify = FALSE)
  players.left <- sum(unlist(ndice) > 0)
  
  # setting game states matrix
  game.states <- generate.game.states(players, num.dice)
  
  # set up reward matrix
  reward <- generate.reward.matrix(game.states)
  reward <- list(raise = reward, call = reward)
  
  # set Q matrix if null
  if(is.null(Q.mat)) Q.mat <- matrix(0, nrow = nrow(reward$raise), ncol = length(reward), dimnames = list(c(), names(reward)))
  
  # while there is at least 2 left in the game
  # who has control
  ctrl <- sample(1:players, 1)
  play.df <- data.frame()
  while(players.left > 1){
    
    # play a round
    results <- liars.dice.round(
      players = players, 
      control = ctrl,
      player.dice.count = ndice, 
      game.states = game.states,
      reward = reward,
      Q.mat = Q.mat,
      agents = agents,
      a = as.numeric(!auto),
      verbose = verbose
    )
    
    # update how many dice the players are left with given the 
    # outcomes of the round
    for(k in seq_along(ndice)){
      ndice[[k]] <- ndice[[k]] + results$penalty[[k]]
      if(ndice[[k]] == 0 & results$penalty[[k]] == -1){
        if(verbose > 0) cat("player", k, "is out of the game\n")
      }
      
      # update who has control so they can start the bidding
      if(results$penalty[[k]] == -1){
        ctrl <- k
        while(ndice[[ctrl]] == 0){
          ctrl <- ctrl %% players + 1
        }
      }
    }
    
    # checking how many are left and if anyone won the game
    players.left <- sum(unlist(ndice) > 0)
    if(players.left == 1){
      if(verbose > 0) cat("player", which(unlist(ndice) > 0), "won the game\n")
    }
    
    # appending play
    play.df <- rbind(play.df, results$play)
  }
  
  if(print.trans) print(play.df)
  
  # update Q
  # rather than training after each action, training at the 
  # end of each game in bulk
  # just easier this way
  if(train) Q.mat <- update.Q(play.df, Q.mat, reward)
  
  # return the winner and Q matrix
  return(list(winner = which(unlist(ndice) > 0), Q.mat = Q.mat))
}


