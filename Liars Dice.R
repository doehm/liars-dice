# liars dice

# liars dice

# function to check

Dice <- function(values){
  
  n <- length(values)
  
  if(n == 0) return("no dice!")
  
  build.dice.block <- paste0(c(
    
    paste0(c(rep("  _________  ", n), "\n"), collapse = ""),
    paste0(c(rep(" /        /| ", n), "\n"), collapse = ""),
    paste0(c(rep("/________/ | ", n), "\n"), collapse = ""),
    paste0(c(rep("| x x x  | | ", n), "\n"), collapse = ""),
    paste0(c(rep("| x x x  | | ", n), "\n"), collapse = ""),
    paste0(c(rep("| x x x  | / ", n), "\n"), collapse = ""),
    paste0(c(rep("|________|/  ", n), "\n"), collapse = "")),
    collapse = ""
  )
  
  dice.box <- lapply(1:6, function(x) matrix(rep(" ", 9), nrow = 3))
  value.pos <- list(
    cbind(2,2),
    cbind(c(1,3), c(1,3)),
    cbind(c(1,2,3), c(1,2,3)),
    cbind(c(1,1,3,3), c(1,3,1,3)),
    cbind(c(1,1,2,3,3), c(1,3,2,1,3)),
    cbind(c(1,1,2,2,3,3), c(1,3,1,3,1,3))
  )
  
  for(k in 1:6) dice.box[[k]][value.pos[[k]]] <- "o"
  
  flatten <- function(x) c(x[1,], x[2,], x[3,])
  rolled.dice.id <- do.call(cbind, dice.box[values]) %>% flatten
  positions <- str_locate_all(pattern = 'x', build.dice.block)[[1]]
  dice <- build.dice.block
  
  for(k in 1:nrow(positions)){
    str_sub(dice, start = positions[k,1], end = positions[k,2]) <- rolled.dice.id[k]
  }
  cat(dice)
}



liars.dice.title <- function(){
  
  cat("
      
      __       _______    __       ____    _____  
     / /|     /__  __/|  /  |     / _  \\  /  __/|
    / / /     |_/ /|_|/ /   |    / /_| |  \\ \\__|/ 
   / / /       / / /   / /| |   / _   /|   \\ \\     
  / /_/_   __ / /_/   / __  |  / / | |/ __ / /|   
 /_____/| /_______/| /_/|_|_| /_/ /|_| /____/ /   
 |_____|/ |_______|/ |_|/ |_| |_|/ |_| |____|/    
      
      ____      _______    _____    ______
     / _  \\    /__  __/|  / ___/|  / ____/|
    / / | |    |_/ /|_|/ / /|__|/ / /___ |/
   / / / /|     / / /   / / /    / ____/|
  / /_/ / / __ / /_/   / /_/_   / /____|/
 /_____/ / /_______/|  |____/| /______/|
 |_____|/  |_______|/  |____|/ |______|/
      
      ")
  
}


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


# agent function chooses the best action
# it needs to take in as imput dice, total dice, dice value and dice quantity
# as output action (raised or called), if raised also new dice value and quantity
# dice, total.dice, dice.value = NULL, dice.quantity = 0
# this is wrapped by a building function to make it easier to change certain 
# parameters and decisions an agent might make and be able to play them off against 
# each other to see which is the better strategy
build.agent <- function(bluff.prob){
  
  return(
    
    function(pars){
      
      # bluff or truth
      bluff <- sample(c(TRUE, FALSE), 1, prob = bluff.prob)
      
      # pobability table
      rt <- table(pars$dice)
      roll.table <- rep(0, 6)
      names(roll.table) <- 1:6
      roll.table[names(rt)] <- rt
      ptable <- roll.table/sum(roll.table)
      
      # if the initial bid do this
      if(is.null(pars$dice.value)){
        new.dice.value <- which.max(ptable[1:6]) %>% names() %>% as.numeric()
        new.dice.quantity <- max(rt) + 1
        return(list(dice.value = new.dice.value, dice.quantity = new.dice.quantity))
      }
      
      # who you gonna call?
      # probability there is less than or equal to the dice quantity bid
      prob <- 1-sum(dbinom(0:max(c(pars$dice.quantity - roll.table[pars$dice.value], 0)), pars$total.dice-length(pars$dice), prob = 1/6))
      call <- sample(c(TRUE, FALSE), 1, prob = c(1-prob, prob))
      if(call){
        return(list(action = "call", dice.value = pars$dice.value, dice.quantity = pars$dice.quantity))
      }else{
        
        # raise
        # if choosing to bluff randomly select a number and increase by 1
        if(bluff){
          if(pars$dice.value == 6){
            new.dice.value <- sample(c(1,6), 1, prob = ptable[c(1, 6)] + 1e-5)
            new.dice.quantity <- pars$dice.quantity + 1
          }else{
            new.dice.value <- sample(pars$dice.value:6, 1, prob = ptable[pars$dice.value:6] + 1e-5)
            new.dice.quantity <- pars$dice.quantity + 1
          }
        }else{
          
          # if not bluffing choose the maximum number in hand and increase by one
          # this should be made to be more flexible however in general raising by
          # 1 occurs 99% of the time
          if(pars$dice.value == 6){
            new.dice.value <- which.max(ptable[c(1,6)]) %>% names() %>% as.numeric()
            new.dice.quantity <- pars$dice.quantity + 1
          }else{
            new.dice.value <- which.max(ptable[pars$dice.value:6]) %>% names() %>% as.numeric()
            new.dice.quantity <- pars$dice.quantity + 1
          }      
        }
        return(list(action = "raised", dice.value = new.dice.value, dice.quantity = new.dice.quantity))
      }
    }
    
  )
  
}




# play a round of liars dice
liars.dice.round <- function(
  players,
  control,
  player.dice.count,
  agents,
  a = 1,
  verbose = 1
){
  
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
  rt <- table(unlist(rolls))
  roll.table <- rep(0, 6)
  names(roll.table) <- 1:6
  roll.table[names(rt)] <- rt
  
  # initial bid
  if(verbose > 0) cat("place first bid\nPlayer", control, "has control\n")
  if(control == a){
    
    dice.value <- set.dice.value("dice value: ", 6)
    dice.quantity <- set.dice.value("quantity; ", sum(rt))
    
  }else{
    
    # agent plays
    pars <- list(dice = rolls[[control]], total.dice = total.dice, dice.value = NULL, dice.quantity = 0)
    agent.action <- agents[[control]](pars = pars)
    dice.value <- agent.action$dice.value
    dice.quantity <- agent.action$dice.quantity
  }
  
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
        pars <- list(dice = rolls[[control]], total.dice = total.dice, dice.value = dice.value, dice.quantity = dice.quantity)
        agent.action <- agents[[control]](pars = pars)
        action <- agent.action$action
      }
      
      # called
      if(action %in% c("call", "c")){
        
        if(verbose > 0) {
          cat("player", control, "called\nRoll table\n")
          print(roll.table)
        }
        
        # check if the quantity of dice value is less or more than the total in the pool
        # if more control loses otherwise control-1 win
        if(dice.quantity > roll.table[dice.value]){
          
          penalty[[prev]] <- penalty[[prev]] - 1
          if(verbose > 0) cat("player", prev, "lost a die\n")
        }else{
          
          penalty[[control]] <- penalty[[control]] - 1
          if(verbose > 0) cat("player", control, "lost a die\n")
        }
        
        called <- TRUE
        
      }else{
        
        if(verbose > 0) cat("player", control, "raised\n")
        
        if(control == a){
          
          # player sets next dice value
          dice.value <- set.dice.value("dice value: ", 6)
          dice.quantity <- set.dice.value("quantity; ", sum(rt))
        }else{
          
          dice.value <- agent.action$dice.value
          dice.quantity <- agent.action$dice.quantity
        }
        if(verbose > 0) cat("dice value", dice.value, "; dice quantity", dice.quantity, "\n")
      }
      
      # set the control player to now be the previous player
      prev <- control
    }
    control <- control %% players + 1
  }
  return(list(penalty = penalty))
}




# play the game of liars dice
play.liars.dice <- function(players = 4, num.dice = 6, auto = FALSE, verbose = 1, agents){
  
  # begin!
  if(verbose > 0) liars.dice.title()
  
  # setting the number of dice each player has
  ndice <- sapply(rep(num.dice, players), function(x) x, simplify = FALSE)
  players.left <- sum(unlist(ndice) > 0)
  
  # while there is at least 2 left in the game
  # who has control
  ctrl <- sample(1:players, 1)
  while(players.left > 1){
    
    # play a round
    results <- liars.dice.round(
      players = players, 
      control = ctrl,
      player.dice.count = ndice, 
      a = as.numeric(!auto),
      verbose = verbose,
      agents = agents
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
  }
  return(which(unlist(ndice) > 0))
}
