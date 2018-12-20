
# agent function chooses the best action e.g. raise or call
# it needs to take in as input dice, total dice, dice value and dice quantity
# as output action (raise or call), if raised also new dice value and quantity
# dice, total.dice, dice.value, dice.quantity.
# this is wrapped by a building function to make it easier to change certain 
# parameters and decisions an agent might make and be able to play them off against 
# each other to see which is the better strategy
build.agent <- function(bluff.prob, method = "random"){
  
  return(
    
    function(pars, Q.mat){
      
      # bluff or truth
      bluff <- sample(c(TRUE, FALSE), 1, prob = bluff.prob)
      
      # pobability table
      roll.table <- roll.table.fn(pars$dice)
      ptable <- roll.table/sum(roll.table)
      
      # if the initial bid do this
      if(is.null(pars$dice.value)){
        
        new.dice.value <- which.max(ptable[1:6]) %>% names() %>% as.numeric()
        new.dice.quantity <- max(roll.table) + 1
        return(list(dice.value = new.dice.value, dice.quantity = new.dice.quantity))
        
      }
      
      
      # are you gonna call?
      # use the Q matrix to make a decision
      if(method == "Q.decide"){
        if(abs(max(Q.mat[pars$p1.state,]) - min(Q.mat[pars$p1.state,])) < 1e-6)
          call <- sample(c(TRUE, FALSE), 1)
        else{
          # exploration vs exploitation
          if(runif(1) < 0.1){ 
            call <- sample(c(TRUE, FALSE), 1)
          }else{
            call <- names(which.max(Q.mat[pars$p1.state,])) == "call"
          }
        }
        
        
        # the random agent  
      }else if(method == "random"){
        
        prob <- 0.5
        call <- sample(c(TRUE, FALSE), 1, prob = c(1-prob, prob))
        
        # playing the actual numbers  
      }else if(method == "true.prob"){
        
        prob <- 1-sum(dbinom(0:max(c(pars$dice.quantity - roll.table[pars$dice.value], 0)), pars$total.dice-length(pars$dice), prob = 1/6))
        call <- sample(c(TRUE, FALSE), 1, prob = c(1-prob, prob))
        
      }
      
      # if called return the values
      if(call){
        return(list(action = "call", dice.value = pars$dice.value, dice.quantity = pars$dice.quantity))
      }else{
        
        # raise
        # if choosing to bluff randomly select a number and increase by 1
        if(bluff){
          
          new.dice.value <- sample(1:6, 1)
          new.dice.quantity <- pars$dice.quantity + 1
          
        }else{
          
          # if not bluffing choose the maximum number in hand and increase by one
          # this should be made to be more flexible however in general raising by
          # 1 occurs 99% of the time
          new.dice.value <- which.max(ptable) %>% names() %>% as.numeric()
          new.dice.quantity <- pars$dice.quantity + 1
          
        }
        
        # return the new values and action
        return(list(action = "raise", dice.value = new.dice.value, dice.quantity = new.dice.quantity))
      }
    }
  )
}
