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
