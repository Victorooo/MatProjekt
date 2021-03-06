library(hash)
library(tidyr)
library(dplyr)
library(profvis)
library(ggplot2)

evaluateGameState <- function(gameState){
  StateMatrix<-matrix(gameState,nrow=3,ncol=3,byrow=TRUE)
  sums<- c(rowSums(StateMatrix),colSums(StateMatrix),
           sum(gameState[c(1,5,9)]),sum(gameState[c(3,5,7)]))
  if (any(sums==3)) 
  {
    return(1)
  } else if (any(sums==-3)) {
    return(-1)
  } else if(!(0%in%gameState)){
    return(0)
  } else{
    return(42) 
  }
}

game <- function(player1, player2, silent = TRUE){
  gameState<-rep(0,9)
  player2move=1
  while(evaluateGameState(gameState)==42) {
    if (player2move==1){
      move<-do.call(paste(player1,".move",sep=""),list(gameState))
    }else{
      move <-do.call(paste(player2,".move",sep=""),list(-gameState))
    }
    gameState[move] <- player2move
    player2move <- -player2move
    if(!silent) print(matrix(gameState,nrow=3,ncol=3,byrow=TRUE))
  }
  return(evaluateGameState(gameState))
}

playerConsole.initialize <- function(){}
playerConsole.move <- function(gameState){
  print(matrix(gameState,nrow=3,ncol=3,byrow=TRUE))
  input<-as.numeric(readline(prompt="Where would you like to put your X?"))
  if (gameState[input]==0) {
    return(input)
  }else {
    print("Den plads er taget") 
    playerConsole.move(gameState)
  }
}
playerConsole.gamefinished <- function(result){}

playerLeft.initialize <- function(){}
playerLeft.move <- function(gameState){
  empty<- which(gameState==0)
  if(length(empty)>0) return(empty[1])
}
playerLeft.gamefinished <- function(result){}

playerRandom.initialize <- function(){}
playerRandom.move <- function(gameState){
  empty<- which(gameState==0)
  if(length(empty)>1) return(sample(empty,size=1))
  else return(empty)
}
playerRandom.gamefinished <- function(result){}

tournament <- function(player1,player2, n=500)
{
  do.call(paste(player1, ".initialize", sep = ""), list())
  do.call(paste(player2, ".initialize", sep = ""), list())
  while(n!=0)
  {
    if (n%%2==1) {
      result <- game(player1, player2, silent = TRUE)
    } else {
      result <- -game(player2, player1, silent = TRUE)
    }
    do.call(paste(player1, ".gamefinished", sep = ""), list(result))
    do.call(paste(player2, ".gamefinished", sep = ""), list(-result))
    n=n-1
  }
}

###Epsilon-greedy player###
playerEps.initialize <- function()
{
  playerEps.args <<- 0.001
  playerEps.history <<-list(results=c(),game=NULL)
  playerEps.strategy <<- hash()
}

playerEps.move <- function(gameState)
{
  init.action.value <- 3/4
  epsilon=playerEps.args
  if(is.null(playerEps.strategy[[toString(gameState)]])) 
  {
    playerEps.strategy[[toString(gameState)]] <<- (gameState==0)*init.action.value
    playerEps.strategy[[toString(gameState)]][gameState!=0] <<- -Inf
    playerEps.strategy[[toString(gameState)]] <<- rbind(playerEps.strategy[[toString(gameState)]],rep(0,9)) 
  }
  explore<- sample(c(TRUE,FALSE),size=1,prob=c(epsilon,1-epsilon))
  if(explore) possMoves <- which(gameState==0)
  else possMoves <- which(playerEps.strategy[[toString(gameState)]][1,]==max(playerEps.strategy[[toString(gameState)]][1,]))
  if(length(possMoves)==1) move <- possMoves
  else move <- sample(possMoves,size = 1)
  if(is.null(playerEps.history$game)) game <- matrix(c(gameState,move),nrow=1)
  else  game <-rbind(playerEps.history$game,c(gameState,move))  
  playerEps.history$game <<- game
  playerEps.strategy[[toString(gameState)]][2,move] <<- playerEps.strategy[[toString(gameState)]][2,move] + 1
  return(move)
}

playerEps.gamefinished <- function(result)
{
  for(i in 1:nrow(playerEps.history$game))
  {
    gameState <- playerEps.history$game[i,]
    move <- gameState[10]
    strat <- playerEps.strategy[[toString(gameState[1:9])]]
    strat[1,move] <-(strat[1,move]*strat[2,move]+result)/(strat[2,move]+1)
    playerEps.strategy[[toString(gameState[1:9])]] <<- strat
  }
  playerEps.history$game <<- NULL
  playerEps.history$results <<-c(playerEps.history$results,result)
}

###UCB player###
playerUCB.initialize <<- function(){
  playerUCB.strategy<<- hash()
  playerUCB.history<<- list(n = 0, results = c(), games = list())
  
  explore<<-1/8 #Our c value
  stopExplore<<-20000 #After which game to stop exploring
  initActVal<<-1/2
}

playerUCB.move <- function(gameState){
  if(is.null(playerUCB.strategy[[toString(gameState)]])){
    playerUCB.strategy[[toString(gameState)]] <<- list(actVals=rep(-Inf,9),aSeen=rep(1,9),uncert=rep(-Inf,9),seen=1)
    i <- which(gameState==0)
    playerUCB.strategy[[toString(gameState)]]$actVals[i] <<- initActVal
    playerUCB.strategy[[toString(gameState)]]$uncert[i] <<- Inf
  } else {
    playerUCB.strategy[[toString(gameState)]]$seen<<-{
      playerUCB.strategy[[toString(gameState)]]$seen+1}
  }
  
  maxUncert<-max(playerUCB.strategy[[toString(gameState)]]$uncert)
  spots<-which(playerUCB.strategy[[toString(gameState)]]$uncert==maxUncert)
  
  if(length(spots)==1) Move<-spots
  else Move<-sample(spots,1)
  
  if (sum(gameState==0)>7) {
    playerUCB.history$games[[1]]<<-matrix(c(gameState,Move), ncol=10,byrow=TRUE)
  }else{
    playerUCB.history$games[[1]]<<- rbind(playerUCB.history$games[[1]],c(gameState,Move))
  }
  
  playerUCB.strategy[[toString(gameState)]]$aSeen[Move]<<-playerUCB.strategy[[toString(gameState)]]$aSeen[Move]+1
  
  return(Move)
}

playerUCB.gamefinished <- function(result){
  playerUCB.history$n<<-playerUCB.history$n+1
  playerUCB.history$results[length(playerUCB.history$results)+1]<<-result
  playerUCB.updatestrategy()
}

playerUCB.updatestrategy <- function() {
  for (q in 1:nrow(playerUCB.history$games[[1]])){
    qMove<-playerUCB.history$games[[1]][q,10]
    qGS<-playerUCB.history$games[[1]][q,1:9]
    qGSseen<-playerUCB.strategy[[toString(qGS)]]$seen
    playerUCB.strategy[[toString(qGS)]]$actVals[qMove] <<-{
      playerUCB.strategy[[toString(qGS)]]$actVals[qMove]+(1/qGSseen)*(playerUCB.history$results[length(playerUCB.history$results)]-playerUCB.strategy[[toString(qGS)]]$actVals[qMove])
    }
    for(j in 1:9){
      moveSeen<-playerUCB.strategy[[toString(qGS)]]$aSeen[j]
      playerUCB.strategy[[toString(qGS)]]$uncert[j]<<-{
        playerUCB.strategy[[toString(qGS)]]$actVals[j]+explore*sqrt(log(qGSseen)/moveSeen) 
      }
    }
  }
}

###MC player###
playerMC.initialize <- function(){
  playerMC.strat <<- hash()
  playerMC.episode <<- hash()
  playerMC.stateaction <<- hash()
  playerMC.epsilon <<- 0.001
  playerMC.history <<- list(n = 0, results = NA, games = list())
}

playerMC.updatestrategy <- function(result){
  for (state in keys(playerMC.episode)){
    key <- paste(state, as.character(playerMC.episode[[state]]), sep=", ")
    if (is.null(playerMC.stateaction[[key]])){
      playerMC.stateaction[[key]] <<- c(result)
      playerMC.strat[[state]][playerMC.episode[[state]]] <<- mean(playerMC.stateaction[[key]]) 
    }else{
      playerMC.stateaction[[key]] <<- c(playerMC.stateaction[[key]], result)
      playerMC.strat[[state]][playerMC.episode[[state]]] <<- mean(playerMC.stateaction[[key]]) 
    }
  }
  clear(playerMC.episode)
}

playerMC.gamefinished <- function(result){
  playerMC.history$n <<- playerMC.history$n+1
  playerMC.history$results[playerMC.history$n] <<- result
  playerMC.updatestrategy(result)
}

playerMC.move <- function(gameState){
  if (is.null(playerMC.strat[[toString(gameState)]])){
    playerMC.strat[[toString(gameState)]] <<- rep(0, 9)
    playerMC.strat[[toString(gameState)]][which(gameState==-1)] <<- -Inf
    playerMC.strat[[toString(gameState)]][which(gameState==1)] <<- -Inf
  }
  if (sample(c(TRUE, FALSE), 1, prob=c(playerMC.epsilon/length(which(gameState==0)), 1-playerMC.epsilon/length(which(gameState==0))))){
    if (length(which(gameState==0)!=1)){
      move <- sample(which(gameState==0), 1)
    }else{
      move <- which(gameState==0)
    }
  }else{
    if (length(which(playerMC.strat[[toString(gameState)]]==max(playerMC.strat[[toString(gameState)]]))) != 1){
      move <- sample(which(playerMC.strat[[toString(gameState)]]==max(playerMC.strat[[toString(gameState)]])), 1)
    }else{
      move <- which(playerMC.strat[[toString(gameState)]]==max(playerMC.strat[[toString(gameState)]]))
    }
  }
  playerMC.episode[[toString(gameState)]] <<- move
  return(move)
}

###Sarsa player###
playerSarsa.initialize<-function()
{
  playerSarsa.history <<-list(n=0,results=NA,games=list())
  playerSarsa.strategy <<-hash()
  playerSarsa.history$results <<-c()
  playerSarsa.Epsilon <<-1
  playerSarsa.Alpha <<-0.5
  playerSarsa.Gamma <<-1
}
playerSarsa.move<-function(gameState)
{
  if(is.null(playerSarsa.strategy[[toString(gameState)]]))
  {
    playerSarsa.strategy[[toString(gameState)]]<<-rep(0,9)
    i<-which(gameState!=0)
    playerSarsa.strategy[[toString(gameState)]][i]<<- -Inf
  }
  
  if (sample(c(TRUE,FALSE),1,prob = c(1-playerSarsa.Epsilon,playerSarsa.Epsilon)))
  {
    if(length(which(playerSarsa.strategy[[toString(gameState)]]==max(playerSarsa.strategy[[toString(gameState)]])))>1)
    {
      move<-sample(which(playerSarsa.strategy[[toString(gameState)]]==max(playerSarsa.strategy[[toString(gameState)]])),1)
    } else
    {
      move <-which(playerSarsa.strategy[[toString(gameState)]]==max(playerSarsa.strategy[[toString(gameState)]]))
    }
  } else 
  {
    if(length(which(gameState==0))>1){
      move<-sample(which(gameState==0),1)
    } else
    {
      move<-which(gameState==0)
    }
  }
  if(length(playerSarsa.history$games)<playerSarsa.history$n+1)
  {
    playerSarsa.history$games[[playerSarsa.history$n+1]] <<- matrix(c(gameState, move),ncol=10, byrow=TRUE)
  } else
  {
    playerSarsa.history$games[[playerSarsa.history$n+1]] <<- rbind(playerSarsa.history$games[[playerSarsa.history$n+1]],c(gameState,move))
  }
  return(move)
}
playerSarsa.gamefinished<-function(result)
{
  playerSarsa.history$n <<- playerSarsa.history$n+1
  playerSarsa.history$results[playerSarsa.history$n]<<- result
  playerSarsa.Epsilon<<-1/(playerSarsa.history$n+1)
  playerSarsa.updateStrategy(result)
}
playerSarsa.updateStrategy<-function(result)
{
  currentGame <- playerSarsa.history$games[[playerSarsa.history$n]]
  for(i in 1:nrow(currentGame))
  {
    currentMove <- playerSarsa.strategy[[toString(playerSarsa.history$games[[playerSarsa.history$n]][i,1:9])]][playerSarsa.history$games[[playerSarsa.history$n]][i,10]]
    if(i<nrow(currentGame))
    {
      nextMove <- playerSarsa.strategy[[toString(currentGame[i+1,1:9])]][currentGame[i+1,10]]
      playerSarsa.strategy[[toString(playerSarsa.history$games[[playerSarsa.history$n]][i,1:9])]][playerSarsa.history$games[[playerSarsa.history$n]][i,10]]<<- currentMove+playerSarsa.Alpha*(nextMove-currentMove)
    }else
    {
      playerSarsa.strategy[[toString(playerSarsa.history$games[[playerSarsa.history$n]][i,1:9])]][playerSarsa.history$games[[playerSarsa.history$n]][i,10]]<<- currentMove+playerSarsa.Alpha*(result-currentMove)
    }
  }
}

###Q player###
playerQ.initialize <<- function(){
  playerQ.strategy<<- hash()
  playerQ.history<<- list(n = 0, results = c(), games = list())
  
  alfa<<-1/8 #step size
  eps<<-1 #epsilon
  discount<<-1
  stopExplore<<-20000
  initActVal<<-3/4
}

playerQ.move <- function(gameState){
  stok<-runif(1)
  if(is.null(playerQ.strategy[[toString(gameState)]])){
    playerQ.strategy[[toString(gameState)]] <<- list(actVals=rep(-Inf,9),seen=1)
    i <- which(gameState==0)
    playerQ.strategy[[toString(gameState)]]$actVals[i] <<- initActVal
  } else {
    playerQ.strategy[[toString(gameState)]]$seen<<-{
      playerQ.strategy[[toString(gameState)]]$seen+1}
  }
  
  maxActVal<-max(playerQ.strategy[[toString(gameState)]]$actVals)
  spots<-which(playerQ.strategy[[toString(gameState)]]$actVals==maxActVal)
  
  
  if(stok<=1-eps) optGreedy<-TRUE
  else optGreedy<-FALSE
  if(length(playerQ.history$results)<stopExplore) explore<-TRUE
  else explore<-FALSE
  
  if((optGreedy)|(!explore)){
    if(length(spots)==1) Move<-spots
    else Move<-sample(spots,1) 
  } else {
    OptNongreedy<-which(gameState==0)
    
    if(length(OptNongreedy)>length(spots)){
      OptNongreedy<-OptNongreedy[!(OptNongreedy%in%spots)]
    }
    if(length(OptNongreedy)==1) Move<-OptNongreedy
    else Move<-sample(OptNongreedy,1)
  }
  
  
  if (sum(gameState==0)>7) {
    playerQ.history$games[[1]]<<-matrix(c(gameState,Move), ncol=10,byrow=TRUE)
  }else{
    playerQ.history$games[[1]]<<- {
      rbind(playerQ.history$games[[1]],c(gameState,Move))
    }
  }
  
  return(Move)
}

playerQ.gamefinished <- function(result){
  playerQ.history$n<<-playerQ.history$n+1
  playerQ.history$results[length(playerQ.history$results)+1]<<-result
  playerQ.updatestrategy()
  eps<<-1/playerQ.history$n
}

playerQ.updatestrategy <- function() {
  Rwd<-0
  gLength<-nrow(playerQ.history$games[[1]])
  for (q in 1:(gLength-1)){
    qMove<-playerQ.history$games[[1]][q,10]
    qGS<-playerQ.history$games[[1]][q,1:9]
    q1GS<-playerQ.history$games[[1]][q+1,1:9]
    OldAV<-playerQ.strategy[[toString(qGS)]]$actVals[qMove]
    maxNext<-max(playerQ.strategy[[toString(q1GS)]]$actVals)
    playerQ.strategy[[toString(qGS)]]$actVals[qMove] <<-{
      OldAV+alfa*(Rwd+discount*maxNext-OldAV)
    }
  }
  Rwd<-playerQ.history$results[length(playerQ.history$results)]
  qMove<-playerQ.history$games[[1]][gLength,10]
  qGS<-playerQ.history$games[[1]][gLength,1:9]
  OldAV<-playerQ.strategy[[toString(qGS)]]$actVals[qMove]
  maxNext<-0
  playerQ.strategy[[toString(qGS)]]$actVals[qMove] <<-{
    OldAV+alfa*(Rwd+discount*maxNext-OldAV)
  }
}

###IPW player###
playerIPW.initialize <- function()
{
  playerIPW.args <<- 100
  playerIPW.history <<-list(n=0,results=c(),games=list(),S2game_dict=hash(),SA2game_dict=hash())
  playerIPW.strategy <<- hash()
}

playerIPW.move <- function(gameState)
{
  stringGS <- toString(gameState)
  if(is.null(playerIPW.strategy[[stringGS]])) 
  {
    playerIPW.strategy[[stringGS]] <<- as.numeric(gameState==0)
    playerIPW.strategy[[stringGS]][gameState!=0] <<- -Inf
  }
  if(!any(is.infinite(exp(playerIPW.strategy[[stringGS]])))) # necessary since extremely high values of theta can become infinite in R which is incompatible with sample
  {
    move <- sample(1:9,size = 1,prob = exp(playerIPW.strategy[[stringGS]]))
  }
  else move <- which(is.infinite(exp(playerIPW.strategy[[stringGS]])))[1]
  prob <- exp(playerIPW.strategy[[stringGS]][move])/sum(exp(playerIPW.strategy[[stringGS]]))
  n <- playerIPW.history$n+1
  if(length(playerIPW.history[['games']]) < n) game <- matrix(c(gameState,move,prob),nrow=1)
  else  game <- rbind(playerIPW.history$games[[n]],c(gameState,move,prob))  
  playerIPW.history$games[[n]] <<- game
  return(move)
}

playerIPW.gamefinished <- function(result)
{
  updateEveryK <- 50
  n <- playerIPW.history[['n']]+1
  playerIPW.history[['n']] <<-  n
  playerIPW.history[['results']] <<-  c(playerIPW.history[['results']],result)
  game <- playerIPW.history[['games']][[n]]
  for(move in 1:nrow(game))
  {
    playerIPW.history$S2game_dict[[toString(game[move,1:9])]] <- c(playerIPW.history$S2game_dict[[toString(game[move,1:9])]],n)
    playerIPW.history$SA2game_dict[[toString(game[move,1:10])]] <- c(playerIPW.history$SA2game_dict[[toString(game[move,1:10])]],n)
  }
  state <- apply(game[ ,-c(10,11)],1,toString)
  playerIPW.history[['games']][[n]] <<- data.frame(game,state,stringsAsFactors = FALSE)
  if(playerIPW.history[['n']] %% updateEveryK==0) playerIPW.updatestrategy()
}

playerIPW.updatestrategy <- function()
{
  lambda <- playerIPW.args
  future_strat <- playerIPW.strategy
  calc_prob_frac <- function(game)
  {
    strats<-NULL
    for(state in game[,12])
    {
      if(is.null(strats)) strats <- playerIPW.strategy[[state]]
      else strats <- rbind(strats,playerIPW.strategy[[state]])
    }
    idx <- cbind(1:nrow(game),game[,10])
    numerator <- prod(exp(strats[idx])/rowSums(exp(strats)))
    return(numerator/prod(game[,11]))
  }
  frac <- sapply(playerIPW.history$games,calc_prob_frac)
  for(gameState in lapply(strsplit(keys(playerIPW.history$S2game_dict), split=','),as.numeric))
  {
    string_GS <- toString(gameState)
    for(i in 1:9)# each unoccupied space
    {
      if(gameState[i]==0)
      {
        string_SA <- paste(string_GS,i,sep=', ') # state, action
        theta <- exp(playerIPW.strategy[[string_GS]])
        S_idx <- playerIPW.history$SA2game_dict[[string_SA]]
        T_idx <- setdiff(playerIPW.history$S2game_dict[[string_GS]],S_idx)
        
        S_idx <- S_idx[S_idx> playerIPW.history$n-1000] ## TEST
        T_idx <- T_idx[T_idx> playerIPW.history$n-1000] ## TEST
        n <- min(1000,playerIPW.history$n) ## TEST
        S_sum <- (1-theta[i]/sum(theta)) * sum(playerIPW.history$results[S_idx]*frac[S_idx])
        T_sum <- sum(-theta[i]/sum(theta) * playerIPW.history$results[T_idx]*frac[T_idx])
        delta <- (S_sum + T_sum)/n
        if(!is.na(delta)) future_strat[[string_GS]][i] <- future_strat[[string_GS]][i] + lambda*delta
      }
    }
  }
  playerIPW.strategy <<- future_strat
}
