library(hash)
library(ggplot2)
library(profvis)
library(dplyr)
library(tidyr)
library(cowplot)

###Epsilon-greedy player###optimeres over epsilon
playerEps.initialize <- function(p2args)
{
  playerEps.args <<- p2args[2]
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

plotEps<- c(0.001,0.005,0.1,0.25,1)
vsRan<-list()
for(i in 1:5) 
{
  tournament('playerRandom','playerEps',p2args=c(0,plotEps[i]),n=20000)
  vsRan[[toString(plotEps[i])]] <- playerEps.history$results
}

vsRanDF <- data.frame(vsRan)
vsRanDF <- data.frame(apply(vsRanDF,2,mov.avg,window=500))
vsRanDF$n <- 1:nrow(vsRanDF)
vsRanDF <- gather(data = vsRanDF,key='Epsilon',value = 'result',-n)
vsRanDF$Epsilon <- substr(vsRanDF$Epsilon,2,nchar(vsRanDF$Epsilon))

p1<- ggplot(data = vsRanDF,aes(x=n,y=result,color=Epsilon))+geom_line(size=1)+ylim(c(-1,1))+theme_bw()+
  scale_color_manual(values=c('red','blue','green','black','orange'))+
  ggtitle(expression(paste('Epsilon-greedy vs Random')))+
  ylab('Score (moving average)')+xlab('Game number')

vsRan<-list()
for(i in 1:5) 
{
  tournament('playerLeft','playerEps',p2args=c(0,plotEps[i]),n=20000)
  vsRan[[toString(plotEps[i])]] <- playerEps.history$results
}

vsRanDF <- data.frame(vsRan)
vsRanDF <- data.frame(apply(vsRanDF,2,mov.avg,window=500))
vsRanDF$n <- 1:nrow(vsRanDF)
vsRanDF <- gather(data = vsRanDF,key='Epsilon',value = 'result',-n)
vsRanDF$Epsilon <- substr(vsRanDF$Epsilon,2,nchar(vsRanDF$Epsilon))

p2<- ggplot(data = vsRanDF,aes(x=n,y=result,color=Epsilon))+geom_line(size=1)+ylim(c(-1,1))+theme_bw()+
  scale_color_manual(values=c('red','blue','green','black','orange'))+
  ggtitle(expression(paste('Epsilon-greedy vs Left')))+
  ylab('Score (moving average)')+xlab('Game number')

###UCB player###
playerUCB.initialize <<- function(p2args){
  #Initializes the hash environment
  playerUCB.strategy<<- hash()
  #Initializes the history with n number of games, an empty vector of results, 
  #and an empty list for putting in game matrices.
  playerUCB.history<<- list(n = 0, results = c(), games = list())
  
  explore<<-p2args[2] #Our c value
  stopExplore<<-20000 #After which game to stop exploring
  initActVal<<-1/2 #Initial action values. 1/2 is standard, but 3/4 works well with eps=0.05
}

playerUCB.move <- function(gameState){
  #checks if gameState has been seen before
  if(is.null(playerUCB.strategy[[toString(gameState)]])){
    #if not, creates an action value vector with 9 zeroes
    #Also creates value of how many times gameState has been seen.
    playerUCB.strategy[[toString(gameState)]] <<- list(actVals=rep(-Inf,9),aSeen=rep(1,9),uncert=rep(-Inf,9),seen=1)
    #assigns an action value for empty spots
    i <- which(gameState==0)
    playerUCB.strategy[[toString(gameState)]]$actVals[i] <<- initActVal
    playerUCB.strategy[[toString(gameState)]]$uncert[i] <<- Inf
  } else {
    playerUCB.strategy[[toString(gameState)]]$seen<<-{
      playerUCB.strategy[[toString(gameState)]]$seen+1}
  }
  
  #The maximum action value
  maxUncert<-max(playerUCB.strategy[[toString(gameState)]]$uncert)
  #The spots that have the maximum action value
  spots<-which(playerUCB.strategy[[toString(gameState)]]$uncert==maxUncert)
  
  if(length(spots)==1) Move<-spots
  else Move<-sample(spots,1)
  
  #adds the move to the history
  if (sum(gameState==0)>7) {
    #If it is the first move of the game, creates the matrix
    playerUCB.history$games[[1]]<<-matrix(c(gameState,Move), ncol=10,byrow=TRUE)
  }else{
    #If it is not the first move, adds the gameState and the move to the history matrix
    playerUCB.history$games[[1]]<<- rbind(playerUCB.history$games[[1]],c(gameState,Move))
  }
  
  playerUCB.strategy[[toString(gameState)]]$aSeen[Move]<<-playerUCB.strategy[[toString(gameState)]]$aSeen[Move]+1
  
  return(Move)
}

playerUCB.gamefinished <- function(result){
  playerUCB.history$n<<-playerUCB.history$n+1
  #appends the result to the list results
  playerUCB.history$results[length(playerUCB.history$results)+1]<<-result
  #calls updatestrategy after every game
  playerUCB.updatestrategy()
}

playerUCB.updatestrategy <- function() {
  #Updates the action values
  for (q in 1:nrow(playerUCB.history$games[[1]])){
    #The move mad for a particular row
    qMove<-playerUCB.history$games[[1]][q,10]
    #The gameState in that row
    qGS<-playerUCB.history$games[[1]][q,1:9]
    #Nr of times the winning gameState has been seen
    qGSseen<-playerUCB.strategy[[toString(qGS)]]$seen
    #For the action values in the gameState at row q updates the value in the spot where we moved
    playerUCB.strategy[[toString(qGS)]]$actVals[qMove] <<-{
      #to the previous action value times the number of times this gamestate has been seen-1
      #plus the reward, which is its latest result, all divided by number of games played.
      playerUCB.strategy[[toString(qGS)]]$actVals[qMove]+(1/qGSseen)*(playerUCB.history$results[length(playerUCB.history$results)]-playerUCB.strategy[[toString(qGS)]]$actVals[qMove])
    }
    #Updates uncertainty values
    for(j in 1:9){
      #number of times the move has been seen in this state
      moveSeen<-playerUCB.strategy[[toString(qGS)]]$aSeen[j]
      playerUCB.strategy[[toString(qGS)]]$uncert[j]<<-{
        playerUCB.strategy[[toString(qGS)]]$actVals[j]+explore*sqrt(log(qGSseen)/moveSeen) 
      }
    }
  }
}

plotUCB<- c(0,1/8,1/4,1/2,1)
vsRan<-list()
for(i in 1:5)
{
  tournament('playerRandom','playerUCB',p2args=c(0,plotUCB[i]),n=20000)
  vsRan[[toString(plotUCB[i])]] <- playerUCB.history$results
}

vsRanDF <- data.frame(vsRan)
vsRanDF <- data.frame(apply(vsRanDF,2,mov.avg,window=500))
vsRanDF$n <- 1:nrow(vsRanDF)
vsRanDF <- gather(data = vsRanDF,key='C',value = 'result',-n)
vsRanDF$C <- substr(vsRanDF$C,2,nchar(vsRanDF$C))

p3<- ggplot(data = vsRanDF,aes(x=n,y=result,color=C))+geom_line(size=1)+ylim(c(-1,1))+theme_bw()+
  scale_color_manual(values=c('red','blue','green','black','orange'))+
  ggtitle(expression(paste('UCB vs Random')))+
  ylab('Score (moving average)')+xlab('Game number')

vsRan<-list()
for(i in 1:5)
{
  tournament('playerLeft','playerUCB',p2args=c(0,plotUCB[i]),n=20000)
  vsRan[[toString(plotUCB[i])]] <- playerUCB.history$results
}

vsRanDF <- data.frame(vsRan)
vsRanDF <- data.frame(apply(vsRanDF,2,mov.avg,window=500))
vsRanDF$n <- 1:nrow(vsRanDF)
vsRanDF <- gather(data = vsRanDF,key='C',value = 'result',-n)
vsRanDF$C <- substr(vsRanDF$C,2,nchar(vsRanDF$C))

p4<- ggplot(data = vsRanDF,aes(x=n,y=result,color=C))+geom_line(size=1)+ylim(c(-1,1))+theme_bw()+
  scale_color_manual(values=c('red','blue','green','black','orange'))+
  ggtitle(expression(paste('UCB vs Left')))+
  ylab('Score (moving average)')+xlab('Game number')

###MC player###
playerMC.initialize <- function(p2args){
  playerMC.strat <<- hash()
  playerMC.episode <<- hash()
  playerMC.stateaction <<- hash()
  playerMC.epsilon <<- p2args[2]
  playerMC.history <<- list(n = 0, results = NA, games = list())
}

playerMC.move <- function(gameState){
  if (is.null(playerMC.strat[[toString(gameState)]])){
    playerMC.strat[[toString(gameState)]] <<- rep(3/4, 9)
    playerMC.strat[[toString(gameState)]][which(gameState==-1)] <<- -Inf
  }
  if (sample(c(TRUE, FALSE), 1, prob=c(playerMC.epsilon/length(which(gameState==0)), 1-playerMC.epsilon/length(which(gameState==0))))){
    move <- sample(which(gameState==0), 1)
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

playerMC.gamefinished <- function(result){
  playerMC.history$n <<- playerMC.history$n+1
  playerMC.history$results[playerMC.history$n] <<- result
  playerMC.updatestrategy(result)
}

playerMC.updatestrategy <- function(result){
  reward <- result
  for (state in keys(playerMC.episode)){
    key <- paste(state, as.character(playerMC.episode[[state]]), sep=", ")
    if (is.null(playerMC.stateaction[[key]])){
      playerMC.stateaction[[key]] <<- c(reward)
      playerMC.strat[[state]][playerMC.episode[[state]]] <<- mean(playerMC.stateaction[[key]]) 
    }else{
      playerMC.stateaction[[key]] <<- c(playerMC.stateaction[[key]], reward)
      playerMC.strat[[state]][playerMC.episode[[state]]] <<- mean(playerMC.stateaction[[key]]) 
    }
  }
  clear(playerMC.episode)
}

plotMC<- c(0.001,0.125,0.25,0.5,1)
vsRan<-list()
for(i in 1:5) 
{
  tournament('playerRandom','playerMC',p2args=c(0,plotMC[i]),n=20000)
  vsRan[[toString(plotMC[i])]] <- playerMC.history$results
}

vsRanDF <- data.frame(vsRan)
vsRanDF <- data.frame(apply(vsRanDF,2,mov.avg,window=500))
vsRanDF$n <- 1:nrow(vsRanDF)
vsRanDF <- gather(data = vsRanDF,key='Epsilon',value = 'result',-n)
vsRanDF$Epsilon <- substr(vsRanDF$Epsilon,2,nchar(vsRanDF$Epsilon))

p5<- ggplot(data = vsRanDF,aes(x=n,y=result,color=Epsilon))+geom_line(size=1)+ylim(c(-1,1))+theme_bw()+
  scale_color_manual(values=c('red','blue','green','black','orange'))+
  ggtitle(expression(paste('MC vs Random')))+
  ylab('Score (moving average)')+xlab('Game number')

vsRan<-list()
for(i in 1:5) 
{
  tournament('playerLeft','playerMC',p2args=c(0,plotMC[i]),n=20000)
  vsRan[[toString(plotMC[i])]] <- playerMC.history$results
}

vsRanDF <- data.frame(vsRan)
vsRanDF <- data.frame(apply(vsRanDF,2,mov.avg,window=500))
vsRanDF$n <- 1:nrow(vsRanDF)
vsRanDF <- gather(data = vsRanDF,key='Epsilon',value = 'result',-n)
vsRanDF$Epsilon <- substr(vsRanDF$Epsilon,2,nchar(vsRanDF$Epsilon))

p6<- ggplot(data = vsRanDF,aes(x=n,y=result,color=Epsilon))+geom_line(size=1)+ylim(c(-1,1))+theme_bw()+
  scale_color_manual(values=c('red','blue','green','black','orange'))+
  ggtitle(expression(paste('MC vs Left')))+
  ylab('Score (moving average)')+xlab('Game number')

###Sarsa player###
playerSarsa.initialize<-function(p2args)
{
  playerSarsa.history <<-list(n=0,results=NA,games=list())
  playerSarsa.strategy <<-hash()
  playerSarsa.history$results <<-c()
  playerSarsa.Epsilon <<-1
  playerSarsa.Alpha <<-p2args[2]
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

plotAlpha<- c(1/8,1/4,1/2,1)
vsRan<-list()
for(i in 1:4) 
{
  tournament('playerRandom','playerSarsa',p2args=c(0,plotAlpha[i]),n=20000)
  vsRan[[toString(plotAlpha[i])]] <- playerSarsa.history$results
}

vsRanDF <- data.frame(vsRan)
vsRanDF <- data.frame(apply(vsRanDF,2,mov.avg,window=500))
vsRanDF$n <- 1:nrow(vsRanDF)
vsRanDF <- gather(data = vsRanDF,key='Alpha',value = 'result',-n)
vsRanDF$Alpha <- substr(vsRanDF$Alpha,2,nchar(vsRanDF$Alpha))

p7<- ggplot(data = vsRanDF,aes(x=n,y=result,color=Alpha))+geom_line(size=1)+ylim(c(-1,1))+theme_bw()+
  scale_color_manual(values=c('red','blue','green','black','orange'))+
  ggtitle(expression(paste('Sarsa vs Random')))+
  ylab('Score (moving average)')+xlab('Game number')

vsRan<-list()
for(i in 1:4)
{
  tournament('playerLeft','playerSarsa',p2args=c(0,plotAlpha[i]),n=20000)
  vsRan[[toString(plotAlpha[i])]] <- playerSarsa.history$results
}

vsRanDF <- data.frame(vsRan)
vsRanDF <- data.frame(apply(vsRanDF,2,mov.avg,window=500))
vsRanDF$n <- 1:nrow(vsRanDF)
vsRanDF <- gather(data = vsRanDF,key='Alpha',value = 'result',-n)
vsRanDF$Alpha <- substr(vsRanDF$Alpha,2,nchar(vsRanDF$Alpha))

p8<- ggplot(data = vsRanDF,aes(x=n,y=result,color=Alpha))+geom_line(size=1)+ylim(c(-1,1))+theme_bw()+
  scale_color_manual(values=c('red','blue','green','black','orange'))+
  ggtitle(expression(paste('Sarsa vs Left')))+
  ylab('Score (moving average)')+xlab('Game number')

###Q player###
playerQ.initialize <<- function(p2args){
  #Initializes the hash environment
  playerQ.strategy<<- hash()
  #Initializes the history with n number of games, an empty vector of results, 
  #and an empty list for putting in game matrices.
  playerQ.history<<- list(n = 0, results = c(), games = list())
  
  alfa<<-p2args[2] #step size
  eps<<-1 #epsilon
  discount<<-1
  stopExplore<<-20000 #After which game to stop exploring
  initActVal<<-3/4 #Initial action values.
}

playerQ.move <- function(gameState){
  stok<-runif(1)
  if(is.null(playerQ.strategy[[toString(gameState)]])){
    #if not, creates an action value vector with 9 times -infinity
    #Also creates value of how many times gameState has been seen.
    playerQ.strategy[[toString(gameState)]] <<- list(actVals=rep(-Inf,9),seen=1)
    #assigns an action value for empty spots
    i <- which(gameState==0)
    playerQ.strategy[[toString(gameState)]]$actVals[i] <<- initActVal
  } else {
    playerQ.strategy[[toString(gameState)]]$seen<<-{
      playerQ.strategy[[toString(gameState)]]$seen+1}
  }
  
  #The maximum action value
  maxActVal<-max(playerQ.strategy[[toString(gameState)]]$actVals)
  #The spots that have the maximum action value
  spots<-which(playerQ.strategy[[toString(gameState)]]$actVals==maxActVal)
  
  
  if(stok<=1-eps) optGreedy<-TRUE
  else optGreedy<-FALSE
  if(length(playerQ.history$results)<stopExplore) explore<-TRUE
  else explore<-FALSE
  
  if((optGreedy)|(!explore)){
    #chooses the greedy option 1-epsilon of the time
    #If there is only one spot with the max action value, that will be the move
    #Else if more spots have the max action value we choose one randomly
    if(length(spots)==1) Move<-spots
    else Move<-sample(spots,1) 
  } else {
    #chooses a random spot epsilon of the time
    #OptNonGreedy is defined as the empty spot
    OptNongreedy<-which(gameState==0)
    
    #The empty spot with max action values are removed from OptNonGreedy if there are other options
    if(length(OptNongreedy)>length(spots)){
      OptNongreedy<-OptNongreedy[!(OptNongreedy%in%spots)]
    }
    if(length(OptNongreedy)==1) Move<-OptNongreedy
    else Move<-sample(OptNongreedy,1)
  }
  
  
  #adds the move to the history
  if (sum(gameState==0)>7) {
    #If it is the first move of the game, creates the matrix
    playerQ.history$games[[1]]<<-matrix(c(gameState,Move), ncol=10,byrow=TRUE)
  }else{
    #If it is not the first move, adds the gameState and the move to the history matrix
    playerQ.history$games[[1]]<<- rbind(playerQ.history$games[[1]],c(gameState,Move))
  }
  
  return(Move)
}

playerQ.gamefinished <- function(result){
  playerQ.history$n<<-playerQ.history$n+1
  #appends the result to the list results
  playerQ.history$results[length(playerQ.history$results)+1]<<-result
  #calls updatestrategy after every game
  playerQ.updatestrategy()
  #we add a moving epsilon
  eps<<-1/playerQ.history$n
}

playerQ.updatestrategy <- function() {
  #the reward is zero for nonlast steps
  Rwd<-0
  #game length
  gLength<-nrow(playerQ.history$games[[1]])
  #updates the nonlast steps
  for (q in 1:(gLength-1)){
    #The move made for a particular row
    qMove<-playerQ.history$games[[1]][q,10]
    #The gameState in that row
    qGS<-playerQ.history$games[[1]][q,1:9]
    #The gameState in the following row
    q1GS<-playerQ.history$games[[1]][q+1,1:9]
    #The action value we wish to update
    OldAV<-playerQ.strategy[[toString(qGS)]]$actVals[qMove]
    #the maximal action value of the following state for whatever action      
    maxNext<-max(playerQ.strategy[[toString(q1GS)]]$actVals)
    
    #calculating the new action value with
    #Q(S,A)<-Q(S,A)+alfa*(R+gamma*max over a (Q(S',a))-Q(S,A))
    playerQ.strategy[[toString(qGS)]]$actVals[qMove] <<-{
      OldAV+alfa*(Rwd+discount*maxNext-OldAV)
    }
  }
  #updates for the last step
  #now the reward is the result
  Rwd<-playerQ.history$results[length(playerQ.history$results)]
  qMove<-playerQ.history$games[[1]][gLength,10]
  qGS<-playerQ.history$games[[1]][gLength,1:9]
  OldAV<-playerQ.strategy[[toString(qGS)]]$actVals[qMove]
  #The next state has no actions, so the value is 0
  maxNext<-0
  playerQ.strategy[[toString(qGS)]]$actVals[qMove] <<-{
    OldAV+alfa*(Rwd+discount*maxNext-OldAV)
  }
}

plotAlpha<- c(1/8,1/4,1/2,1)
vsRan<-list()
for(i in 1:4) 
{
  tournament('playerRandom','playerQ',p2args=c(0,plotAlpha[i]),n=20000)
  vsRan[[toString(plotAlpha[i])]] <- playerQ.history$results
}

vsRanDF <- data.frame(vsRan)
vsRanDF <- data.frame(apply(vsRanDF,2,mov.avg,window=500))
vsRanDF$n <- 1:nrow(vsRanDF)
vsRanDF <- gather(data = vsRanDF,key='Alpha',value = 'result',-n)
vsRanDF$Alpha <- substr(vsRanDF$Alpha,2,nchar(vsRanDF$Alpha))

p9<- ggplot(data = vsRanDF,aes(x=n,y=result,color=Alpha))+geom_line(size=1)+ylim(c(-1,1))+theme_bw()+
  scale_color_manual(values=c('red','blue','green','black','orange'))+
  ggtitle(expression(paste('Q-learning vs Random')))+
  ylab('Score (moving average)')+xlab('Game number')

vsRan<-list()
for(i in 1:4)  
{
  tournament('playerLeft','playerQ',p2args=c(0,plotAlpha[i]),n=20000)
  vsRan[[toString(plotAlpha[i])]] <- playerQ.history$results
}

vsRanDF <- data.frame(vsRan)
vsRanDF <- data.frame(apply(vsRanDF,2,mov.avg,window=500))
vsRanDF$n <- 1:nrow(vsRanDF)
vsRanDF <- gather(data = vsRanDF,key='Alpha',value = 'result',-n)
vsRanDF$Alpha <- substr(vsRanDF$Alpha,2,nchar(vsRanDF$Alpha))

# og plottet laves med ggplot2
p10<- ggplot(data = vsRanDF,aes(x=n,y=result,color=Alpha))+geom_line(size=1)+ylim(c(-1,1))+theme_bw()+
  scale_color_manual(values=c('red','blue','green','black','orange'))+
  ggtitle(expression(paste('Q-learning vs Left')))+
  ylab('Score (moving average)')+xlab('Game number')


###IPW player###
playerIPW.initialize <- function(p2args)
{
  playerIPW.args <<- p2args
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
  lambda <- playerIPW.args[2]
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

plotIPW<- c(1,10,25,100)
vsRan<-list()
for(i in 1:4)  
{
  tournament('playerRandom','playerIPW',p2args=c(0,plotIPW[i]),n=20000)
  vsRan[[toString(plotIPW[i])]] <- playerIPW.history$results
}

vsRanDF <- data.frame(vsRan)
vsRanDF <- data.frame(apply(vsRanDF,2,mov.avg,window=500))
vsRanDF$n <- 1:nrow(vsRanDF)
vsRanDF <- gather(data = vsRanDF,key='Lambda',value = 'result',-n)
vsRanDF$Lambda <- substr(vsRanDF$Lambda,2,nchar(vsRanDF$Lambda))

p11<- ggplot(data = vsRanDF,aes(x=n,y=result,color=Lambda))+geom_line(size=1)+ylim(c(-1,1))+theme_bw()+
  scale_color_manual(values=c('red','blue','green','black','orange'))+
  ggtitle(expression(paste('IPW vs Random')))+
  ylab('Score (moving average)')+xlab('Game number')

vsRan<-list()
for(i in 1:4)  
{
  tournament('playerLeft','playerIPW',p2args=c(0,plotIPW[i]),n=20000)
  vsRan[[toString(plotIPW[i])]] <- playerIPW.history$results
}

vsRanDF <- data.frame(vsRan)
vsRanDF <- data.frame(apply(vsRanDF,2,mov.avg,window=500))
vsRanDF$n <- 1:nrow(vsRanDF)
vsRanDF <- gather(data = vsRanDF,key='Lambda',value = 'result',-n)
vsRanDF$Lambda <- substr(vsRanDF$Lambda,2,nchar(vsRanDF$Lambda))

p12<- ggplot(data = vsRanDF,aes(x=n,y=result,color=Lambda))+geom_line(size=1)+ylim(c(-1,1))+theme_bw()+
  scale_color_manual(values=c('red','blue','green','black','orange'))+
  ggtitle(expression(paste('IPW vs Left')))+
  ylab('Score (moving average)')+xlab('Game number')

plot_grid(plotlist = list(p1,p2,p3,p4,p5,p6),nrow = 3,labels = c('A1','A2','B1','B2','C1','C2'))
plot_grid(plotlist = list(p7,p8,p9,p10,p11,p12),nrow = 3,labels = c('D1','D2','E1','E2','F1','F2'))