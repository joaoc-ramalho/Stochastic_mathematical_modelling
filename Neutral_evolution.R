###############################
######## Ricardo's classes ####
######## Mooran Model #########

# Initial conditions 

N <- c(rep(1, 70), rep(0 ,30)) #populations

i <- sum(N == 1) # determing frequency

p_i <- (sum(N == 1)/length(N)) # portion of i in the population

p_ni <- ((sum(N==0))/length(N)) #proportion of n-i
 
N <- c(N, 1)

choose <-  runif(500, 0, 1)

#Probabilities:

birth_a <- p_i*p_ni
death_a <- p_ni*p_i
stay <- p_i*p_i + p_ni*p_ni


######### Solving Phenomenollogic, just to see the fluctuations
states <- c()

for(i in 1:length(choose)){
  if(choose[i]<2*((sum(N == 1)/length(N))*((sum(N==0))/length(N)))){
    if(runif(1,0,1)>0.5){
      states[i] <- "birth"
    } else{
      states[i] <- "death"
    }
  } else{
    states[i] <- "stay"
  }
}


###### Solving numerically

#creating the vectors were the populations will be stored,
#already whith the initial populations

position_i <- c(51) 
position_k <- c(49)

#The code


# My random numbers which will guide the probabilistic chooses
choose <- runif(1000000, min = 0, max = 1)

# The trajectories code
for(j in 1:length(choose)){
  if(choose[j]<2*(position_i[1]/100)*(position_k[1]/100)){
    if(runif(1,0,1)>0.5){
      position_i[j+1] <- position_i[j]+1
      position_k[j+1] <- position_k[j]-1
    } else{
      position_i[j+1] <- position_i[j]-1
      position_k[j+1] <- position_k[j]+1
    }
  } else{
    position_i[j+1] <- position_i[j]
    position_k[j+1] <- position_k[j]
  } 
}

# making a code to plot only until the populations reach the fixed points
if((which(position_i == 0)[1])<(which(position_k == 0)[1])){
  par(mfrow=c(1,2))
  plot1 <- plot(position_i[1:(which(position_i == 0)[1])], type = "l")
  plot2 <- plot(position_k[1:(which(position_i == 0)[1])], type = "l")
  par(mfrow=c(1,1))
}else {
  par(mfrow=c(1,2))
  plot1 <- plot(position_i[1:(which(position_k == 0)[1])], type = "l")
  plot2 <- plot(position_k[1:(which(position_k == 0)[1])], type = "l")
  par(mfrow=c(1,1))
}

#To consider the generation time, we can create other distribution (vector)
#just for the time

