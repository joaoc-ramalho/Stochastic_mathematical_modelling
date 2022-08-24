###############################
###############################
##Stochastic Bacterial Growth##
####Guillespie Algorithm ######
###############################

#------------Population Dynamics

#defining the general probabilities, just to remember
Pbirth <- (1/3)
Pdeath <- (1/5)

#defining the initial population size
N <- c(30) 

#generating the random numbers which will decide the births or deaths, based 
# the probabilities
choose <- runif(1000, 0, 1)

#finding the birth probability in the guillespie time (time in which some 
#population change will happens for sure, calculated above)

#this probability will be the probability of births divided by the total 
#probability of changes

Pbirth_g <- (Pbirth)/(Pbirth+Pdeath)

for(j in 1:999){
  if(choose[j]<(Pbirth_g)){
    N[j+1] <- N[j]+1
  } else{
    N[j+1] <- N[j]-1
  }
}
 
#Having the population dynamics, we can calculate the times
#Its important to have the population before because its the population size
#that will control the time necessary for any event to happens (if we have more
# individuals, then, less time is needed)

#------------------- Time distribution:

#Creating an empty vector to contain the times
tvals <- c() 

# Making the distribution os times of change. 
# The times of change will varry from each step of population, because they will
# depend on the population size, as previously said. 
# The times will be randon numbers from exponential distributions, and its here
# where we can see the relationship between the times of change and the population
# size. The lambda parameter of this exponential distribuion will be the sum of 
# all rates envolved in the procces but the rate itself is a multiplication of the
# probability of this event by the number of trials. 
# Here, the sum of the rates will be: the probability of birth times the number
# of indibiduals (N) in this step plus the probability of death times the number
# of indibiduals (N) in this step 
#
#To do this, I'm creating a loop that will take each value of "N"  and calculates
# the sum of the rates. Having this, it will be lambda for the generation of a 
#random exponential number. Then, this generated number will be saved in the 
# vector "tvals" in a position (index) correspondent to the N which created this

for(i in 1:length(N)) {
  tvals[i] <- c(rexp(1, (((1/3)*N[i])+((1/5)*N[i]))))
}

#Timeline
# after this, we need to make the time intervals succecives. To do this, I'm creating
# a loop which will make that a given time will be the sum of the randon exponential
#number with all the others that came before:

for(i in 2:length(tvals)){
  tvals[i] <- tvals[i-1] + tvals[i]
}
# With this, our time-line is ready

#Then, we just need to plot
plot(tvals, N, type = "l")

