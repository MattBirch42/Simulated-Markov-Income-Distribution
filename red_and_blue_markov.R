################################################################################
###  Simulation 1: Markov Simulation  ##########################################
################################################################################

# n reds and 100 blues. 
# 2 levels of wealth, 0 and 1. 
# Reds startwith higher proportion of wealthy
# Wealth evolves randomly according to a Markov process


# INPUTS: ----------------------------------------------------------------------
n <- 10              # number of time periods

red_w_prob <- .9     # prob of red being wealthy in initial state. 
blue_w_prob <- .1    # prob of blue being wealthy in initial state. 

p_stay_r0 <- .7      # prob of red staying poor if poor
p_stay_r1 <- .9      # prob of red staying rich if rich

p_stay_b0 <- .7      # prob of blue staying poor if poor -- default setting is equal probs
p_stay_b1 <- .9      # prob of blue staying rich if rich -- default setting is equal probs


# Markov stochastic transition setup -------------------------------------------
# setting up initial red and blue states.
s0_red <- matrix(c(red_w_prob,1-red_w_prob))
s0_blue <- matrix(c(blue_w_prob,1-blue_w_prob))


#setting up transition matrices
P_red <- matrix(c(p_stay_r1, 1-p_stay_r1,1-p_stay_r0,p_stay_r0),2,2)
P_blue <- matrix(c(p_stay_b1, 1-p_stay_b1,1-p_stay_b0,p_stay_b0),2,2)

st_red <- s0_red
st_blue <- s0_blue

# Transitions ------------------------------------------------------------------

pr_wealthy <- matrix(0,n+1,2)
pr_wealthy[1,] <- c(red_w0_prob,blue_w0_prob)

for (t in 1:n) {
st_red <- P_red %*% st_red 
st_blue <- P_blue %*% st_blue

pr_wealthy[t+1,1] <- st_red[1]
pr_wealthy[t+1,2] <- st_blue[1]
}

pr_wealthy

#line graph over time showing how long until equal distributions for red and blue
