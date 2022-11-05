library(ggplot2)
library(gganimate)
library(transformr)
#library(gifski)
################################################################################
###  Section 1: Markov Simulation  #############################################
################################################################################

# n reds and 100 blues. 
# 2 levels of wealth, 0 and 1. 
# Reds startwith higher proportion of wealthy
# Wealth evolves randomly according to a Markov process


# INPUTS: ----------------------------------------------------------------------
n <- 10              # number of time periods

red_w_prob <- .9     # prob of red being wealthy in initial state. 
blue_w_prob <- .1    # prob of blue being wealthy in initial state. 

p_stay_r0 <- .6      # prob of red staying poor if poor
p_stay_r1 <- .9      # prob of red staying rich if rich

p_stay_b0 <- .8      # prob of blue staying poor if poor -- default setting is equal probs
p_stay_b1 <- .7      # prob of blue staying rich if rich -- default setting is equal probs


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

pr_wealthy <- matrix(0,n+1,3)
pr_wealthy[1,] <- c(0,red_w_prob,blue_w_prob)

for (t in 1:n) {
st_red <- P_red %*% st_red 
st_blue <- P_blue %*% st_blue

pr_wealthy[t+1,1] <- t
pr_wealthy[t+1,2] <- st_red[1]
pr_wealthy[t+1,3] <- st_blue[1]
}

pr_wealthy <- as.data.frame(pr_wealthy)
colnames(pr_wealthy) <- c("Generation", "wealthy_red_rate","wealthy_blue_rate")


################################################################################
###  Section 2: As a function and with visualization  ##########################
################################################################################
# input vectors have 7 elements, the inputs from section 1
# input_vector_fake <- c(n, red_w_prob, blue_w_prob, p_stay_r0, p_stay_r1, p_stay_b0, p_stay_b1)

rb_markov = function(input) {
  n <- input[1]
  red_w_prob <- input[2]
  blue_w_prob <- input[3]
  p_stay_r0 <- input[4]
  p_stay_r1 <- input[5]
  p_stay_b0 <- input[6]
  p_stay_b1 <- input[7]

  s0_red <- matrix(c(red_w_prob,1-red_w_prob))
  s0_blue <- matrix(c(blue_w_prob,1-blue_w_prob))

  P_red <- matrix(c(p_stay_r1, 1-p_stay_r1,1-p_stay_r0,p_stay_r0),2,2)
  P_blue <- matrix(c(p_stay_b1, 1-p_stay_b1,1-p_stay_b0,p_stay_b0),2,2)

  st_red <- s0_red
  st_blue <- s0_blue

  pr_wealthy <- matrix(0,n+1,3)
  pr_wealthy[1,] <- c(0,red_w_prob,blue_w_prob)*100

  for (t in 1:n) {
    st_red <- P_red %*% st_red 
    st_blue <- P_blue %*% st_blue
  
    pr_wealthy[t+1,1] <- t
    pr_wealthy[t+1,2] <- st_red[1]*100
    pr_wealthy[t+1,3] <- st_blue[1]*100
}

  pr_wealthy <- as.data.frame(pr_wealthy)
  colnames(pr_wealthy) <- c("Generation", "wealthy_red_rate","wealthy_blue_rate")

  return(pr_wealthy)
}



# input_vector_fake <- c(n, red_w_prob, blue_w_prob, p_stay_r0, p_stay_r1, p_stay_b0, p_stay_b1)
input_vector <- c(10,.9,.2,.4,.9,.7,.7)
pr_wealthy <- rb_markov(input_vector)

rb_plot <- ggplot(pr_wealthy, aes(x = Generation)) +
             geom_line(aes(y = wealthy_red_rate), color = "red") +
             geom_line(aes(y = wealthy_blue_rate), color = "blue") +
             labs(title = "Wealth Inequality of Red and Blue",
               y = "Percent of Color that are Wealthy") +
             theme_light()


#this is not going anywhere. 
# can I do one that shows the curves change as I change initial probabilities?

anim <- rb_plot + 
  transition_states(Generation,
                    transition_length = 2,
                    state_length = 1)   

rb_plot + transition_time(Generation) +
  labs(title = "Generation: {Generation}")

rb_plot + transition_states(Generation,transition_length = 2,
                            state_length = 1) +
  labs(title = "Generation: {Generation}")
