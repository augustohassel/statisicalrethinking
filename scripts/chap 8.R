# Markov Chain Monte Carlo ----------------------------------------

source(file = "scripts/basePackages.R")

# 8.1 Good king markov and his island kingdom --------------------
num_week <- 1e5
position <- rep(0, num_week)
current <- 10
for (i in 1:num_week) {
  position[i] <- current
  
  proposal <- current + sample(x = c(-1, 1), size = 1)
  
  proposal <- case_when(
    proposal < 1 ~ 10,
    proposal > 10 ~ 1,
    TRUE ~ proposal
  )
  
  prob_move <- proposal/current
  
  current <- ifelse(runif(1) < prob_move, proposal, current)
}

ggplot() +
  geom_point(aes(x=1:100, y=position[1:100])) +
  labs(x="week", y="island")

ggplot() +
  geom_histogram(aes(position[1:50]))








