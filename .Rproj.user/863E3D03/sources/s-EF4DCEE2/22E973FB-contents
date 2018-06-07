# The Garden of forking data ####
source(file = "scripts/basePackages.R")

# 2.1 ####
ways <- c(0, 3, 8, 9, 0)
ways/sum(ways)

# 2.2 ####
plot(dbinom(1:9, size = 9, prob = 0.5), type="b")
# this is the relative number of ways to get six W's holding p at .5 and n at 9. Is the probability
# of observing w W's in n toses, with a probability p of W > it's the likelihood

# 2.3 grid approximation ####
# define grid
p_grid <- seq(from=0, to=1, length.out = 100)

# define prior
prior <- rep(1, length(p_grid))
# prior <- ifelse(p_grid < 0.5, 0, 1) # truncated prior
# prior <- exp(-5*abs(p_grid - 0.5)) # peaked prior

# compute likelihood at each value in grid
likelihood <- dbinom(x = 6, size = 9, prob = p_grid)

# compute product of likelihood and prior
unstd_posterior <- likelihood * prior

# standarize the posterior, so it sums to 1
posterior <- unstd_posterior / sum(unstd_posterior)

# plot posterior distribution
plot(p_grid, posterior, type="b", xlab="probability of water", ylab="posterior probability")
mtext("20 points")

# 2.6 quadratic approximation ####
globe_qa <- rethinking::map(
  flist =
    alist(
      w ~ dbinom(size = 9, prob = p),   # binomial likelihood
      p ~ dunif(min = 0, max = 1)       # uniform prior
      ),
  data = list(w = 6)
)
globe_qa_summary <- precis(globe_qa)

# 2.7 ####
w <- 6
n <- 9
curve(dbeta(x = x, shape1 = w + 1, shape2 = n-w+1), from = 0, to = 1)
# cuadratic approximation
curve(dnorm(x, mean = globe_qa_summary@output$Mean, sd = globe_qa_summary@output$StdDev), lty=2, add = T)





