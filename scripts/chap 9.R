# Big Entropy and the Generalized Linear Model  ----------------------------------------
source(file = "scripts/basePackages.R")

# 9.1 Maximum entropy ----------------------------------------

# the distribution that can happen the most is also the distribution with the biggest information entropy.
# The distribution with the biggest entropy is the most conservative distribution that obeys its contraints.

p <- list()
p$A <- c(0,0,10,0,0)
p$B <- c(0,1,8,1,0)
p$C <- c(0,2,6,2,0)
p$D <- c(1,2,4,2,1)
p$E <- c(2,2,2,2,2)

p_norm <- purrr::map(p, function(x) x / sum(x))

purrr::map_dbl(p_norm, function(x) -sum(case_when(x==0 ~ 0, TRUE ~ x*log(x))))

# 9.1.1 Gaussian ####

# 9.1.2 Binomial ####

?binomial

# calculating expecting value
p <- list()
p[[1]] <- c(1/4, 1/4, 1/4, 1/4)
p[[2]] <- c(1/6, 2/6, 2/6, 1/6)

sapply(p, function(p) sum(p*c(0,1,1,2)))
sapply(p, function(p) -sum(p*log(p)))

p <- 0.7
A <- c( (1-p)^2,  p*(1-p), (1-p)*p, p^2)
-sum(A*log(A))

sim.p <- function(ExpectedValue=1.4) {
  x123 <- runif(3)
  x4 <- (ExpectedValue*sum(x123) - x123[2] - x123[3])/(2-ExpectedValue)
  z <- sum(x123, x4)
  p <- c(x123, x4)/z
  
  list(H=-sum(p*log(p)), 
         p=p)
}

H <- replicate(n = 1e5, expr = sim.p())

dens(as.numeric(H[1,]), adj = 0.1)

entropies <- as.numeric(H[1,])
distributions <- H[2,]

distributions[which.max(entropies)]

# 9.2 Generalized linear models ----------------------------------------























