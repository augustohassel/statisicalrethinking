# Sampling the imaginary ####
source(file = "scripts/basePackages.R")

# 3.1 ####
PrPV <- 0.95
PrPM <- 0.01
PrV <- 0.001
PrP <- PrPV*PrV + PrPM*(1-PrV)
(PrVP <- (PrPV*PrV)/PrP)

# 3.2 Reminder on how to compute the posterior from the globe tossing model ####
p_grid <- seq(from = 0, to = 1, length.out = 10000)
prior <- rep(1, 10000)
likelihood <- dbinom(x = 6, size = 9, prob = p_grid)
posterior <- likelihood*prior
posterior <- posterior/sum(posterior)
plot(p_grid, posterior)

# SAMPLING TO SUMMARIZE ####

# 3.3. #####
samples <- sample(x = p_grid, size = 10000, replace = T, prob = posterior)
plot(samples)

dens(samples)
ggplot() + aes(samples) + geom_density(adjust=1/2)


# 3.6 intervals of defined boundaries ####
sum(posterior[p_grid < 0.5])
sum(samples < 0.5) / length(samples)
sum(samples > 0.5 & samples < 0.75) / length(samples)


# 3.9 intervals of defined mass ####
quantile(samples, 0.8)
quantile(samples, c(0.1, 0.9))

# 3.11 ####
p_grid <- seq(from=0, to=1, length.out = 1000)
prior <- rep(1,1000)
likelihood <- dbinom(x = 3, size = 3, prob = p_grid)
posterior <- likelihood*prior
posterior <- posterior/sum(posterior)
samples <- sample(x = p_grid, size = 1e6, replace = T, prob = posterior)

PI(samples = samples, prob = 0.5)
HPDI(samples = samples, prob = 0.5)

ggplot() + aes(samples) + geom_density(adjust=1/2)

# Point estimates ####
# maximum a posteriori 
p_grid[which.max(posterior)]
chainmode(samples, adj=0.01)

# supose we decide p=0.5 will be our decision, then the expected loss is 
sum(posterior * abs(0.5 - p_grid))

loss <- sapply(p_grid, function(d) sum(sum(posterior * abs(d - p_grid))))
plot(loss)
p_grid[which.min(loss)]
median(samples)

# SAMPLING TO SIMULATE PREDICTION ####
dbinom(x = 0:2, size = 2, prob = 0.7)
rbinom(n = 1, size = 2, prob = 0.7)
rbinom(n = 10, size = 2, prob = 0.7)
dummy_w <- rbinom(1e5, size = 2, prob = 0.7)
table(dummy_w)/1e5

dummy_w <- rbinom(1e5, size = 100, prob = 0.2)
simplehist(dummy_w, xlab="dummy water count")

w <- rbinom(1e4, size = 9, prob = 0.6)
simplehist(w)

# all you need to propagate parameter uncertainty into these predictions es replace the value 0.6 with samples from the posterior
p_grid <- seq(from=0, to=1, length.out = 1e3)
prior <- rep(1, 1e3)
likelihood <- dbinom(x = 6, size = 9, prob = p_grid)
posterior <- likelihood*prior
posterior <- posterior/sum(posterior)
samples <- sample(x = p_grid, size = 1e4, replace = T, prob = posterior)

# posterior predictive distribution:
w <- rbinom(1e4, size = 9, prob = samples)
simplehist(w)
plot(table(w)/1e4)

# Practice - Easy ####
ggplot() + aes(samples) + geom_density(adjust=1/2)
sum(samples < 0.2)/length(samples)
sum(samples > 0.8)/length(samples)
sum(samples > 0.2 & samples < 0.8) / length(samples)
quantile(samples, 0.2)
quantile(samples, 0.8)
HPDI(samples = samples, prob = 0.66) # narrowest interval
PI(samples = samples, prob = 0.66) # assuming equal posterior probability both below and above the inteval

# Practice - Medium ####
p_grid <- seq(from=0, to=1, length.out = 1e3)
prior <- rep(1, 1e3)
likelihood <- dbinom(x = 8, size = 15, prob = p_grid)
posterior <- likelihood*prior
posterior <- posterior/sum(posterior)
samples <- sample(x = p_grid, size = 1e4, replace = T, prob = posterior)

ggplot() + aes(samples) + geom_density(adjust=1/2)
HPDI(samples = samples, prob = 0.90) # narrowest interval

w <- rbinom(1e4, size = 15, prob = samples)
simplehist(w)
sum(w==8)/length(w)
mean(w==8)

predictive_9 <- rbinom(1e4, size = 9, prob = samples)
simplehist(predictive_9)
mean(predictive_9==6)

p_grid <- seq(from=0, to=1, length.out = 1e3)
prior <- ifelse(p_grid < 0.5, 0, 1) 
likelihood <- dbinom(x = 8, size = 15, prob = p_grid)
posterior <- likelihood*prior
posterior <- posterior/sum(posterior)
samples <- sample(x = p_grid, size = 1e4, replace = T, prob = posterior)
ggplot() + aes(samples) + geom_density(adjust=1/2)

HPDI(samples = samples, prob = 0.90) # narrowest interval




