# Markov Chain Monte Carlo ----------------------------------------

source(file = "scripts/basePackages.R")

# 8.1 Good king markov and his island kingdom --------------------

num_week <- 1e5
position <- rep(0, num_week)
current <- 10

# Metropolis Algorithm
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
  geom_histogram(aes(position[1:100]))

# 8.3 Easy HMC: map2stan --------------------
data("rugged")
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[complete.cases(d$rgdppc_2000), ]


m8.1 <- rethinking::map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa,
    a ~ dnorm(0, 100),
    bR ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ),
  data = dd
)
precis(m8.1)

# 8.3.2 Estimation --------------------
dd.trim <- dd %>%
  select(log_gdp, rugged, cont_africa)

m8.1stan <- map2stan(
    alist(
      log_gdp ~ dnorm(mu, sigma),
      mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa,
      a ~ dnorm(0, 100),
      bR ~ dnorm(0, 10),
      bA ~ dnorm(0, 10),
      bAR ~ dnorm(0, 10),
      sigma ~ dcauchy(0, 2)
      ),
    data = dd.trim
)
precis(m8.1stan)

# 8.3.3 Sampling again, in parallel --------------------
m8.1stan_4chains <- map2stan(m8.1stan, chains = 4, cores = 8)
precis(m8.1stan_4chains)

# 8.3.4 Visualization --------------------
post <- extract.samples(m8.1stan)
pairs(post)
pairs(m8.1stan)

# 8.3.5 Using the samples --------------------
show(m8.1stan)

# 8.3.6 Checking the chain --------------------
plot(m8.1stan)

stancode(m8.1stan)


# 8.4.3 Taming a wild chain --------------------
y <- c(-1, 1)
m8.2 <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- alpha
  ),
  data = list(y = y), start = list(alpha = 0, sigma = 1), chains = 2, iter = 4000, warmup = 1000
)
precis(model = m8.2)
plot(m8.2)


# setting some priors to stop de foolishnes
m8.3 <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- alpha,
    alpha <- dnorm(1, 10),
    sigma <- dcauchy(0, 1)
  ),
  data = list(y = y), start = list(alpha = 0, sigma = 1), chains = 2, iter = 4000, warmup = 1000
)
precis(model = m8.3)
plot(m8.3)


# Overthingking: Cauchy Distributio --------------------
y <- rcauchy(n = 1e3, location = 0, scale = 5)
mu <- purrr::map_dbl(as.list(1:length(y)), function(i) sum(y[1:i])/i)

ggplot() +
  geom_line(aes(x=1:length(mu) ,y=mu))

ggplot() +
  geom_histogram(aes(x=mu))

# 8.4.4 Non identifiable parameters --------------------

y <- rnorm(n = 100, mean = 0, sd = 1)
mu <- purrr::map_dbl(as.list(1:length(y)), function(i) sum(y[1:i])/i)
ggplot() +
  geom_line(aes(x=1:length(mu) ,y=mu))

m8.4 <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a1 + a2,
    sigma ~ dcauchy(0, 1)
  ),
  data = list(y=y), start = list(a1=0, a2=0, sigma=1), chains = 2, cores = 2, iter = 4000, warmup = 1000
)
precis(m8.4)
plot(m8.4)

m8.5 <- map2stan(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- a1 + a2,
    a1 ~ dnorm(0, 10),
    a2 ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 1)
  ),
  data = list(y=y), start = list(a1=0, a2=0, sigma=1), chains = 2, cores = 2, iter = 4000, warmup = 1000
)
precis(m8.5)
plot(m8.5)
















