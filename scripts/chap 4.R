# Linear Models ####
source(file = "scripts/basePackages.R")

# why normal distributions are normal ####
# normal by addition
pos <- replicate(n = 1e3, sum(runif(n = 16, min = -1, max = 1)))
hist(pos)
plot(density(pos))

# normal by multiplication
growth <- replicate(1e3, prod(1 + runif(12, min = 0, max = .1)))
plot(growth)
hist(growth)
dens(x = growth, norm.comp = T)

big <- replicate(1e3, prod(1 + runif(12, min = 0, max = 1)))
dens(x = big, norm.comp = T) # as long as they are sufficiently small, converge to a gaussian distribution


# normal by log-multiplication
log_big <- replicate(1e3, log(prod(1 + runif(12, min = 0, max = 1))))
dens(x = log_big, norm.comp = T)

# from model definition to baye's theorem
w <- 6; n <- 9
p_grid <- seq(from = 0, to = 1, length.out = 100)
posterior <- dbinom(x = w, size = n, prob = p_grid)*dunif(x = p_grid, min = 0, max = 1)
posterior <- posterior/sum(posterior)
plot(p_grid, posterior)

# A Gaussian model of heigth ####
data("Howell1")

ggplot(data = Howell1 %>% filter(age>=18), aes(x=age, y=height)) + 
  geom_jitter(aes(color=weight)) + 
  facet_wrap(~male) + 
  stat_smooth(method = "auto")

d <- Howell1
d2 <- d %>% filter(age >= 18)
dens(d2$height)

# prior for mu
curve(dnorm(x, mean = 178, sd = 20), from = 100, to = 250)
# prior for sigma
curve(dunif(x, min = 0, max = 50), from = -10, to = 60)

sample_mu <- rnorm(n = 1e4, mean = 178, sd = 20)
sample_sigma <- runif(n = 1e4, min = 0, max = 50)
prior_h <- rnorm(n = 1e4, mean = sample_mu, sd = sample_sigma)
dens(prior_h)

# grid approximation of the posterior distribution
mu_list <- seq(from = 140, to = 160, length.out = 200)
sigma_list <- seq(from = 4, to = 9, length.out = 200)
post <- expand.grid(mu = mu_list, sigma = sigma_list)
post$LL <- sapply(1:nrow(post), function(i) sum(dnorm(d2$height, mean = post$mu[i], sd = post$sigma[i], log = T))) # log likelihood
post$prod <- post$LL + dnorm(post$mu, 178, 20, T) + dunif(post$sigma, 0, 50, T)
post$prob <- exp(post$prod - max(post$prod))
contour_xyz(x = post$mu, y = post$sigma, z = post$prob)
image_xyz(x = post$mu, y = post$sigma, z = post$prob)

# sampling from the posterior 
sample_rows <- sample(x = 1:nrow(post), size = 1e4, replace = T, prob = post$prob)
sample_mu <- post$mu[sample_rows]
sample_sigma <- post$sigma[sample_rows]

plot(x = sample_mu, y = sample_sigma, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.1))

dens(sample_mu)
dens(sample_sigma)
HPDI(sample_mu)
HPDI(sample_sigma)

# sample size and the normality of sigma distribution
d3 <- sample(d2$height, size = 20)

mu_list <- seq(from = 150, to = 170, length.out = 200) # range of mu
sigma_list <- seq(from = 4, to = 20, length.out = 200) # rango of sigma 
post2 <- expand.grid(mu = mu_list, sigma = sigma_list) # combination of mu and sigma
post2$LL <- sapply(1:nrow(post2), function(i) sum(dnorm(d3, mean = post2$mu[i], sd = post2$sigma[i], log = T))) # log likelihood
post2$prod <- post2$LL + dnorm(post2$mu, 178, 20, T) + dunif(post2$sigma, 0, 50, T) # multiply log likelihood by the prior > proportional to the posterior dentisty
post2$prob <- exp(post2$prod - max(post2$prod)) # probability scale

sample2_rows <- sample(x = 1:nrow(post2), size = 1e4, replace = T, prob = post2$prob)
sample2_mu <- post2$mu[sample2_rows]
sample2_sigma <- post2$sigma[sample2_rows]

plot(post2$prob)

plot(x = sample2_mu, y = sample2_sigma, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.1))

dens(sample2_sigma, norm.comp = T)
dens(sample2_mu, norm.comp = T)

# fitting the model with MAP (maximum a posteriori estimate) ####
data("Howell1")
d <- Howell1
d2 <- d %>% filter(age >= 18)

flist <- alist(
  height ~ dnorm(mean = mu, sd = sigma),
  mu ~ dnorm(mean = 182, sd = 20),
  sigma ~ dunif(min = 0, max = 50)
)

model <- rethinking::map(flist = flist, data = d2)
precis(model)

# sampling from a map fit ####
vcov(model)
model@vcov # variance - covariance matrix

diag(model@vcov) # varianzas
diag(model@vcov) %>% sqrt # desvio estandar
cov2cor(model@vcov) # matriz de correlaciones

post <- extract.samples(object = model, n = 1e4) # extracting samples of mu and sigma from the posterior

precis(post)
plot(post, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.2))

# the linear model strategy ####
ggplot(data = d2, aes(weight, height)) + geom_hex(show.legend = F)

m4_3 <- rethinking::map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight,
    a ~ dnorm(182, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ),
  data = d2
)

ggplot(data = d2, aes(weight, height)) + 
  geom_jitter() + 
  geom_abline(slope = m4_3@coef["b"], intercept = m4_3@coef["a"], color="red", size=1.5)

precis(m4_3, corr = T)

# centering 
d2$weight_c <- d2$weight - mean(d2$weight)
m4_4 <- rethinking::map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight_c,
    a ~ dnorm(182, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ),
  data = d2
)
ggplot(data = d2, aes(weight_c, height)) + 
  geom_jitter(shape=1, size=2, alpha=0.5, color="navyblue") + 
  geom_abline(slope = m4_4@coef["b"], intercept = m4_4@coef["a"], color="red", size=1.5)

precis(m4_4, corr = T)

# adding uncertainty around the mean
post <- extract.samples(m4_3)

ggplot(data = d2, aes(weight, height)) + 
  geom_jitter(shape=1, size=2, alpha=0.5, color="navyblue") + 
  geom_abline(slope = m4_3@coef["b"], intercept = m4_3@coef["a"], color="red", size=1.5) +
  geom_abline(slope = post$b[1:20], intercept = post$a[1:20], size = 1/3, alpha = .3)

mu_at_50 <- post$a + post$b*50
dens(mu_at_50)
ggplot() + 
  aes(mu_at_50) + 
  geom_density(adjust=1/2) +
  labs(x=expression(paste(mu, " at 50"))) +
  geom_vline(xintercept = HPDI(mu_at_50), linetype=2)
HPDI(mu_at_50)

mu <- link(fit = m4_3)
mu %>% str

weight_seq <- seq(from = 25, to = 70, by = 1)
mu <- link(fit = m4_3, data = data.frame(weight=weight_seq))
mu_mean <- mu %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(height=V1)
mu_hpdi <- mu %>% as.tibble() %>% map(HPDI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)

data_ribbon <- bind_cols(mu_mean, mu_hpdi, tibble(weight=weight_seq))

ggplot(data = d2, aes(weight, height)) + 
  geom_jitter(shape=1, size=2, alpha=0.5, color="navyblue") + # data points
  geom_abline(slope = m4_3@coef["b"], intercept = m4_3@coef["a"], color="red", size=1) + # mean
  geom_ribbon(data =  data_ribbon, aes(x = weight, ymin=Lower, ymax=Upper), fill = "grey70", alpha=2/3) # 89% interval fot the mean

# prediction intervals
sim_height <- sim(fit = m4_3, data = list(weight=weight_seq))
pred_mean <- sim_height %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(height=V1)
pred_height <- sim_height %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(PredLower=V1, PredUpper=V2)
data_ribbon_pred <- bind_cols(pred_mean, pred_height, tibble(weight=weight_seq))

ggplot(data = d2, aes(weight, height)) + 
  geom_ribbon(data =  data_ribbon_pred, aes(x = weight, ymin=PredLower, ymax=PredUpper), fill = "grey80", alpha=2/3) + # 89% posterior prediction interval
  geom_ribbon(data =  data_ribbon, aes(x = weight, ymin=Lower, ymax=Upper), fill = "grey60", alpha=2/3) + # 89% interval fot the mean
  geom_jitter(shape=1, size=2, alpha=0.5, color="navyblue") + # data points
  geom_abline(slope = m4_3@coef["b"], intercept = m4_3@coef["a"], color="red", size=1)  # mean
  
# simulate my own weight
sim(fit = m4_3, data = list(weight=75)) %>% PI
sim(fit = m4_3, data = list(weight=75)) %>% mean

# 4.5 Ppolynomial regression ####
ggplot(data = d, aes(weight_stan, height)) + 
  geom_jitter(shape=1, size=2, alpha=0.5, color="navyblue") # data points

# fitting a polinomial regression
d$weight_stan <- (d$weight - mean(d$weight))/sd(d$weight) # standarize weight
d$weight_stan_2 <- d$weight_stan^2

m4_5 <- rethinking::map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*weight_stan + b2*weight_stan_2,
    a ~ dnorm(182, 100),
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ),
  data = d
) # model
precis(m4_5, corr = T)

weight_seq <- seq(from = -2, to = 2, length.out = 30)

# busco el 89 de la media
mu <- link(fit = m4_5, data = list(weight_stan=weight_seq, weight_stan_2=weight_seq^2))
mu_mean <- mu %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(height=V1)
mu_hpdi <- mu %>% as.tibble() %>% map(HPDI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
data_mu_ribbon <- bind_cols(mu_mean, mu_hpdi, tibble(weight=weight_seq))

# busco el 89 del intervalo de predicciones 
predicted_height <- sim(fit = m4_5, data = list(weight_stan=weight_seq, weight_stan_2=weight_seq^2))
predicted_mean <- predicted_height %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(height=V1)
predicted_height <- predicted_height %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(PredLower=V1, PredUpper=V2)
data_predicted_ribbon <- bind_cols(predicted_mean, predicted_height, tibble(weight=weight_seq))

# converting back to natural scale
at <- c(-2, -1, 0, 1, 2)

ggplot(data = d, aes(weight_stan, height)) + 
  geom_ribbon(data =  data_predicted_ribbon, aes(x = weight, ymin=PredLower, ymax=PredUpper), fill = "grey80", alpha=2/3) + # 89% posterior prediction interval
  geom_ribbon(data =  data_mu_ribbon, aes(x = weight, ymin=Lower, ymax=Upper), fill = "grey60", alpha=2/3) + # 89% interval fot the mean
  geom_line(data = data_mu_ribbon, aes(x = weight, y = height)) +
  geom_jitter(shape=1, size=2, alpha=0.5, color="navyblue") + # data points
  scale_x_continuous(breaks = at, labels = round(at*sd(d$weight) + mean(d$weight), 1)) # converting back to natural scale

# predigo la altura en funcion de mi propio peso
sim(fit = m4_5, data = list(weight_stan=(75-mean(d$weight))/sd(d$weight), weight_stan_2=((75-mean(d$weight))/sd(d$weight))^2)) %>% PI
sim(fit = m4_5, data = list(weight_stan=(75-mean(d$weight))/sd(d$weight), weight_stan_2=((75-mean(d$weight))/sd(d$weight))^2)) %>% mean

# voy a recalcular todo con un modeulo cubico ####
d$weight_stan <- (d$weight - mean(d$weight))/sd(d$weight) # standarize weight
d$weight_stan_2 <- d$weight_stan^2
d$weight_stan_3 <- d$weight_stan^3

m4_5_2 <- rethinking::map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b1*weight_stan + b2*weight_stan_2 + b3*weight_stan_3,
    a ~ dnorm(182, 100),
    b1 ~ dnorm(0, 10),
    b2 ~ dnorm(0, 10),
    b3 ~ dnorm(0, 10),
    sigma ~ dunif(0, 50)
  ),
  data = d
) # model
precis(m4_5_2, corr = T)

weight_seq <- seq(from = -2, to = 2, length.out = 30)

# busco el 89 de la media
mu <- link(fit = m4_5_2, data = list(weight_stan=weight_seq, weight_stan_2=weight_seq^2, weight_stan_3=weight_seq^3))
mu_mean <- mu %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(height=V1)
mu_hpdi <- mu %>% as.tibble() %>% map(HPDI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
data_mu_ribbon <- bind_cols(mu_mean, mu_hpdi, tibble(weight=weight_seq))

# busco el 89 del intervalo de predicciones 
predicted_height <- sim(fit = m4_5_2, data = list(weight_stan=weight_seq, weight_stan_2=weight_seq^2, weight_stan_3=weight_seq^3))
predicted_mean <- predicted_height %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(height=V1)
predicted_height <- predicted_height %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(PredLower=V1, PredUpper=V2)
data_predicted_ribbon <- bind_cols(predicted_mean, predicted_height, tibble(weight=weight_seq))

# converting back to natural scale
at <- c(-2, -1, 0, 1, 2)

ggplot(data = d, aes(weight_stan, height)) + 
  geom_ribbon(data =  data_predicted_ribbon, aes(x = weight, ymin=PredLower, ymax=PredUpper), fill = "grey80", alpha=2/3) + # 89% posterior prediction interval
  geom_ribbon(data =  data_mu_ribbon, aes(x = weight, ymin=Lower, ymax=Upper), fill = "grey60", alpha=2/3) + # 89% interval fot the mean
  geom_line(data = data_mu_ribbon, aes(x = weight, y = height)) +
  geom_jitter(shape=1, size=2, alpha=0.5, color="navyblue") + # data points
  scale_x_continuous(breaks = at, labels = round(at*sd(d$weight) + mean(d$weight), 1)) # converting back to natural scale

# predigo la altura en funcion de mi propio peso
sim(fit = m4_5_2, data = list(
  weight_stan=(75-mean(d$weight))/sd(d$weight), 
  weight_stan_2=((75-mean(d$weight))/sd(d$weight))^2,
  weight_stan_3=((75-mean(d$weight))/sd(d$weight))^3
  )) %>% PI
sim(fit = m4_5_2, data = list(
  weight_stan=(75-mean(d$weight))/sd(d$weight), 
  weight_stan_2=((75-mean(d$weight))/sd(d$weight))^2,
  weight_stan_3=((75-mean(d$weight))/sd(d$weight))^3
)) %>% mean















  

























