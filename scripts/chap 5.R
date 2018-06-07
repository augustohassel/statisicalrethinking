# Multivariate Linear Models ####
source(file = "scripts/basePackages.R")

data("WaffleDivorce")

d <- WaffleDivorce

# our first model ####

# standarize predictors 
d$MedianAgeMarriage_s <- (d$MedianAgeMarriage-mean(d$MedianAgeMarriage))/sd(d$MedianAgeMarriage) # se puede usar "scale" directamente

# fit model
m5_1 <- rethinking::map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + bA * MedianAgeMarriage_s,
    a ~ dnorm(10, 10),
    bA ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d
)

seqcuence <- seq(from = -3, to = 3.5, length.out = 30)

# busco el 89 de la media
mu <- link(fit = m5_1, data = list(MedianAgeMarriage_s=seqcuence))
mu_mean <- mu %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Divorce=V1)
mu_hpdi <- mu %>% as.tibble() %>% map(HPDI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
data_mu_ribbon <- bind_cols(mu_mean, mu_hpdi, tibble(MedianAgeMarriage_s=seqcuence))

# busco el 89 del intervalo de predicciones 
predicted <- sim(fit = m5_1, data = list(MedianAgeMarriage_s=seqcuence))
predicted_mean <- predicted %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Divorce=V1)
predicted <- predicted %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(PredLower=V1, PredUpper=V2)
data_predicted_ribbon <- bind_cols(predicted_mean, predicted, tibble(MedianAgeMarriage_s=seqcuence))

# converting back to natural scale
at <- c(-3, -2, -1, 0, 1, 2, 3)

ggplot(data = d, aes(MedianAgeMarriage_s, Divorce)) + 
  geom_ribbon(data =  data_predicted_ribbon, aes(x = MedianAgeMarriage_s, ymin=PredLower, ymax=PredUpper), fill = "grey80", alpha=2/3) + # 89% posterior prediction interval
  geom_ribbon(data =  data_mu_ribbon, aes(x = MedianAgeMarriage_s, ymin=Lower, ymax=Upper), fill = "grey60", alpha=2/3) + # 89% interval fot the mean
  geom_line(data = data_mu_ribbon, aes(x = MedianAgeMarriage_s, y = Divorce)) +
  geom_jitter(shape=1, size=2, alpha=0.5, color="navyblue")  # data points
  scale_x_continuous(breaks = at, labels = round(at*sd(d$MedianAgeMarriage) + mean(d$MedianAgeMarriage), 1), name = "Back to normal scale") # converting back to natural scale

precis(m5_1)

# fitting a multivariate linear model ####
d$Marriage_s <- as.vector(scale(d$Marriage))

# fit model
m5_3 <- rethinking::map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- a + b1 * Marriage_s + b2 * MedianAgeMarriage_s,
    a ~ dnorm(10, 10),
    b1 ~ dnorm(0, 1),
    b2 ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d
)
precis(m5_1)
precis(m5_3)
plot(precis(m5_3))

# to compute predictor residuals for either, we just use the other predictor to model it
m5_4 <- rethinking::map(
  alist(
    Marriage_s ~ dnorm(mu, sigma),
    mu <- a + b * MedianAgeMarriage_s,
    a ~ dnorm(0, 10),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d
)



mu <- coef(m5_4)["a"] + coef(m5_4)["b"]*d$MedianAgeMarriage_s
m_residual <- d$Marriage_s - mu

d_4 <- d %>% 
  bind_cols(Marriage_s_Estimate=mu) %>%
  bind_cols(Marriage_s_Residual=m_residual)

# residual marriage rate
ggplot(data = d_4, aes(x = MedianAgeMarriage_s, y = Marriage_s_Estimate)) + 
  geom_point() + 
  geom_segment(aes(xend = MedianAgeMarriage_s, yend = Marriage_s_Residual)) +
  geom_point(aes(x = MedianAgeMarriage_s, y = Marriage_s_Residual), size = 2, shape =1, color = "red")

# predictor residual plot
ggplot(data = d_4, aes(x = Marriage_s_Residual, y = Divorce)) +
  geom_point() +
  stat_smooth(method = "lm") +
  geom_vline(xintercept = 0)

# counterfactual plots
A.avg <- mean(d$MedianAgeMarriage_s)
R.seq <- seq(from = -3, to = 3, length.out = 30)
pred.data <- data.frame(Marriage_s=R.seq, 
                        MedianAgeMarriage_s=A.avg)
mu <- link(fit = m5_3, data = pred.data)

mu_mean <- mu %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Divorce=V1)
mu_pi <- mu %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
data_mu_ribbon <- bind_cols(mu_mean, mu_pi)
data_mu_ribbon$Marriage_s <- R.seq

predicted <- sim(fit = m5_3, data = pred.data)

predicted_mean <- predicted %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Divorce=V1)
predicted <- predicted %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(PredLower=V1, PredUpper=V2)
data_predicted_ribbon <- bind_cols(predicted_mean, predicted)
data_predicted_ribbon$Marriage_s <- R.seq

ggplot(data = d, aes(Marriage_s, Divorce)) + 
  geom_ribbon(data =  data_predicted_ribbon, aes(x = Marriage_s, ymin=PredLower, ymax=PredUpper), fill = "grey80", alpha=2/3) + # 89% posterior prediction interval
  geom_ribbon(data =  data_mu_ribbon, aes(x = Marriage_s, ymin=Lower, ymax=Upper), fill = "grey60", alpha=2/3) + # 89% interval fot the mean
  geom_line(data = data_mu_ribbon, aes(x = Marriage_s, y = Divorce))

# posterior prediction plots
mu <- link(fit = m5_3)
mu_mean <- mu %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Divorce=V1)
mu_pi <- mu %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
data_mu_ribbon <- bind_cols(mu_mean, mu_pi)

predicted <- sim(fit = m5_3)
predicted <- predicted %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(PredLower=V1, PredUpper=V2)

# masked relationship ####
library(rethinking)
data("milk")
d <- milk

m5.5 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn * neocortex.perc,
    a ~ dnorm(0, 100),
    bn ~ dnorm(0,1),
    sigma ~ dunif(0, 1)
  ),
  data = d
)

dcc <- d[complete.cases(d), ]
dcc$log.mass <- log(dcc$mass)


# plot the prediction mean > for kcal.per.g
m5.5 <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn * neocortex.perc,
    a ~ dnorm(0, 100),
    bn ~ dnorm(0,1),
    sigma ~ dunif(0, 1)
  ),
  data = dcc
)

precis(m5.5, digits = 3)

np.seq <- 0:100
pred.data <- data.frame(neocortex.perc=np.seq)

# busco el 89 de la media
mu <- link(fit = m5.5, data = pred.data)
mu_mean <- mu %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(kcal.per.g=V1)
mu_pi <- mu %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
data_mu_ribbon <- bind_cols(mu_mean, mu_pi, pred.data)

ggplot(data = dcc, aes(neocortex.perc, kcal.per.g)) + 
  geom_point() +
  # geom_ribbon(data =  data_predicted_ribbon, aes(x = Marriage_s, ymin=PredLower, ymax=PredUpper), fill = "grey80", alpha=2/3) + # 89% posterior prediction interval
  geom_ribbon(data =  data_mu_ribbon, aes(x = neocortex.perc, ymin=Lower, ymax=Upper), fill = "grey60", alpha=2/3) + # 89% interval fot the mean
  geom_line(data = data_mu_ribbon, aes(x = neocortex.perc, y = kcal.per.g))

# plot the prediction mean > for log(mass)
m5.6 <- rethinking::map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bm * log.mass,
    a ~ dnorm(0, 100),
    bm ~ dnorm(0,1),
    sigma ~ dunif(0, 1)
  ),
  data = dcc
)

precis(m5.6, digits = 3)

lm.seq <- seq(from = -2, to = 4, length.out = 100)
pred.data <- data.frame(log.mass=lm.seq)

# busco el 89 de la media
mu <- link(fit = m5.6, data = pred.data)
mu_mean <- mu %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(kcal.per.g=V1)
mu_pi <- mu %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
data_mu_ribbon <- bind_cols(mu_mean, mu_pi, pred.data)

ggplot(data = dcc, aes(log.mass, kcal.per.g)) + 
  geom_point() +
  # geom_ribbon(data =  data_predicted_ribbon, aes(x = Marriage_s, ymin=PredLower, ymax=PredUpper), fill = "grey80", alpha=2/3) + # 89% posterior prediction interval
  geom_ribbon(data =  data_mu_ribbon, aes(x = log.mass, ymin=Lower, ymax=Upper), fill = "grey60", alpha=2/3) + # 89% interval fot the mean
  geom_line(data = data_mu_ribbon, aes(x = log.mass, y = kcal.per.g))


# add both predictors variables at the same time 
m5.7 <- rethinking::map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn * neocortex.perc + bm * log.mass,
    a ~ dnorm(0, 100),
    bn ~ dnorm(0,1),
    bm ~ dnorm(0,1),
    sigma ~ dunif(0, 1)
  ),
  data = dcc
)

precis(m5.7, digits = 3)












