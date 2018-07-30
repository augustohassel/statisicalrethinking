# Multivariate Linear Models ####
source(file = "scripts/basePackages.R")

data("WaffleDivorce")

d <- WaffleDivorce

# 5.1 - Supurious Association ####

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

# 5.1.1  fitting a multivariate linear model ####
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

# 5.1.3.1 - Predictor Residual Plot ####
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

# 5.1.3.2 - counterfactual plots ####
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


# 5.1.3.3 - posterior prediction plots ####
mu <- link(fit = m5_3)
mu_mean <- mu %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Divorce=V1)
mu_pi <- mu %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
data_mu_ribbon <- bind_cols(mu_mean, mu_pi)

predicted <- sim(fit = m5_3)
predicted <- predicted %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(PredLower=V1, PredUpper=V2)

# 5.2 - Masked Relationship ####
library(rethinking)
data("milk")
d <- milk

m5.5 <- rethinking::map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn * neocortex.perc,
    a ~ dnorm(0, 100),
    bn ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = d
)

dcc <- d[complete.cases(d), ]
dcc$log.mass <- log(dcc$mass)

pairs(~ kcal.per.g + log.mass + neocortex.perc, data=dcc)
# plot the prediction mean > for kcal.per.g
m5.5 <- rethinking::map(
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

# counter factual plot 
neocortex.perc_avg <- mean(dcc$neocortex.perc)
log.mass_seq <- seq(from = -4, to = 4, length.out = 100)

pred.data <- data.frame(neocortex.perc=neocortex.perc_avg, 
                        log.mass=log.mass_seq)
mu <- link(fit = m5.7, data = pred.data)

mu_mean <- mu %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(kcal.per.g=V1)
mu_pi <- mu %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
data_mu_ribbon <- bind_cols(mu_mean, mu_pi)
data_mu_ribbon$log.mass <- log.mass_seq

predicted <- sim(fit = m5.7, data = pred.data)

predicted_mean <- predicted %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(kcal.per.g=V1)
predicted <- predicted %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(PredLower=V1, PredUpper=V2)
data_predicted_ribbon <- bind_cols(predicted_mean, predicted)
data_predicted_ribbon$log.mass <- log.mass_seq

ggplot(data = dcc, aes(log.mass, kcal.per.g)) + 
  geom_point() +
  geom_ribbon(data =  data_predicted_ribbon, aes(x = log.mass, ymin=PredLower, ymax=PredUpper), fill = "grey80", alpha=2/3) + # 89% posterior prediction interval
  geom_ribbon(data =  data_mu_ribbon, aes(x = log.mass, ymin=Lower, ymax=Upper), fill = "grey60", alpha=2/3) + # 89% interval fot the mean
  geom_line(data = data_mu_ribbon, aes(x = log.mass, y = kcal.per.g))

# 5.3 When adding variables hurts ####
# 5.3.1 Multicollinear legs ####
n <- 100
height <- rnorm(n, 10, 2)
leg_prop <- runif(n, 0.4, 0.5)
leg_left <- leg_prop*height + rnorm(n, 0, 0.02)
leg_right <- leg_prop*height + rnorm(n, 0, 0.02)

d <- data.frame(height, leg_left, leg_right)

m5.8 <- rethinking::map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl * leg_left + br * leg_right,
    a ~ dnorm(10, 100),
    bl ~ dnorm(2, 10),
    br ~ dnorm(2, 10),
    sigma ~ dunif(0, 10)
  ),
  data = d
)
plot(precis(m5.8))

post <- extract.samples(m5.8)
ggplot(data = post, aes(bl, br)) + 
  geom_jitter()
ggplot(data = post, aes(x = bl+br)) +
  geom_density() +
  geom_vline(aes(xintercept=mean(bl+br)), color = "blue", linetype = "dashed")

m5.9 <- rethinking::map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl * leg_left,
    a ~ dnorm(10, 100),
    bl ~ dnorm(2, 10),
    sigma ~ dunif(0, 10)
  ),
  data = d
)
plot(precis(m5.9))

predicted <- sim(fit = m5.9)

predicted_mean <- predicted %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(predicted_height=V1)

d$predicted_height <- predicted_mean$predicted_height

ggplot(data = d, aes(x = height, y = predicted_height)) +
  geom_jitter() +
  geom_abline(slope = 1)

# 5.3.2 Multicollinear milk ####
data("milk")
d <- milk

corrplot::corrplot(cor(d %>% select(kcal.per.g, perc.fat, perc.lactose)))

apropos(what = "FindCorr")
pairs(~kcal.per.g + perc.fat + perc.lactose, data = d)

# kcal.per.g regressed on perc.fat
m5.10 <- rethinking::map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bf * perc.fat,
    a ~ dnorm(0.6, 10),
    bf ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d
)

# kcal.per.g regressed on perc.lactose
m5.11 <- rethinking::map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bl * perc.lactose,
    a ~ dnorm(0.6, 10),
    bl ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d
)
precis(m5.10, digits = 3)
precis(m5.11, digits = 3)

# kcal.per.g regressed on perc.fat and per.lactose
m5.12 <- rethinking::map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bf * perc.fat + bl *  perc.lactose,
    a ~ dnorm(0.6, 10),
    bf ~ dnorm(0, 1),
    bl ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d
)
precis(m5.12, digits = 3)

cor(d$perc.fat, d$perc.lactose)

# Post-treatment bias ####
n <- 100
h0 <- rnorm(n, 10, 2)
treatment <- rep(0:1, each=n/2)
fungus <- rbinom(n, size = 1, prob = 0.5 - treatment*0.4)
h1 <- h0 + rnorm(n, mean = 5-3*fungus)

d <- data.frame(
  h0 = h0,
  h1 = h1,
  treatment = treatment,
  fungus = fungus
)

m5.13 <- rethinking::map(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- a + bh * h0 + bt * treatment + bf * fungus,
    a ~ dnorm(0, 100),
    c(bh, bt, bf) ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ),
  data = d
)
plot(precis(m5.13))

m5.14 <- rethinking::map(
  alist(
    h1 ~ dnorm(mu, sigma),
    mu <- a + bh * h0 + bt * treatment,
    a ~ dnorm(0, 100),
    c(bh, bt) ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ),
  data = d
)
plot(precis(m5.14))

# para ver el efecto usando un modelo lineal
library(modelr)
mod1 <- lm(h1 ~ h0 + treatment, data=d)
grid <- d %>%
  data_grid(h0, treatment) %>% 
  add_predictions(mod1)
ggplot(data = d, aes(h0, h1, color=treatment))+
  geom_jitter() +
  geom_line(data = grid, aes(x= h0, y=pred, group=treatment))


# 5.4 Categorical Variables ####
rm(list=ls())
data("Howell1")
d <- Howell1

m5.15 <- rethinking::map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bm*male,
    a ~ dnorm(182, 100),
    bm ~ dnorm(0, 20),
    sigma ~ dunif(0, 50)
  ),
  data=d
)
plot(precis(m5.15))

post <- extract.samples(m5.15)  
mu_male <- post$a + post$bm
PI(mu_male)

m5.15b <- rethinking::map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- af*(1-male) + am*male,
    c(af, am) ~ dnorm(182, 100),
    sigma ~ dunif(0, 50)
  ),
  data=d
)
plot(precis(m5.15b))

# 5.4.2 Many categories ####
rm(list=ls())
data("milk")
d <- milk
unique(d$clade)

d <- d %>% 
  mutate(
    clade.NWM = case_when(
      clade == 'New World Monkey' ~ 1L,
      TRUE ~ 0L
    ),
    clade.OWM = case_when(
      clade == 'Old World Monkey' ~ 1L,
      TRUE ~ 0L
    ),
    clade.S = case_when(
      clade == 'Strepsirrhine' ~ 1L,
      TRUE ~ 0L
    )
  ) 

m5.16 <- rethinking::map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + b.NWM * clade.NWM + b.OWM * clade.OWM + b.S * clade.S,
    c(a, b.NWM, b.OWM, b.S) ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data=d
)
plot(precis(m5.16))

post <- extract.samples(m5.16)

mu.ape <- post$a
mu.NWM <- post$a + post$b.NWM
mu.OWM <- post$a + post$b.OWM
mu.S <- post$a + post$b.S

precis(data.frame(mu.ape, mu.NWM, mu.OWM, mu.S))

# difrerece between two groups
diff.NWM.OWM <- mu.NWM - mu.OWM
plot(diff.NWM.OWM)
quantile(diff.NWM.OWM, probs = c(0.025, 0.5, 0.975))
plot(density(diff.NWM.OWM))

# 5.4.4 another approach: unique intercepts ####
d$clade_id <- rethinking::coerce_index(d$clade)

m5.16_alt <- rethinking::map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a[clade_id],
    a[clade_id] ~ dnorm(0.6, 10),
    sigma ~ dunif(0, 10)
  ),
  data=d
)
precis(m5.16_alt, depth = 2)

# 5.5.3 Glimmer ####
rm(list=ls())
data(cars)
rethinking::glimmer(formula = dist ~ speed, data = cars)
















































