# Overfitting, Regularization, and Information Criteria --------------------------------------------------------------------------------
source(file = "scripts/basePackages.R")

d <- data.frame(
  species = c("afarensis", "africanus", "habilis", "boisei", "rudolfensis", "ergaster", "sapiens"),
  brain = c(438, 452, 612, 521, 752, 871, 1350), 
  mass = c(37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5)
)

plot_1 <- ggplot(data = d, aes(x = mass, y = brain, label = species)) +
  geom_point(color = "plum") +
  theme_classic() + 
  coord_cartesian(xlim = 30:65) +
  geom_text_repel(size = 3, color = "plum") +
  labs(x = "body mass (kg)", 
       y = "brain volume (cc)", 
       subtitle = "Average brain volume by body mass \nfor hominin species")

# 6.1.1 More parameters always improve fit ####
# firs degree
m6.1 <- lm(brain ~ mass, data=d)

mu <- link(m6.1, data = data.frame(mass=30:65))

mu_mean <- mu %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(brain=V1)
mu_pi <- mu %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
data_mu_ribbon <- bind_cols(mu_mean, mu_pi)
data_mu_ribbon$mass <- 30:65

plot_1 +
  geom_line(data = data_mu_ribbon, aes(x = mass, y = brain), inherit.aes = F) +
  geom_ribbon(data = data_mu_ribbon, aes(x = mass, ymin = Lower, ymax = Upper), fill = "grey80", alpha=1/3, inherit.aes = F) +
  labs(title=str_c("First Degree Model With R^2 = ", round(summary(m6.1)$r.squared, 2)))

# r^2
r_2 <- 1 - (var(m6.1$residuals) / var(d$brain))

# second degree
m6.2 <- lm(brain ~ mass + I(mass^2), data=d)

mu <- link(m6.2, data = data.frame(mass=30:65, "I(mass^2)"=(30:65)^2))

mu_mean <- mu %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(brain=V1)
mu_pi <- mu %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
data_mu_ribbon <- bind_cols(mu_mean, mu_pi)
data_mu_ribbon$mass <- 30:65

plot_1 +
  geom_line(data = data_mu_ribbon, aes(x = mass, y = brain), inherit.aes = F) +
  geom_ribbon(data = data_mu_ribbon, aes(x = mass, ymin = Lower, ymax = Upper), fill = "grey80", alpha=1/3, inherit.aes = F) +
  labs(title=str_c("Second Degree Model With R^2 = ", round(summary(m6.2)$r.squared, 2)))

# Third degree
m6.3 <- lm(brain ~ mass + I(mass^2) + I(mass^3), data=d)

mu <- link(m6.3, data = data.frame(mass=30:65, "I(mass^2)"=(30:65)^2, "I(mass^3)"=(30:65)^3))

mu_mean <- mu %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(brain=V1)
mu_pi <- mu %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
data_mu_ribbon <- bind_cols(mu_mean, mu_pi)
data_mu_ribbon$mass <- 30:65

plot_1 +
  geom_line(data = data_mu_ribbon, aes(x = mass, y = brain), inherit.aes = F) +
  geom_ribbon(data = data_mu_ribbon, aes(x = mass, ymin = Lower, ymax = Upper), fill = "grey80", alpha=1/3, inherit.aes = F) +
  labs(title=str_c("Third Degree Model With R^2 = ", round(summary(m6.3)$r.squared, 2)))

# Fifth degree
m6.5 <- lm(brain ~ mass + I(mass^2) + I(mass^3) + I(mass^4) + I(mass^5), data=d)

mu <- link(m6.5, data = data.frame(mass=30:65, "I(mass^2)"=(30:65)^2, "I(mass^3)"=(30:65)^3, "I(mass^4)"=(30:65)^4, "I(mass^5)"=(30:65)^5))

mu_mean <- mu %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(brain=V1)
mu_pi <- mu %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
data_mu_ribbon <- bind_cols(mu_mean, mu_pi)
data_mu_ribbon$mass <- 30:65

plot_1 +
  geom_line(data = data_mu_ribbon, aes(x = mass, y = brain), inherit.aes = F) +
  geom_ribbon(data = data_mu_ribbon, aes(x = mass, ymin = Lower, ymax = Upper), fill = "grey80", alpha=1/3, inherit.aes = F) +
  labs(title=str_c("Fifth Degree Model With R^2 = ", round(summary(m6.5)$r.squared, 2)))

# 6.1.2 too few parameters hurts, too ####
m6.7 <- lm(brain ~ 1, data = d)

mu <- link(m6.7, data = data.frame(mass=30:65))

mu_mean <- mu %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(brain=V1)
mu_pi <- mu %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
data_mu_ribbon <- bind_cols(mu_mean, mu_pi)
data_mu_ribbon$mass <- 30:65

plot_1 +
  geom_line(data = data_mu_ribbon, aes(x = mass, y = brain), inherit.aes = F) +
  geom_ribbon(data = data_mu_ribbon, aes(x = mass, ymin = Lower, ymax = Upper), fill = "grey80", alpha=1/3, inherit.aes = F) +
  labs(title=str_c("Underfit Model With R^2 = ", round(summary(m6.7)$r.squared, 2)))


# 6.2.2 information and uncertainity ####
# la probabilidad de que obtenga el trabajo en coehen es de .8.
p <- c(0.8, 0.2)
# the information entropy 
-sum(p*log(p)) # esto es la incertidumbre asociada a la probabilidad antes expuesta
# si las probabilidades cambian y se vuelve mas inseguro, entonces la incertidumbre asociada va a aumentar
p_2 <- c(0.5, 0.5)
-sum(p_2*log(p_2))

prob_hired <- data.frame(P_Hired = round(seq(from = 0, to = 1, length.out = 100), 2))

f_entropy <- function(p) {
  prob <- c(p, 1-p)
  return(-sum(prob*log(prob)))
}

prob_hired <- prob_hired %>% 
  mutate(
    InformationEntropy = map(P_Hired, f_entropy)
  ) %>% unnest()

ggplot(data=prob_hired, aes(x = P_Hired, y = InformationEntropy)) +
  geom_line() +
  labs(x = "Probabilidad de que me contraten (p)",
       y = "Incertidumbre asociada",
       subtitle = "La incertidumbre asociada a que me contraten \ndisminuye a medida que la probabilidad se define mejor.")

# 6.2.3 from entropy to accuracy ####
# divergence 
p_target <- c(0.5, 0.5)
q_model <- c(0.8, 0.2)

sum(p_target*log(p_target/q_model))

f_divergence <- function(p, q) {
  target <- c(p, 1-p)
  model <- c(q, 1-q)
  
  return(sum(target*log(target/model)))
}
f_divergence(p = .5, q = .9)

data_divergence <- data.frame(Target=rep(0.8, 100),
                              Model = round(seq(from = 0, to = 1, length.out = 100), 2))
data_divergence <-data_divergence %>% 
  mutate(
    Divergence = map2(Target, Model, f_divergence)
  ) %>% unnest()

ggplot(data=data_divergence, aes(x = Model, y = Divergence)) +
  geom_line() +
  labs(x = "Probabilidad de que me contraten (q)",
       y = "Divergencia con el modelo real",
       subtitle = "No hay divergencia cuando p=q, \npero aumenta a medida que el modelo se aleja del target")

f_divergence(p = 0.01, q = 0.7) # from earth to mars 
f_divergence(p = 0.7, q = 0.01) # from mars to earth 

f_entropy(0.01) # model with low entropy (earth)
f_entropy(0.7) # model with hhigh entropy (earth)

# 6.2.4 from divergence to deviance ####
(-2)*logLik(object = m6.1)

# computing deviance
d$mass_s <- as.vector(scale(d$mass))
m6.8 <- rethinking::map(
  alist(
    brain ~ dnorm(mu, sigma),
    mu <- a + b*mass_s
  ),
  data = d, 
  start = list(a=mean(d$brain), b=0, sigma=sd(d$brain)), 
  method = "Nelder-Mead"
)
precis(m6.8)
theta <- coef(m6.8)

(-2)*(sum(
  dnorm(d$brain, 
        mean = theta[1]+theta[2]*d$mass_s, 
        sd = theta[3], 
        log = T)
  ))

# 6.2.5 From deviance to out-of-sample ####
n <- 20
kseq <- 1:5
?sim.train.test()

sapply(kseq, function(k) {
  print(k);
  r <- replicate(100, rethinking::sim.train.test(N = n, k));
  c(mean(r[1,]), mean(r[2,]), sd(r[1,]), sd(r[2,]))
})

# pararllel
cl <- parallel::makeCluster(7)
doParallel::registerDoParallel(cl)

dev <- foreach(k=kseq, .packages = "rethinking", .combine = "rbind") %dopar% {
  r <- replicate(100, rethinking::sim.train.test(N = n, k));
  c(mean(r[1,]), mean(r[2,]), sd(r[1,]), sd(r[2,]))
}

getDoParWorkers()
stopCluster(cl)

plot(1:5, 
     dev[, 1], 
     ylim=c(min(dev[1:2,])-5, max(dev[1:2,])+10), 
     xlim=c(1,5.1),
     xlab="Number of parameters",
     ylab="deviance (red:out, black:in)"
     )
points((1:5)+0.1, 
       dev[,2], col="red")

# 6.3 Regularization ####
curve(dnorm(x), from = -5, to = 5)
curve(dnorm(x, mean = 0, sd = 100), from = -5, to = 5)
curve(dnorm(x, mean = 0, sd = 0.2), from = -5, to = 5)

# 6.4 Information Criteria ####
# WAIC calculations
data(cars)
m <- rethinking::map(
  alist(
    dist ~ dnorm(mu, sigma),
    mu <- a + b*speed,
    a ~ dnorm(0, 100),
    b ~ dnorm(0, 10),
    sigma ~ dunif(0, 30)
  ),
  data=cars
)
precis(m)
post <- extract.samples(m)

n_samples <- 1000
log_likelohood <- sapply(1:n_samples, 
                         function(s) {
                           mu <- post$a + post$b*cars$speed
                           dnorm(cars$dist, mu, post$sigma[s], log = T)
                         })
n_cases <- nrow(cars)
lppd <- sapply(1:n_cases, 
               function(i) log_sum_exp(log_likelohood[i,] - log(n_samples)))

pWAIC <- sapply(1:n_cases, 
                function(i) var(log_likelohood[i,]))

-2 * (sum(lppd) - sum(pWAIC))
WAIC(m, pointwise = T)
WAIC(m)

# 6.5.1 Using Information Criteria >>> Model Comparison ####
data(milk)
d <- milk[complete.cases(milk),]
d$neocortex <- d$neocortex.perc/100

a.start <- mean(d$kcal.per.g)
sigma.start <- log(sd(d$kcal.per.g))

m6.11 <- rethinking::map(
  alist(
    kcal.per.g ~ dnorm(a, exp(log.sigma))
  ),
  data=d, start = list(a=a.start, log.sigma=sigma.start)
)
m6.12 <- rethinking::map(
  alist(
    kcal.per.g ~ dnorm(mu, exp(log.sigma)),
    mu <- a + bn*neocortex
  ),
  data=d, start = list(a=a.start, bn=0, log.sigma=sigma.start)
)
m6.13 <- rethinking::map(
  alist(
    kcal.per.g ~ dnorm(mu, exp(log.sigma)),
    mu <- a + bm*log(mass)
  ),
  data=d, start = list(a=a.start, bm=0, log.sigma=sigma.start)
)
m6.14 <- rethinking::map(
  alist(
    kcal.per.g ~ dnorm(mu, exp(log.sigma)),
    mu <- a + bn*neocortex + bm*log(mass)
  ),
  data=d, start = list(a=a.start, bn=0, bm=0, log.sigma=sigma.start)
)

milk.models <- compare(m6.11, m6.12, m6.13, m6.14)
plot(milk.models)

sum(rnorm(1e5, 7.1, 7.05)<0)/1e5

# 6.5.1.2 comparing estimates ####
coeftab(m6.11, m6.12, m6.13, m6.14)
plot(coeftab(m6.11, m6.12, m6.13, m6.14))


# 6.5.2 Model Averaging ####
mu <- link(m6.14, 
           data = data.frame(
             neocortex = seq(from = 0.5, to = 0.8, length.out = 30),
             mass = rep(mean(d$mass), 30)
           ))

mu_mean <- mu %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(kcal.per.g=V1)
mu_pi <- mu %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
data_mu_ribbon <- bind_cols(mu_mean, mu_pi)
data_mu_ribbon$neocortex <- seq(from = 0.5, to = 0.8, length.out = 30)


milk.ensamble <- ensemble(m6.11, m6.12, m6.13, m6.14, 
                          data = data.frame(
                            neocortex = seq(from = 0.5, to = 0.8, length.out = 30),
                            mass = rep(mean(d$mass), 30)
                          ))
ensable_mu_mean <- milk.ensamble$link %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(kcal.per.g=V1)
ensable_mu_pi <- milk.ensamble$link %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
ensable_data_mu_ribbon <- bind_cols(ensable_mu_mean, ensable_mu_pi)
ensable_data_mu_ribbon$neocortex <- seq(from = 0.5, to = 0.8, length.out = 30)


ggplot(data = d, aes(x = neocortex, y = kcal.per.g)) +
  geom_point(color = "plum") +
  theme_classic() +
  labs(x = "neocortex", 
       y = "kcal.per.g") +
  coord_cartesian(ylim = c(0.4, 1),
                  xlim = c(0.55, 0.75)) + 
  geom_line(data = data_mu_ribbon, aes(x = neocortex, y = kcal.per.g), inherit.aes = F) +
  geom_ribbon(data = data_mu_ribbon, aes(x = neocortex, ymin = Lower, ymax = Upper), fill = "grey50", alpha=1/3, inherit.aes = F) +
  geom_line(data = ensable_data_mu_ribbon, aes(x = neocortex, y = kcal.per.g), inherit.aes = F, colour = "red") +
  geom_ribbon(data = ensable_data_mu_ribbon, aes(x = neocortex, ymin = Lower, ymax = Upper), fill = "grey90", alpha=1/3, inherit.aes = F)


























