# Interactions --------------------------------------------------------------------------------
source(file = "scripts/basePackages.R")

# 7.1 Building an interaction ####
data("rugged")
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)

dd <- d[complete.cases(d$rgdppc_2000), ]

dd <- dd %>%
  mutate(
    country_ggrepel=case_when(
      country=='Argentina' ~ 'Argentina',
      country=='Germany' ~ 'Germany',
      TRUE ~ ''
    )
  )

# Africa ####
d.A1 <- dd %>% 
  filter(cont_africa==1)

plot_africa <- ggplot(data = d.A1, aes(x = rugged, y = log_gdp)) +
  geom_point(color = "plum") +
  theme_classic() + 
  labs(x = "rugged", 
       y = "log gdp", subtitle = "Africa")

m7.1 <- rethinking::map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged,
    a ~ dnorm(8, 10),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), 
  data = d.A1
)

mu <- link(m7.1, data = data.frame(rugged=seq(from = 0, to = 8, length.out = 100)))

mu_mean <- mu %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(log_gdp=V1)
mu_pi <- mu %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
data_mu_ribbon <- bind_cols(mu_mean, mu_pi)
data_mu_ribbon$rugged <- seq(from = 0, to = 8, length.out = 100)

plot_africa +
  geom_line(data = data_mu_ribbon, aes(x = rugged, y = log_gdp), inherit.aes = F) +
  geom_ribbon(data = data_mu_ribbon, aes(x = rugged, ymin = Lower, ymax = Upper), fill = "grey80", alpha=1/3, inherit.aes = F)

# Not Africa ####
d.A0 <- dd %>% 
  filter(cont_africa==0) %>% 
  mutate(
    country_ggrepel=case_when(
      country=='Argentina' ~ 'Argentina',
      country=='Germany' ~ 'Germany',
      TRUE ~ ''
    )
  )

plot_not_africa <- ggplot(data = d.A0, aes(x = rugged, y = log_gdp, label = country_ggrepel)) +
  geom_point(color = "plum") +
  theme_classic() + 
  geom_text_repel(size = 3, color = "plum") +
  labs(x = "rugged", 
       y = "log gdp", subtitle = "not Africa")

m7.2 <- rethinking::map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged,
    a ~ dnorm(8, 10),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ), 
  data = d.A0
)
mu <- link(m7.2, data = data.frame(rugged=seq(from = 0, to = 8, length.out = 100)))

mu_mean <- mu %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(log_gdp=V1)
mu_pi <- mu %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
data_mu_ribbon <- bind_cols(mu_mean, mu_pi)
data_mu_ribbon$rugged <- seq(from = 0, to = 8, length.out = 100)

plot_not_africa +
  geom_line(data = data_mu_ribbon, aes(x = rugged, y = log_gdp), inherit.aes = F) +
  geom_ribbon(data = data_mu_ribbon, aes(x = rugged, ymin = Lower, ymax = Upper), fill = "grey80", alpha=1/3, inherit.aes = F)


plot(coeftab(m7.1, m7.2))

# 7.1.1 Adding a dummy variable doesn't work ####
dd <- dd %>% 
  mutate(
    country_ggrepel=case_when(
      country=='Argentina' ~ 'Argentina',
      country=='Germany' ~ 'Germany',
      TRUE ~ ''
    )
  )

plot_all <- ggplot(data = dd, 
                   aes(x = rugged, 
                       y = log_gdp, 
                       colour = factor(cont_africa), 
                       label = country_ggrepel)) +
  guides(colour = guide_legend(title = "Africa")) +
  geom_point() +
  geom_text_repel(size = 2) +
  theme_classic() + 
  labs(x = "rugged", 
       y = "log gdp", subtitle = "All")

# todo junto 
m7.3 <- rethinking::map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged,
    a ~ dnorm(8, 10),
    bR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = dd
)

mu <- link(m7.3, data = data.frame(rugged=seq(from = 0, to = 8, length.out = 100)))

mu_mean <- mu %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(log_gdp=V1)
mu_pi <- mu %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
data_mu_ribbon <- bind_cols(mu_mean, mu_pi)
data_mu_ribbon$rugged <- seq(from = 0, to = 8, length.out = 100)

plot_all +
  geom_line(data = data_mu_ribbon, aes(x = rugged, y = log_gdp), inherit.aes = F) +
  geom_ribbon(data = data_mu_ribbon, aes(x = rugged, ymin = Lower, ymax = Upper), fill = "grey80", alpha=1/3, inherit.aes = F)

# agregar variable dummy de continente
m7.4 <- rethinking::map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + bR*rugged + bA*cont_africa,
    a ~ dnorm(8, 10),
    bR ~ dnorm(0, 1),
    bA ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = dd
)

mu_africa <- link(m7.4, data = 
                    data.frame(
                      rugged=seq(from = 0, to = 8, length.out = 100), 
                      cont_africa = 1))

mu_africa_mean <- mu_africa %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(log_gdp=V1)
mu_africa_pi <- mu_africa %>% as.tibble() %>% map(PI, prob = 0.97) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
data_mu_africa_ribbon <- bind_cols(mu_africa_mean, mu_africa_pi)
data_mu_africa_ribbon$rugged <- seq(from = 0, to = 8, length.out = 100)

mu_not_africa <- link(m7.4, data = 
                    data.frame(
                      rugged=seq(from = 0, to = 8, length.out = 100), 
                      cont_africa = 0))

mu_not_africa_mean <- mu_not_africa %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(log_gdp=V1)
mu_not_africa_pi <- mu_not_africa %>% as.tibble() %>% map(PI, prob = 0.97) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
data_mu_not_africa_ribbon <- bind_cols(mu_not_africa_mean, mu_not_africa_pi)
data_mu_not_africa_ribbon$rugged <- seq(from = 0, to = 8, length.out = 100)


plot_all +
  geom_line(data = data_mu_africa_ribbon, aes(x = rugged, y = log_gdp), inherit.aes = F, color="blue") +
  geom_ribbon(data = data_mu_africa_ribbon, aes(x = rugged, ymin = Lower, ymax = Upper), fill = "grey80", alpha=1/3, inherit.aes = F) +
  annotate(geom = "text", x = 6, y = 7, label = "Africa") +
  geom_line(data = data_mu_not_africa_ribbon, aes(x = rugged, y = log_gdp), inherit.aes = F, color="red") +
  geom_ribbon(data = data_mu_not_africa_ribbon, aes(x = rugged, ymin = Lower, ymax = Upper), fill = "grey80", alpha=1/3, inherit.aes = F) +
  annotate(geom = "text", x = 6, y = 8.2, label = "Not Africa")


compare(m7.4, m7.3)
plot(coeftab(m7.4, m7.3))

# 7.1.2 Adding a linear interaction doesnt work ####
m7.5 <- rethinking::map(
  alist(
    log_gdp ~ dnorm(mu, sigma),
    mu <- a + gamma*rugged + bA*cont_africa,
    gamma <- bR + bAR*cont_africa,
    a ~ dnorm(8, 10), 
    bA ~ dnorm(0, 1),
    bR ~ dnorm(0, 1),
    bAR ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = dd
)
plot(precis(m7.5))

compare(m7.5, m7.4, m7.3)
plot(coeftab(m7.5, m7.4, m7.3))

# plootting the interaction
# africa
mu_inter_africa <- link(m7.5, 
                        data = data.frame(
                          rugged=seq(from = 0, to = 8, length.out = 100),
                          cont_africa=1))

mu_inter_mean <- mu_inter_africa %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(log_gdp=V1)
mu_inter_pi <- mu_inter_africa %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
data_mu_inter_ribbon <- bind_cols(mu_inter_mean, mu_inter_pi)
data_mu_inter_ribbon$rugged <- seq(from = 0, to = 8, length.out = 100)


plot_africa +
  geom_line(data = data_mu_inter_ribbon, aes(x = rugged, y = log_gdp), inherit.aes = F) +
  geom_ribbon(data = data_mu_inter_ribbon, aes(x = rugged, ymin = Lower, ymax = Upper), fill = "grey80", alpha=1/3, inherit.aes = F)

# not africa 
mu_inter_not_africa <- link(m7.5, 
                        data = data.frame(
                          rugged=seq(from = 0, to = 8, length.out = 100),
                          cont_africa=0))

mu_inter_not_mean <- mu_inter_not_africa %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(log_gdp=V1)
mu_inter_not_pi <- mu_inter_not_africa %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
data_mu_inter_not_ribbon <- bind_cols(mu_inter_not_mean, mu_inter_not_pi)
data_mu_inter_not_ribbon$rugged <- seq(from = 0, to = 8, length.out = 100)


plot_not_africa +
  geom_line(data = data_mu_inter_not_ribbon, aes(x = rugged, y = log_gdp), inherit.aes = F) +
  geom_ribbon(data = data_mu_inter_not_ribbon, aes(x = rugged, ymin = Lower, ymax = Upper), fill = "grey80", alpha=1/3, inherit.aes = F)


post <- extract.samples(m7.5)

gamma_africa <- post$bR + post$bAR*1
gamma_not_africa <- post$bR + post$bAR*0

ggplot() + aes(gamma_africa) + 
  geom_density() +
  geom_vline(xintercept = mean(gamma_africa)) +
  labs(title="Gamma density for africa", subtitle=str_c("Media: ", round(mean(gamma_africa), 4)))

ggplot() + aes(gamma_not_africa) + 
  geom_density() +
  geom_vline(xintercept = mean(gamma_not_africa)) +
  labs(title="Gamma density for not africa", subtitle=str_c("Media: ", round(mean(gamma_not_africa), 4)))

ggplot() +
  geom_density(mapping = aes(gamma_not_africa)) +
  geom_density(mapping = aes(gamma_africa), col="blue") +
  labs(title="Gamma density for not africa and africa")

# de cuanto es la probabilidad de que la pendiente de africa sea menor que la pendiente del resto del mundo?
diff <- gamma_africa - gamma_not_africa
(sum(diff < 0) / length(diff))


# 7.2.2 Africa depends upon ruggedness ####
mu_rugged_min <- link(m7.5,
                      data = data.frame(
                        rugged=range(dd$rugged)[[1]],
                        cont_africa=0:1))

mu_rugged_min_mean <- mu_rugged_min %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(log_gdp=V1)
mu_rugged_min_pi <- mu_rugged_min %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
mu_rugged_min_ribbon <- bind_cols(mu_rugged_min_mean, mu_rugged_min_pi)
mu_rugged_min_ribbon$cont_africa <- 0:1

mu_rugged_max <- link(m7.5,
                      data = data.frame(
                        rugged=range(dd$rugged)[[2]],
                        cont_africa=0:1))

mu_rugged_max_mean <- mu_rugged_max %>% as.tibble() %>% map(mean) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(log_gdp=V1)
mu_rugged_max_pi <- mu_rugged_max %>% as.tibble() %>% map(PI) %>% bind_rows() %>% t() %>% as.tibble() %>% rename(Lower=V1, Upper=V2)
mu_rugged_max_ribbon <- bind_cols(mu_rugged_max_mean, mu_rugged_max_pi)
mu_rugged_max_ribbon$cont_africa <- 0:1


ggplot(data = dd,
       aes(x = cont_africa,
           y = log_gdp,
           label = country_ggrepel)) +
  geom_jitter(aes(colour=rugged>median(rugged))) +
  geom_text_repel(size = 2) +
  theme_classic() + 
  labs(x = "Continent", 
       y = "log gdp") +
  geom_line(data = mu_rugged_min_ribbon, aes(x = cont_africa, y = log_gdp), inherit.aes = F) +
  geom_ribbon(data = mu_rugged_min_ribbon, aes(x = cont_africa, ymin = Lower, ymax = Upper), fill = "grey80", alpha=1/3, inherit.aes = F) +
  geom_line(data = mu_rugged_max_ribbon, aes(x = cont_africa, y = log_gdp), inherit.aes = F, color="blue") +
  geom_ribbon(data = mu_rugged_max_ribbon, aes(x = cont_africa, ymin = Lower, ymax = Upper), fill = "grey80", alpha=1/3, inherit.aes = F)

# 7.3 Continuous interactions ----------------------------------------

# the Data 

data("tulips")

m7.6 <- rethinking::map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW*water + bS*shade,
    a ~ dnorm(0, 100), 
    bW ~ dnorm(0, 100), 
    bS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100) 
  ), 
  data=tulips, 
  method = "Nelder-Mead", 
  control=list(maxit=1e4)
)

m7.7 <- rethinking::map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW*water + bS*shade + bWS*water*shade,
    a ~ dnorm(0, 100), 
    bW ~ dnorm(0, 100), 
    bS ~ dnorm(0, 100),
    bWS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100) 
  ), 
  data=tulips,
  method = "Nelder-Mead", 
  control=list(maxit=1e4)
)
plot(coeftab(m7.6, m7.7))
coeftab(m7.6, m7.7)
compare(m7.6, m7.7)

# 7.3.3 centering and re-estimating --------------------

tulips$water.c <- as.integer(scale(x = tulips$water, center = T, scale = F))
tulips$shade.c <- as.integer(scale(x = tulips$shade, center = T, scale = F))

m7.8 <- rethinking::map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW*water.c + bS*shade.c,
    a ~ dnorm(0, 100), 
    bW ~ dnorm(0, 100), 
    bS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100) 
  ), 
  data=tulips, 
  start=list(a=mean(tulips$blooms), bW=0, bS=0, sigma=sd(tulips$blooms))
)

m7.9 <- rethinking::map(
  alist(
    blooms ~ dnorm(mu, sigma),
    mu <- a + bW*water.c + bS*shade.c + bWS*water.c*shade.c,
    a ~ dnorm(0, 100), 
    bW ~ dnorm(0, 100), 
    bS ~ dnorm(0, 100),
    bWS ~ dnorm(0, 100),
    sigma ~ dunif(0, 100) 
  ), 
  data=tulips,
  start=list(a=mean(tulips$blooms), bW=0, bS=0, bWS=0, sigma=sd(tulips$blooms))
)
plot(coeftab(m7.8, m7.9))
coeftab(m7.8, m7.9)
compare(m7.8, m7.9)
precis(m7.9)

# a is the expected value of blooms when water and shade are at the average values

# bW is the expected change in blooms when water increases by one UNIT and shade is at its average value (of zero). This parameters
# does not tell you the expected rate of change foe any other value of shade. 

# bS is the expected change in bloombs when shade increases by one UNIT and water is at its average value. 

# bWS is the intraction effect. First, the estimate tells us the expected change in the influence of water on bloombs when increasing shade
# by one unit. Second, it tells us the expected change in the influence of shade on bloombs when increasing water by one unit. 

# 7.3.4 Plotting implied predictions --------------------

for_tryptych <- function(var) {
  mu <- link(m7.9, data = data.frame(shade.c=-1:1, water.c=var))
  
  for_invoke <- function(func) {
    mu %>% as.tibble() %>% map_df(func) %>% t() %>% as.tibble()
  }
  mu <- invoke_map_dfc(for_invoke, list(list(mean), list(PI)))
  
  mu <- mu %>% rename(blooms=V1, Lower=V11, Upper=V2) %>% add_column(shade.c=-1:1, water.c=var)
  
  return(mu)
}

data_7.9 <- purrr::map_dfr(as.list(-1:1), for_tryptych)

ggplot(data = tulips,
       aes(x = shade.c,
           y = blooms)) +
  geom_point() +
  geom_line(data = data_7.9, aes(x = shade.c, y = blooms), inherit.aes = F) +
  facet_grid(~water.c) +
  geom_ribbon(data = data_7.9, aes(x = shade.c, ymin = Lower, ymax = Upper), fill = "grey80", alpha=1/3, inherit.aes = F) +
  facet_grid(cols = vars(water.c))+
  ggtitle("Triptych", subtitle = "Predicted blooms across water tratments with interaction effect")

# 7.4 Interactions in design formulas --------------------

x <- z <- w <- 1
colnames(model.matrix(~x*z*w)) 
  
  



























