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
















































