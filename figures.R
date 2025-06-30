################################################################################
## Figure 1
################################################################################

color_diseased <- "#B24745FF"
color_healthy <- "#79AF97FF"
color_uncertain <- "#FDD262"

# Figure 1a

n <- nrow (uphs)

fev1_fvc <- pull (uphs, fev1_fvc)

healthy <- dBCCG (
  fev1_fvc,
  mu = mu_healthy,
  sigma = sigma_healthy,
  nu = nu_healthy
)

diseased <- dBCCG (
  fev1_fvc,
  mu = 0.62,
  sigma = 0.1,
  nu = 5
)

data <- tibble (
  fev1_fvc = fev1_fvc,
  healthy = healthy * 0.02 * n * 0.6,
  diseased = diseased * 0.02 * n * 0.4
)

data <- data %>% 
  mutate (healthy_min = case_when (
    diseased >= healthy ~ 0,
    healthy > diseased ~ diseased)
  ) %>% 
  mutate (healthy_max = case_when (
    diseased >= healthy ~ 0,
    healthy > diseased ~ healthy)
  ) %>% 
  mutate (diseased_min = case_when (
    healthy >= diseased ~ 0,
    diseased > healthy ~ healthy)
  ) %>% 
  mutate (diseased_max = case_when (
    healthy >= diseased ~ 0,
    diseased > healthy ~ diseased)
  ) %>% 
  mutate (uncertain_min = 0) %>% 
  mutate (uncertain_max = case_when (
    healthy >= diseased ~ diseased,
    diseased > healthy ~ healthy)
  )

figure_1a <- data %>%
  ggplot +
  geom_line (aes (fev1_fvc, healthy)) +
  geom_ribbon (aes (fev1_fvc, ymax = healthy_max, ymin = healthy_min, fill = "Healthy"), alpha = 0.5) +
  geom_line (aes (fev1_fvc, diseased)) +
  geom_ribbon (aes (fev1_fvc, ymax = diseased_max, ymin = diseased_min, fill = "Diseased"), alpha = 0.5) +
  geom_ribbon (aes (fev1_fvc, ymax = uncertain_max, ymin = uncertain_min, fill = "Uncertain"), alpha = 0.5) +
  theme_classic (base_size = 10) +
  scale_fill_manual (
    name = "",
    breaks = c (
      "Healthy",
      "Uncertain",
      "Diseased"
    ),
    values = c (
      color_healthy,
      color_uncertain,
      color_diseased
    ),
    labels = c (
      "Healthy",
      "Uncertain",
      "Diseased"
    )
  ) +
  scale_x_continuous (
    expression ("FEV"["1"] * "/FVC"),
    limits = c (min (fev1_fvc), 1.0),
    breaks = NULL,
    labels = NULL
  ) +
  scale_y_continuous (
    "",
    limits = c (0, 4000),
    breaks = NULL,
    labels = NULL,
    #breaks = c (0, 1000, 2000, 3000, 4000),
    #labels = c ("", "", "", "", "")
    #labels = c ("0", "1000", "2000", "3000", "4000")
  ) +
  guides (fill = guide_legend (override.aes = list (alpha = 0.5))) +
  geom_segment (x = min (fev1_fvc), y = 0, xend = 1.0, yend = 0)


ggsave ("../figures/figure-1a.png", width = 15, height = 10, units = "cm")

# Figure 1b

n <- nrow (uphs)

fev1_fvc <- pull (uphs, fev1_fvc)

healthy <- dBCCG (
  fev1_fvc,
  mu = mu_healthy,
  sigma = sigma_healthy,
  nu = nu_healthy
)

diseased <- dBCPE (
  fev1_fvc,
  mu = mu_diseased,
  sigma = sigma_diseased,
  nu = nu_diseased,
  tau = tau_diseased
)

data <- tibble (
  fev1_fvc = fev1_fvc,
  healthy = healthy * 0.02 * n * prior_healthy,
  diseased = diseased * 0.02 * n * prior_diseased
)

data <- data %>% 
  mutate (healthy_min = case_when (
    diseased >= healthy ~ 0,
    healthy > diseased ~ diseased)
  ) %>% 
  mutate (healthy_max = case_when (
    diseased >= healthy ~ 0,
    healthy > diseased ~ healthy)
  ) %>% 
  mutate (diseased_min = case_when (
    healthy >= diseased ~ 0,
    diseased > healthy ~ healthy)
  ) %>% 
  mutate (diseased_max = case_when (
    healthy >= diseased ~ 0,
    diseased > healthy ~ diseased)
  ) %>% 
  mutate (uncertain_min = 0) %>% 
  mutate (uncertain_max = case_when (
    healthy >= diseased ~ diseased,
    diseased > healthy ~ healthy)
  )

figure_1b <- data %>%
  ggplot +
  geom_histogram (aes (x = fev1_fvc), color = "black", fill = "white", binwidth = 0.02) +
  geom_line (aes (fev1_fvc, healthy)) +
  geom_ribbon (aes (fev1_fvc, ymax = healthy_max, ymin = healthy_min, fill = "Healthy"), alpha = 0.5) +
  geom_line (aes (fev1_fvc, diseased)) +
  geom_ribbon (aes (fev1_fvc, ymax = diseased_max, ymin = diseased_min, fill = "Diseased"), alpha = 0.5) +
  geom_ribbon (aes (fev1_fvc, ymax = uncertain_max, ymin = uncertain_min, fill = "Uncertain"), alpha = 0.5) +
  xlab (expression ("FEV"["1"] * "/FVC")) +
  theme_classic (base_size = 10) +
  scale_fill_manual (
    name = "",
    breaks = c (
      "Healthy",
      "Uncertain",
      "Diseased"
    ),
    values = c (
      color_healthy,
      color_uncertain,
      color_diseased
    ),
    labels = c (
      "Healthy",
      "Uncertain",
      "Diseased"
    )
  ) +
  scale_y_continuous (
    "Number of Patients",
    limits = c (0, 4000),
    breaks = c (0, 1000, 2000, 3000, 4000),
    labels = c ("0", "1000", "2000", "3000", "4000")
  ) +
  guides (fill = guide_legend (override.aes = list (alpha = 0.5)))  


ggsave ("../figures/figure-1b.png", width = 15, height = 10, units = "cm")

figure_1 <- plot_grid (
  figure_1a + theme (legend.position = "none"),
  figure_1b + theme (legend.position = "none"),
  align = "vh",
  labels = c ("A", "B"),
  label_size = 10,
  nrow = 2
)

legend = get_legend (figure_1a)

plot_grid (figure_1, legend, ncol = 2, rel_widths = c (0.8, 0.2), label_size = 50)

ggsave ("../figures/figure-1.png", width = 15, height = 20, units = "cm")
