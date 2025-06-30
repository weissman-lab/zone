mu_healthy <- get_mu (model_healthy)
sigma_healthy <- get_sigma (model_healthy)
nu_healthy <- get_nu (model_healthy)

lln_healthy <- qBCCG (0.05, mu_healthy, sigma_healthy, nu_healthy)

mu_diseased <- get_mu (model_diseased_bcpe$model)
sigma_diseased <- get_sigma (model_diseased_bcpe$model)
nu_diseased <- get_nu (model_diseased_bcpe$model)
tau_diseased <- get_tau (model_diseased_bcpe$model)

uln_diseased <- qBCPE (0.95, mu_diseased, sigma_diseased, nu_diseased, tau_diseased)

prior_healthy <- model_diseased_bcpe$priors [1]
prior_diseased <- model_diseased_bcpe$priors [2]

nhanes_number <- nrow (nhanes)
nhanes_number_healthy <- nrow (nhanes_healthy)

uphs_number <- nrow (uphs)

uphs_age_median <- uphs %>% 
  pull (age) %>% 
  median ()

uphs_age_iqr <- uphs %>% 
  pull (age) %>% 
  IQR ()

uphs_women_number <- uphs %>% 
  filter (sex == 2) %>% 
  nrow ()

uphs_women_percentage <- uphs %>% 
  filter (sex == 2) %>% 
  nrow () %>%
  divide_by (nrow (uphs)) %>%
  multiply_by (100) %>%
  round (digits = 1) %>% 
  format (nsmall = 1)

uphs_white_number <- uphs %>% 
  filter (race == 1) %>% 
  nrow ()

uphs_white_percentage <- uphs %>% 
  filter (race == 1) %>%
  nrow () %>%
  divide_by (nrow (uphs)) %>%
  multiply_by (100) %>%
  round (digits = 1) %>% 
  format (nsmall = 1)

uphs_fev1_fvc_median <- uphs %>% 
  pull (fev1_fvc) %>%
  median () %>%
  round (digits = 2)
  
uphs_fev1_fvc_iqr <- uphs %>%   
  pull (fev1_fvc) %>%
  IQR () %>%
  round (digits = 2)

uphs_fev1_fvc_abnormal_number <- uphs %>% 
  filter (fev1_fvc < lln_healthy) %>%
  nrow ()

uphs_fev1_fvc_abnormal_percentage <- uphs %>% 
  filter (fev1_fvc < lln_healthy) %>%
  nrow () %>%
  divide_by (nrow (uphs)) %>%
  multiply_by (100) %>%
  round (digits = 1) %>% 
  format (nsmall = 1)

zone_upper <- qBCPE (
  0.99,
  mu = mu_diseased,
  sigma = sigma_diseased,
  nu = nu_diseased,
  tau = tau_diseased
) 

zone_lower <- qBCCG (
  0.01,
  mu = mu_healthy,
  sigma = sigma_healthy,
  nu = nu_healthy
)

uphs_in_zone_number <- uphs %>% 
  filter (fev1_fvc >= zone_lower & fev1_fvc <= zone_upper) %>% 
  nrow ()

uphs_in_zone_percentage <- uphs %>% 
  filter (fev1_fvc >= zone_lower & fev1_fvc <= zone_upper) %>% 
  nrow () %>% 
  divide_by (nrow (uphs)) %>%
  multiply_by (100) %>%
  round (digits = 1) %>% 
  format (nsmall = 1)

uphs_normal_in_zone_number <- uphs %>% 
  filter (fev1_fvc >= lln_healthy) %>% 
  filter (fev1_fvc >= zone_lower & fev1_fvc <= zone_upper) %>%
  nrow ()  

uphs_normal_in_zone_percentage <- uphs %>% 
  filter (fev1_fvc >= lln_healthy) %>% 
  filter (fev1_fvc >= zone_lower & fev1_fvc <= zone_upper) %>%
  nrow () %>% 
  divide_by (nrow (filter (uphs, fev1_fvc >= lln_healthy))) %>%
  multiply_by (100) %>%
  round (digits = 1) %>% 
  format (nsmall = 1)

uphs_abnormal_in_zone_number <- uphs %>% 
  filter (fev1_fvc < lln_healthy) %>% 
  filter (fev1_fvc >= zone_lower & fev1_fvc <= zone_upper) %>%
  nrow ()

uphs_abnormal_in_zone_percentage <- uphs %>% 
  filter (fev1_fvc < lln_healthy) %>% 
  filter (fev1_fvc >= zone_lower & fev1_fvc <= zone_upper) %>%
  nrow () %>% 
  divide_by (nrow (filter (uphs, fev1_fvc < lln_healthy))) %>%
  multiply_by (100) %>%
  round (digits = 1) %>% 
  format (nsmall = 1)

results <- paste (
  "Number of NHANES participants:",
  nhanes_number, "\n",
  "Number of healthy NHANES participants:",
  nhanes_number_healthy, "\n",  
  "Number of UPHS patients:",
  uphs_number, "\n",  
  "Median age of UPHS participants:",
  uphs_age_median, "\n",  
  "IQR of ages of UPHS participants:",
  uphs_age_iqr, "\n",  
  "Number of women among UPHS partients:",
  uphs_women_number, "\n",  
  "Percentage of women among UPHS patients:",
  uphs_women_percentage, "\n",  
  "Number of UPHS participants who were non-Hispanic and White:",
  uphs_white_number, "\n",
  "Percentage of UPHS participants who were non-Hispanic and White:",
  uphs_white_percentage, "\n",
  "Median FEV1/FVC among UPHS patients:",
  uphs_fev1_fvc_median, "\n",
  "IQR of FEV1/FVC values among UPHS patients:",
  uphs_fev1_fvc_iqr, "\n",
  "Mu healthy:",
  format (round (mu_healthy, digits = 2), nsmall = 2), "\n",
  "Sigma healthy:",
  format (round (sigma_healthy, digits = 2), nsmall = 2), "\n",
  "Nu healthy:",
  format (round (nu_healthy, digits = 2), nsmall = 2), "\n",
  "Healthy LLN:",
  format (round (lln_healthy, digits = 2), nsmall = 2), "\n",
  "Number of UPHS patients with an FEV1/FVC less than the LLN:",
  uphs_fev1_fvc_abnormal_number, "\n",
  "Percentage of UPHS patients with an FEV1/FVC less than the LLN:",
  uphs_fev1_fvc_abnormal_percentage, "\n",
  "Mu diseased:",
  format (round (mu_diseased, digits = 2), nsmall = 2), "\n",  
  "Sigma diseased:",
  format (round (sigma_diseased, digits = 2), nsmall = 2), "\n",
  "Nu diseased:",
  format (round (nu_diseased, digits = 2), nsmall = 2), "\n",
  "Tau diseased:",
  format (round (tau_diseased, digits = 2), nsmall = 2), "\n",
  "Prior healthy:",
  format (round (100 * prior_healthy, digits = 1), nsmall = 1), "\n",
  "Prior diseased:",
  format (round (100 * prior_diseased, digits = 1), nsmall = 1), "\n",
  "Zone of uncertainty lower limit:",
  format (round (zone_lower, digits = 2), nsmall = 2), "\n",
  "Zone of uncertainty upper limit:",
  format (round (zone_upper, digits = 2), nsmall = 2), "\n",
  "Number of UPHS patients in the zone of uncertainty:",
  uphs_in_zone_number, "\n",
  "Percentage of UPHS patients in zone of uncertainty:",
  uphs_in_zone_percentage, "\n",
  "Number of UPHS patients with a normal FEV1/FVC in the zone of uncertainty:",
  uphs_normal_in_zone_number, "\n",
  "Percentage of UPHS patients with a normal FEV1/FVC in the zone of uncertainty:",
  uphs_normal_in_zone_percentage, "\n",
  "Number of UPHS patients with an abnormal FEV1/FVC in the zone of uncertainty:",
  uphs_abnormal_in_zone_number, "\n",
  "Percentage of UPHS patients with an abnormal FEV1/FVC in the zone of uncertainty:",
  uphs_abnormal_in_zone_percentage, "\n"
)  

write_lines (results, "../results/results.txt")