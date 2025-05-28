
################################################################################
#### NHANES 2007-2008 - "E" ####
################################################################################

BIOPRO_E <- as_tibble (read.xport ("../data/BIOPRO_E.xpt"))
BMX_E <- as_tibble (read.xport ("../data/BMX_E.xpt"))
BPX_E <- as_tibble (read.xport ("../data/BPX_E.xpt"))
BPQ_E <- as_tibble (read.xport ("../data/BPQ_E.xpt"))
CBC_E <- as_tibble (read.xport ("../data/CBC_E.xpt"))
CDQ_E <- as_tibble (read.xport ("../data/CDQ_E.xpt"))
DEMO_E <- as_tibble (read.xport ("../data/DEMO_E.xpt"))
DIQ_E <- as_tibble (read.xport ("../data/DIQ_E.xpt"))
ENX_E <- as_tibble (read.xport ("../data/ENX_E.xpt"))
GHB_E <- as_tibble (read.xport ("../data/GHB_E.xpt"))
HDL_E <- as_tibble (read.xport ("../data/HDL_E.xpt"))
HIQ_E <- as_tibble (read.xport ("../data/HIQ_E.xpt"))
HSQ_E <- as_tibble (read.xport ("../data/HSQ_E.xpt"))
HUQ_E <- as_tibble (read.xport ("../data/HUQ_E.xpt"))
INQ_E <- as_tibble (read.xport ("../data/INQ_E.xpt"))
KIQ_U_E <- as_tibble (read.xport ("../data/KIQ_U_E.xpt"))
MCQ_E <- as_tibble (read.xport ("../data/MCQ_E.xpt"))
PFQ_E <- as_tibble (read.xport ("../data/PFQ_E.xpt"))
RDQ_E <- as_tibble (read.xport ("../data/RDQ_E.xpt"))
SMQ_E <- as_tibble (read.xport ("../data/SMQ_E.xpt"))
SMQRTU_E <- as_tibble (read.xport ("../data/SMQRTU_E.xpt"))
SPX_E <- as_tibble (read.xport ("../data/SPX_E.xpt"))
TCHOL_E <- as_tibble (read.xport ("../data/TCHOL_E.xpt"))
TRIGLY_E <- as_tibble (read.xport ("../data/TRIGLY_E.xpt"))

nhanes_2007_2008 <- DEMO_E %>% 
  left_join (BIOPRO_E, by = "SEQN") %>% 
  left_join (BMX_E, by = "SEQN") %>%
  left_join (BPQ_E, by = "SEQN") %>% 
  left_join (BPX_E, by = "SEQN") %>% 
  left_join (CBC_E, by = "SEQN") %>% 
  left_join (CDQ_E, by = "SEQN") %>%
  left_join (DIQ_E, by = "SEQN") %>% 
  left_join (ENX_E, by = "SEQN") %>%
  left_join (GHB_E, by = "SEQN") %>% 
  left_join (HDL_E, by = "SEQN") %>% 
  left_join (HIQ_E, by = "SEQN") %>% 
  left_join (HSQ_E, by = "SEQN") %>% 
  left_join (HUQ_E, by = "SEQN") %>% 
  left_join (INQ_E, by = "SEQN") %>%
  left_join (KIQ_U_E, by = "SEQN") %>%
  left_join (MCQ_E, by = "SEQN") %>% 
  left_join (PFQ_E, by = "SEQN") %>% 
  left_join (RDQ_E, by = "SEQN") %>% 
  left_join (SMQ_E, by = "SEQN") %>% 
  left_join (SMQRTU_E, by = "SEQN") %>% 
  left_join (SPX_E, by = "SEQN") %>% 
  left_join (TCHOL_E, by = "SEQN") %>% 
  left_join (TRIGLY_E, by = "SEQN")

rm (DEMO_E, BIOPRO_E, BMX_E, BPQ_E, BPX_E, CBC_E, CDQ_E, DIQ_E, ENX_E, GHB_E,
  HDL_E, HIQ_E, HSQ_E, HUQ_E, INQ_E, KIQ_U_E, MCQ_E, PFQ_E, RDQ_E, SMQ_E,
  SMQRTU_E, SPX_E, TCHOL_E, TRIGLY_E)

nhanes_2007_2008 <- nhanes_2007_2008 %>% 
  select (
    id = SEQN, # ID number
    weight = WTMEC2YR, # 2 year MEC Exam Weight
    age = RIDAGEYR, # Age in years  
    sex = RIAGENDR, # Sex (1 = men, 2 = women)
    race = RIDRETH1,
    height = BMXHT, # Height in cm
    fev1 = SPXNFEV1,
    fvc = SPXNFVC,
    acceptable = SPDNACC,
    cigarettes_5 = SMQ690A,
    cigars_5 = SMQ690C,
    pipes_5 = SMQ690B,
    asthma = MCQ010, # Told you have asthma 
    bronchitis = MCQ160K, # Told you have chronic bronchitis
    emphysema = MCQ160G, # Told you have emphysema 
    cancer = MCQ220,
    coughing = RDQ031, # Coughing most days over a 3 month period 
    phlegm = RDQ050,
    dyspnea = CDQ010, # Shortness of breath on stairs/inclines
    wheezing = RDQ070, # Wheezing in chest the past year
    cigarettes = SMQ020
  ) %>%
  mutate (id = id + 100000) %>%   
  mutate (cigars = NA) %>% 
  mutate (pipes = NA) %>% 
  mutate (race = case_when (
    race == 1 | race == 2 ~ 1, # Mexican-American or other Hispanic
    race == 4 ~ 2, # Non-Hispanic Black
    race == 3 ~ 3, # Non-Hispanic White
    race == 5 ~ 4) # Other race, including multi-racial
  )

################################################################################
#### NHANES 2009-2010 - "F" ####
################################################################################

BIOPRO_F <- as_tibble (read.xport ("../data/BIOPRO_F.xpt"))
BMX_F <- as_tibble (read.xport ("../data/BMX_F.xpt"))
BPX_F <- as_tibble (read.xport ("../data/BPX_F.xpt"))
BPQ_F <- as_tibble (read.xport ("../data/BPQ_F.xpt"))
CBC_F <- as_tibble (read.xport ("../data/CBC_F.xpt"))
CDQ_F <- as_tibble (read.xport ("../data/CDQ_F.xpt"))
DEMO_F <- as_tibble (read.xport ("../data/DEMO_F.xpt"))
DIQ_F <- as_tibble (read.xport ("../data/DIQ_F.xpt"))
ENX_F <- as_tibble (read.xport ("../data/ENX_F.xpt"))
GHB_F <- as_tibble (read.xport ("../data/GHB_F.xpt"))
HDL_F <- as_tibble (read.xport ("../data/HDL_F.xpt"))
HIQ_F <- as_tibble (read.xport ("../data/HIQ_F.xpt"))
HSQ_F <- as_tibble (read.xport ("../data/HSQ_F.xpt"))
HUQ_F <- as_tibble (read.xport ("../data/HUQ_F.xpt"))
INQ_F <- as_tibble (read.xport ("../data/INQ_F.xpt"))
KIQ_U_F <- as_tibble (read.xport ("../data/KIQ_U_F.xpt"))
MCQ_F <- as_tibble (read.xport ("../data/MCQ_F.xpt"))
PFQ_F <- as_tibble (read.xport ("../data/PFQ_F.xpt"))
RDQ_F <- as_tibble (read.xport ("../data/RDQ_F.xpt"))
SMQ_F <- as_tibble (read.xport ("../data/SMQ_F.xpt"))
SMQRTU_F <- as_tibble (read.xport ("../data/SMQRTU_F.xpt"))
SPX_F <- as_tibble (read.xport ("../data/SPX_F.xpt"))
TCHOL_F <- as_tibble (read.xport ("../data/TCHOL_F.xpt"))
TRIGLY_F <- as_tibble (read.xport ("../data/TRIGLY_F.xpt"))

nhanes_2009_2010 <- DEMO_F %>% 
  left_join (BIOPRO_F, by = "SEQN") %>% 
  left_join (BMX_F, by = "SEQN") %>%
  left_join (BPQ_F, by = "SEQN") %>% 
  left_join (BPX_F, by = "SEQN") %>% 
  left_join (CBC_F, by = "SEQN") %>% 
  left_join (CDQ_F, by = "SEQN") %>%
  left_join (DIQ_F, by = "SEQN") %>% 
  left_join (ENX_F, by = "SEQN") %>%
  left_join (GHB_F, by = "SEQN") %>% 
  left_join (HDL_F, by = "SEQN") %>% 
  left_join (HIQ_F, by = "SEQN") %>% 
  left_join (HSQ_F, by = "SEQN") %>% 
  left_join (HUQ_F, by = "SEQN") %>% 
  left_join (INQ_F, by = "SEQN") %>%
  left_join (KIQ_U_F, by = "SEQN") %>%
  left_join (MCQ_F, by = "SEQN") %>% 
  left_join (PFQ_F, by = "SEQN") %>% 
  left_join (RDQ_F, by = "SEQN") %>% 
  left_join (SMQ_F, by = "SEQN") %>% 
  left_join (SMQRTU_F, by = "SEQN") %>% 
  left_join (SPX_F, by = "SEQN") %>% 
  left_join (TCHOL_F, by = "SEQN") %>% 
  left_join (TRIGLY_F, by = "SEQN")

rm (DEMO_F, BIOPRO_F, BMX_F, BPQ_F, BPX_F, CBC_F, CDQ_F, DIQ_F, ENX_F, GHB_F,
  HDL_F, HIQ_F, HSQ_F, HUQ_F, INQ_F, KIQ_U_F, MCQ_F, PFQ_F, RDQ_F, SMQ_F,
  SMQRTU_F, SPX_F, TCHOL_F, TRIGLY_F)

nhanes_2009_2010 <- nhanes_2009_2010 %>% 
  select (
    id = SEQN, # ID number
    weight = WTMEC2YR, # 2 year MEC Exam Weight
    age = RIDAGEYR, # Age in years  
    sex = RIAGENDR, # Sex (1 = men, 2 = women)
    race = RIDRETH1,
    height = BMXHT, # Height in cm
    fev1 = SPXNFEV1,
    fvc = SPXNFVC,
    acceptable = SPDNACC,
    cigarettes_5 = SMQ690A,
    cigars_5 = SMQ690C,
    pipes_5 = SMQ690B,
    asthma = MCQ010, # Told you have asthma 
    bronchitis = MCQ160K, # Told you have chronic bronchitis
    emphysema = MCQ160G, # Told you have emphysema 
    cancer = MCQ220,
    coughing = RDQ031, # Coughing most days over a 3 month period 
    phlegm = RDQ050,
    dyspnea = CDQ010, # Shortness of breath on stairs/inclines
    wheezing = RDQ070, # Wheezing in chest the past year
    cigarettes = SMQ020
  ) %>%
  mutate (id = id + 100000) %>%   
  mutate (cigars = NA) %>% 
  mutate (pipes = NA) %>%   
  mutate (race = case_when (
    race == 1 | race == 2 ~ 1, # Mexican-American or other Hispanic
    race == 4 ~ 2, # Non-Hispanic Black
    race == 3 ~ 3, # Non-Hispanic White
    race == 5 ~ 4) # Other race, including multi-racial
  )

################################################################################
#### NHANES 2011-2012 - "G" ####
################################################################################

BIOPRO_G <- as_tibble (read.xport ("../data/BIOPRO_G.xpt"))
BMX_G <- as_tibble (read.xport ("../data/BMX_G.xpt"))
BPX_G <- as_tibble (read.xport ("../data/BPX_G.xpt"))
BPQ_G <- as_tibble (read.xport ("../data/BPQ_G.xpt"))
CBC_G <- as_tibble (read.xport ("../data/CBC_G.xpt"))
CDQ_G <- as_tibble (read.xport ("../data/CDQ_G.xpt"))
DEMO_G <- as_tibble (read.xport ("../data/DEMO_G.xpt"))
DIQ_G <- as_tibble (read.xport ("../data/DIQ_G.xpt"))
ENX_G <- as_tibble (read.xport ("../data/ENX_G.xpt"))
GHB_G <- as_tibble (read.xport ("../data/GHB_G.xpt"))
HDL_G <- as_tibble (read.xport ("../data/HDL_G.xpt"))
HIQ_G <- as_tibble (read.xport ("../data/HIQ_G.xpt"))
HSQ_G <- as_tibble (read.xport ("../data/HSQ_G.xpt"))
HUQ_G <- as_tibble (read.xport ("../data/HUQ_G.xpt"))
INQ_G <- as_tibble (read.xport ("../data/INQ_G.xpt"))
KIQ_U_G <- as_tibble (read.xport ("../data/KIQ_U_G.xpt"))
MCQ_G <- as_tibble (read.xport ("../data/MCQ_G.xpt"))
PFQ_G <- as_tibble (read.xport ("../data/PFQ_G.xpt"))
RDQ_G <- as_tibble (read.xport ("../data/RDQ_G.xpt"))
SMQ_G <- as_tibble (read.xport ("../data/SMQ_G.xpt"))
SMQRTU_G <- as_tibble (read.xport ("../data/SMQRTU_G.xpt"))
SPX_G <- as_tibble (read.xport ("../data/SPX_G.xpt"))
TCHOL_G <- as_tibble (read.xport ("../data/TCHOL_G.xpt"))
TRIGLY_G <- as_tibble (read.xport ("../data/TRIGLY_G.xpt"))

nhanes_2011_2012 <- DEMO_G %>% 
  left_join (BIOPRO_G, by = "SEQN") %>% 
  left_join (BMX_G, by = "SEQN") %>%
  left_join (BPQ_G, by = "SEQN") %>% 
  left_join (BPX_G, by = "SEQN") %>% 
  left_join (CBC_G, by = "SEQN") %>% 
  left_join (CDQ_G, by = "SEQN") %>%
  left_join (DIQ_G, by = "SEQN") %>% 
  left_join (ENX_G, by = "SEQN") %>%
  left_join (GHB_G, by = "SEQN") %>% 
  left_join (HDL_G, by = "SEQN") %>% 
  left_join (HIQ_G, by = "SEQN") %>% 
  left_join (HSQ_G, by = "SEQN") %>% 
  left_join (HUQ_G, by = "SEQN") %>% 
  left_join (INQ_G, by = "SEQN") %>%
  left_join (KIQ_U_G, by = "SEQN") %>%
  left_join (MCQ_G, by = "SEQN") %>% 
  left_join (PFQ_G, by = "SEQN") %>% 
  left_join (RDQ_G, by = "SEQN") %>% 
  left_join (SMQ_G, by = "SEQN") %>% 
  left_join (SMQRTU_G, by = "SEQN") %>% 
  left_join (SPX_G, by = "SEQN") %>% 
  left_join (TCHOL_G, by = "SEQN") %>% 
  left_join (TRIGLY_G, by = "SEQN")

rm (DEMO_G, BIOPRO_G, BMX_G, BPQ_G, BPX_G, CBC_G, CDQ_G, DIQ_G, ENX_G, GHB_G,
  HDL_G, HIQ_G, HSQ_G, HUQ_G, INQ_G, KIQ_U_G, MCQ_G, PFQ_G, RDQ_G, SMQ_G,
  SMQRTU_G, SPX_G, TCHOL_G, TRIGLY_G)

nhanes_2011_2012 <- nhanes_2011_2012 %>% 
  select (
    id = SEQN, # ID number
    weight = WTMEC2YR, # 2 year MEC Exam Weight
    age = RIDAGEYR, # Age in years  
    sex = RIAGENDR, # Sex (1 = men, 2 = women)
    race = RIDRETH1,
    height = BMXHT, # Height in cm
    fev1 = SPXNFEV1,
    fvc = SPXNFVC,
    acceptable = SPDNACC,
    cigarettes_5 = SMQ690A,
    cigars_5 = SMQ690C,
    pipes_5 = SMQ690B,
    asthma = MCQ010, # Told you have asthma 
    bronchitis = MCQ160K, # Told you have chronic bronchitis
    emphysema = MCQ160G, # Told you have emphysema 
    cancer = MCQ220,
    coughing = RDQ031, # Coughing most days over a 3 month period 
    phlegm = RDQ050,
    dyspnea = CDQ010, # Shortness of breath on stairs/inclines
    wheezing = RDQ070, # Wheezing in chest the past year
    cigarettes = SMQ020
  ) %>%
  mutate (id = id + 100000) %>%   
  mutate (cigars = NA) %>% 
  mutate (pipes = NA) %>%   
  mutate (race = case_when (
    race == 1 | race == 2 ~ 1, # Mexican-American or other Hispanic
    race == 4 ~ 2, # Non-Hispanic Black
    race == 3 ~ 3, # Non-Hispanic White
    race == 5 ~ 4) # Other race, including multi-racial
  )

#### Combine NHANES data ####

nhanes_2007_2012 <- rbind (
  nhanes_2007_2008,
  nhanes_2009_2010,
  nhanes_2011_2012
)

rm (nhanes_2007_2008, nhanes_2009_2010, nhanes_2011_2012)

nhanes_2007_2012 <- nhanes_2007_2012 %>%
  filter (weight > 0) %>% 
  filter (age > 17) %>%
  filter (age < 80) %>%
  filter (acceptable > 1) %>%
  filter (fev1 > 0) %>%
  filter (fev1 != 8888) %>% 
  filter (fvc > 0) %>%
  filter (fvc != 88888) %>% 
  filter (height > 0) %>%
  filter (height != 88888) %>%
  mutate (fev1 = fev1 / 1000) %>% 
  mutate (fvc = fvc / 1000) %>% 
  mutate (fev1_fvc = fev1 / fvc) %>%
  mutate (tobacco = case_when (
    cigarettes == 1 | cigars == 1 | pipes == 1 |
    cigarettes_5 > 0 | cigars_5 > 0 | pipes_5 > 0 ~ 1,
    TRUE ~ 0)
  ) %>%
  mutate (symptoms = case_when (
    dyspnea == 1 | coughing == 1 | phlegm == 1 | wheezing == 1 ~ 1,
    TRUE ~ 0)
  ) %>%  
  mutate (healthy = case_when (
    tobacco == 0 & symptoms == 0 ~ 1, 
    TRUE ~ 0)
  )

nhanes <- nhanes_2007_2012 %>% 
  select (
    id,
    weight,
    age,
    sex,
    height,
    race,
    healthy,
    fev1,
    fvc,
    fev1_fvc    
  )

nhanes_healthy <- filter (nhanes, healthy == 1)

#### Get UPHS Data ####

uphs <- rbind (
  read_csv ("../data/2000.csv"),
  read_csv ("../data/2001.csv"),
  read_csv ("../data/2002.csv"),
  read_csv ("../data/2003.csv"),
  read_csv ("../data/2004.csv"),
  read_csv ("../data/2005.csv"),
  read_csv ("../data/2006.csv"),
  read_csv ("../data/2007.csv"),
  read_csv ("../data/2008.csv"),
  read_csv ("../data/2009.csv"),
  read_csv ("../data/2010.csv"),
  read_csv ("../data/2011.csv"),
  read_csv ("../data/2012.csv"),
  read_csv ("../data/2013.csv"),
  read_csv ("../data/2014.csv"),
  read_csv ("../data/2015.csv"),
  read_csv ("../data/2016.csv"),
  read_csv ("../data/2017.csv"),
  read_csv ("../data/2018.csv"),
  read_csv ("../data/2019.csv"),
  read_csv ("../data/2020.csv"),
  read_csv ("../data/2021.csv"),
  read_csv ("../data/2022.csv"),
  read_csv ("../data/2023.csv")
)

uphs <- uphs %>%
  filter (age > 17) %>%
  filter (age < 80) %>%
  filter (fev1 > 0) %>%
  filter (fvc > 0) %>%
  filter (height > 0) %>%
  filter (
    str_detect (comments, "All data acceptable and repeatable") |
    str_detect (comments, "All data is valid and reproducible") |
    str_detect (comments, "All data met \"ATS\" criteria for reproduciblity and validity") |
    str_detect (comments, "All data valid & reproducible") |
    str_detect (comments, "All data valid and reproducible") |
    str_detect (comments, "All manuevers valid & reproducible") |
    str_detect (comments, "All reported data is valid and reproducible") |
    str_detect (comments, "All reported data valid and reproducible") |
    str_detect (comments, "All reported results are acceptable and repeatable") |
    str_detect (comments, "All reported results are repeatable and acceptable according to ATS standards") |
    str_detect (comments, "All reported results are valid and reproducible.") |
    str_detect (comments, "All reported results meet ATS criteria for acceptability and repeatability") |
    str_detect (comments, "All reported results meet ATS standards") |
    str_detect (comments, "All reported results met ATS criteria") |
    str_detect (comments, "All reported results valid and reproducible.") |
    str_detect (comments, "All reported results were valid & reproducible") |
    str_detect (comments, "All results acceptable and repeatable") |
    str_detect (comments, "All results are acceptable and repeatable per 2005 ATS criteria") |
    str_detect (comments, "All results are reproduced and valid.") |
    str_detect (comments, "All results are valid & reproducible") |
    str_detect (comments, "All results are valid and accurate") |
    str_detect (comments, "All results are valid and reproducible") |
    str_detect (comments, "All results met ATS criteria") |
    str_detect (comments, "All results valid & reproducible") |
    str_detect (comments, "All results valid and reproducible") |
    str_detect (comments, "All results were reproduced and are valid") |
    str_detect (comments, "All results were reproduced and valid based on ATS criteria") |
    str_detect (comments, "All tests meet ATS/ERS criteria for acceptability and repeatability") |
    str_detect (comments, "All values valid & reproducible") |
    str_detect (comments, "All values valid and reproducible") |
    str_detect (comments, "Data are valid and reproducible") |
    str_detect (comments, "Data valid & reproducible") |
    str_detect (comments, "Data valid and reproducible") |
    str_detect (comments, "Good, acceptable and repeatable effort.") |
    str_detect (comments, "Good effort and reproducibility") |
    str_detect (comments, "Meets ATS standards") |
    str_detect (comments, "Reported data acceptable and repeatable") |
    str_detect (comments, "Reported data is valid and reproducible") |
    str_detect (comments, "Reported data valid & reproducible") |
    str_detect (comments, "Reported data valid and reproducible") |
    str_detect (comments, "Reported result is valid and reproducible") |
    str_detect (comments, "Reported results acceptable and repeatable") |
    str_detect (comments, "Reported results are acceptable and repeatable") |
    str_detect (comments, "Reported resuts are acceptable and repeatable by ATS criteria") |
    str_detect (comments, "Reported result are acceptable and repeatable by ATS criteria") |
    str_detect (comments, "Reported results are repeatable and acceptable") |
    str_detect (comments, "Reported results are valid and repeatable") |
    str_detect (comments, "Reported results are valid and reproducible") |
    str_detect (comments, "Reported results are within ATS criteria for validity and repeatability") |
    str_detect (comments, "Reported results meet ATS/ETS criteria for acceptability") |
    str_detect (comments, "Reported results meet ATS/ETS criteria for repeatablity and acceptability") |
    str_detect (comments, "Reported results meet ATS criteria for acceptability and repeatability") |
    str_detect (comments, "Reported results meet ATS criteria for validity and reproduciblity") |
    str_detect (comments, "Reported results valid & reproducible") |
    str_detect (comments, "Reported results valid and reproducible") |
    str_detect (comments, "Reported values are valid and reproducible") |
    str_detect (comments, "Reported values valid & reproducible") |
    str_detect (comments, "Reported Spirometry and Lung Volume results meet ATS/ETS criteria for repeatablity and acceptability") |
    str_detect (comments, "Results acceptable and repeatable") |
    str_detect (comments, "Results are acceptable and repeatable") |
    str_detect (comments, "Results are acceptable and valid") |
    str_detect (comments, "Results are acceptable based on ATS criteria") |
    str_detect (comments, "Results are accurate and reproducible") |
    str_detect (comments, "Results are valid and accurate") |
    str_detect (comments, "Results are valid and acceptable") |
    str_detect (comments, "Results are valid and repeatable.") |
    str_detect (comments, "Results are valid and reproduced") |
    str_detect (comments, "Results are valid and reproducible") |
    str_detect (comments, "Results reproduced per ATS criteria") |
    str_detect (comments, "Results reproduced within ATS critieria") |
    str_detect (comments, "Results reproduced without difficulty and are valid.") |
    str_detect (comments, "Results valid & reproducible") |
    str_detect (comments, "Results valid and reproducible") |
    str_detect (comments, "Results were reproduced and are valid") |
    str_detect (comments, "Results were reproduced and valid") |
    str_detect (comments, "Results were reproduced per ATS criteria") |
    str_detect (comments, "Results were valid and reproducible") |
    str_detect (comments, "Spriometry and lung volumes are acceptable and repeatable") |
    str_detect (comments, "Spirometry and Lung volumes are acceptable and repeatable") |
    str_detect (comments, "Studies were acceptable and repeatable at this time") |
    str_detect (comments, "The reported results are acceptable and repeatable") |
    str_detect (comments, "The reported results are acceptable and reproducible") |
    str_detect (comments, "The reported results are valid and reproducible") |
    str_detect (comments, "The reported results meet ATS criteria for acceptability and repeatability") |
    str_detect (comments, "The reported results meet the ATS criteria for acceptabilty and repeatability") |
    str_detect (comments, "The reported results met all ATS criteria") |
    str_detect (comments, "The reported results met all ATS criteria for acceptability and repeatability") |
    str_detect (comments, "The reported results met all ATS/ERS recommendations for acceptability and repeatability") |
    str_detect (comments, "The reported results met all necessary ATS criteria") |
    str_detect (comments, "The reported results met ATS criteria") |
    str_detect (comments, "The reported results met ATS criteria for acceptability and repeatability") |
    str_detect (comments, "The reported results were all valid and reproducible") |
    str_detect (comments, "The reported results were valid and reproducible.") |
    str_detect (comments, "The reported results were valid and reproucible.") |
    str_detect (comments, "The results meet the ATS standards for acceptability and repeatability") |
    str_detect (comments, "The results of this test meet the ATS standards for acceptability and repeatability") |
    str_detect (comments, "Values are valid & reproducible") |
    str_detect (comments, "Values valid and reproducible") |
    str_detect (comments, "Very good, acceptable and repeatable effort")) %>%
  arrange (date) %>%
  distinct (mrn, .keep_all = TRUE) %>%
  mutate (height = height * 2.54) %>%
  mutate (fev1_fvc = fev1 / fvc) %>%
  mutate (asthma = case_when (
    str_detect (diagnosis, regex ("asthma", ignore_case = TRUE)) == 1 ~ 1,
    TRUE ~ 0)
  ) %>%
  mutate (bronchitis = case_when (
    str_detect (diagnosis, regex ("chronic bronchitis", ignore_case = TRUE)) == 1 ~ 1,
    TRUE ~ 0)
  ) %>%
  mutate (copd = case_when (
    str_detect (diagnosis, regex ("chronic obstructive pulmonary", ignore_case = TRUE)) == 1 |
    str_detect (diagnosis, regex ("copd", ignore_case = TRUE)) ~ 1,
    TRUE ~ 0)
  ) %>%
  mutate (emphysema = case_when (
    str_detect (diagnosis, regex ("emphysema", ignore_case = TRUE)) == 1 ~ 1,
    TRUE ~ 0)
  ) %>%
  rowid_to_column ("id") %>% 
  select (
    id,
    age,
    sex,
    height,
    race,
    asthma,
    bronchitis,
    copd,
    emphysema,
    fev1,
    fvc,
    fev1_fvc
  )

uphs_men <- filter (uphs, sex == 1)
uphs_women <- filter (uphs, sex == 2)
