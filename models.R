################################################################################
## Healthy lung function ##
################################################################################

model_healthy <- gamlss (
  fev1_fvc ~ 1,
  family = BCCGo,
  weights = weight,
  data = nhanes_healthy
)

saveRDS (model_healthy, "../models/model_healthy.RDS")

model_healthy <- readRDS ("../models/model_healthy.RDS")

################################################################################
## Diseased lung function ##
################################################################################

values <- pull (uphs, fev1_fvc)

#### Box-Cox Cole and Green distribution ####

model_diseased_bccg <- em (values, model_healthy, "bccg")

saveRDS (model_diseased_bccg, "../models/model_diseased_bccg.RDS")

#### Box-Cox Power Exponential ####

model_diseased_bcpe <- em (values, model_healthy, "bcpe")

saveRDS (model_diseased_bcpe, "../models/model_diseased_bcpe.RDS")

#### Normal distribution ####

model_diseased_no <- em (values, model_healthy, "no")

saveRDS (model_diseased_no, "../models/model_diseased_no.RDS")
