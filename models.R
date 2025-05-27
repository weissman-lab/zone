################################################################################
## Healthy lung function ##
################################################################################

# model_healthy <- gamlss (
#   fev1_fvc ~ 1,
#   family = BCCGo,
#   weights = weight,
#   data = nhanes_healthy
# )
# 
# saveRDS (model_healthy, "../models/model_healthy.RDS")

model_healthy <- readRDS ("../models/model_healthy.RDS")

################################################################################
## Diseased lung function ##
################################################################################

values <- pull (uphs, fev1_fvc)

#### Box-Cox Cole and Green distribution ####

# bccg <- em (values, model_healthy, "bccg")
# 
# saveRDS (bccg, "../models/model_diseased_bccg.RDS")

model_diseased_bccg <- readRDS ("../models/model_diseased_bccg.RDS")

#### Box-Cox Power Exponential ####

bcpe <- em (values, model_healthy, "bcpe")

saveRDS (bcpe, "../models/model_diseased_bcpe.RDS")

model_diseased_bcpe <- readRDS ("../models/model_diseased_bcpe.RDS")

#### Normal distribution ####

#no <- em (values, model_healthy, "no")

#saveRDS (no, "../models/model_diseased_no.RDS")

model_diseased_no <- readRDS ("../models/model_diseased_no.RDS")
