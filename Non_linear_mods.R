#### Non-linear models for Objective 1
#### modeling relationships between forest cover, agriculture, and urbanisation, and macroeconoimc and socioeconomic variables

#### Load data & libraries ####
library('FlexParamCurve')
library('nlme')
library('ggplot2')
library('cowplot')
library('minpack.lm')

dat_master <- read.table("ForCov_LU_econVars_PCs.txt", header=T)
attach(dat_master)

#### Forest cover (RoC) ~ EconPC1 ####

### subset data to deal with NA in for_cov_roc
econ_PC1_sub <- econ_PC1[2:23]
for_cov_roc_sub <- for_cov_roc[2:23]
year_sub <- year[2:23]

### run modpar() to estimate initial parameter estimates and parameter bounds
modpar(econ_PC1_sub, for_cov_roc_sub, grp = NA, pn.options = "myoptions1") 

### Run the two model selection tools - mod.compare and mod.step
FCroc.epc1.modcompare <- pn.mod.compare(econ_PC1_sub, for_cov_roc_sub, grp = NA,
                                        pn.options = "myoptions.1")

FCroc.epc1.modstep <- pn.modselect.step(econ_PC1_sub, for_cov_roc_sub, grp = NA, existing = FALSE,
                                        pn.options = "myoptions.1")

# best applicable model
pn.bestmodel.lis

### now fit a model using the above model with known best number of parameters 
FCroc.ecp1.mod1 <- nlsLM(for_cov_roc_sub ~ SSposnegRichards(econ_PC1_sub, Asym = Asym, K = K, 
                       Infl = Infl, M = M, RAsym = RAsym, Rk = Rk, Ri = Ri, RM = RM, modno = 1, 
                       pn.options = "myoptions1"))
