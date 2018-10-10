#### Non-linear models for Objective 1
#### modeling relationships between forest cover, agriculture, and urbanisation, and macroeconoimc and socioeconomic variables

##### Load data & libraries ####
library('FlexParamCurve')
library('nlme')
library('ggplot2')
library('cowplot')
library('minpack.lm')

dat_master <- read.table("ForCov_LU_econVars_PCs.txt", header=T)

### subset data to deal with NA in for_cov_roc and to look at time lags
econ_PC1_sub <- dat_master$econ_PC1[2:23]
econ_PC1_lag1 <- dat_master$econ_PC1[1:22]
econ_PC1_lag2 <- dat_master$econ_PC1[1:21]
econ_PC2_sub <- dat_master$econ_PC2[2:23]
econ_PC2_lag1 <- dat_master$econ_PC2[1:22]
econ_PC2_lag2 <- dat_master$econ_PC2[1:21]
econ_PC3_sub <- dat_master$econ_PC3[2:23]
econ_PC3_lag1 <- dat_master$econ_PC3[1:22]
econ_PC3_lag2 <- dat_master$econ_PC3[1:21]

com_PC1_sub  <- dat_master$com_PC1[2:23]
com_PC1_lag1 <- dat_master$com_PC1[1:22]
com_PC1_lag2 <- dat_master$com_PC1[1:21]
com_PC2_sub  <- dat_master$com_PC2[2:23]
com_PC2_lag1 <- dat_master$com_PC2[1:22]
com_PC2_lag2 <- dat_master$com_PC2[1:21]
com_PC3_sub  <- dat_master$com_PC3[2:23]
com_PC3_lag1 <- dat_master$com_PC3[1:22]
com_PC3_lag2 <- dat_master$com_PC3[1:21]

prod_PC1_sub  <- dat_master$prod_PC1[2:23]
prod_PC1_lag1 <- dat_master$prod_PC1[1:22]
prod_PC1_lag2 <- dat_master$prod_PC1[1:21]
prod_PC2_sub  <- dat_master$prod_PC2[2:23]
prod_PC2_lag1 <- dat_master$prod_PC2[1:22]
prod_PC2_lag2 <- dat_master$prod_PC2[1:21]
prod_PC3_sub  <- dat_master$prod_PC3[2:23]
prod_PC3_lag1 <- dat_master$prod_PC3[1:22]
prod_PC3_lag2 <- dat_master$prod_PC3[1:21]


for_cov_roc_sub <- dat_master$for_cov_roc[2:23]
for_cov_roc_lag2 <- dat_master$for_cov_roc[3:23]
agric_roc_sub <- dat_master$agric_roc[2:23]
urban_roc_sub <- dat_master$urban_roc[2:23]
year_sub <- dat_master$year[2:23]


#### Scatter plots with time lags ####

## Econ
#lag 1
par(mfrow=c(3,2))
plot(econ_PC1_sub, for_cov_roc_sub)
plot(econ_PC1_lag1, for_cov_roc_sub)
plot(econ_PC2_sub, for_cov_roc_sub)
plot(econ_PC2_lag1, for_cov_roc_sub)
plot(econ_PC3_sub, for_cov_roc_sub)
plot(econ_PC3_lag1, for_cov_roc_sub)

#lag 2
plot(econ_PC1_sub, for_cov_roc_sub)
plot(econ_PC1_lag2, for_cov_roc_lag2)
plot(econ_PC2_sub, for_cov_roc_sub)
plot(econ_PC2_lag2, for_cov_roc_lag2)
plot(econ_PC3_sub, for_cov_roc_sub)
plot(econ_PC3_lag2, for_cov_roc_lag2)

## Comm
#lag 1 & 2
par(mfrow=c(3,3))
plot(com_PC1_sub, for_cov_roc_sub)
plot(com_PC1_lag1, for_cov_roc_sub)
plot(com_PC1_lag2, for_cov_roc_lag2)
plot(com_PC2_sub, for_cov_roc_sub)
plot(com_PC2_lag1, for_cov_roc_sub)
plot(com_PC2_lag2, for_cov_roc_lag2)
plot(com_PC3_sub, for_cov_roc_sub)
plot(com_PC3_lag1, for_cov_roc_sub)
plot(com_PC3_lag2, for_cov_roc_lag2)

## Prod
#lag 1 & 2
par(mfrow=c(3,3))
plot(prod_PC1_sub, for_cov_roc_sub)
plot(prod_PC1_lag1, for_cov_roc_sub)
plot(prod_PC1_lag2, for_cov_roc_lag2)
plot(prod_PC2_sub, for_cov_roc_sub)
plot(prod_PC2_lag1, for_cov_roc_sub)
plot(prod_PC2_lag2, for_cov_roc_lag2)
plot(prod_PC3_sub, for_cov_roc_sub)
plot(prod_PC3_lag1, for_cov_roc_sub)
plot(prod_PC3_lag2, for_cov_roc_lag2)

#### Forest cover (RoC) ~ EconPC1 ####


### FlexParamCurve ####

### run modpar() to estimate initial parameter estimates and parameter bounds
modpar(econ_PC1_sub, for_cov_roc_sub, grp = NA, pn.options = "myoptions1") 

### Run the two model selection tools - mod.compare and mod.step
FCroc.epc1.modcompare <- pn.mod.compare(econ_PC1_sub, for_cov_roc_sub, grp = NA,
                                        pn.options = "myoptions.1")

FCroc.epc1.modstep <- pn.modselect.step(econ_PC1_sub, for_cov_roc_sub, grp = NA, existing = FALSE,
                                        pn.options = "myoptions1")

# best applicable model
pn.bestmodel.lis

### now fit a model using the above model with known best number of parameters 
FCroc.ecp1.mod1 <- nls(for_cov_roc_sub ~ SSposnegRichards(econ_PC1_sub, Asym = Asym, K = K, 
                       Infl = Infl, M = M, RAsym = RAsym, Rk = Rk, Ri = Ri, RM = RM, modno = 1, 
                       pn.options = "myoptions1"),control = nls.control(maxiter = 1000))



### Manual models ####

## Michaelis-Menton y = (a*x)/(1+b*x) ####
plot(for_cov_roc_sub~econ_PC1_sub)
identify(econ_PC1_sub,for_cov_roc_sub)

# asymptotic value of y
a <- -2
# value of x where a/2
b <- 2
mod.mm <- nls(for_cov_roc_sub ~ SSmicmen(econ_PC1_sub,a,b))
summary(mod)
xv<-seq(-3, 4, 0.1)
yv<-predict(mod.mm,list(econ_PC1_sub=xv))
lines(xv,yv)



## 3 parameter Asymptotic regression ####
plot(for_cov_roc_sub~econ_PC1_sub)

# self starting
mod.assym <- nls(for_cov_roc_sub ~ SSasymp(econ_PC1_sub,Asym,RO,lrc))
summary(mod.assym)
curve(predict(mod.assym, newdata = data.frame(econ_PC1_sub=x)), add=TRUE)

# custom model based on equation from Crawley p662 and using parameter estimates from above self starter

# Parameter representing horizontal asymptote on right hand side
Asym <- -4.6
# Parameter representing response when x = 0
RO <- 0.73
# Parameter b, where b = a - RO
b <- Asym-RO
# Parameter representing natural log of rate constant 
lrc <- (log((Asym - 1.25)/ b))/-2

mod.assym2 <- nlsLM(for_cov_roc_sub ~ Asym-b*exp(-lrc*econ_PC1_sub), 
                  start = list(Asym=Asym, b=b, lrc=lrc),
                  control = nls.control(maxiter = 1000))
summary(mod.assym2)
curve(predict(mod.assym2, newdata = data.frame(econ_PC1_sub=x)), add=TRUE)

## 2 parameter asymptotic exponential ####
plot(for_cov_roc_sub~econ_PC1_sub)

# eqn
# y = a(1 - exp(-b*x))
Asym <- 0
b <- 12
mod.assym3 <- nlsLM(for_cov_roc_sub ~ Asym*(1-exp(-b*econ_PC1_sub)), 
                    start = list(Asym=Asym, b=b),
                    control = nls.control(maxiter = 1000))
summary(mod.assym3)
curve(predict(mod.assym3, newdata = data.frame(econ_PC1_sub=x)), add=TRUE)



## Logistic model ####
plot(for_cov_roc_sub~econ_PC1_sub)

# eqn for SSlogis self starting model
# y = a / (1+exp((b-x)/c))

Asym <- -0.5
xmid <- -1
scal <- 1

mod.logis <- nls(for_cov_roc_sub ~ SSlogis(econ_PC1_sub, Asym, xmid, scal))
summary(mod.logis)
curve(predict(mod.logis, newdata = data.frame(econ_PC1_sub=x)), add=TRUE)

xv<-seq(-3,4,0.1)
yv<-predict(mod.logis,list(econ_PC1_sub=xv))
lines(xv,yv)



## bioexponential model ####
plot(for_cov_roc_sub~econ_PC1_sub)

# eqn from Crawley
# y = a*exp(b*x) + c*exp(d*x)

mod.bioex <- nls(for_cov_roc_sub ~ a*exp(b*econ_PC1_sub)+c*exp(d*econ_PC1_sub), 
                 start = list(a=-5, b=2.7, c=0.69, d=-1.053),
                 control = nls.control(maxiter = 1000))

# eqn from ?SSbiexp
# a*exp(-exp(b)*x)+c*exp(-exp(d)*x)
# recreating the self starting model with same parameters as estimated in SSbiexp. Same model shape. Now I can tweak the starting values
mod.bioex2 <- nlsLM(for_cov_roc_sub ~ a*exp(-exp(b)*econ_PC1_sub)+c*exp(-exp(d)*econ_PC1_sub),
                  start = list(a=-5.238e-3, b=2.696, c=0.69, d=-1.053))
curve(predict(mod.bioex2, newdata = data.frame(econ_PC1_sub=x)), add=TRUE)

# self starting
mod.ssbioex <- nls(for_cov_roc_sub ~ SSbiexp(econ_PC1_sub,A1,lrc1,A2,lrc2))
summary(mod.ssbioex)
curve(predict(mod.ssbioex, newdata = data.frame(econ_PC1_sub=x)), add=TRUE)
plot(resid(mod.ssbioex))

# using nlsLM
mod.bioex2LM.fcroc = nlsLM(for_cov_roc_sub ~ a*exp(-exp(b)*econ_PC1_sub)+c*exp(-exp(d)*econ_PC1_sub),
                     start=list(a=(max(for_cov_roc_sub)-min(for_cov_roc_sub)),
                                b=-min(for_cov_roc_sub),
                                c= (max(for_cov_roc_sub)-1),
                                d= -1.053),
                     control = nls.control(maxiter = 1000))
curve(predict(mod.bioex2LM.fcroc, newdata = data.frame(econ_PC1_sub=x)), add=TRUE)

## exponential model ####

plot(for_cov_roc_sub~econ_PC1_sub)
a <- 1.5
b <- 2*log(2)/a

mod.exp <- nls(for_cov_roc_sub ~ a*exp(-b*econ_PC1_sub), 
               start = list(a=a, b=b))
curve(predict(mod.exp, newdata = data.frame(econ_PC1_sub=x)), add=TRUE)

# testing without outlier
econPC1_test <- econ_PC1_sub[2:22]
for_cov_roc_sub_test <- for_cov_roc_sub[2:22]
plot(for_cov_roc_sub_test~econPC1_test)

mod.exp <- nls(for_cov_roc_sub_test ~ a*exp(-b*econPC1_test), 
               start = list(a=a, b=b))
curve(predict(mod.exp, newdata = data.frame(econPC1_test=x)), add=TRUE)


# Inverse polynomials (Crawley, p200) ####
x <-seq(0,10,0.1)
x <- seq(-3,4,0.1)
y <- 1/(x-3.5+6/x)

plot(for_cov_roc_sub~econ_PC1_sub)
plot(x,y,type = "l")

mod.invpol <- nlsLM(for_cov_roc_sub ~ 1/(econ_PC1_sub^2-a+b/econ_PC1_sub), start = list(a=2, b=-1))
summary(mod.invpol)
curve(predict(mod.invpol, newdata = data.frame(econ_PC1_sub=x)), add=TRUE)







#### Forest cover (RoC) ~ EconPC2 ####

plot(econ_PC2_sub, for_cov_roc_sub)
ln.mod <- lm(for_cov_roc_sub ~ econ_PC2_sub)
summary(ln.mod)
plot(ln.mod)
curve(predict(ln.mod, newdata = data.frame(econ_PC2_sub=x)), add=TRUE)

#### Forest cover (RoC) ~ EconPC3 ####

plot(econ_PC3_sub, for_cov_roc_sub)

# Polynomial function
mod.poly <- nls(for_cov_roc_sub ~ a+b*econ_PC3_sub-c*econ_PC3_sub^2, 
                start = list(a=2, b=5, c=0.2))
curve(predict(mod.poly, newdata = data.frame(econ_PC3_sub=x)), add=TRUE)

#### Agriculture cover (RoC) ~ EconPC1 ####

plot(econ_PC1_sub, agric_roc_sub)

## Biexponential model
# self starting
mod.ssbioex.agr <- nls(agric_roc_sub ~ SSbiexp(econ_PC1_sub,A1,lrc1,A2,lrc2))
summary(mod.ssbioex)
curve(predict(mod.ssbioex, newdata = data.frame(econ_PC1_sub=x)), add=TRUE)
plot(resid(mod.ssbioex))

# SS formula but custom starting values
mod.bioex2.agr <- nls(agric_roc_sub ~ a*exp(-exp(b)*econ_PC1_sub)+c*exp(-exp(d)*econ_PC1_sub),
                    start = list(a=-5.238e-16, b=2.696, c=0.6925, d=-1.053),
                    control = nls.control(maxiter = 1000))
curve(predict(mod.bioex2.agr, newdata = data.frame(econ_PC1_sub=x)), add=TRUE)

# trying usin nlsLM
mod.bioex2LM = nlsLM(agric_roc_sub ~ a*exp(-exp(b)*econ_PC1_sub)+c*exp(-exp(d)*econ_PC1_sub),
                     start=list(a=(max(agric_roc_sub)-min(agric_roc_sub)),
                                b=-min(agric_roc_sub),
                                c= (max(agric_roc_sub)-1),
                                d= -1.053),
                     control = nls.control(maxiter = 1000))
curve(predict(mod.bioex2LM, newdata = data.frame(econ_PC1_sub=x)), add=TRUE)

#### Agriculture cover (RoC) ~ EconPC2 ####

plot(econ_PC2_sub, agric_roc_sub)
lm.agroc.epc2 <- lm(agric_roc_sub ~ econ_PC2_sub)
summary(lm.mod.agroc.epc2)
par(mfrow=c(2,2))
plot(lm.mod.agroc.epc2)
curve(predict(lm.agroc.epc2, newdata = data.frame(econ_PC2_sub=x)), add=TRUE)
plot(resid(lm.agroc.epc2))
acf(residuals(lm.agroc.epc2))

#### Agriculture cover (RoC) ~ EconPC3 ####

plot(agric_roc_sub~econ_PC3_sub)



3 parameter asymptotic
Nils papaer migration, journal animal ecology