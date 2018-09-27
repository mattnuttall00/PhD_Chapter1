#### Non-linear models for Objective 1
#### modeling relationships between forest cover, agriculture, and urbanisation, and macroeconoimc and socioeconomic variables

##### Load data & libraries ####
library('FlexParamCurve')
library('nlme')
library('ggplot2')
library('cowplot')
library('minpack.lm')

dat_master <- read.table("ForCov_LU_econVars_PCs.txt", header=T)
attach(dat_master)

### subset data to deal with NA in for_cov_roc
econ_PC1_sub <- econ_PC1[2:23]
econ_PC2_sub <- econ_PC2[2:23]
econ_PC3_sub <- econ_PC3[2:23]
for_cov_roc_sub <- for_cov_roc[2:23]
agric_roc_sub <- agric_roc[2:23]
year_sub <- year[2:23]
dat_master <- c(econ_PC1_sub,for_cov_roc_sub, econ_PC2_sub, econ_PC3_sub, agric_roc_sub)

#### Forest cover (RoC) ~ EconPC1 ####


### FlexParamCurve ####

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
                       pn.options = "myoptions1"),control = nls.control(maxiter = 1000))



### Manual models ####

## Michaelis-Menton y = (a*x)/(1+b*x) ####
plot(for_cov_roc_sub~econ_PC1_sub)

# asymptotic value of y
a <- 0
# value of x where a/2
b <- -1
mod.mm <- nls(for_cov_roc_sub ~ SSmicmen(econ_PC1_sub,a,b))
summary(mod)
xv<-seq(-3, 4, 0.1)
yv<-predict(mod,list(econ_PC1_sub=xv))
lines(xv,yv)



## 3 parameter Asymptotic regression ####
plot(for_cov_roc_sub~econ_PC1_sub)

# Parameter representing horizontal asymptote on right hand side
Asym <- -1
# Parameter representing response when x = 0
RO <- 2
# Parameter b, where b = a - RO
b <- Asym-RO
# Parameter representing natural log of rate constant 
lrc <- (log((Asym - 1.25)/ b))/-2

mod.assym <- nls(for_cov_roc_sub ~ SSasymp(econ_PC1_sub,Asym,RO,lrc))
summary(mod.assym)
curve(predict(mod.assym, newdata = data.frame(econ_PC1_sub=x)), add=TRUE)



## 2 parameter asymptotic exponential ####
plot(for_cov_roc_sub~econ_PC1_sub)

# eqn
# y = a(1 - exp(-b*x))

mod.assym2 <- nlsLM(for_cov_roc_sub ~ Asym*(1-exp(-b*econ_PC1_sub)), start = list(Asym=Asym, b=b))
summary(mod.assym2)
xv<-seq(-3,4,0.1)
yv<-predict(mod.assym2,list(econ_PC1_sub=xv))
lines(xv,yv)



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
                 start = list(a=-0.5, b=-1, c=1, d=-0.5))

# eqn from ?SSbiexp
# a*exp(-exp(b)*x)+c*exp(-exp(d)*x)
# recreating the self starting model with same parameters as estimated in SSbiexp. Same model shape. Now I can tweak the starting values
mod.bioex2 <- nlsLM(for_cov_roc_sub ~ a*exp(-exp(b)*econ_PC1_sub)+c*exp(-exp(d)*econ_PC1_sub),
                  start = list(a=-5.238e-3, b=2.696, c=1.2, d=-1.053))
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

# Inverse polynomials (Crawley, p200) ####
x <-seq(0,10,0.1)
x <- seq(-3,4,0.1)
y <- 1/(x-3.5+6/x)

plot(for_cov_roc_sub~econ_PC1_sub)
plot(x,y,type = "l")

mod.invpol <- nlsLM(for_cov_roc_sub ~ 1/(econ_PC1_sub^2-a+b/econ_PC1_sub), start = list(a=2, b=-1))
summary(mod.invpol)
curve(predict(mod.invpol, newdata = data.frame(econ_PC1_sub=x)), add=TRUE)






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
mod.bioex2.agr <- nls(agric_roc_sub ~ a*exp(-exp(b)*econ_PC1_sub)+c*exp(-exp(d)*econ_PC1_sub)
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
