##example of PREDICTIONS from Poisson mixed model with offset term
##assign values
##Modified from http://www.inside-r.org/packages/cran/AICcmodavg/docs/predictSE.mer
##5/3/2015
##JM

library(lme4)

# Set some parameters for simulated data:
set.seed(seed = 222)
beta0 <- -3.45
beta.CWD <- 0.01
beta.basal.area <- 0.1
block.var <- 0.28
n.blocks <- 10
rep.per.block <- 9
n.obs <- n.blocks * rep.per.block
CWD <- rnorm(n = n.obs, mean = 190, sd = 50)
effort <- sample(10:20, size = n.obs, replace = TRUE)
basal.area <- rnorm(n = n.obs, mean = 60, sd = 20)
block.id <- sort(rep(1:n.blocks, rep.per.block))

##generate data
##random intercept (block)
block.int <- rnorm(n = 10, mean = 0, sd = sqrt(block.var))
lin.pred <- beta0 + beta.CWD * CWD + beta.basal.area * basal.area + log(effort)

y.val <- rep(NA, n.obs)
for (i in 1:n.obs) {
  y.val[i] <- rpois(n = 1, lambda = exp(lin.pred[i] + block.int[block.id[i]]))
}
sim.data <- data.frame(Y.val = y.val, CWD = CWD, Basal.area = basal.area,
                       Block = as.factor(block.id), Effort = effort)
sim.data$log.Effort <- log(sim.data$Effort)

par(mfrow=c(2,5))
for(i in 1:nlevels(sim.data$Block) ) {
  plot(sim.data$CWD[sim.data$Block==levels(sim.data$Block)[i]], sim.data$Y.val[sim.data$Block==levels(sim.data$Block)[i]])
}

##run model with log transformation of offset variable within call
m1<- glmer(Y.val ~ CWD + Basal.area + (1 | Block) + offset(log(Effort)), data = sim.data,
            family = poisson)
##predictions
pred.data1 <- expand.grid(CWD = mean(sim.data$CWD), Basal.area = seq(from = 20, to = 50, by = 10),
                         Effort = 20)
predict(m1, newdata = pred.data1, type = "response", re.form=NA)

##run model with offset already on log scale
m2 <- glmer(Y.val ~ CWD + Basal.area + (1 | Block) + offset(log.Effort), data = sim.data,
            family = poisson)
##predictions
pred.data2 <- expand.grid(CWD = mean(sim.data$CWD), Basal.area = seq(from = 20, to = 50, by = 10),
                         log.Effort = log(20))
predict(m2, newdata = pred.data2, type = "response", re.form=NA)

##both are identical

# Alternative coding - offset as ARGUMENT, not as term in model - note that this avoids things like MuMIn having to
# 'handle' the offset term somehow - as it is an argument it automatically gets included in all models fitted by
# dredge()!
m3 <- glmer(Y.val ~ CWD + Basal.area + (1 | Block), data = sim.data, offset=log(Effort),family = poisson)
# So estimates from this model are identical to above estimates?
summary(m3)$coef
summary(m2)$coef
# Try to predict as above:
pred.data3 <- expand.grid(CWD = mean(sim.data$CWD), Basal.area = seq(from = 20, to = 50, by = 10))
predict(m3, newdata = pred.data3, type = "response", re.form=NA)
# So this IS WRONG.

# The problem is that because there is no 'term' for the offset, we can't easily specify what value to predict for... so
# we are not taking the 'effort' into account as per previous predictions. We can demonstrate this by doing the
# predictions manually instead. First make a prediction 'frame' (equivalent to newdata but need to be a matrix, and you
# need to explicitly code the intercept column, etc):
predframe <- cbind(rep(1,4), rep(mean(sim.data$CWD)), seq(from = 20, to = 50, by = 10))

# We can now make a prediction ignoring the effort to check we can replicate the wrong calculation above:
# (note you need to backtransform, here this is on a log scale so we can simply use exp()):
exp(predframe %*% fixef(m3))
# These are indeed what predict() tells us.
# So what we need to do is to add the offset to these predictions manually. You don't need to multiply the 
#  value with anything (as it's an offset, and therefore not a parameter!):
# Be careful to do this on the LINK scale, so ADD the logged offset before you backtransform:
exp(predframe %*% fixef(m3) + log(20))
# Hurrah! These are the same as the previous predictions!

