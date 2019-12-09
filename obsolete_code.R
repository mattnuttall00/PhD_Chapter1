# Obsolete script from chapter 1 that I don't need right now but may be helpful in the future!


### Predicting from linear models ####
# pop_den when FOR_REM SET TO MEAN
df.me.pop_den <- crossing(pop_den.lag2.cen = seq(min(dat_me_lag_dredge$pop_den.lag2.cen),
                                    max(dat_me_lag_dredge$pop_den.lag2.cen),length=100),
                          for_rem.lag2.cen = mean(dat_me_lag_dredge$for_rem.lag2.cen),
                          ind_gdp.lag2 = rep(0.8368),
                          time = mean(dat_me_lag_dredge$time))
head(df.me.pop_den)
df.me.pop_den <- data.frame(df.me.pop_den)

# predict
pred_pop_den <- predict(me.mod2lag.5, newdata = df.me.pop_den, int="c")


# pop_den when FOR_REM SET TO MIN
df.me.pop_den.for_remMIN <- crossing(pop_den.lag2.cen = seq(min(dat_me_lag_dredge$pop_den.lag2.cen),
                                    max(dat_me_lag_dredge$pop_den.lag2.cen),length=100),
                          for_rem.lag2.cen = min(dat_me_lag_dredge$for_rem.lag2.cen),
                          ind_gdp.lag2 = rep(0.8368),
                          time = mean(dat_me_lag_dredge$time))
# predict
pred_pop_den.for_remMIN <- predict(me.mod2lag.5, newdata = df.me.pop_den.for_remMIN, int="c")

# pop_den when FOR_REM SET TO MAX
df.me.pop_den.for_remMAX <- crossing(pop_den.lag2.cen = seq(min(dat_me_lag_dredge$pop_den.lag2.cen),
                                    max(dat_me_lag_dredge$pop_den.lag2.cen),length=100),
                          for_rem.lag2.cen = max(dat_me_lag_dredge$for_rem.lag2.cen),
                          ind_gdp.lag2 = rep(0.8368),
                          time = mean(dat_me_lag_dredge$time))
# predict
pred_pop_den.for_remMAX <- predict(me.mod2lag.5, newdata = df.me.pop_den.for_remMAX, int="c")


# dataframe for plotting
df.pred.pop_den <- data.frame(newpop_den = df.me.pop_den$pop_den.lag2.cen,
                           newy = as.numeric(pred_pop_den[ ,"fit"]),
                           newupr = as.numeric(pred_pop_den[ ,"upr"]),
                           newlwr = as.numeric(pred_pop_den[ ,"lwr"]),
                           newyMIN = as.numeric(pred_pop_den.for_remMIN[ ,"fit"]),
                           newuprMIN = as.numeric(pred_pop_den.for_remMIN[ ,"upr"]),
                           newlwrMIN = as.numeric(pred_pop_den.for_remMIN[ ,"lwr"]),
                           newyMAX = as.numeric(pred_pop_den.for_remMAX[ ,"fit"]),
                           newuprMAX = as.numeric(pred_pop_den.for_remMAX[ ,"upr"]),
                           newlwrMAX = as.numeric(pred_pop_den.for_remMAX[ ,"lwr"]))

test <- melt(df.pred.pop_den, id="newpop_den")
head(test)

ggplot(data=test, aes(x=newpop_den, y=value, colour=variable))+
  geom_line()



# Plot effect of pop_den with varying values of for_rem
ggplot(df.pred.pop_den, aes(x=newpop_den))+
  geom_line(aes(y=newy, color="mean"), size=1.5)+
  geom_ribbon(aes(ymin=newlwr, ymax=newupr, alpha=0.4), fill="#56B4E9", show.legend=F)+
  
  geom_line(aes(y=newyMIN,color="min"), size=1.5)+
  geom_ribbon(aes(ymin=newlwrMIN, ymax=newuprMIN, alpha=0.4), fill="#009E73", show.legend=F)+
 
  geom_line(aes(y=newyMAX,color="max"), size=1.5)+
  geom_ribbon(aes(ymin=newlwrMAX, ymax=newuprMAX, alpha=0.4), fill="#D55E00", show.legend=F)+
  
  scale_color_manual(name = "Remaining forest", 
                     values = c("mean"="#56B4E9", "min"="#009E73", "max"="#D55E00"))+
 
  xlab("Changes in population density (centered)")+
  ylab("Amount of forest lost (ha)")+
  theme(text = element_text(size=15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
  
### Old modeling approach for macroeconomic predictors ####
  ## economic data ####

# dredge (AICc used as small sample size)
system.time(dredge.me <- dredge(me.mod1, beta = "none", evaluate = TRUE, rank = AICc))
write.csv(dredge.me, file="dredge.me.csv")
coefTable(dredge.me)

# There is only one top model (AICc < 2). It contains for_rem, time, pop_den



# fit the model, using full data as no missing data in these variables
me.mod2 <- lm(for_cov ~ for_rem + pop_den + time, data = dat_me)
summary(me.mod2)
par(mfrow=c(2,2))
plot(me.mod2)
# I think the plots look ok - residual plot suggests homoscedasticity (although very slight possibility this isn't true towards the right hand side of the plot, but hard to say with such small sample size). Q-Q plot not particularly pretty, but I don't think this suggests any major issues, more just an issue with few data points. 
vif(me.mod2)
# variance inflation are all <5 so I think are fine

# data point 2 (which is actually rowname 1) appears to have undue influence - I'll check to se what happens when it is removed

me.mod3 <- update(me.mod2, data=dat_me[-1, ])
plot(me.mod3)
summary(me.mod3)
# coefficients change slightly but it makes very little difference. So I'll keep the point in



# test a model with the same terms but with interactions
me.mod4 <- lm(for_cov ~ scale(for_rem,center = T,scale = F) * scale(pop_den,center = T,scale = F) * 
                scale(time,center = T,scale = F), data = dat_me)

# same model but without centering (jsut to test)
me.mod4a <- lm(for_cov ~ for_rem * pop_den * time, data = dat_me)
# Model inference is similar, but with centering more terms are significant. This is due to the centering reducing the variance inflation caused by colinearity between interaction terms 

plot(me.mod4)
# residual plot and QQ plot look good - slightly better than me.mod2 I think
summary(me.mod4)
vif(me.mod4)
# When centered, all terms are significant. Potentially an interesting interaction between for_rem and pop_den - as pop_den increases, the positive effect (ie more forest loss with higher for_rem) gets smaller. So an increaseing population makes the amount of forest remaining less important for forest loss. But the p value for the interaction is nearly 0.1

# compare models
anova(me.mod2,me.mod4)
# model with no interactions is better



# try a simpler interaction model with only an interaction between for_rem and pop_den
me.mod5 <- lm(for_cov ~ scale(for_rem, center=T, scale=F) * scale(pop_den,center=T,scale=F) + time, data = dat_me)
plot(me.mod5)
# plots look fine - similar to above
summary(me.mod5)
# interaction term is not significant. All other terms are significant 

# compare models
anova(me.mod2, me.mod5)
# the more complex model is not significantly better

# It looks like me.mod2 is the best unlagged macroeconomic model. Compare the "importance" of the effects
me.mod2a <- lm(for_cov ~ scale(for_rem) + scale(pop_den) + scale(time), data = dat_me)
summary(me.mod2a)
# time and for_rem have similar effects on for_cov, pop_den has less of an effect



## Models with time lags in the predictors

# create data
dat_me_lag <- data.frame(year = dat_me$year,
                         for_cov = dat_me$for_cov,
                         time = dat_me$time,
                         gdp.lag1 = lag(dat_me$gdp),
                         gdp.lag2 = lag(dat_me$gdp, n=2L),
                         gdp_gr.lag1 = lag(dat_me$gdp_gr),
                         gdp_gr.lag2 = lag(dat_me$gdp_gr, n=2L),
                         fdi.lag1 = lag(dat_me$fdi),
                         fdi.lag2 = lag(dat_me$fdi, n=2L),
                         ind_gdp.lag1 = lag(dat_me$ind_gdp),
                         ind_gdp.lag2 = lag(dat_me$ind_gdp, n=2L),
                         agr_gdp.lag1 = lag(dat_me$agr_gdp),
                         agr_gdp.lag2 = lag(dat_me$agr_gdp, n=2L),
                         dev_agr.lag1 = lag(dat_me$dev_agri),
                         dev_agr.lag2 = lag(dat_me$dev_agri, n=2L),
                         dev_env.lag1 = lag(dat_me$dev_env),
                         dev_env.lag2 = lag(dat_me$dev_env, n=2L),
                         pop_den.lag1 = lag(dat_me$pop_den),
                         pop_den.lag2 = lag(dat_me$pop_den, n=2L),
                         for_rem.lag1 = lag(dat_me$for_rem),
                         for_rem.lag2 = lag(dat_me$for_rem, n=2L))

# remove the rows that have NAs
dat_me_lag_sub <- dat_me_lag[c(5:22), ]

# saturated model - WARNING TAKES AGES TO RUN
me.modlag.all <- lm(for_cov ~ gdp.lag1+gdp.lag2+gdp_gr.lag1+gdp_gr.lag2+fdi.lag1+fdi.lag2+ind_gdp.lag1+
                   ind_gdp.lag2+agr_gdp.lag1+agr_gdp.lag2+dev_agr.lag1+dev_agr.lag2+dev_env.lag1+dev_env.lag2+
                   pop_den.lag1+pop_den.lag2+for_rem.lag1+for_rem.lag2+time, 
                   data=dat_me_lag_sub, na.action="na.fail")
summary(me.modlag.all)

dredge.me.lag.all <- dredge(me.modlag.all, beta = "none", evaluate = TRUE, rank = AICc)


# just lag 1 saturated model
me.modlag.1 <- lm(for_cov ~ gdp.lag1+gdp_gr.lag1+fdi.lag1+ind_gdp.lag1+agr_gdp.lag1+dev_agr.lag1+dev_env.lag1+
                 pop_den.lag1+for_rem.lag1+time, data=dat_me_lag_sub, na.action="na.fail")

summary(me.modlag.1)

dredge.me.lag.1 <- dredge(me.modlag.1, beta = "none", evaluate = TRUE, rank = AICc)
write.csv(dredge.me.lag.1, file="dredge.me.lag.1.csv")

# when a 1 year lag is applied, time (unlagged) is still in the best model, and for_rem, ind_gdp, and pop_den are all in the model. Compared with the best model from the unlagged data, the effect size of for_rem is virtually identical between lagged and un-lagged, the effect size of pop_den is virtually identical between lagged and unlagged.  ind_gdp appears in the top model when it is lagged, which it does not when unlagged. The effect is positive for lagged ind_gdp, so that when ind_gdp gets larger in time t, forest loss increases in t+1, which is not what I would have hypothesised.  



# re-create the model.  Because of the variables in the top dredged model, I can keep in more rows from the original dataset (fewer NAs)
dat_me_lag_dredge <- dat_me_lag[c(3:22), ]

me.modlag.2 <- lm(for_cov ~ time + for_rem.lag1 + ind_gdp.lag1 + pop_den.lag1, data=dat_me_lag_dredge)
summary(me.modlag.2)
par(mfrow=c(2,2))
plot(me.modlag.2)
# plots look fine. point 5 appears to be having large influence

# remove point 5 (row 3)
me.modlag.3 <- update(me.modlag.2, data=dat_me_lag_dredge[-3, ])
plot(me.modlag.3)
summary(me.modlag.3)
# plots and coefficients very similar so I'm not worried aobut the point



# test for interactions
me.modlag.4 <- lm(for_cov ~ scale(time,center=T,scale=F) * scale(for_rem.lag1,center=T,scale=F) * 
                  scale(ind_gdp.lag1,center=T,scale=F) * scale(pop_den.lag1,center=T,scale=F), 
                  data=dat_me_lag_dredge)
summary(me.modlag.4)
plot(me.modlag.4)
# Not liking the residuals plot - some heterosced

# compare models
anova(me.modlag.2,me.modlag.4)
# More complex model has some support, but simpler model just beats it I think


# reduce complexity - only interaction between for_rem and pop_den
me.modlag.5 <- lm(for_cov ~ scale(for_rem.lag1,center=T,scale=F) * scale(pop_den.lag1,center=T,scale=F) + 
                  ind_gdp.lag1 + time , data=dat_me_lag_dredge)
summary(me.modlag.5)
plot(me.modlag.5)
vif(me.modlag.5)



## Models with a 2 year time lag
me.mod2lag.1 <- lm(for_cov ~ gdp.lag2+gdp_gr.lag2+fdi.lag2+ind_gdp.lag2+agr_gdp.lag2+dev_agr.lag2+dev_env.lag2+
                 pop_den.lag2+for_rem.lag2+time, data=dat_me_lag_sub, na.action="na.fail")
summary(me.mod2lag.1)

dredge.me.lag.2 <- dredge(me.mod2lag.1, beta = "none", evaluate = TRUE, rank = AICc)
write.csv(dredge.me.lag.2, file="dredge.me.lag.2.csv")

# when a 2 year lag is applied, the model has the same terms as before -  time (unlagged), and then lagged versions of for_rem, ind_gdp, pop_den.  See excel spreadsheet "macroecon_models" for summary comparisons

# re-create the model using all available data
me.mod2lag.2 <- lm(for_cov ~ for_rem.lag2 + pop_den.lag2 + ind_gdp.lag2 + time, data=dat_me_lag_dredge)
summary(me.mod2lag.2)
plot(me.mod2lag.2)
# I think the plots look ok

# check for variance inflation
vif(me.mod2lag.2)
# for_rem.lag2 has a vif of just over 5, but the rest are <5.  Fine I think



# test interactions
me.mod2lag.3 <- lm(for_cov ~ scale(for_rem.lag2,center=T,scale=F) * scale(pop_den.lag2,center=T,scale=F) * 
                   scale(ind_gdp.lag2,center=T,scale=F) + scale(time,center=T,scale=F), 
                   data=dat_me_lag_dredge)
summary(me.mod2lag.3)
plot(me.mod2lag.3)
vif(me.mod2lag.3)
# Plots look fine. VIF's for for_rem:ind_gdp and pop_den:ind_gdp are too high, but they are already centered so I'm not exactly sure what I can do about it.



# Only for_rem:pop_den is sig, so I will simplify the interactions

# first add the centered data to the dataframe so I don't have to center the variables in the model formula
dat_me_lag_dredge$for_rem.lag2.cen <- c(scale(dat_me_lag_dredge$for_rem.lag2,center=T,scale=F))
dat_me_lag_dredge$pop_den.lag2.cen <- c(scale(dat_me_lag_dredge$pop_den.lag2,center=T,scale=F))

me.mod2lag.4 <- lm(for_cov ~ for_rem.lag2.cen * pop_den.lag2.cen + 
                   ind_gdp.lag2 + time, data=dat_me_lag_dredge)

plot(me.mod2lag.4)
# QQ plot doesn't look that great -  the top end shoots off
summary(me.mod2lag.4)
vif(me.mod2lag.4)
# vifs are ok - all <6


# change the order of the interaction
me.mod2lag.5 <- lm(for_cov ~ pop_den.lag2.cen * for_rem.lag2.cen + 
                   ind_gdp.lag2 + time, data=dat_me_lag_dredge)

plot(me.mod2lag.5)
# QQ plot same as above
summary(me.mod2lag.5)
vif(me.mod2lag.5)
# vifs are ok - all <6

# the order of the terms in the interaction doesn't make any difference to the coefficients.

### summary

# FOR_REM - the effect size of for_rem is very consistent throughout all models, even between lagged and unlagged. It remains highly signifcant throughout, and so I can conclude that it is an important predictor. The effect is positive, which suggests that the more forest there is available, the more likely it is to be lost. See below under pop_den for details on interactions. There seems to be no increase/decrease in effect size when the variable is lagged or not.

# TIME - As above. This is also a 'control' variable so will definitely be in the final model(s). There was one signifcant case of interaction between time and pop_den (see below udner pop_den)

# IND_GDP - This is not an important variable when there is no time lag.  Yet it appears in the top dredged models for 1year and 2year lag.  The effect size varies quite a bit, although is much larger (and more significant) with a 2-year time lag.  The effect is positive, and therefore suggests that when there are positive changes to industrial proportion of GDP at time t, this predicts larger forest loss in time t+1 and t+2. This is opposite of what I would have expected. There were no instances of interactions with this variable

# POP_DEN - This is an important predictor, and is signifcant in all models (except me.mod4a which wasn't centered and so is not correct anyway).  The effect size is large - with every unit increase of population density (i.e. another person per square km), the changes (loss) in forest cover goes down by between 400-600ha (taken from coefficients for models with no interaction). This is the opposite of what I would expect - I would have expected increases in population density to drive more forest loss, but this doesn't seem to be the case. When I run a simple model with just pop_den, and pop_den + time, the coefficient is positive. It's only when for_rem is added that it turns negative, highlighting the interaction.  There is decent evidence of an interaction between forest remaining and population density, and to a lesser extent population density and time.  The interaction effect for for_rem:pop_den is negative, and so with every unit increase in population density, the effect of remaining forest is reduced by between 13-66% (depending on the model - see 'macroecon_models.xlsx'). Conversely, for every unit increase of remaining forest, the effect of population density goes up. In one case, when a 1-year lag is applied, there is an interaction whereby one unit increase of time (1 year) reduces the effect of pop_den, but only by 0.1%, so not really that useful. Although perhaps this suggest that if we had more data, we would see that the effect of population density on forest loss is getting smaller over time. 

# In terms of models, the models with 2 year lag have the lowest AICc values.  All of the models have very similar R2 values. Based on all of the above, me.mod2lag.4 constitutes the best model when each component is taken into account, and the AICc is the lowest of all the models.


### plot the effects of me.mod2lag.4

## pop_den

# expand grid and predict pop_den with min, max, and mean values of for_rem
df.pop_den <- expand.grid(pop_den.lag2.cen = seq(min(dat_me_lag_dredge$pop_den.lag2.cen),
                                    max(dat_me_lag_dredge$pop_den.lag2.cen),length=100),
                       for_rem.lag2.cen = c(-6313.347, -6.548762e-12, 10056.03),
                       ind_gdp.lag2 = 0.8368, 
                       time = mean(dat_me_lag_dredge$time))
  
pop_den.pred <- predict(me.mod2lag.5, df.pop_den, int="c")
pop_den.pred <- as.data.frame(pop_den.pred)
df.pop_den$fit <- pop_den.pred$fit
df.pop_den$upr <- pop_den.pred$upr
df.pop_den$lwr <- pop_den.pred$lwr

# predictions for pop_den when for_rem at minimum
df.pop_den.min <- df.pop_den[1:100, ]

# predictions for pop_den when for_rem at mean
df.pop_den.mean <- df.pop_den[101:200, ]

# predictions for pop_den when for_rem at max
df.pop_den.max <- df.pop_den[201:300, ]

# plot pop_den partial effect with varying values of for_rem
p1 <- ggplot(df.pop_den, aes(x=pop_den.lag2.cen))+
  geom_line(data=df.pop_den.mean, aes(y=fit, color="mean"), size=1.5)+
  geom_ribbon(data=df.pop_den.mean, aes(ymin=lwr, ymax=upr, alpha=0.4), fill="#56B4E9", show.legend=F)+
  
  geom_line(data=df.pop_den.min, aes(y=fit, color="min (2015)"), size=1.5)+
  geom_ribbon(data=df.pop_den.min, aes(ymin=lwr, ymax=upr, alpha=0.4), fill="#009E73", show.legend=F)+
 
  geom_line(data=df.pop_den.max, aes(y=fit,color="max (1996)"), size=1.5)+
  geom_ribbon(data=df.pop_den.max, aes(ymin=lwr, ymax=upr, alpha=0.4), fill="#D55E00", show.legend=F)+
  
  scale_color_manual(name = "Remaining forest\nat time t-2", 
                     values = c("max (1996)"="#D55E00","mean"="#56B4E9", "min (2015)"="#009E73"),
                     breaks = c("max (1996)", "mean", "min (2015)"))+
  
 
  xlab("Changes in population density at time t-2 (centered)")+
  ylab("Amount of forest lost at time t (ha)")+
  theme(text = element_text(size=15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))


  
## for_rem

# expand grid and predict for_rem with min, max, and mean values of pop_den
df.for_rem <- expand.grid(for_rem.lag2.cen = seq(min(dat_me_lag_dredge$for_rem.lag2.cen),
                                    max(dat_me_lag_dredge$for_rem.lag2.cen),length=100),
                       pop_den.lag2.cen = c(-0.2865, -8.880632e-17, 0.5235),
                       ind_gdp.lag2 = 0.8368, 
                       time = mean(dat_me_lag_dredge$time))
  
for_rem.pred <- predict(me.mod2lag.5, df.for_rem, int="c")
for_rem.pred <- as.data.frame(for_rem.pred)
df.for_rem$fit <- for_rem.pred$fit
df.for_rem$upr <- for_rem.pred$upr
df.for_rem$lwr <- for_rem.pred$lwr

# predictions for for_rem when pop_den at minimum
df.for_rem.min <- df.for_rem[1:100, ]

# predictions for for_ren when pop_den at mean
df.for_rem.mean <- df.for_rem[101:200, ]

# predictions for for_rem when pop_den at max
df.for_rem.max <- df.for_rem[201:300, ]


# plot for_rem partial effect with varying values of pop_den
p2 <- ggplot(df.for_rem, aes(x=for_rem.lag2.cen))+
  geom_line(data=df.for_rem.mean, aes(y=fit, color="mean"), size=1.5)+
  geom_ribbon(data=df.for_rem.mean, aes(ymin=lwr, ymax=upr, alpha=0.4), fill="#56B4E9", show.legend=F)+
  
  geom_line(data=df.for_rem.min, aes(y=fit, color="min"), size=1.5)+
  geom_ribbon(data=df.for_rem.min, aes(ymin=lwr, ymax=upr, alpha=0.4), fill="#009E73", show.legend=F)+
 
  geom_line(data=df.for_rem.max, aes(y=fit,color="max"), size=1.5)+
  geom_ribbon(data=df.for_rem.max, aes(ymin=lwr, ymax=upr, alpha=0.4), fill="#D55E00", show.legend=F)+
  
  
  scale_color_manual(name = "Changes in population\ndensity at time t-2", 
                     values = c("mean"="#56B4E9", "min"="#009E73", "max"="#D55E00"))+
 
  xlab("Amount of forest remaining at time t-2 (centered)")+
  ylab("Amount of forest lost at time t (ha)")+
  theme(text = element_text(size=15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
 

## ind_gdp

## for_rem

# expand grid and predict for_rem with min, max, and mean values of pop_den
df.ind_gdp <- expand.grid(ind_gdp.lag2 = seq(-0.8000,3.4000, length=100),
                       pop_den.lag2.cen = mean(dat_me_lag_dredge$pop_den.lag2.cen),
                       for_rem.lag2.cen = mean(dat_me_lag_dredge$for_rem.lag2.cen),  
                       time = mean(dat_me_lag_dredge$time))
  
ind_gdp.pred <- predict(me.mod2lag.5, df.ind_gdp, int="c")
ind_gdp.pred <- as.data.frame(ind_gdp.pred)
df.ind_gdp$fit <- ind_gdp.pred$fit
df.ind_gdp$upr <- ind_gdp.pred$upr
df.ind_gdp$lwr <- ind_gdp.pred$lwr


# plot ind_gdp partial effect 
p3 <- ggplot(df.ind_gdp, aes(x=ind_gdp.lag2, y=fit))+
  geom_line(color="#56B4E9", size=1.5)+
  geom_ribbon(aes(ymin=lwr, ymax=upr, alpha=0.4), fill="#56B4E9", show.legend=F)+
  xlab("Changes in Industrial sector proportion of GDP (%) at time t-2")+
  ylab("Amount of forest lost at time t (ha)")+
  theme(text = element_text(size=15))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))
  
  
ggsave("Results/Macroeconomics/Plots/me.plot.pop_den.png", p1, width = 30, height = 20, units = "cm", dpi=300)
ggsave("Results/Macroeconomics/Plots/me.plot.for_rem.png", p2, width = 30, height = 20, units = "cm", dpi=300)
ggsave("Results/Macroeconomics/Plots/me.plot.ind_gdp.png", p3, width = 20, height = 20, units = "cm", dpi=300) 


  ## commodity data ####


# model for dredge with no armi
com.mod1 <- lm(for_cov ~ cpi + nfi + rice_med + rub_med + corn_med + sug_med + for_prod + for_rem + time,
               data = dat_com, na.action="na.fail")
summary(com.mod1)

# dredge
dredge.com1 <- dredge(com.mod1, beta = "none", evaluate = TRUE, rank = AICc)
write.csv(dredge.com1, file="dredge.com1.csv")

# for_rem + time, plus rice_med, rub_med, sug_med are in the top candidate models


# now lets check dredge with armi and rice_med only
com.mod2 <- lm(for_cov ~ cpi + nfi + rice_med + armi + for_prod + for_rem + time,
               data = dat_com, na.action="na.fail")
summary(com.mod2)

# dredge
dredge.com2 <- dredge(com.mod2, beta = "none", evaluate = TRUE, rank = AICc)
write.csv(dredge.com2, file="dredge.com2.csv")

# for_rem + time, plus armi, rice_med.  I will use armi instead of rub_med and sug_med as it reduces the number of terms in the models, and armi reflects the trends in those commodities anyway.

# fit the model
com.mod3 <- lm(for_cov ~ armi + rice_med + for_rem + time, data=dat_com)
summary(com.mod3)
# neither armi or rice_med are sig in this model, which was to be expected as in the dredge they do not appear together in the top models. I will test models with both in indiviudally

# remove rice_med
com.mod4 <- lm(for_cov ~ armi + for_rem + time, data = dat_com)
summary(com.mod4)
par(mfrow=c(2,2))
plot(com.mod4)
# Plots look ok.  armi is still not signifcant which is interesting
vif(com.mod4)
# vifs are good (~1)

# remove armi but leave rice_med in
com.mod5 <- lm(for_cov ~ rice_med + for_rem + time, data=dat_com)
summary(com.mod5)
plot(com.mod5)
# perhaps very slight signs of heterosced
vif(com.mod5)
# vifs good

# test interactions
com.mod6 <- lm(for_cov ~ armi * rice_med * for_rem * time, data = dat_com)
summary(com.mod6)
# only for_rem is sig

# reduce complexity
com.mod7 <- lm(for_cov ~ armi * rice_med * for_rem + time, data = dat_com)
summary(com.mod7)
# result is the same


# out of curiosity I will test some of the other vars 
com.mod8 <- lm(for_cov ~ cpi + for_rem + time, data = dat_com)
summary(com.mod8)

com.mod9 <- lm(for_cov ~ nfi + for_rem + time, data = dat_com)
summary(com.mod9)

com.mod10 <- lm(for_cov ~ for_prod + for_rem + time, data = dat_com)
summary(com.mod10)

com.mod11 <- lm(for_cov ~ rub_med + for_rem + time, data = dat_com)
summary(com.mod11)

# nope


## now I will test with time lags

# create data

dat_com_lag <- data.frame(year = dat_com$year,
                          time = dat_com$time,
                          for_cov = dat_com$for_cov,
                          for_rem.lag1 = lag(dat_com$for_rem),
                          for_rem.lag2 = lag(dat_com$for_rem, n=2L),
                          armi.lag1 = lag(dat_com$armi),
                          armi.lag2 = lag(dat_com$armi, n=2L),
                          cpi.lag1 = lag(dat_com$cpi),
                          cpi.lag2 = lag(dat_com$cpi, n=2L),
                          nfi.lag1 = lag(dat_com$nfi),
                          nfi.lag2 = lag(dat_com$nfi, n=2L),
                          rice_med.lag1 = lag(dat_com$rice_med),
                          rice_med.lag2 = lag(dat_com$rice_med, n=2L),
                          rub_med.lag1 = lag(dat_com$rub_med),
                          rub_med.lag2 = lag(dat_com$rub_med, n=2L),
                          corn.med.lag1 = lag(dat_com$corn_med),
                          corn_med.lag2 = lag(dat_com$corn_med, n=2L),
                          sug_med.lag1 = lag(dat_com$sug_med),
                          sug_med.lag2 = lag(dat_com$sug_med, n=2L),
                          for_prod.lag1 = lag(dat_com$for_prod),
                          for_prod_lag2 = lag(dat_com$for_prod, n=2L))

# remove first row for 1-year lag
dat_com_lag1 <- dat_com_lag[2:22, ]
str(dat_com_lag1)
head(dat_com_lag1)

# remove first two rows for 2-year lag
dat_com_lag2 <- dat_com_lag[3:22, ]
str(dat_com_lag2)
head(dat_com_lag2)


## model dredge for 1-year lag with no armi
com.modlag.1 <- lm(for_cov ~ cpi.lag1+nfi.lag1+rice_med.lag1+rub_med.lag1+corn.med.lag1+sug_med.lag1+
                     for_prod.lag1 + for_rem.lag1 + time, data = dat_com_lag1, na.action="na.fail")
summary(com.modlag.1)

dredge.com.lag1 <- dredge(com.modlag.1, beta = "none", evaluate = TRUE, rank = AICc)
write.csv(dredge.com.lag1, file="dredge.comm.lag1.csv")

# for_prod, rub_med (and for_rem, time) are now in the top candidate models from dredge

# re-create full model with those terms
com.modlag.2 <- lm(for_cov ~ for_prod.lag1 + rub_med.lag1 + for_rem.lag1 + time, data=dat_com_lag1)
summary(com.modlag.2)
# for_prod.lag1 only slightly signficant
plot(com.modlag.2)
# definitely looks like some hetersced. Not dramatic but its there. But could be being caused by those two points on the far right of the resid v fitted plot.  I bet you one of the points is 2015 - the really small value which was the same as 1993 and just a bit weird

# remove 2015
dat_com_lag1.sub <- dat_com_lag1[1:20, ]

# re-run model
com.modlag.3 <- lm(for_cov ~ for_prod.lag1 + rub_med.lag1 + for_rem.lag1 + time, data=dat_com_lag1.sub)
summary(com.modlag.3)
plot(com.modlag.3)
# ok so that made no difference 

# based on cooks distance point 8 (rowname 9) is having large effect. I'll try and remove that
dat_com_lag1.sub <- dat_com_lag1[c(1:6,8:21), ]

# re-run model
com.modlag.4 <- lm(for_cov ~ for_prod.lag1 + rub_med.lag1 + for_rem.lag1 + time, data=dat_com_lag1.sub)
summary(com.modlag.4)
plot(com.modlag.4)
# removing point 8 doesn't really reduce the heterosced, but does make the QQ plot worse!

# the problem could be non-linearity.  Try adding quadratic terms
# first have to center
dat_com_lag1$for_prod.lag1.cen <- scale(dat_com_lag1$for_prod.lag1, center = T,scale = F)
dat_com_lag1$rub_med.lag1.cen <- scale(dat_com_lag1$rub_med.lag1, center = T, scale = F)

# add quadratic term to for_prod
com.modlag.5 <- lm(for_cov ~ for_prod.lag1.cen + I(for_prod.lag1.cen^2) + 
                     rub_med.lag1 + for_rem.lag1 + time, data=dat_com_lag1)
summary(com.modlag.5)                   
plot(com.modlag.5)                   
# helps very slightly

# add quadratic term to rub_med
com.modlag.6 <- lm(for_cov ~ for_prod.lag1 + rub_med.lag1.cen + I(rub_med.lag1.cen^2) + 
                     for_rem.lag1 + time, data=dat_com_lag1)
summary(com.modlag.6)                   
plot(com.modlag.6)                   
# I think that makes the non-linearity worse

# both terms quadratic
com.modlag.7 <- lm(for_cov ~ for_prod.lag1.cen + I(for_prod.lag1.cen^2) + 
                     rub_med.lag1.cen + I(rub_med.lag1.cen^2) + 
                     for_rem.lag1 + time, data=dat_com_lag1)
summary(com.modlag.7)                   
plot(com.modlag.7)                   
# there are a handful of points on the left of the resid v fit plot that are causing the issue

# try log-transforming the y
com.modlag.8 <- lm(log(for_cov) ~ for_prod.lag1.cen + rub_med.lag1.cen + for_rem.lag1 + time, 
                   data=dat_com_lag1)
summary(com.modlag.8)                   
plot(com.modlag.8)                   
# point 22 is causing issues (which is 2015)

# remove that point
dat_com_lag1.sub <- dat_com_lag1[1:20, ]
                   
com.modlag.9 <- lm(log(for_cov) ~ for_prod.lag1 + rub_med.lag1 + for_rem.lag1 + time, 
                   data=dat_com_lag1.sub)
summary(com.modlag.9)                   
plot(com.modlag.9)               
# distinctly non-linear. Point 21 now causing the issue

# remove that point
dat_com_lag1.sub <- dat_com_lag1.sub[1:19, ]
         
com.modlag.10 <- lm(log(for_cov) ~ for_prod.lag1 + rub_med.lag1 + for_rem.lag1 + time, 
                   data=dat_com_lag1.sub)
summary(com.modlag.10)                   
plot(com.modlag.10)            
# still very non-linear

# try quadratic terms with log-transformed y
com.modlag.11 <- lm(log(for_cov) ~ for_prod.lag1.cen + I(for_prod.lag1.cen^2) + 
                    rub_med.lag1.cen + I(rub_med.lag1.cen^2) + 
                    for_rem.lag1 + time, data=dat_com_lag1.sub)
summary(com.modlag.11)                   
plot(com.modlag.11)
# worse                   


## move on to GLM.  I will start with Gamma distribution (continuous non-negative Y) with inverse link 
com.glmlag.1 <- glm(for_cov ~ for_prod.lag1 + rub_med.lag1 + for_rem.lag1 + time, 
                    family = Gamma(link="inverse"), data=dat_com_lag1)
summary(com.glmlag.1) 
com.glmlag.1_diag <- glm.diag(com.glmlag.1)
glm.diag.plots(com.glmlag.1, com.glmlag.1_diag)                   
# still the problem with non-linear residuals. But this isn't an issue with GLMs right?!

# compare with log link
com.glmlag.2 <- glm(for_cov ~ for_prod.lag1 + rub_med.lag1 + for_rem.lag1 + time, 
                    family = Gamma(link="log"), data=dat_com_lag1)
summary(com.glmlag.2) 
com.glmlag.2_diag <- glm.diag(com.glmlag.2)
glm.diag.plots(com.glmlag.2, com.glmlag.2_diag) 

# compare with identity link
com.glmlag.3 <- glm(for_cov ~ for_prod.lag1 + rub_med.lag1 + for_rem.lag1 + time, 
                    family = Gamma(link="identity"), data=dat_com_lag1)
summary(com.glmlag.3) 
com.glmlag.3_diag <- glm.diag(com.glmlag.3)
glm.diag.plots(com.glmlag.3, com.glmlag.3_diag) 