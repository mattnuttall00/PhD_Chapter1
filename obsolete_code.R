# Obsolete script from chapter 1 that I don't need right now but may be helpful in the future!

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
  