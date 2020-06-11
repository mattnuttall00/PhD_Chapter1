library(lme4)
library(ggplot2)

# load data
dat <- read.csv("Data/commune/dat_use.csv", header = TRUE, stringsAsFactors = TRUE)
dat <- dat[ ,-1]

# re-classify the variables
dat$year <- as.numeric(dat$year)
dat$elc <- as.factor(dat$elc)
dat$PA <- as.factor(dat$PA)

# scale vars
dat1 <- dat
vrs <- c("year","tot_pop","prop_ind","pop_den","M6_24_sch","propPrimSec","propSecSec","Les1_R_Land",
        "pig_fam","dist_sch","garbage","KM_Comm","land_confl","crim_case","Pax_migt_in",
        "Pax_migt_out","mean_elev","dist_border","dist_provCap")
dat1[,vrs] = apply(dat1[,vrs],2,scale)

# create unique commune name
dat1$Provcomm <- paste(dat1$Province, dat1$Commune, sep = "_")


# model
popdem.m4 <- glmer(ForPix ~ tot_pop * pop_den + offset(log(areaKM)) + (year|Province/Provcomm), 
                   data = dat1, family = "poisson")


# save the popdem.m4 commune-level random effects
m4.re.com <- ranef(popdem.m4)[[1]]
plot_model(popdem.m4, type="re")

# re-order
m4.re.com <- m4.re.com[order(m4.re.com[ ,"(Intercept)"], decreasing = TRUE),]

# the 4 communes closest to 0 are:
# Kampot_Chres:Kampot, 
# Koh Kong_Tuol Kokir Leu:Koh Kong, 
# Kampong Cham_Cheyyou:Kampong Cham, 
# Kracheh_Ta Mau:Kracheh

# the 4 communes furthest above 0 are:
# Phnom Penh_Chak Angrae Kraom:Phnom Penh
# Kampong Cham_Pongro:Kampong Cham
# Kampong Chhnang_Chieb:Kampong Chhnang
# Kampot_Preaek Tnaot:Kampot

# the 4 communes furthest below 0 are:
# Kampong Thom_Chaeung Daeung:Kampong Thom
# Kracheh_Han Chey:Kracheh
# Siem Reap_Nokor Pheas:Siem Reap
# Pursat_Boeng Bat Kandaol:Pursat

# which communes
coms <- c("Kampot_Chres","Koh Kong_Tuol Kokir Leu","Kampong Cham_Cheyyou","Kracheh_Ta Mau",
          "Phnom Penh_Chak Angrae Kraom","Kampong Cham_Pongro","Kampong Chhnang_Chieb","Kampot_Preaek Tnaot",
          "Kampong Thom_Chaeung Daeung","Kracheh_Han Chey","Siem Reap_Nokor Pheas","Pursat_Boeng Bat Kandaol")

# which provinces
provs <- c("Kampot","Koh Kong", "Kampong Cham","Kracheh","Phnom Penh","Kampong Cham","Kampong Chhnang","Kampot",
           "Kampong Thom","Kracheh","Siem Reap","Pursat")

# create new prediction grid for specific communes with varying pop_den
m4_newdat_com <- data.frame(Provcomm = rep(coms, each=100),
                            Province = rep(provs, each=100),
                            pop_den = seq(from=min(dat1$pop_den), to=max(dat1$pop_den), length.out = 100),
                            tot_pop = mean(dat1$tot_pop),
                            year = mean(dat1$year))

# add commune-specific areaKM offset                         
m4_newdat_com$areaKM <-  dat1$areaKM[match(m4_newdat_com$Provcomm, dat1$Provcomm)]

# re-order levels so they plot in the correct sets
m4_newdat_com$Provcomm <- factor(m4_newdat_com$Provcomm, 
                                 levels = c("Kampot_Chres","Koh Kong_Tuol Kokir Leu","Kampong Cham_Cheyyou",
                                            "Kracheh_Ta Mau","Phnom Penh_Chak Angrae Kraom",
                                            "Kampong Cham_Pongro","Kampong Chhnang_Chieb","Kampot_Preaek Tnaot",
                                            "Kampong Thom_Chaeung Daeung","Kracheh_Han Chey","Siem Reap_Nokor Pheas",
                                            "Pursat_Boeng Bat Kandaol"))


# attach commune-specific predictions
m4_newdat_com$pred.com <- as.vector(predict(popdem.m4, type="response", newdata=m4_newdat_com, 
                                        re.form=~(year|Province/Provcomm)))


# create new prediction grid for global effects with varying pop_den
m4_newdat_glo <- data.frame(pop_den = seq(from=min(dat1$pop_den), to=max(dat1$pop_den), length.out = 100),
                            tot_pop = mean(dat1$tot_pop),
                            areaKM = mean(dat1$areaKM))


# global predictions (i.e. ignoring RE's)
pred.glo <- as.vector(predict(popdem.m4, type="response", newdata=m4_newdat_glo, re.form=NA))

# attach global predictions
m4_newdat_com$pred.glo <- rep(pred.glo, times=12)


# plot
ggplot(m4_newdat_com, aes(x=pop_den))+
  geom_line(aes(y=pred.glo))+
  geom_line(aes(y=pred.com), linetype="dashed")+
  facet_wrap(m4_newdat_com$Provcomm, nrow=3)+
  ylim(0,1000)+
  ylab("Predicted forest pixels")+
  xlab("Population density (scaled)")+
  theme(element_blank())