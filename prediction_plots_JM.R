library(lme4)
library(ggplot2)

# load data
dat <- read.csv("dat_use.csv", header = TRUE, stringsAsFactors = TRUE)
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
#plot_model(popdem.m4, type="re")   ### I don't know this function, doesn't work for me...

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

### If you are making commune-specific predictions, you probably should customise both the mean total population size,
### as well as the range of population densities you are predicting for, on the basis of each commune.

### Easiest to define the range of pop_den to predict for, first. Min/max per commune:
pop_den_min = tapply(dat1$pop_den, dat1$Provcomm, min)
pop_den_max = tapply(dat1$pop_den, dat1$Provcomm, min)
### Min/max within your selection of communes:
pop_den_min = min(pop_den_min[names(pop_den_min) %in% coms])
pop_den_max = max(pop_den_max[names(pop_den_max) %in% coms])
### Note these are quite different to the overall min/max:
min(dat1$pop_den)
max(dat1$pop_den)

# create new prediction grid for specific communes with varying pop_den
m4_newdat_com <- data.frame(Provcomm = rep(coms, each=100),
                            Province = rep(provs, each=100),
                            pop_den = seq(from=pop_den_min, to=pop_den_max, length.out = 100),
                            #tot_pop = mean(dat1$tot_pop),
                            year = mean(dat1$year))
### Add community-specific mean pop size:
provcomm_mean_totpop = as.data.frame(tapply(dat1$tot_pop, dat1$Provcomm, mean))
m4_newdat_com$tot_pop = provcomm_mean_totpop[,1][match(m4_newdat_com$Provcomm, row.names(provcomm_mean_totpop))]



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


### This is fine but given the choice of predicting for specific communes below I don't think it's needed.

# create new prediction grid for global effects with varying pop_den
# m4_newdat_glo <- data.frame(pop_den = seq(from=min(dat1$pop_den), to=max(dat1$pop_den), length.out = 100),
#                             tot_pop = mean(dat1$tot_pop),
#                             areaKM = mean(dat1$areaKM))
# global predictions (i.e. ignoring RE's)
#pred.glo <- as.vector(predict(popdem.m4, type="response", newdata=m4_newdat_glo, re.form=NA))
# attach global predictions
#m4_newdat_com$pred.glo <- rep(pred.glo, times=12)     

### To me doing the above doesn't make a huge amount of sense - you should really bind this to m4_newdat_glo - the
### commune-specific predictions of course 12 times more observations, so it's not really as if you have 12
### "measurements" here...


# # plot
# ggplot(m4_newdat_com, aes(x=pop_den))+
#   geom_line(aes(y=pred.glo))+
#   geom_line(aes(y=pred.com), linetype="dashed")+
#   facet_wrap(m4_newdat_com$Provcomm, nrow=3)+
#   ylim(0,1000)+
#   ylab("Predicted forest pixels")+
#   xlab("Population density (scaled)")+
#   theme(element_blank())

### The following plot "overlays" the observed ForPix count against observed population densities for the communes.

### Pick some colours using RColorBrewer using a completely overelaborate piece of crap code... Anyway this is just to
#try to help see the differences between communes more clearly in the observed points in particular (you can comment the
#following lines out if you want)
require(RColorBrewer)
com_colours = brewer.pal(11, "RdYlBu")
com_colours = c(head(com_colours,4),tail(com_colours,4))
com_colours_greys = tail(brewer.pal(9, "Greys"),4)
com_colours = c(com_colours, com_colours_greys)
com_colours = com_colours[sample(1:length(com_colours),12,replace=F)]

### This is just to check if you have commented tbe above out: :)
if(!exists("com_colours")) {
  com_colours = rep("black", 12)
}
provcomm_lvls = levels(m4_newdat_com$Provcomm) 
par(mfrow = c(1,1))
### Note the scales are important here - we need to set a scale that encompasses all the communes and the full
### population density range across all communes, so we need to do this overall:
ylo = min(m4_newdat_com$pred.com)*0.9
yhi = max(m4_newdat_com$pred.com)*1.1
xlo = min(dat1[dat1$Provcomm %in% levels(m4_newdat_com$Provcomm),"pop_den"])
xhi =  max(dat1[dat1$Provcomm %in% levels(m4_newdat_com$Provcomm),"pop_den"])
### Iterate through the communes (levels in m4_newdat_com$Provcomm):
for(i in 1:length(provcomm_lvls)) {
  ### Pick commune i data from the predictions:
  preddat_i = m4_newdat_com[m4_newdat_com$Provcomm==provcomm_lvls[i],]
  ### Pick commune i data from observed data:
  dat_i = dat1[dat1$Provcomm==provcomm_lvls[i],]
  ## If this is the first plot, use plot(), otherwise lines() to add to an existing plot:
  if(i == 1) {
    # Plot predicted ForPix as function of pop_den; as "line" type. Note this is where you set axis limits.
    plot(preddat_i$pop_den,preddat_i$pred.com, 
         type = "l", 
         col = com_colours[i], 
         ylim = c(ylo,yhi), xlim = c(xlo,xhi),
         xlab = "Population density (scaled & standardised",
         ylab = "Predicted forest cover (forest pixel count)")
  } else {
    lines(preddat_i$pop_den,preddat_i$pred.com, col = com_colours[i])
  }
  ## Add points for "observed" ForPix for commune i across all observed pop_den across communes.
  points(dat_i$pop_den, dat_i$ForPix, pch = 21, bg = com_colours[i], col = com_colours[i])
}
### So I think OVERALL this looks like a reasonable fit, especially given the large variance across communes. Bear in
### mind that the model estimates are based on a RE for all communes, so some of the "effects" that may not be strong in
### one commune are "shrunk" towards others where such effects may be stronger.

### In some ways, given the apparent fit (just based on eyeballing it), it's worth noting that this is for a
### deliberately "extreme" set of communes in terms of intercept; so that's pretty good I think. You could repeat the
### above process taking arbitrary samples of communes each time; and/or plot Province-level predictions (although
### you'll probably need to do a bit more judcious averageing of some values etc.)

