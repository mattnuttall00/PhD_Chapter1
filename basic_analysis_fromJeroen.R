library(lme4)

# load data
dat <- read.csv("dat_use.csv", header = TRUE, stringsAsFactors = TRUE)
dat <- dat[ ,-1]

# re-classify the variables
dat$year <- as.numeric(dat$year)
dat$elc <- as.factor(dat$elc)
dat$PA <- as.factor(dat$PA)

str(dat)

dat$year.s = as.vector(scale(dat$year))
dat$tot_pop.s = as.vector(scale(dat$tot_pop))

m1 = glmer(ForPix ~ tot_pop.s + (year.s|Province/Commune), data = dat, family = "poisson")
m2 = glmer(ForPix ~ tot_pop.s + year.s + (year.s|Province/Commune), data = dat, family = "poisson")

# this gives the fixed and random effects respectively for the first model:
fixef(m1)
ranef(m1)
# coef() I think just adds the fixed effect estimates as columns to what would have been ranef()
coef(m1)
# Because when we look at coef(m2):
coef(m2)
# The number that is reported in the year.s column is kind of the combined slope for year, for a given Commune/Province
# combination. I think it's a bit more general to be able to handle/interpret the random and fixed components
# separately, as below.
# To try to illustrate, we can look the fixed effect estimate for year.s in m2 like this:
fixed(m2)
# It also shows as significantly different from zero, but see at the very end of this script!
summary(m2)
# Anyway, how does this relate to the numbers reported in fixef() and ranef()?
# Let's try to reconstruct for a given Commune/Province (I've just picked a random one here).
# This is the commune-level "effect" for year.s for Lumpong:
ranef(m2)[["Commune:Province"]]["Lumpong:Takeo","year.s"]
# This is the same but for the Province level:
ranef(m2)[["Province"]]["Takeo","year.s"]
# This is the overall fixed effect of year.s:
fixef(m2)["year.s"]
# So for this Commune, the "effect" of Province is the sum of these:
fixef(m2)["year.s"] + ranef(m2)[["Province"]]["Takeo","year.s"] + ranef(m2)[["Commune:Province"]]["Lumpong:Takeo","year.s"]
# However, the numbers reported for year.s in coef() are sort of intermediates of this - they are the "part-effects" of
# year.s in Province, and then the part-effects of a specific Commune in a specific Province:
# So this:
coef(m2)[["Province"]]["Takeo","year.s"]
# ... equals this:
fixef(m2)["year.s"] + ranef(m2)[["Province"]]["Takeo","year.s"]
# And this:
coef(m2)[["Commune:Province"]]["Lumpong:Takeo","year.s"]
# equal this:
fixef(m2)["year.s"] + ranef(m2)[["Commune:Province"]]["Lumpong:Takeo","year.s"]
# Note that althought this makes sense, the latter number probably isn't really useful outside purely statistical terms
# - because the terms are nested, the effect of Commune is always contingent on the effect of Province - and the latter
# aren't explicitly accounted for the the last number. Probably more importantly, if you just use coef() in this case,
# you can't "see" anything about the overall effect of year.s (the fixed effect) - it is not reported in the tables,
# presumably because the assumption is that the user already knows that it will be part of the Province- and
# Communue-specific effects of year.s!
# If you instead use fixef() and ranef() at least you are very explicit about what nunbers you are extracting.

### To intepret the estimates - predicting from/with them. 

### Predicting from m1 - this is very easy using predict(). This makes predictions for every observed value in your data:
m1_pred = as.vector(predict(m1, type = "response"))

### Let's manually reconstruct this to work out what is going on:

# Just to keep things easy, extract a subset dataset relevant to this model:
m1_est = subset(dat, select = c("ForPix","year.s","tot_pop.s","Province","Commune"))
# Now to this subset data set, we add the relevant estimates from the model, varying by line (case) as appropriate.
# The global intercept is obviously just the global intercept@
m1_est$Iglobal = fixef(m1)[["(Intercept)"]]
# We set the intercept to vary by Province, and then by Commune w/i Province. So we need to match each of these to our
# subset data frame by extracting them from the appropriate fixed effect structures. These following commands look a
# little hairy but are actually pretty easy. They just use match() to match indexes between Province names in m1_est and
# Province names (listed as row names) in ranef(m1)$Province.
m1_est$Iprovince = ranef(m1)$Province[,"(Intercept)"][match(m1_est$Province, row.names(ranef(m1)$Province))]
# With the next "level" (Commune), we need to do the same matching but based on the row names in
# ranef(m1)$`Commune:Province`. These are concatenations of Commune:Province so we create a helper column to match on
# this first:
m1_est$CommProv = paste(m1_est$Commune,m1_est$Province,sep=":")
# We now use this to match random intercepts to the data subset. ranef(m1)[[1]] is the same as
# ranef(m1)$`Commune:Province`.
m1_est$Icommune = ranef(m1)[[1]][,"(Intercept)"][match(m1_est$CommProv, row.names(ranef(m1)[[1]]))]

# For m1, there is only the further effect of year.s in the random structure. This again varies by Province, and then by
# Commune in Province. We need to extract these in the same way as above:
m1_est$b_year_province = ranef(m1)$Province[,"year.s"][match(m1_est$Province, row.names(ranef(m1)$Province))]
m1_est$b_year_commune = ranef(m1)[[1]][,"year.s"][match(m1_est$CommProv, row.names(ranef(m1)[[1]]))]

# The last estimate we need to get out is the (fixed) slope effect for tot_pop.s:
m1_est$b_totpops = fixef(m1)[["tot_pop.s"]]

### Quick look at what we created:
head(m1_est)

### So the predictions for this first model, in words, are essentially: (global_intercept+(IProvince+Icommune)_given
#province/commune) + tot_pop.s*b_totpops + (year.s*(b_year_province+b_year_commune)_given province_commune). To do this
#we can essentially just use all the column values we have just extracted. In the below I use with() to avoid having to
#repeat the reference to the dataframe for each predictor - I hope this makes it a bit more readable:

mpred_m1 = with(m1_est, {
    Iglobal+                                                      # Fixed intercept (does not vary by Province/Commune)
    tot_pop.s*b_totpops +                                         # Fixed effect of population size (does not vary by Province/Commune)
    Iprovince +                                                   # Random intercept for province
    Icommune +                                                    # Random intercept for commune in province 
    year.s*(b_year_province+b_year_commune) # Random slope effect of year.s, varying by province/commune. 
      # Note that the "slope" is the sum of the RE estimate for each commune/province combination. 
})

# The above "manual prediction" is on the model scale, so log-link, which means you want to "back-transform" it to get
# it on to the real, Forest Pixel count scale.
# In other words, this
exp(mpred_m1)
# Should be the same as:
pred_m1 = as.vector(predict(m1, type = "response"))
# Which it is, near enough:
plot(exp(mpred_m1),pred_m1)
abline(a = 0, b = 1, col = "red")

# So essentially what this means is that in this case, any "effect" of year is purely present in the random effects.
# That is to say, if you want, you can get specific predictions for the specific Communes/Provinces in your observed
# data, as per the adding of the RE components in the calculations above. Of course you are more interested in making
# statements "in general", so given differences between communes, what are the effects of total population size on
# forest cover? To do that, you can just use the re.form argument in predict():
pred_m1_noRE = predict(m1, type = "response", re.form = NA)
# This essentially "sets" any random effects to zero, so predicting for an "average" commune/province. The following
# calculation should repeat this:
mpred_m1_noRE = with(m1_est, {
  Iglobal+                            # Fixed intercept (does not vary by Province/Commune)
    tot_pop.s*b_totpops              # Fixed effect of population size (does not vary by Province/Commune)
})
plot(mpred_m1_noRE, pred_m1_noRE)
# Note that here, you didn't specify any changes that occurred by year, because your model did not specify any effect of
# year that did not vary by Province or Commune. So essentially, in this case you can't make any specific predictions
# for a given year - because you are predicting for an "average" commune and you did not specify an "overall" slope for
# year. This may or may not be what you actually want - do you want to make predictions for a specific year? Do you want
# to test whether, regardless of anything else, there is an effect of year? (I suspect not - as I was suggesting
# earlier, from what I understand you want to make statements about the effect of socio-economic variables, and the only
# reason you have multiple year measurements is because they give you variation in measurements within communes - so
# "year" per se is just a nuisance, something you want to make sure you control for)
# If yes, I think you would want your other model specification which also includes year as a fixed effect:

summary(m2)
# To predict with this:
pred_m2 = predict(m2, type = "response")
# Or, overall for an "average" Commune:
pred_m2_noRE = predict(m2, type = "response", re.form = NA)

# To repeat these calculations quickly we need to make another "prediction matrix". I like doing this by hand as above.
# The following just repeats the process above but for m2 and without comments:
m2_est = subset(dat, select = c("ForPix","year.s","tot_pop.s","Province","Commune"))
m2_est$Iglobal = fixef(m2)[["(Intercept)"]]
m2_est$Iprovince = ranef(m2)$Province[,"(Intercept)"][match(m2_est$Province, row.names(ranef(m2)$Province))]
m2_est$CommProv = paste(m2_est$Commune,m2_est$Province,sep=":")
m2_est$Icommune = ranef(m2)[[1]][,"(Intercept)"][match(m2_est$CommProv, row.names(ranef(m2)[[1]]))]
m2_est$b_year_province = ranef(m2)$Province[,"year.s"][match(m2_est$Province, row.names(ranef(m2)$Province))]
m2_est$b_year_commune = ranef(m2)[[1]][,"year.s"][match(m2_est$CommProv, row.names(ranef(m2)[[1]]))]
m2_est$b_totpops = fixef(m2)[["tot_pop.s"]]
# The only extra thing we need here is the fixed slope for year:
m2_est$b_years = fixef(m2)[["year.s"]]

# So to manually repeat the predictions for m2 - note the extra fixed effect term:
mpred_m2 = with(m2_est, {
  Iglobal+                            
    tot_pop.s*b_totpops +             
    year.s*b_years +                  # Fixed effect of year independent of Province/Commune
    Iprovince +                       
    Icommune +                         
    year.s*(b_year_province+b_year_commune) 
})
# Double check if we got that right - note the backtransformation!:
plot(exp(mpred_m2), pred_m2)
abline(a = 0, b = 1, col = "red")
# Yup.

# So now, if we use/interpret using m2, if we want to make statements about an "average" Commune/Province, like so:
predict(m2, type = "response", re.form = NA)
# ... we are making predictions for a specific year:
mpred_m2_noRE = with(m2_est, {
  Iglobal+                            
    tot_pop.s*b_totpops +             
    year.s*b_years          
})
# In the case of the above, we are predicting for OBSERVED VALUES, i.e. values for tot_pop.s and year.s that are
# actually in your data. But, if you think about it, if you want to make "general" predictions, that for example, if
# we'd want to plot the model predictions over a range of population sizes, this model m2 requires us to specify what
# YEAR we want to make these predictions for - because that is a term in the model. This is not a problem, and the usual
# approach is to just take the "average" for whatever predictor you are not plotting. This is where
# standardising/scaling comes in handy, because it sets the mean to 0 - which means that predicting for 0 values for a
# given term, means the term "drops out" of the equation, which means that you can just ignore whatever predictor you
# are not interested in. So lets say for the sake of argument you want to plot model predictions foor ForPix as a
# function of population size for an "average" year (yes - what is that - but never mind) and an "average" Commune, you
# would just do this:
# Create a range of population sizes you want to predict for - here just a run between min and max observed year:
population = seq(min(dat$tot_pop.s),max(dat$tot_pop.s),0.1)
# Predicting overall, means being able to ignore the RE's, which makes the calculating of predictions VERY simple
# (overall intercept plues overall slope for popilation times whatever population size:)
forpix = exp(fixef(m2)[1]+fixef(m2)[2]*population)
# Or, alternatively, you can use predict() to do exactly the same, using newdata = to specify whatever new values you
# want to predict for. Note that this argument expects a dataframe and it needs ALL the input values with the same names
# as in teh origianl model input, so you need to specify year as 0 manually:
forpix = predict(m2, type = "response", re.form = NA, newdata = data.frame(tot_pop.s = population, year.s = 0))
# Incidentally, it helps to check what `data.frame(tot_pop.s = population, year.s = 0)` actually does - its useful to be
# able to set such prediction data frames up separately (see below) for making more intricate predictions.

# Plotting these predictions:
plot(dat$tot_pop.s, dat$ForPix) # Plots observations
lines(population, forpix, col = "red")  # Adds prediction line
# This obviously looks flat - no effect - which seems to be reflected in the significance of the year term, but also
# LOOKS like a pretty bad fit - particularly underpredicting in the low range of population sizes.
# Remember, however, that these predictions ignore the RE's for Commune/Province.
# And obviously, we know that the Communes in particular vary quite a lot. As an example, it's instructive to try to do 
#  the exact same but for a selection of Commune/Province combinations with particularly high or low values.
# Let's find some of these. These are your COmmune-level RE's:
ranef(m2)[[1]]
# Lets just store this with a handier name:
commune_re = ranef(m2)[[1]]
# Sort this from high to low intercept:
commune_re = commune_re[order(commune_re[,"(Intercept)"], decreasing = TRUE),]
# So the top five are:
head(commune_re, 5)
# Save the names of these
top5 = row.names(head(commune_re, 5))
top5
# There are easy ways to actually extract these values and split to Province/Commune but to save time, I'll just copy 
#  (some of) them into the code below:

# So to add prediction lines for specific communes to the existing plot.
# The predicts for a specific Commune/Province, here the one with the highest intercept:
forpix_com1 = predict(m2, type = "response", re.form = ~(year.s|Province/Commune), 
                      newdata = data.frame(tot_pop.s = population, year.s = 0, Province = "Kampot", Commune = "Trapeang Plang"))
lines(population, forpix_com1, col = "blue")
# Add another:
forpix_com2 = predict(m2, type = "response", re.form = ~(year.s|Province/Commune), 
                      newdata = data.frame(tot_pop.s = population, year.s = 0, Province = "Battambang", Commune = "Chhnal Moan"))
lines(population, forpix_com2, col = "blue")


### You can extend this same process as much as you like, for whatever predictors in the model.

### As an example, here for the "effect" of year: 

# Extract all "observed" random effect levels (on a commune level):
provcomm = row.names(ranef(m2)[[1]])
# Add these to a dataframe, repeating each of them for each unique observed year:
pred_frame = expand.grid(provcomm = as.vector(provcomm), year.s = unique(dat$year.s))
# We need to pick a tot_pop.s value to predict for. Given that we are now predicting for
# specific Communes/Provinces, we probably should do this for the actual observed pop sizes in each of the
# Commune/Provinces - but given the minimal statistical effect of tot_pop.s, it probably matters little.
# This just matches from observed data:
pred_frame$tot_pop.s = dat$tot_pop.s[match(pred_frame$provcomm, paste(dat$Commune, dat$Province,sep=":"))]

# Split that "provcomm" column into Province and Commune values:
prov = unlist(lapply(strsplit(provcomm,":"),function(x) x[2]))
comm = unlist(lapply(strsplit(provcomm,":"),function(x) x[1]))
pred_frame$Commune = comm
pred_frame$Province = prov
# So this is everything we need to predict with, I think:
head(pred_frame)
# Make the predictions for each line in this "new" data set:
pred_frame$pred = m2_commprov = predict(m2, type = "response", re.form = ~(year.s|Province/Commune), newdata = pred_frame)
# Plot observed ForPix - years relationship:
plot(dat$year.s, dat$ForPix)
# Doesn't look like much. Let's add the overall prediction (sans RE's):
pred_m2_noRE = predict(m2, type = "response", re.form = NA, 
                           newdata = data.frame(year.s =unique(dat$year.s), tot_pop.s = mean(dat$tot_pop.s)))
lines(unique(dat$year.s), pred_m2_noRE, col = "red", lwd = 2)
### This obviously looks terrible in terms of fit - but again you have to bear in mind this has been done without much
### model validation, we haven't really considered big differences in Commune size, and we know there are big
### differences in Communes in terms of overall intercepts (which is of course reflected in our RE intercepts, but it's
### still likely to cause some distributional issues.). Having said this, my main intent here was just to illustrate how
### to understand estimated coefficients - and as a result, when you can plot predictions, you can start to really
### assess the validity (or perhaps limiations, rather) of your model. Because of the nature of GLMMs, stronger (small)
### effects tend to "shrink" the effects of weaker big effects, if that makes sense? Put another way, even if there is a
### (relatively) small number of Communes with a strong decline over years, this effect may be "swamped" by the nil
### effect in the majortity.

# We can now add predictions fo each of the Communes individually. To do that, we need to step through our "bigger"
# predictions data frame made above:
for(i in 1:nlevels(pred_frame$provcomm)) {
  provcomm_i = pred_frame[pred_frame$provcomm == levels(pred_frame$provcomm)[i],]
  lines(unique(dat$year.s), provcomm_i$pred, col = "red")
}
# This really looks like there is not much change overall - but it's probably because you can't really see those that
# are declining. It's hard to see but there are indeed a few with negative slopes - replot for a limited Y axis range:
plot(dat$year.s, dat$ForPix, ylim = c(0,5000))
for(i in 1:nlevels(pred_frame$provcomm)) {
  provcomm_i = pred_frame[pred_frame$provcomm == levels(pred_frame$provcomm)[i],]
  lines(unique(dat$year.s), provcomm_i$pred, col = "red")
}

# I must admit, however, that look at this I am quite amazed that the effect of year.s is actually reported as
# "significant" in m2:
summary(m2)
# It clearly is a very small effect, but more importantly, looking at the plot we just did I think this really
# illustrates how tricky the interpretation of overall p-values in GLMMs are.

### A few quick summary notes after having done this: 

###1.  the "effect" of year per se is probably not that interesting, you really want to focus on the socioeconomic
###      effects GIVEN some account for (random) year variation.

### 2. be very careful about interpreting p-values in mixed effect models particularlif you have large variation in RE
###     effects - I would strongly suggest focusing on assessment of effects through predictions. PLOT PLOT PLOT.

### 3. Looking at the extent of variation in intercepts in particular in the above models I really think you need some
###     way of accounting for variation in physical size of Communes. I hope that doing that will make the distribution of
###     ForPix a bit more manageable.