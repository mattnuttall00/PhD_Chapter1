### Distributions

## POISSON

# y >= 0 and is an integer.  Once we know u (mean) then we can estimate values of y.  For small values of y the density curve is skewed but for larger values the curve becomes symetrical.  The mean (u) can be a non-integer but the y values are non-nagative integers. P(Y<0) = 0.  The mean is the variance Var(Y) = u.  This is why the probability distributions get wider with larger mean value.  The Poisson distribution with large mean values looks like a normal distibution but it is not equal to a normal distribution because a normal distribution has two parameters, the mean and the variance, whereas Poission only has u which is the mean and the variance. 

x1 <- 0:10 
Y1 <- dpois(x1, lambda = 3)
x2 <- 0:10 
Y2 <- dpois(x2, lambda = 5)
x3 <- 0:40 
Y3 <- dpois(x3, lambda = 10)
x4 <- 50:150 
Y4 <- dpois(x4, lambda = 100)
XLab <- "Y values" 
YLab <- "Probabilities"
op <- par(mfrow = c(2, 2))
plot(x1, Y1, type = "h", xlab = XLab, ylab = YLab, main = "Poisson with mean 3")
# the type="h" command ensures that vertical lines are plotted. We use vertical lines because poisson distributions use discreet data
plot(x2, Y2, type = "h", xlab = XLab, ylab = YLab, main = "Poisson with mean 5")
plot(x3, Y3, type = "h", xlab = XLab, ylab = YLab, main = "Poisson with mean 10")
plot(x4, Y4, type = "h", xlab = XLab, ylab = YLab, main = "Poisson with mean 100")
par(op)

# In the above graphs we pretend we know the value of u. In real life we rarely do, and so GLMs model the value of u as a function of the explanatory variables. 

# Poisson distribution is commonly used for count data and its main advantages are that the probability for negative values is 0 and that the mean variance relationship allows for heterogeneity. However, in ecology, it is quite common to have data for which the variance is even larger than the mean, and this is called overdispersion. Depending how much larger the variance is compared to the mean, one option is to use the correction for overdispersion within the Poisson GLM, Alternatively, we may have to choose a different distribution, e.g. the negative binomial distribution.

## NEGATIVE BINOMIAL

# This is also a discrete distribution for non-negative data. It is presented in the literature as a combination of two distributions, giving a combined Poisson-gamma distribution. This means we first assume that the Ys are Poisson distributed with the mean μ assumed to follow a gamma distribution. With some mathematicalmanipulation, we end up with the negative binomial distribution for Y.

# Nowadays, the negative binomial distribution is considered a stand-alone distribution, and it is not necessary to dig into the Poisson-gamma mixture background.The distribution function has two parameters: μ and k.

# Mean and variance are given by E(Y) = μ, and Var(Y) = μ + (μ^2/k)

# We have overdispersion if the variance is larger than the mean. The second term in the variance of Y determines the amount of overdispersion. In fact, it is indirectly determined by k, where k is also called the dispersion parameter. If k is large (relative to μ2), the term μ2/k approximates 0, and the variance of Y is μ; in such cases the negative binomial converges to the Poisson distribution. In this case, you might as well use the Poisson distribution. The smaller k, the larger the overdispersion.

# note that for a small mean μ and large overdispersion (small k), the value of 0 has by far the highest probability. In Fig. 8.3 we know the values of μ and k. In reality we do not know these values, and in GLM models, the mean μ is a function of covariates. Estimation of k depends on the software, but can for example be done in a 2-stage iterative approach

## GAMMA

# The gamma distribution can be used for a continuous response variable Y that has positive values (Y > 0), and the distribution function has various forms.

# the mean and variance are: E(Y) = u and var(Y) = u^2 / v

# v is the equivalent to k in the negative binomial. The dispersion is determined by v^–1; a small value of v (relative to μ2) implies that the spread in the data is large.

# For a large v, the gamma distribution becomes bell shaped and symmetric. In such cases, the Gaussian distribution can be used as well. Faraway (2006) gives an example of a linear regression model and a gamma GLM with a small (0.0045) dispersion parameter v–1; estimated parameters and standard errors obtained by both methods are nearly identical. However, for larger values of v–1, this is not the necessarily the case. Note that the allowable range of Y values is larger then 0. So, you cannot use this distribution if your response variable takes negative values or has a value of zero.

## Bernoulli & binomial

# The mean and variance of a Binomial distribution are given by
# E(Y) = N × π 
# var(Y) = N × π × (1 − π)

# A Bernoulli distribution is obtained if N = 1; hence, we only toss once or we only sample one animal on the farm.

## Multinomial

# useful for a response variable that is a categorical variable with more than two levels

### SELECTING A DISTRIBUTION

# We have discussed a large number of distributions for the response variable, but which one should we use? This choice should, in first instance, be made a priori based on the available knowledge on the response variable. For example, if you model the presence and absence of animals at M sites as a function of a couple of covariates, then your choice is simple: the binomial distribution should be used because your response variable contains zeros and ones. This is probably the only scenario where the choice is so obvious. Having said that, if we aggregate the response variable into groups, we (may) have a Poisson distribution. If your data are counts (of animals, plants, etc.) without an upper limit, then the Poisson distribution is an option. This is because counts are always non-negative, and tend to be heterogeneous and both comply with the Poisson distribution. If there is high overdispersion, then the negative binomial distribution is an alternative to the Poisson for count data. You can also use the Normal distribution for counts (potentially combined with a data transformation), but the Poisson or negative binomial may be more appropriate. However, the Normal distribution does not exclude negative realisations. You can also have counts with an upper limit. For example, if you count the number of animals on a farm that are infected with a disease, out of a total of N animals. The maximum number of infected animals is then N. If you consider each individual animal as an independent trial and each animal has the same probability of being infected, then we are in the world of a binomial distribution. But, what do you do with densities? Density is often defined as the numbers (which are counts!) per volume (or area, depth range, etc.).We will see in Chapter 9 that this can be modelled with the Poisson (or NB) distribution and an offset variable. If the response variable is a continuous variable like weight of the animal, then the Normal distribution is your best option, but the gamma distribution may be an alternative choice. The important thing to realise is that these distributions are for the response variable, not for explanatory variables. The choice of which distribution to use is an a priori choice. A list of all discussed distributions in this section is given in Table 8.1. If you are hesitating between two competing distributions, e.g. the Normal distribution and the gamma distribution, or the Poisson distribution and the negative binomial distribution, then you could plot the mean versus the variance of the response variable and see what type of mean–variance relationship you have and select a distribution function accordingly.