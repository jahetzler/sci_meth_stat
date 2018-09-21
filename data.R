setwd("~/R/Genomikk/3")

newts <- read.csv("newts.csv", header = TRUE)
lamps <- read.csv("lamps.csv", header = TRUE)
pollination <- read.csv("pollination.csv", header = TRUE)
lizard <- read.csv("lizard.csv", header = TRUE, sep = ";")


mode(newts)
mode(lamps)
mode(pollination)

#___________________________________________________________________________
#lamps
#Lifetime of bulb

#1. Check data
summary(lamps)

sd(lamps$lamplifetimes)
var(lamps$lamplifetimes)

#2. Visualize data
boxplot(lamps$lamplifetimes)

# 3. check for normality
#small sample n < 30, check weather the data follow normal distribution 
shapiro.test(lamps$lamplifetimes)

#visual inspection of normality using Q-Q plot
qqnorm(lamps$lamplifetimes)
qqline(lamps$lamplifetimes)

# 4. Compute one-sample t-test
#proclaimed life time for bulbs are 850h µ = 850
d = lamps[,1]

#one sample t-test
t.test(d, mu = 850, alternative = "two.sided") # read alternative hypothesis
t.test(d, mu = 850, alternative = "less") #one sided test, Conf.int: ~ 821

#_____________________________________________________________________________________
#pollination

#1. data analysis
summary(pollination)


#2. visualise data
plot(pollination)
plot(pollination$selfpol, pollination$crosspol)


ggplot(pollination, aes(selfpol, crosspol) ) +
  geom_smooth( method = "lm") +
  geom_point(color = factor(pollination$mother_plant))

#3. Check for normality


#4. statistical test - we would like to test if cross-pollination results in plantsthat are 
#higher than if the seeds were produced by self-pollination.
# - the data is paired, each pair of data was derived from the same mother plant
t.test(pollination$selfpol, pollination$crosspol, paired = TRUE, alternative = "two.sided")

#____________________________________________________________________________________________
#newts
library(ggplot2)

#1. data exploration
#taking mean of data.frame based on sex variable. aggregate funciton
aggregate.data.frame(newts[,1], list(newts$sex), mean)
aggregate.data.frame(newts[,1], list(newts$sex), sd)
#check for variance between length in females and males
aggregate.data.frame(newts[,1], list(newts$sex), var)

# 2. visialise data 
#boxplot based on length of gender
boxplot(length ~ sex, data = newts, col = "light gray")

#3. check for variance homology
library(car)
leveneTest(length~sex, newts) # significant deviation from homogeneity




#4. Statistical test
#t-test newt length

#Ho: mean length of female newts = male newts
#two-sided test

#defult t.test settings
t.test(newts$length ~ newts$sex, alt="two.sided", mu=0, conf= 0.95, var=F, paired=F)
#same output
t.test(length ~ sex, var.equal = FALSE, alt="two.sided", data = newts)
#T = 12.537, we throw away the NULL hypothesis and can show that males are longer than females

#5. Visualise statistics
#graph of variance in length for newts
#histogram with normal distribution curve
hist(newts_female$length, freq = FALSE, xlab = "length", main = "Distribution of Length", col = "pink", xlim = c(8,25), ylim = c(0,0.45))
hist(newts_male$length, add = T, freq = FALSE, col = "blue", xlim = c(8,22), ylim = c(0,0.45))
curve(dnorm(x, mean=mean(newts_female$length), sd = sd(newts$length)), add = TRUE, col="black", lwd = 2)
curve(dnorm(x, mean=mean(newts_male$length), sd = sd(newts$length)), add = TRUE, col="darkblue", lwd = 2)

#Try to do this with ggplot!
ggplot(newts, aes(x = length)) + 
  geom_histogram(aes(y =..density..),
                 breaks = seq(0, 25, by = 2), 
                 colour = "black", 
                 fill = "white") +
  stat_function(fun = dnorm, args = list(mean = mean(newts$length), sd = sd(newts$length)))

# TEST CODE FOR NEWTS
#create new subset of data frame
newts_female <- subset.data.frame(newts, sex == "female")
newts_male <- subset.data.frame(newts, sex == "male")

#mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]}

  getmode(newts$length)
  
#distribution of length between females and males  
ggplot(newts, aes(x = length, fill = factor(sex))) +
  geom_histogram(binwidth = 1) +
  xlab("Lenght") +
  ylab("total count") +
  labs(fill = "Gender")  
#_________________________________________________________________________
#KING GIZZARD AND THE LIZZARD WIZZARD
library(ggplot2)   

#Data analysis
ls(lizard)
length(unique(lizard$ID_FATHER))
length(unique(lizard$ID_MOTHER))
length(unique(lizard$ID_YOUNG))

length(table(lizard$ID_MOTHER))
length(table(lizard$ID_FATHER)) # 29 Females, to 26 males, some must be polygamus
plot(table(lizard$ID_MOTHER))

table(lizard$SEX_YOUNG)

#mean, standard deviation and variance between sex in children
aggregate.data.frame(lizard[,7], list(lizard$SEX_YOUNG), mean) #slighty higher in females
aggregate.data.frame(lizard[,7], list(lizard$SEX_YOUNG), sd)   #higher SD in females
aggregate.data.frame(lizard[,7], list(lizard$SEX_YOUNG), var)  #higher variance in female dataset

#Correlation test
cor.test(lizard$DORS_MOTHER, lizard$DORS_FATHER)

plot(lizard)

#Regression analysis which I don't understand
linearMod <- lm(DORS_MOTHER ~ DORS_YOUNG, data = lizard)
print(linearMod)
summary(linearMod)

#what the fuck is going on here
modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["DORS_YOUNG", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["DORS_YOUNG", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
f_statistic <- linearMod$fstatistic[1]  # fstatistic
f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)
AIC(linearMod)
BIC(linearMod)


#GRAPH TEST:
#_____________________________________________________________________________________
#plot count dors size compared to sex
ggplot(lizard, aes(x = DORS_YOUNG, fill = factor(SEX_YOUNG))) +
  geom_histogram(binwidth = 1) +
  xlab("DORS") +
  ylab("total count") +
  labs(fill = "Sex")

#plot comparing Mothers DORS size to Offsprings
ggplot(lizard, aes(x = DORS_MOTHER, y = DORS_YOUNG)) +
  #geom_jitter() +
  geom_point() +
  geom_smooth(method = 'lm') + 
  stat_summary(fun.y = "mean", colour = "red", size = 2, geom = "point") + 
  stat_summary(fun.y = mean, fun.ymin = min, fun.ymax = max, colour = "red")


#what to test:
# DORS difference between year
# Size of parents to size of children, (ID_FATHER: 8 polygamous, with different sized offspring)
# Statistics behind DORS size of mothers and daughters, seems to correlate in plot
  
  
#what to observe in dataset
# monogamy in partnership
# no consecutive mating from one year to another
# constant variables for FAMELE and MALE

# check regression example in help for fitting linear models ?lm

#Variance tests methods:
#Bartlett       normality       modified likelihood ratio test
#BrownForsythe  robust          robust Levene test
#Conover        symmetry        Conover's squared ranks test
#FisherRatio    normality       based on variance ratio
#Levene         robust,symmetry compares individual and group variances 