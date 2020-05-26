#create my.slice object
library(gtools)
set.seed(2019-1-19)


nt <- 20000
hosp.probs <- rdirichlet(1, c(1:500))
hospid <- as.factor(sample(1:500, size = nt, replace = TRUE, prob = hosp.probs))
nhospt <- length(unique(hospid))
proc.probs <- rdirichlet(1, c(1:32))
proc_type <- as.factor(sample(1:32, size = nt, replace = TRUE, prob = proc.probs))
comorb <- round(rgamma(nt, shape = 1.9, scale = 1))
emergency <- as.numeric(runif(nt) < 0.6) #do I need to set certain procedures to be non-emergency only?  Don't mess around with that at this point.
mf <- as.numeric(runif(nt) < 0.43)
age <- round(rnorm(nt, mean = 55, sd = 8))
exp.dec <- sample(c(1:10), size = nt, replace = TRUE, prob = c(0.09,0.08, 0.08, 0.07, 0.04, 0.05, 0.05, 0.12, 0.12, 0.24))
#need to sample unevenly from this distribution
exp <- rep(NA, nt)
exp[exp.dec == 1] <- round((1 - rbeta(sum(exp.dec == 1), shape1 = 1.05, shape2 = 1.5))*4.5)
exp[exp.dec == 2] <- round(runif(sum(exp.dec == 2))*3 + 4.5)
exp[exp.dec == 3] <- round(runif(sum(exp.dec == 3))*3 + 7.5)
exp[exp.dec == 4] <- round(runif(sum(exp.dec == 4))*2 + 10.5)
exp[exp.dec == 5] <- round(runif(sum(exp.dec == 5))*2 + 12.5)
exp[exp.dec == 6] <- round(runif(sum(exp.dec == 6))*3 + 14.5)
exp[exp.dec == 7] <- round(runif(sum(exp.dec == 7))*3 + 17.5)
exp[exp.dec == 8] <- round(runif(sum(exp.dec == 8))*4 + 20.5)
exp[exp.dec == 9] <- round(runif(sum(exp.dec == 9))*6 + 24.5)
exp[exp.dec == 10] <- round(rbeta(sum(exp.dec == 10), shape1 = 1.05, shape2 = 4)*20 + 30.5)


my.slice <- data.frame(hospid, proc_type, comorb, emergency, mf, age, exp)


#outcome
all.betas <- c(rnorm(500 + 32 - 1), 0.2, 0.5, 0, 0.01, -0.001)


my.slice$died <- as.numeric(runif(nt) < 0.015)
my.slice$exp[which(runif(nt) < 0.05)] <- NA


nc <- 58000
hosp.probs <- rdirichlet(1, c(1:500))
hospid <- as.factor(sample(1:500, size = nc, replace = TRUE, prob = hosp.probs))
nhospt <- length(unique(hospid))
proc.probs <- rdirichlet(1, c(1:32))
proc_type <- as.factor(sample(1:32, size = nc, replace = TRUE, prob = proc.probs))
comorb <- round(rgamma(nc, shape = 2, scale = 1))
emergency <- as.numeric(runif(nc) < 0.5) #do I need to set certain procedures to be non-emergency only?  Don't mess around with that at this point.
mf <- as.numeric(runif(nc) < 0.41)
age <- round(rnorm(nc, mean = 55, sd = 8))

#create experience by decile
exp <- rep(NA, nc)
exp.dec <- sample(c(1:10), size = nc, replace = TRUE, prob = c(0.1,0.1, 0.14, 0.1, 0.1, 0.12, 0.1, 0.12, 0.07, 0.04))
#need to sample unevenly from this distribution
exp[exp.dec == 1] <- round((1 - rbeta(sum(exp.dec == 1), shape1 = 1.05, shape2 = 1.5))*4.5)
exp[exp.dec == 2] <- round(runif(sum(exp.dec == 2))*3 + 4.5)
exp[exp.dec == 3] <- round(runif(sum(exp.dec == 3))*3 + 7.5)
exp[exp.dec == 4] <- round(runif(sum(exp.dec == 4))*2 + 10.5)
exp[exp.dec == 5] <- round(runif(sum(exp.dec == 5))*2 + 12.5)
exp[exp.dec == 6] <- round(runif(sum(exp.dec == 6))*3 + 14.5)
exp[exp.dec == 7] <- round(runif(sum(exp.dec == 7))*3 + 17.5)
exp[exp.dec == 8] <- round(runif(sum(exp.dec == 8))*4 + 20.5)
exp[exp.dec == 9] <- round(runif(sum(exp.dec == 9))*6 + 24.5)
exp[exp.dec == 10] <- round(rbeta(sum(exp.dec == 10), shape1 = 1.05, shape2 = 4)*20 + 30.5)


my.slice2 <- data.frame(hospid, proc_type, comorb, emergency, mf, age, exp)
my.slice2$died <- as.numeric(runif(nc) < 0.017)
my.slice2$exp[which(runif(nc) < 0.01)] <- NA

my.slice <- rbind(my.slice, my.slice2)
my.slice$img <- c(rep(1, nt), rep(0, nc))
my.slice$med_income <- round(rgamma(nt+nc, shape = 20, scale = 4))*1000
race.probs <- c(0.75, 0.15, 0.1)
my.slice$race_cat <- as.factor(sample(1:3, size = nt+nc, replace = TRUE, prob = race.probs))
my.slice$comorb_cat <- as.character(my.slice$comorb)
my.slice$comorb_cat[my.slice$comorb >= 3] <- '3+'
my.slice$comorb_cat <- as.factor(my.slice$comorb_cat)

my.slice$med_income[which(runif(nrow(my.slice)) < 0.2)] <- NA


#make some additional variables to match on
my.slice$exp.quint <- cut(my.slice$exp, breaks = c(-Inf,quantile(my.slice$exp, c(1:5)/5, na.rm = TRUE)))
#exp.quint[is.na(exp.quint)] <- levels
my.slice$exp.quint[is.na(my.slice$exp)] <- NA
#nopers.quint[is.na(nopers.quint)] <- levels(nopers.quint)[1]
my.slice$age.quint <- cut(my.slice$age, breaks = c(-Inf, quantile(my.slice$age, c(1:5)/5)))



my.slice$exp.dec <- cut(my.slice$exp, breaks = c(-Inf,
	quantile(my.slice$exp, c(1:10)/10, na.rm = TRUE)))
my.slice$exp.dec[is.na(my.slice$exp)] <- NA

save(my.slice, file = 'optimal_tradeoffs_code/data/my_slice.Rdata')


