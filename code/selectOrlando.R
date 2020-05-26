#load libraries
library(plyr)
library(foreign)

#enter parent directory (assuming we start in directory where files are unzipped)
setwd('optimal_tradeoffs_code')

#read in data
dat <- read.dta('data/TwoStateIMGUSMG.dta')
#
ny.fips <- read.csv('data/NYfips.csv')
fl.fips <- read.csv('data/FLfips.csv')



#################
#extract columns with relevant variables
col.idx <- c(1:40,42:43,47,50,107:138,216:247,249,252,280)
subdat <- dat[,col.idx]
subdat$img <- as.numeric(subdat$img)-1

#preprocess covariates
#convert binary factors to 0/1 values
for(i in which(colnames(subdat) %in% c("chf", "valve", "pulmcirc", "perivasc",
 "para",  "neuro", "chrnlung", "dm", "dmcx", "hypothy", "renlfail", "liver", 
 "aids",  "lymph", "mets",  "tumor", "arth",  "coag",  "obese", "wghtloss",
 "lytes", "bldloss", "anemdef", "alcohol", "drug",  "psych", "depress", "htn_c",
 "emergency"))){
	subdat[[i]] <- as.numeric(subdat[[i]] == 'Yes')
 }
 subdat$mf <- as.numeric(subdat$mf == 'male')

#convert race to factor
subdat$race_cat <- as.factor(subdat$race_cat)
subdat$race_cat <- addNA(subdat$race_cat)

#drop NAs
ndropped <- sum(is.na(subdat$proc_type))
all.dat <- subdat[-which(is.na(subdat$proc_type)),]


############### ADD GEOGRAPHIC LABELS TO RAW DATA ####################
geo.info <- dat[c('hospid', 'state', 'fac_county', 'fips_code')]

#look at county-fips correspondence for patients so I can apply to hospitals
fips.county <- table(dat$ptcounty, dat$fips_code)
fipsv <- apply(fips.county, 2, function(x) sum(x >0 ))
countyv <- apply(fips.county, 2, function(x) sum(x >0 ))

fips.county.lookup <- ddply(dat, .(state, fips_code, ptcounty), nrow)
dat$fac_fips <- fips.county.lookup$fips_code[match(paste(dat$state,dat$fac_county), paste(fips.county.lookup$state, fips.county.lookup$ptcounty))]


colnames(fl.fips) <- colnames(ny.fips)

fips.names <- rbind(
	cbind('state' = rep('New York',nrow(ny.fips)), ny.fips),
	cbind('state' = rep('Florida',nrow(fl.fips)), fl.fips)
	)

dat$fac_named <- fips.names$County.Name[match(paste(dat$state, dat$fac_fips),paste(fips.names$state, fips.names$County.FIPS))]	
head(dat[c('state','fac_fips','fac_named','fips_code')])	



########### TRANSFER GEOGRAPHIC LABELS TO SELECTED DATA ################
lookup.idx <- match(all.dat$obsid, dat$obsid)
all.dat$fac_fips <- dat$fac_fips[lookup.idx]
all.dat$fac_named <- dat$fac_named[lookup.idx]
all.dat$state <- dat$state[lookup.idx]


########### CREATE EXPERIENCE AND VOLUME VARIABLES IN RAW DATA #############
#assign nopers variable 
surgyr.tab <- table(dat$licno, dat$year)
#surg.nopers <- apply(surgyr.tab, 1, function(x) mean(x[x > 0]))
#dat$nopers <- surg.nopers[match(dat$licno,names(surg.nopers))]
dat$nopers <- surgyr.tab[cbind(match(dat$licno, rownames(surgyr.tab)), match(dat$year, colnames(surgyr.tab)))]


########### MOVE EXPERIENCE AND VOLUME VARIABLES INTO SELECTED DATA #########
map.idx <- match(all.dat$obsid, dat$obsid)
all.dat$nopers <- dat$nopers[map.idx]
all.dat$exp <- dat$year[map.idx] - dat$YOT[map.idx]
for(outc in c('died','complication','failresc','prolonged')) {
	all.dat[[outc]] <- dat[[outc]][map.idx]
}

######## SELECT ORLANDO METRO AREA #############

my.slice <-  all.dat[all.dat$fac_named %in% c('ORANGE','LAKE','OSCEOLA','SEMINOLE','VOLUSIA','FLAGLER','SUMTER'),]

#make some additional variables to match on
my.slice$exp.quint <- cut(my.slice$exp, breaks = c(-Inf,quantile(my.slice$exp, c(1:5)/5, na.rm = TRUE)))
#exp.quint[is.na(exp.quint)] <- levels
my.slice$exp.quint[is.na(my.slice$exp)] <- NA
my.slice$nopers.quint <- cut(my.slice$nopers, breaks = c(-Inf, quantile(my.slice$nopers, c(1:5)/5)))
#nopers.quint[is.na(nopers.quint)] <- levels(nopers.quint)[1]
my.slice$age.quint <- cut(my.slice$age, breaks = c(-Inf, quantile(my.slice$age, c(1:5)/5)))



my.slice$exp.dec <- cut(my.slice$exp, breaks = c(-Inf,
	quantile(my.slice$exp, c(1:10)/10, na.rm = TRUE)))
my.slice$exp.dec[is.na(my.slice$exp)] <- NA


save(my.slice, file = 'data/my_slice.Rdata')