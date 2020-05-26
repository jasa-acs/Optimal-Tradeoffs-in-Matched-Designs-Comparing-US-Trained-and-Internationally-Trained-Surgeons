#load libraries
library(plyr)
library(optmatch)
library(rcbalance)
library(matchMulti)

#set higher memory limit on Windows to avoid crashes on certain architectures
#memory.limit(size=64000)

#enter parent directory (assuming we start in directory where files are unzipped)
setwd('optimal_tradeoffs_code/data_analysis')

#read in additional source code
source('tradeoff_functions_v5.R')
source('obj_to_match.R')
source('descr_stats.R')
source('attributable_effects_v3.R')

setwd('..')

load('data/my_slice.Rdata')

#define propensity score 
my.slice$race_cat <- addNA(my.slice$race_cat)
my.slice$med_income_missing <- is.na(my.slice$med_income)
my.slice$exp_missing <- is.na(my.slice$exp)

my.slice$med_income_impute <- my.slice$med_income
my.slice$med_income_impute[my.slice$med_income_missing] <- mean(my.slice$med_income, na.rm = TRUE)
my.slice$exp_impute <- my.slice$exp
my.slice$exp_impute[my.slice$exp_missing] <- mean(my.slice$exp, na.rm = TRUE)

pscore <- glm(img ~ proc_type + comorb_cat + age + emergency + mf + race_cat + med_income_impute + med_income_missing, 
#+ exp_impute + exp_missing, 
data = my.slice, family = binomial("logit"))
#summary(my.slice[c("proc_type","comorb_cat","age","emergency","mf","race_cat","med_income","exp","nopers")])
my.slice$ps <- predict(pscore)


#construct nets
base.net <- netFlowMatch(my.slice$img, my.slice$pid)

#define distance
dist.i <- build.dist.struct(
		z = my.slice$img, X = my.slice[c('proc_type', 'comorb_cat', 'emergency', 'mf', 'age')], 
		exact = my.slice$hospid, calip.option = "user", 
		calip.cov = my.slice$ps, caliper = 0.25)
net.i <- addExclusion(makeSparse(base.net, dist.i))
my.bal <- apply(my.slice[,match('exp.dec', colnames(my.slice)), drop = FALSE], 1, function(x) paste(x, collapse = "."))
net.i <- addBalance(net.i, treatedVals = 
		my.bal[my.slice$img==1], controlVals = my.bal[my.slice$img==0])
paircost.v <- unlist(dist.i)	
	

### Create edge costs for each of the objective functions we will trade off
pair.objective.edge.costs <- pairCosts(dist.i, net.i)	
excl.objective.edge.costs <-  excludeCosts(net.i,1)
rho.min <- 1e-2
rho.max.factor <- 10
bal.objective.edge.costs <- balanceCosts(net.i, max(paircost.v)*rho.max.factor)

#create an alternative set of costs for exclusion and balance objectives 
excl.objective.edge.costs.alt <- excludeCosts(net.i, 2000)
bal.objective.edge.costs.alt <-  balanceCosts(net.i, 1)


#################### PAIR MATCH TRADEOFF -- section 4.3 in manuscript ##################

rho.v <- c(rho.min, 0.2,0.5,1,2, 
4,
4.5,5,5.5,
6,8,
max(min(paircost.v[paircost.v > 0]), quantile(paircost.v)[2]),20,50, rho.max.factor*max(paircost.v))
	
#### EXPENSIVE PART ####	
	
solutions <- list()
solution.nets <- list()
for(rho in rho.v){
		temp.sol <- solveP(net.i, 
				f1.list = list(pair.objective.edge.costs, excl.objective.edge.costs.alt), 
				f2.list = list(bal.objective.edge.costs.alt), rho = rho, tol = 1e-2)		
		solutions[[as.character(rho)]] <- temp.sol$x
		solution.nets[[as.character(rho)]] <- temp.sol$net
		save(solutions, solution.nets, file = paste('solution_intermed_',which(rho == rho.v),'.Rdata',sep = ''))
		print(paste('Matches finished for rho=', rho))
}

match.list <- obj.to.match(list('net' = net.i, 'costs' = pair.objective.edge.costs, 'balance' = bal.objective.edge.costs.alt, 'rho.v' = rho.v, 'solutions' = solutions))



solutions.old <- solutions
solution.nets.old <- solution.nets
save(match.list, solutions, paircost.v, rho.v, file = 'pairmatch_tradeoff_onefile.Rdata')
#### END EXPENSIVE PART ####

	my.stats.old <- t(laply(match.list, descr.stats, b.vars = c('exp.dec'), pair.vars = c('proc_type', 'comorb_cat', 'emergency', 'mf', 'age.quint','exp.dec'), extra = TRUE))

colnames(my.stats.old) <- rho.v
#trim rownames
colnames(my.stats.old) <- substr(colnames(my.stats.old), start = 1, stop = 4)
rownames(my.stats.old) <- substr(rownames(my.stats.old), start = 1, stop = 12)
round(my.stats.old,4)

pair.dist <- laply(solutions, function(x) sum(paircost.v*x[1:length(paircost.v)]))	

my.stats.old.save <- my.stats.old

#PLOT
samp.size <- laply(match.list, nrow)

f1 <- my.stats.old[1,]*samp.size
f2 <- pair.dist
f1.alt <- my.stats.old[	which(rownames(my.stats.old) == 'SD.exp'),]

plot.idx <- c(1:14)
tab.idx <- c(1,4,8,11,15)



#### Create Figure 2(a) in manuscript  #####

pdf('pairmatch_tradeoff_TV.pdf')
xmax <- ceiling(max(pair.dist)/500)*500
ymax <- ceiling(max(my.stats.old[1,]*samp.size)/500)*500
plot(f2[plot.idx],f1[plot.idx], pch = 20, xlab = 'Sum of pairwise distances', ylab = 'Total variation imbalance, experience deciles', cex = 1.2, cex.lab = 1.2, cex.axis= 1.2, col = rgb(0,0,0,0.3), ylim = c(0,ymax), xlim = c(0,xmax))
points(f2[tab.idx], f1[tab.idx], col = rgb(0,0,1,1),pch = 20)
text(labels = LETTERS[1:length(tab.idx)]#round(rho.v[tab.idx],2)
, y = f1[tab.idx], x = f2[tab.idx], pos = c(rep(2,3),1,1))

dev.off()

#### Create Figure 2(b) in manuscript	

pdf('pairmatch_tradeoff_SD.pdf')
ymax.alt <- ceiling(max(f1.alt*10))/10
plot(f2, abs(f1.alt), pch = 20, xlab = 'Sum of pairwise distances', ylab = 'Abs. std. diff on experience', cex = 1.2, cex.lab = 1.2, cex.axis= 1.2, col = rgb(0,0,0,0.3), xlim = c(0,xmax), ylim = c(0,ymax.alt))
points(f2[tab.idx], abs(f1.alt)[tab.idx], col = rgb(0,0,1,1),pch = 20)
text(labels = LETTERS[1:length(tab.idx)]#round(rho.v[tab.idx],2)
, y = f1.alt[tab.idx], x = f2[tab.idx], pos = c(rep(2,3),1,1))
dev.off()



#### Create Table 3 in manuscript

sel.rows <- c(1,
	which(rownames(my.stats.old) == 'SD.exp'),
	13:23,
	which(rownames(my.stats.old) == 'SD.comorb'),
	11:12,
	which(rownames(my.stats.old) == 'SD.age'),
	4:7,8:9)
max.pt.sd <- apply(my.stats.old[24:55,],2, function(x)x[which.max(abs(x))])

out.tab <- rbind(pair.dist/samp.size, pair.dist,my.stats.old[1,]*samp.size, my.stats.old[sel.rows[1:17],], max.pt.sd, my.stats.old[sel.rows[18:23],])


rownames(out.tab) <- 
c('Avg. pairwise Mahalanobis distance',
'f1 (Sum of pairwise distances)',
'f2 (flow through penalized edges)',
'Total variation distance, exp. deciles',
'Experience',
'Experience <5 yrs',
'Experience 5-7 yrs',
'Experience 8-10 yrs',
'Experience 11-12 yrs',
'Experience 13-14 yrs',
'Experience 15-17 yrs',
'Experience 18-20 yrs',
'Experience 21-24 yrs',
'Experience 25-30 yrs',
'Experience 31+ yrs',
'Experience missing',
'Elixhauser index',
'Emergency admission',
'Sex',
'Age',
'Largest over 32 procedure types',
'Procedure type',
'eElixhauser index',
'eEmergency admission',
'eSex',
'Age quintile',
'Experience decile'
)
round(out.tab,2)
colnames(out.tab) <- round(rho.v,2)


library(xtable)
round(out.tab,2)
round(out.tab[,tab.idx],2)
digit.mat <- out.tab
digit.mat[,] <- 2
digit.mat[3,] <- 0


xtable(round(out.tab[,tab.idx],2), digits = cbind(rep(0, nrow(digit.mat)), digit.mat[,tab.idx]))


############## EXACT MATCH ON EXP DECILE #####################

#define distance
dist.exact <- build.dist.struct(
		z = my.slice$img, X = my.slice[c('proc_type', 'comorb_cat', 'emergency', 'mf', 'age')], 
		exact = paste(my.slice$hospid, my.slice$exp.dec), calip.option = "user", 
		calip.cov = my.slice$ps, caliper = 0.25)

m.out <- rcbalance(dist.exact, exclude.treated = TRUE, tol = 1e-3)

#compute pair distance and save it 
exact.pairdist <- 0

for(i in 1:nrow(m.out$matches)){
	treat.lookup <- as.numeric(rownames(m.out$matches)[i])
	ctrl.lookup <- which(names(dist.exact[[treat.lookup]]) == m.out$matches[i])
	exact.pairdist <- exact.pairdist + dist.exact[[treat.lookup]][ctrl.lookup]
}
	


################# EXCLUSION TRADEOFF -- section 4.4 in manuscript #####################


#compute rhos
rho.v <- c(rho.min, quantile(paircost.v)[2], 200, 350, rho.max.factor*max(paircost.v)*c(0.99,1,1.01), 500, rho.max.factor*max(paircost.v)*c(2,10))
#first refinement
rho.v <- c(rho.v[1:4], seq(from=rho.v[5], to = rho.v[6], length.out = 5)[-5], seq(from=rho.v[6], to = rho.v[7], length.out = 5), rho.v[8:10])
#second refinement
rho.v <- c(rho.v[1:7],  seq(from=rho.v[8], to = rho.v[9], length.out = 5)[-5], seq(from=rho.v[9], to = rho.v[10], length.out = 5), rho.v[11:16])
#last refinement
rho.v <- c(rho.v[1:10],seq(from=rho.v[11], to = rho.v[12], length.out = 5), rho.v[13:22])

#### EXPENSIVE PART ####	
	
solutions <- list()
solution.nets <- list()

for(rho in rho.v){
		temp.sol <- solveP(net.i, 
			f1.list = list(pair.objective.edge.costs, bal.objective.edge.costs), 
			f2.list = list(excl.objective.edge.costs), rho = rho, tol = 1e-2)

		solutions[[as.character(rho)]] <- temp.sol$x
		solution.nets[[as.character(rho)]] <- temp.sol$net
		print(paste('Matches finished for rho=', rho))
#	}
} 



match.list <- obj.to.match(list('net' = net.i, 'costs' = pair.objective.edge.costs, 'balance' = bal.objective.edge.costs, 'rho.v' = rho.v, 'solutions' = solutions))
match.list <- match.list[order(as.numeric(names(match.list)))]
	
#run this once you're sure new solutions are good
solutions.old <- solutions
solution.nets.old <- solution.nets

pair.dist.excl <- laply(solutions, function(x) sum(paircost.v*x[1:length(paircost.v)]))	


match.list[['-1']] <- m.out$matches
rho.v.long <- c(rho.v, -1)
save(match.list, solutions, rho.v.long, pair.dist.excl, exact.pairdist, file = 'exclude_tradeoff_onefile.Rdata')


#### end expensive part ####


library(matchMulti)
	my.stats <- t(laply(match.list, descr.stats, b.vars = c('exp.dec'), pair.vars = c('proc_type', 'comorb_cat', 'emergency', 'mf', 'age', 'age.quint', 'exp.dec'), extra = TRUE))
colnames(my.stats) <- rho.v.long

colnames(my.stats) <- substr(colnames(my.stats), start = 1, stop = 4)
rownames(my.stats) <- substr(rownames(my.stats), start = 1, stop = 12)
round(my.stats,4)
cbind(rho.v.long, laply(match.list, nrow))


#### PLOTS ####


samp.size <- laply(match.list, nrow)
plot.idx <- c(1:2,4:23,26)
tab.idx <- plot.idx[c(23,1,3,7:9,14,21:22)]

f1 <- my.stats[1,]*samp.size
f2 <- sum(my.slice$img) - samp.size
f1.alt <- my.stats[which(rownames(my.stats) == 'SD.exp'),]


#### Create Figure 2(c) in manuscript

pdf('exclusion_tradeoff_TV_w_exact.pdf')
ymax <- ceiling(max(f1[plot.idx])/500)*500
plot(f2[plot.idx],f1[plot.idx], pch = 20, xlab = 'IMG patients dropped (out of 20,024)', ylab = 'Total variation imbalance, experience deciles', cex = 1.2, xlim = c(0, sum(my.slice$img)), ylim = c(0,ymax), cex.lab = 1.2, cex.axis= 1.2, col = rgb(0,0,0,0.3))
abline(h=0, lty = 'dotted')
points(f2[tab.idx], f1[tab.idx], col = rgb(0,0,1,1),pch = 20)
text(labels = rev(LETTERS[5 + 1:length(tab.idx)]), y = f1[tab.idx], x = f2[tab.idx], pos = c(3,3,2,4,2, rep(4,4)))
dev.off()


### Create Figure 2(d) in manuscript

pdf('exclusion_tradeoff_SD_w_exact.pdf')
ymax.alt <- ceiling(max(f1.alt*10))/10
plot(f2[plot.idx], abs(f1.alt)[plot.idx], pch = 20, xlab = 'IMG patients dropped (out of 20,024)', ylab = 'Abs. std. diff on experience', cex = 1.2, xlim = c(0, 20024), ylim = c(0,ymax.alt), cex.lab = 1.2, cex.axis= 1.2, col = rgb(0,0,0,0.3))
abline(h=0, lty = 'dotted')
points(f2[tab.idx], abs(f1.alt)[tab.idx], col = rgb(0,0,1,1),pch = 20)

text(labels = rev(LETTERS[5 + 1:length(tab.idx)]), y = abs(f1.alt)[tab.idx], x = f2[tab.idx], pos = c(3,1,3,1,2,2,rep(4,3)), offset = 0.35)
dev.off()


sel.rows <- c(1,67,15:25,68,12:14,4:7,9:10)

max.pt.sd <- apply(my.stats[26:57,],2, function(x)x[which.max(abs(x))])



### Create Table 4 in manuscript

out.tab <- rbind(1 - samp.size/20024, 20024 - samp.size, samp.size*my.stats[1,]*449.86 + c(pair.dist.excl, exact.pairdist), my.stats[1,]*samp.size, my.stats[sel.rows[1:17],], max.pt.sd, my.stats[sel.rows[18:23],])

round(out.tab[,tab.idx],3)

rownames(out.tab) <- 
c('Proportion IMG patients dropped',
'f2 (IMG patients dropped)',
'f1 (linear combination)', 
'Total variation imbalance',
'Total variation distance, exp. deciles',
'Experience',
'Experience <5 yrs',
'Experience 5-7 yrs',
'Experience 8-10 yrs',
'Experience 11-12 yrs',
'Experience 13-14 yrs',
'Experience 15-17 yrs',
'Experience 18-20 yrs',
'Experience 21-24 yrs',
'Experience 25-30 yrs',
'Experience 31+ yrs',
'Experience missing',
'Elixhauser index',
'Emergency admission',
'Sex',
'Age',
'Largest over 32 procedure types',
'Procedure type',
'Coarsened Elixhauser index',
'eEmergency admission',
'eSex',
'Age quintile',
'Experience decile'
)
round(out.tab,2)
colnames(out.tab) <- round(rho.v.long,2)

library(xtable)
round(out.tab,2)
round(out.tab[,tab.idx],2)
digit.mat <- out.tab
digit.mat[,] <- 2
digit.mat[c(2,4),] <- 0


xtable(round(out.tab[,rev(tab.idx)],2), digits = cbind(rep(0, nrow(digit.mat)), digit.mat[,tab.idx]))







################# OUTCOMES ########################

load('exclude_tradeoff_onefile.Rdata')
#select my match - the one with the lowest SD on experience
my.match <- match.list[[which.min(abs(f1.alt))]]
z <- my.slice$img
my.slice$match_id <- NA
my.slice$match_id[z==1][as.numeric(rownames(my.match))] <- 1:nrow(my.match)
my.slice$match_id[z==0][my.match] <- 1:nrow(my.match)

matched.only <- my.slice[!is.na(my.slice$match_id),]

my.outcomes <- c('died') #c('complication','x','died')
get.paired <- function(outcome, matched.only){
	paired.outcomes <- daply(matched.only, .(match_id), function(x) c(x[[outcome]][x$img == 1], x[[outcome]][x$img == 0]))
	colnames(paired.outcomes) <- c('IMGs','USMGs')
	paired.outcomes <- as.data.frame(paired.outcomes)
	paired.outcomes
}
paired.lists <- alply(my.outcomes, 1, get.paired, matched.only = matched.only)
names(paired.lists) <- my.outcomes 





analyze.outc <- function(paired.outcomes, gamv = c(1)){

	tab2x2 <- table(paired.outcomes$USMGs, paired.outcomes$IMGs, useNA = 'ifany')
	if(nrow(tab2x2) > 2) tab2x2 <- tab2x2[1:2,1:2]
	
	pt.est <- pt.ae(tab2x2)/sum(tab2x2)

	two.side.sens <- t(sapply(gamv, function(x) 2*min(test.ae(tab2x2, gam = x),test.ae(tab2x2, gam = x, null.less = FALSE))))

	conf.ints <- t(sapply(gamv, function(x) conf.ae(tab2x2, gam = x)/(sum(tab2x2) - tab2x2[1,1])) )
	
	return(list('tab' = tab2x2, 'est' = pt.est, 'pvals' = two.side.sens, 'conf.ints' = conf.ints, 'gam' = gamv))
}

gamv <- 1 + c(0:15)/10
died.out <- analyze.outc(paired.lists[['died']], gamv = gamv)

nsamp <- nrow(my.match)


#### OUTPUT FOR PAPER ####
#USMG mortality rate
sum(died.out$tab[2,])/nsamp
#IMG mortality rate
sum(died.out$tab[,2])/nsamp

#p-value for McNemar's Test
#test.ae(died.out$tab, gam = 1)
mcnemar.test(died.out$tab)

#equivalence testing
#need to establish an iota - choose it as 1/4 the 2008 age-75 mortality rate.
iota.est <- round(.039506/4*nrow(my.match))
#iota.est <- round(0.005*nrow(my.match))

equiv.out <- round(aaply(gamv, 1, function(gm) eq.test(died.out$tab, iota=iota.est, gm)),3)

#p-values for equivalence test, Gamma from 1 to 2 in increments of 0.1
equiv.out

#p-values for Gamma = 1
equiv.out[1]

#largest Gamma for which test still rejects
threshold.g <- max(which(equiv.out < 0.05))
gamv[threshold.g]

#p-values for Gamma at threshold
equiv.out[threshold.g]

#p-values for Gamma just above threshold
equiv.out[threshold.g + 1]
