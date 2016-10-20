##Data Imported
load("ICPSR_21600/DS0001/21600-0001-Data.rda")
d <- da21600.0001
stopifnot(require(data.table))
## Loading required package: data.table
## Warning: package 'data.table' was built under R version 3.2.3
Data Cleaning
1.Identify Outliers are NA

outlierReplace <- function(dataframe, cols, rows, newValue = NA) {
  if(any(rows)){
    set(dataframe, rows, cols, newValue)
  }
}
Cleaning Data for Independece Parenting Index
d$I.hometime <- as.numeric(d$H1WP1)
outlierReplace(d, "I.hometime", which(d$I.hometime > 2), NA)
summary(d$I.hometime)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.3288  1.0000  1.0000     151
d$I.friends <- as.numeric(d$H1WP2)
outlierReplace(d, "I.friends", which(d$I.friends > 2), NA)
summary(d$I.friends)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  1.0000  1.0000  0.8519  1.0000  1.0000     142
d$I.outfit <- as.numeric(d$H1WP3)
outlierReplace(d, "I.outfit", which(d$I.outfit > 2), NA)
summary(d$I.outfit)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  1.0000  1.0000  0.9062  1.0000  1.0000     141
d$I.tv <- as.numeric(d$H1WP4)
outlierReplace(d, "I.tv", which(d$I.tv > 2), NA)
summary(d$I.tv)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  1.0000  1.0000  0.8248  1.0000  1.0000     141
d$I.tvp <- as.numeric(d$H1WP5)
outlierReplace(d, "I.tvp", which(d$I.tvp > 2), NA)
summary(d$I.tvp)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  1.0000  1.0000  0.7719  1.0000  1.0000     142
d$I.sleep <- as.numeric(d$H1WP6)
outlierReplace(d, "I.sleep", which(d$I.sleep > 2), NA)
summary(d$I.sleep)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  1.0000  0.6524  1.0000  1.0000     141
d$I.eat <- as.numeric(d$H1WP7)
outlierReplace(d, "I.eat", which(d$I.eat > 2), NA)
summary(d$I.eat)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  1.0000  1.0000  0.8156  1.0000  1.0000     139
stopifnot(require(RSNNS))
## Loading required package: RSNNS
## Warning: package 'RSNNS' was built under R version 3.2.3
## Loading required package: Rcpp
## Warning: package 'Rcpp' was built under R version 3.2.2
d$I.momI <- 8 - as.numeric(d$H1PF2) #the higher of this number, the more agreement of "Your mother encourages you to be independent"
d$I.zmomI <- normalizeData(d$I.momI, type = "0_1") * 3
summary(d$I.zmomI)
##        V1       
##  Min.   :0.000  
##  1st Qu.:2.571  
##  Median :2.571  
##  Mean   :2.527  
##  3rd Qu.:3.000  
##  Max.   :3.000
Generating the Independence Parenting Index
I.vars <- c("I.hometime", "I.friends", "I.outfit", "I.tv", "I.tvp", "I.sleep", "I.eat", "I.zmomI")
sub.I <- d[ ,I.vars]

stopifnot(require(psych))
## Loading required package: psych
## Warning: package 'psych' was built under R version 3.2.3
summary(alpha(sub.I)) #Here Cronbach's a is 0.59, even though it's less than 0.65 which is what we usually looking for, but it's still ok right ;) Cut him some slack plz!!!
## 
## Reliability analysis   
##  raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd
##       0.52      0.59    0.57      0.15 1.4 0.011 0.95 0.22
d$Inde <- rowSums(sub.I, na.rm = TRUE)
d$Inde.s <- normalizeData(d$Inde, type = "norm")
summary(d$Inde.s)
##        V1         
##  Min.   :-3.8812  
##  1st Qu.:-0.5102  
##  Median : 0.2226  
##  Mean   : 0.0000  
##  3rd Qu.: 0.7356  
##  Max.   : 1.2486
Cleaning Data for Attention(Intimacy) Parenting Index
d$c.dinner <- as.numeric(d$H1WP8)
outlierReplace(d, "c.dinner", which(d$c.dinner > 10), NA)
summary(d$c.dinner) ## now from 0 - 7 days for a week at least one of the parents would have dinner togetehr
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   3.000   5.000   4.609   7.000   7.000     154
d$c.mom <- as.numeric(d$H1WP9)
outlierReplace(d, "c.mom", which(d$c.mom > 5), NA)
summary(d$c.mom) # from 1 to 5 how close you are to your mom
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    1.00    4.00    5.00    4.55    5.00    5.00     375
d$c.momc <- as.numeric(d$H1WP10)
outlierReplace(d, "c.momc", which(d$c.momc > 5), NA)
summary(d$c.momc) # how much you mom cares about you from 1 to 5
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   1.000   5.000   5.000   4.857   5.000   5.000     374
d$c.dad <- as.numeric(d$H1WP13)
outlierReplace(d, "c.dad", which(d$c.dad > 5), NA)
summary(d$c.dad) # from 1 to 5 how close you are to your dad, BTW there are 1947 NA
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   1.000   4.000   5.000   4.278   5.000   5.000    1957
d$c.dadc <- as.numeric(d$H1WP14)
outlierReplace(d, "c.dadc", which(d$c.dadc > 5), NA)
summary(d$c.dadc) # how much you dad cares about you from 1 to 5
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   1.000   5.000   5.000   4.747   5.000   5.000    1957
d$c.mshopping <- as.numeric(d$H1WP17A)
outlierReplace(d, "c.mshopping", which(d$c.mshopping > 2), NA)
summary(d$c.mshopping)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  1.0000  0.7286  1.0000  1.0000     381
d$c.dshopping <- as.numeric(d$H1WP18A)
outlierReplace(d, "c.dshopping", which(d$c.dshopping > 2), NA)
summary(d$c.dshopping)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.2638  1.0000  1.0000    1962
d$c.msport <- as.numeric(d$H1WP17B)
outlierReplace(d, "c.msport", which(d$c.msport > 2), NA)
summary(d$c.msport)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.0892  0.0000  1.0000     381
d$c.dsport <- as.numeric(d$H1WP18B)
outlierReplace(d, "c.dsport", which(d$c.dsport > 2), NA)
summary(d$c.dsport)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.3021  1.0000  1.0000    1962
d$c.mreligious <- as.numeric(d$H1WP17C)
outlierReplace(d, "c.mreligious", which(d$c.mreligious > 2), NA)
summary(d$c.mreligious)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.3987  1.0000  1.0000     381
d$c.dreligious <- as.numeric(d$H1WP18C)
outlierReplace(d, "c.dreligious", which(d$c.dreligious > 2), NA)
summary(d$c.dreligious)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.3199  1.0000  1.0000    1962
d$c.mdate <- as.numeric(d$H1WP17D)
outlierReplace(d, "c.mdate", which(d$c.mdate > 2), NA)
summary(d$c.mdate)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.4728  1.0000  1.0000     381
d$c.ddate <- as.numeric(d$H1WP18D)
outlierReplace(d, "c.ddate", which(d$c.ddate > 2), NA)
summary(d$c.ddate)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.2732  1.0000  1.0000    1962
d$c.mevent <- as.numeric(d$H1WP17E)
outlierReplace(d, "c.mevent", which(d$c.mevent > 2), NA)
summary(d$c.mevent)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.2654  1.0000  1.0000     381
d$c.devent <- as.numeric(d$H1WP18E)
outlierReplace(d, "c.devent", which(d$c.devent > 2), NA)
summary(d$c.devent)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.2453  0.0000  1.0000    1962
d$c.mpp <- as.numeric(d$H1WP17F)
outlierReplace(d, "c.mpp", which(d$c.mpp > 2), NA)
summary(d$c.mpp)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.3872  1.0000  1.0000     381
d$c.dpp <- as.numeric(d$H1WP18F)
outlierReplace(d, "c.dpp", which(d$c.dpp > 2), NA)
summary(d$c.dpp)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   0.000   0.000   0.192   0.000   1.000    1962
d$c.mar <- as.numeric(d$H1WP17G)
outlierReplace(d, "c.mar", which(d$c.mar > 2), NA)
summary(d$c.mar)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.3294  1.0000  1.0000     381
d$c.dar <- as.numeric(d$H1WP18G)
outlierReplace(d, "c.dar", which(d$c.dar > 2), NA)
summary(d$c.dar)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.2457  0.0000  1.0000    1962
d$c.mschool <- as.numeric(d$H1WP17H)
outlierReplace(d, "c.mschool", which(d$c.mschool > 2), NA)
summary(d$c.mschool)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  1.0000  0.6289  1.0000  1.0000     381
d$c.dschool <- as.numeric(d$H1WP18H)
outlierReplace(d, "c.dschool", which(d$c.dschool > 2), NA)
summary(d$c.dschool)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  1.0000  0.5189  1.0000  1.0000    1962
d$c.mproject <- as.numeric(d$H1WP17I)
outlierReplace(d, "c.mproject", which(d$c.mproject > 2), NA)
summary(d$c.mproject)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.1318  0.0000  1.0000     381
d$c.dproject <- as.numeric(d$H1WP18I)
outlierReplace(d, "c.dproject", which(d$c.dproject > 2), NA)
summary(d$c.dproject)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.1118  0.0000  1.0000    1962
d$c.mtalk <- as.numeric(d$H1WP17J)
outlierReplace(d, "c.mtalk", which(d$c.mtalk > 2), NA)
summary(d$c.mtalk)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    0.00    1.00    0.52    1.00    1.00     381
d$c.dtalk <- as.numeric(d$H1WP18J)
outlierReplace(d, "c.dtalk", which(d$c.dtalk > 2), NA)
summary(d$c.dtalk)  # Just spent an hour of my lifetime on Copy and Paste, hope it will be statistical significant... I'm using my next birthday wish quota here... Sweet Please...!!!
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##  0.0000  0.0000  0.0000  0.4377  1.0000  1.0000    1962
Generating Attention(Intimacy) Parenting Index
C.vars <- c("c.mom","c.momc","c.dad","c.dadc","c.dinner", "c.mshopping","c.dshopping", "c.msport", "c.dsport", "c.mreligious", "c.dreligious", "c.mdate", "c.mevent", "c.mpp", "c.mar", "c.mschool", "c.mproject", "c.mtalk",  "c.ddate", "c.devent", "c.dpp", "c.dar", "c.dschool", "c.dproject", "c.dtalk")
sub.C <- d[ ,C.vars]

stopifnot(require(psych))
summary(alpha(sub.C)) #Yay!!! Cronbach's a looks yummy!  
## Warning in alpha(sub.C): Some items were negatively correlated with the
## total scale and probably should be reversed. To do this, run the function
## again with the 'check.keys=TRUE' option
## Some items ( c.mar ) were negatively correlated with the total scale and probably should be reversed.  To do this, run the function again with the 'check.keys=TRUE' option
## Reliability analysis   
##  raw_alpha std.alpha G6(smc) average_r S/N    ase mean   sd
##       0.64      0.74    0.81       0.1 2.9 0.0072  1.3 0.29
d$atten <- rowSums(sub.C, na.rm = TRUE)
d$atten.s <- normalizeData(d$atten, type = "norm")
summary(d$atten.s)
##        V1         
##  Min.   :-2.9529  
##  1st Qu.:-0.7464  
##  Median : 0.1826  
##  Mean   : 0.0000  
##  3rd Qu.: 0.7632  
##  Max.   : 2.5051
Health Conditions and Academic Performance data Washing Machine
Let’s check out kids’ health first, it’s always at the first place isn’t it?

d$health <- 6 - as.numeric(d$H1GH1) ## Ranging from 1 to 5, poor to excelent
outlierReplace(d, "health", which(d$health < 0), NA)
summary(d$health)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   3.000   4.000   3.897   5.000   5.000       5
Now let’s take a look of how they are doing at school

d$english <- 5 - as.numeric(d$H1ED11) ## Ranging from 0 to 4, not take the subject to D, C, B, A
outlierReplace(d, "english", which(d$english < 0), NA)
summary(d$english)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   2.000   3.000   2.796   4.000   4.000     234
d$math <- 5 - as.numeric(d$H1ED12) ## Ranging from 0 to 4, not take the subject to D, C, B, A
outlierReplace(d, "math", which(d$math < 0), NA)
summary(d$math)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   2.000   3.000   2.533   3.000   4.000     229
d$ss <- 5 - as.numeric(d$H1ED13) ## Ranging from 0 to 4, not take the subject to D, C, B, A
outlierReplace(d, "ss", which(d$ss < 0), NA)
summary(d$ss)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   2.000   3.000   2.596   4.000   4.000     222
d$science <- 5 - as.numeric(d$H1ED14) ## Ranging from 0 to 4, not take the subject to D, C, B, A
outlierReplace(d, "science", which(d$science < 0), NA)
summary(d$science)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   2.000   3.000   2.521   4.000   4.000     212
A.vars <- c("english", "math", "ss", "science")
sub.A <- d[ ,A.vars]

stopifnot(require(psych))
summary(alpha(sub.A)) # SEEMS LIKE THERE IS DIFFERENCE BETWEEN GOOD STUDENTS AND BAD STUDNETS... The difference is whether they're coding from 9 till now on a Sunday, it should be brunch time!!!
## 
## Reliability analysis   
##  raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd
##       0.68      0.68    0.62      0.35 2.2 0.011  2.6 0.87
d$acaps <- rowSums(sub.A, na.rm = TRUE)
d$acap <- normalizeData(d$acaps, type = "norm")
summary(d$acap)
##        V1          
##  Min.   :-2.61927  
##  1st Qu.:-0.54190  
##  Median :-0.02256  
##  Mean   : 0.00000  
##  3rd Qu.: 0.75646  
##  Max.   : 1.53547
Now let’s wash the demographic # Almost there T_T
d$age <- 98 - d$H1GI1Y
outlierReplace(d, "age", which(d$age < 3), NA)
summary(d$age) #AGE
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   15.00   18.00   19.00   19.04   20.00   24.00       3
d$race <- d$H1GI8
outlierReplace(d, "race", which(d$race > 5), NA)
summary(d$race)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   1.000   1.000   2.000   2.071   3.000   5.000    6194
d$paedu <- d$PA12
outlierReplace(d, "paedu", which(d$paedu > 9), NA)
summary(d$paedu)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   1.000   4.000   6.000   5.578   7.000   9.000     894
d$income <- d$PA55
outlierReplace(d, "income", which(d$income > 9995), NA)
summary(d$income)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##     0.0    22.0    40.0    47.7    60.0   999.0    1575
hist(d$income)


d$lnincome <- log(d$income)
hist(d$lnincome)


Initial Models
FINALLY…Now the house if pretty clean, let’s have some fun!!!

vars <- c("health", "acap", "age", "paedu", "lnincome", "income", "Inde.s", "atten.s")
sub <- d[ ,vars]

sub$health[which(is.nan(sub$health))] = NA
sub$health[which(sub$health == Inf)] = NA

sub$acap[which(is.nan(sub$acap))] = NA
sub$acap[which(sub$acap == Inf)] = NA

sub$age[which(is.nan(sub$age))] = NA
sub$age[which(sub$age == Inf)] = NA

sub$paedu[which(is.nan(sub$paedu))] = NA
sub$paedu[which(sub$paedu == Inf)] = NA

sub$lnincome[which(is.nan(sub$lnincome))] = NA
sub$lnincome[which(sub$lnincome == Inf)] = NA

sub$Inde.s[which(is.nan(sub$Inde.s))] = NA
sub$Inde.s[which(sub$Inde.s == Inf)] = NA

sub$atten.s[which(is.nan(sub$atten.s))] = NA
sub$atten.s[which(sub$atten.s == Inf)] = NA

d$agesquare <- d$age ^ 2

lm.health <- glm(formula = health ~ age + agesquare  + paedu + income + Inde.s + atten.s, data = d, na.action = na.omit)
summary(lm.health)
## 
## Call:
## glm(formula = health ~ age + agesquare + paedu + income + Inde.s + 
##     atten.s, data = d, na.action = na.omit)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.5042  -0.7670   0.0634   0.8854   1.6515  
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  4.5879186  1.3783048   3.329 0.000879 ***
## age         -0.1000809  0.1460891  -0.685 0.493334    
## agesquare    0.0027042  0.0038467   0.703 0.482088    
## paedu        0.0370252  0.0057306   6.461 1.14e-10 ***
## income       0.0002837  0.0002351   1.207 0.227668    
## Inde.s       0.0239795  0.0145225   1.651 0.098762 .  
## atten.s      0.1164255  0.0138949   8.379  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.7782482)
## 
##     Null deviance: 3926.1  on 4895  degrees of freedom
## Residual deviance: 3804.9  on 4889  degrees of freedom
##   (1608 observations deleted due to missingness)
## AIC: 12676
## 
## Number of Fisher Scoring iterations: 2
lm.acap <- lm(formula = acap ~ age + agesquare + paedu + health + income + Inde.s + atten.s, data = d, na.action = na.omit)
summary(lm.acap)
## 
## Call:
## lm(formula = acap ~ age + agesquare + paedu + health + income + 
##     Inde.s + atten.s, data = d, na.action = na.omit)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.3955 -0.5615  0.0761  0.6754  2.5675 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -1.7166693  1.4293418  -1.201 0.229801    
## age          0.2238059  0.1513344   1.479 0.139236    
## agesquare   -0.0093828  0.0039848  -2.355 0.018581 *  
## paedu        0.0655038  0.0059614  10.988  < 2e-16 ***
## health       0.1217168  0.0148146   8.216 2.67e-16 ***
## income       0.0007428  0.0002436   3.049 0.002306 ** 
## Inde.s       0.0571919  0.0150474   3.801 0.000146 ***
## atten.s      0.1959575  0.0144961  13.518  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.9138 on 4888 degrees of freedom
##   (1608 observations deleted due to missingness)
## Multiple R-squared:  0.1685, Adjusted R-squared:  0.1673 
## F-statistic: 141.5 on 7 and 4888 DF,  p-value: < 2.2e-16
