library(magrittr)
library(car)
library(dplyr)
library(MASS)
library(corrplot)

cor(crime[-c(1,2)])

corrplot(cor(crime[-c(1,2)]),  method='shade')

?corrplot
summary(crime)
str(crime)

options(digits=10)
crime <- read.csv('crime_v2.csv')

# Remove rcords with "'"
crime <- crime[crime$prbconv != "`",]
# Conver prbconv to numeric
crime$prbconv <- as.numeric(as.character(crime$prbconv))
# Remove NAs
crime <- crime[!is.na(crime$prbconv),]
summary(crime)


scatterplotMatrix(crime[3:10])
scatterplotMatrix(
  crime %>% select(3, c(11:14)))
scatterplotMatrix(
  crime %>% select(3, c(15:20)))
scatterplotMatrix(
  crime %>% select(3, c(21:25)))



# Crime rate --------------------------------------------------------------
hist(crime$crmrte)

# Right skewed, possibly log transform
hist(log(crime$crmrte))

# Probability of arrest ---------------------------------------------------
hist(crime$prbarr)

summary(crime)


# Probability of conviction -----------------------------------------------
hist(crime$prbconv)

# Right skewed, possibly log transform
hist(log(crime$prbconv))


# Probability of prison sentence ------------------------------------------
hist(crime$prbpris)


# Avg sentence, days ------------------------------------------------------
hist(crime$avgsen)

# Right skewed, possibly log transform
hist(log(crime$avgsen))


# Police per cap ----------------------------------------------------------
hist(crime$polpc)

# Right skewed, possibly log transform
hist(log(crime$polpc))


# Density -----------------------------------------------------------------
hist(crime$density, breaks = 20)

# hist(log(crime$density), breaks = 20)  
# Correlation fo 0.73 without transformation, recommend for inclusion
cor(crime %>% transmute(crmrte, log(crmrte), density, log(density)))


# Tax revenue per cap -----------------------------------------------------

hist(crime$taxpc)

hist(log(crime$taxpc))
# Better correlation without transformation
# Correlation of 0.45, recommend for inclusion
corrplot(cor(crime %>% transmute(crmrte, log(crmrte), taxpc, log(taxpc))))



# Location ----------------------------------------------------------------
mean(crime$west)
mean(crime$central)
mean(crime$urban)

# Urban and west have relatively high correlations
cor(crime %>% transmute(crmrte,  west, central, urban))


# Pct minority ------------------------------------------------------------
hist(crime$pctmin80)
hist(log(crime$pctmin80))

# Higher correlation with log transformation of 0.31
cor(crime %>% transmute(crmrte, log(crmrte), pctmin80, log(pctmin80)))



# Weekly wage -------------------------------------------------------------
# Construction
hist(crime$wcon)
# Trns, util, commun
hist(crime$wtuc)
# Wholesale, retail
hist(crime$wtrd)
# Finance, nsurance, re
hist(crime$wfir)
# Service
hist(crime$wser)
# Manufacturing
hist(crime$wmfg)
# Fed emps
hist(crime$wfed)
# State emps
hist(crime$wsta)
# Local gov emps
hist(crime$wloc)

# Features in this subset don't appear to require additional transformation
cor(crime %>% transmute(crmrte, wcon, wtuc, wtrd, wfir, wser, wmfg, wfed, wsta, wloc))


# Mix ---------------------------------------------------------------------
hist(crime$mix)
hist(log(crime$mix))
# Correlations appear low, even lower with log
cor(crime %>% transmute(crmrte, log(crmrte), mix, log(mix)))

# Pct young male ----------------------------------------------------------
hist(crime$pctymle)
# Highly skewed, may require log transformation
hist(log(crime$pctymle))
# Cor 0.32 with log pctmyle
cor(crime %>% transmute(crmrte, log(crmrte), pctymle, log(pctymle)))

summary(full.model)
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)


summary(step.model)

summary(crime)
