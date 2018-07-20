options(digits=10)
crime <- read.csv('crime_v2.csv')

# Remove rcords with "'"
crime <- crime[crime$prbconv != "`",]
# Conver prbconv to numeric
crime$prbconv <- as.numeric(as.character(crime$prbconv))
# Remove NAs
crime <- crime[!is.na(crime$prbconv),]
summary(crime)


# Crime rate --------------------------------------------------------------
hist(crime$crmrte)

# Right skewed, possibly log transform

# Probability of arrest ---------------------------------------------------
hist(crime$prbarr)


library(car)
scatterplotMatrix(crime[3:10])
scatterplotMatrix(crime[10])


cor(crime[-c(1,2)])

summary(crime)
str(crime)

crime