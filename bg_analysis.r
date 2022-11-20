##--------------------------------------------------------------
## generate fake BG data
df <- '
BG1   Carbs   Insulin    BG2
220   25       5          NA
220   30       5          NA
220   50       5          NA
220   90       5          NA
350   25       5          NA
350   30       6          NA
350   50      10          NA
200   25       5          NA
200   30       5          NA
200   50       5          NA
200   90       5          NA
200   90      12          NA
'
df <- read.table(text=df, header=TRUE, stringsAsFactors=FALSE)
## asssume sensitivity and carb ratio for fake data
sensitivity.input <- 85
carb.ratio.input  <-  6
A <- rnorm(nrow(df), sensitivity.input, 1)
B <- rnorm(nrow(df), carb.ratio.input , 0.1)
df$BG2 <- df$BG1 - A * df$Insulin + A/B * df$Carbs
print(df)



##--------------------------------------------------------------
## read real data
df.bio <- read.csv('biometrics.csv')
## bg <- df.bio[df.bio$Metric == 'Blood Glucose',]
## insulin <- df.bio[df.bio$Metric == 'Insulin', ]
bg <- subset(df.bio, select=c('Day', 'Time', 'Metric', 'Unit', 'Amount'))
bg <- bg[bg$Metric == 'Blood Glucose' | bg$Metric == 'Insulin',]

df.srv <- read.csv('servings.csv')
carbs <- df.srv
carbs$Metric <- 'Carbs'
carbs$Unit <- 'grams'
carbs$Amount <- carbs$Carbs..g.
carbs <- subset(carbs, select=c('Day', 'Time', 'Metric', 'Unit', 'Amount'))

## combine and sort by date and time
df <- rbind(bg, carbs)
Date <- paste(df$Day, df$Time, sep=" ")
df$Date <- as.POSIXct(Date, format="%Y-%m-%d %I:%M %p")
df <- df[order(df$Date),]

## reorder to put Date first
df <- subset(df, select=c('Date', 'Day', 'Time', 'Metric', 'Unit', 'Amount'))

write.csv(df, file="bg.carbs.insulin.csv")
## combine bg, insulin, and carbs to be same format as in fake data
## or change logic below to handle a different structure

##--------------------------------------------------------------
## By hand, create and read following with columns for: BG1, Carbs, Insulin, and BG2
df <- read.csv('bg.carbs.insulin.modified.csv')
Date <- paste(df$Day, df$Time, sep=" ")
df$Date <- as.POSIXct(Date, format="%Y-%m-%d %I:%M %p")
df <- subset(df, select=c('Date', 'Day', 'Time', 'BG1', 'Carbs', 'Insulin', 'BG2'))
df$Insulin_85_6 <- (df$BG1-120)/85 + df$Carbs/6
df$BGtarget_85_6 <- df$BG1 - 85 * df$Insulin + 85/6 * df$Carbs
print(df)

##--------------------------------------------------------------
## determine sensitivity and carb ratio from data

## limit data to 2022-11-05 and later


## determine sensitivity and carb ratio for selected data
fitit <- function(df) {
    fit <- lm(I(BG2 - BG1) ~ 0 + Insulin + Carbs, data=df)
    summary(fit)
    c1 <- fit$coefficients[[1]]
    c2 <- fit$coefficients[[2]]
    sensitivity <- -c1
    carb.ratio  <- sensitivity / c2
    pvalue <- as.data.frame(t(summary(fit)$coefficients[,4]))
    cat('sensitivity =', sensitivity, '\n')
    cat('carb.ratio  =', carb.ratio, '\n')
    coef <- data.frame(sensitivity=sensitivity, carb.ratio=carb.ratio, 
                       sensitivity.pvalue=pvalue$Insulin, carb.ratio.pvalue=pvalue$Carbs)
    return(list(coef=coef, fit=fit))
}
fit  <- fitit(df)
fits <- fit$coef

fit  <- fitit(df[df$Date > '2022-11-03',])
fits <- rbind(fits, fit$coef)

fit  <- fitit(df[df$Date > '2022-11-04',])
fits <- rbind(fits, fit$coef)

fit  <- fitit(df[df$Date > '2022-11-05',])
fits <- rbind(fits, fit$coef)

fits
