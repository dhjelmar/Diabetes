##-----------------------------------------------------------------------------
## setup
os <- .Platform$OS.type
if (os == 'windows') {
    ## load generic modules
    source("F:\\Documents\\01_Dave\\Programs\\GitHub_home\\R-setup\\setup.r")
    ## identify working folder
    path <- c("f:/Documents/01_Dave/Programs/GitHub_home/Diabetes/")
} else {
    ## os == unix
    source('~/GitHub_repos/R-setup/setup.r')
    path <- c('~/GitHub_repos/Diabetes/')
}
## install.packages('xts')  # this is only needed one time
## set working folder
setwd(path)
## load local modules
r_files <- list.files(paste(path, 'modules/', sep=''), pattern="*.[rR]$", full.names=TRUE)
for (f in r_files) {
    ## cat("f =",f,"\n")
    source(f)
}
Sys.setenv(TZ=Sys.timezone())

pdf(file = 'bg_analysis.pdf', onefile = TRUE,          # creates a multi-page PDF file
    width = 11,  # The width of the plot in inches
    height = 8.5) # The height of the plot in inches


##--------------------------------------------------------------
## generate fake BG data
df.fake <- fake()

##--------------------------------------------------------------
## read real data  (this is a work in progress)
## df.real <- build_real_data()
## print(df.real)


##--------------------------------------------------------------
## By hand, create and read following with columns for: BG1, Carbs, Insulin, and BG2
who <- 'sam'
if (who == 'brad') {
    data.df <- read.csv('F:/Documents/01_Dave/Sync/diabetes/data/raw/analyzed.csv')
    ## data.df <- read.csv('f:/Documents/01_Dave/Programs/GitHub_home/Diabetes/analyzed.png.csv')
    data.df$date <- as.POSIXct(data.df$date, format="%Y-%m-%d %H:%M:%S")   # convert date to actual date format
    names(data.df) <- c('Date', 'Hour', 'Carbs', 'Insulin', 'BG1', 'BG2')
} else {
    data.df <- readall('sam.xlsx', header.row=14198, data.start.row=14199)
    data.df$Date <- as.POSIXct(data.df$EventDateTime, format="%Y-%m-%dT%H:%M:%S") 
    data.df$Hour <- NA
    data.df$Carbs <- data.df$CarbSize
    data.df$Insulin <- data.df$InsulinDelivered
    data.df$BG1 <- data.df$BG
    data.df$ActiveIns <- NA      # assume linear decrease over 
    ## pull next BG and associated date to define BG2
    Date.BG2 <- data.df$Date[2:nrow(data.df)] 
    BG2      <- data.df$BG1[2:nrow(data.df)]
    ## drop last row of data.df and combing with BG2 info
    data.df <- data.df[-nrow(data.df),]
    data.df$Date.BG2 <- Date.BG2
    data.df$BG2 <- BG2
    ## only keep variables I want    
    data.df <- subset(data.df, select=c('Date', 'Hour', 'Carbs', 'Insulin', 'BG1', 'Date.BG2', 'BG2'))
    ## need logic for active insulin
    ## alternately, need logic to combine carbs and insulin if given within an hour then set BG2 ~2 hrs later
}
cat('data as read in:\n')
print(data.df)

## drop zero insulin data
#data.df <- data.df[-which(data.df$Insulin == 0),]
#cat('\ndata after dropping 0 Insulin:\n')
#print(data.df)

## change dataframe to xts object to better handle dates
## can change back to dataframe with as.data.frame(data.xts)
## or to a matrix with time as row index with as.matrix(data.xts)
## or to a maatrix with numeric index with zoo::coredata(data.xts)
data.xts <- xts::as.xts(zoo::read.zoo(data.df,   index.column = 1, format = "%Y-%m-%d %H:%M:%S", tz=Sys.timezone()))
## print(data.xts)

if (who == 'sam') {
    ## determine onboard insulin and BG2

    ## first create another xts object with data shifted 1 row earlier
    advanced <- xts::lag.xts(data.xts, k=-1)     # this changes columns to character for some reason dlh
    data.xts$BG2 <- advanced$BG1
    data.xts$Date.BG2 <- advanced$Date.BG2
    head(data.xts)
    tail(data.xts)

    ## determine time BG1 and BG2
    data.xts$hours.passed <- data.xts$Date.BG2 - data.xts$Date
}

##--------------------------------------------------------------
## determine form of model to use
plotspace(1,2)

## fit on change in BG
out <- data.xts
dbg.fit <- lm(I(BG2 - BG1) ~ 0   + Insulin + Carbs, data=out)      # BG2 - BG1 = C1 * Insulin + C2 * Carbs
summary(dbg.fit)
dbg.coef <- as.data.frame(t(coef(dbg.fit)))
S <- -dbg.coef$Insulin
R <- S / dbg.coef$Carbs
cat('fit on BG2 - BG1: S=', S, ', R=', R, '\n')
out$BG2.dbg.fit     <- out$BG1 + S * out$Insulin + -S/R * out$Carbs
out$Insulin.dbg.fit <- (out$BG2 - out$BG1) / S + out$Carbs / R

## fit on insulin (I do not think this is the right approach)
out$dBG <- out$BG2 - out$BG1
insulin.fit <- lm(I(Insulin) ~ 0 + dBG + Carbs, data=out)  # Insulin = C1 * (BG2-BG1) + C2 * Carbs
summary(insulin.fit)                                       # Reject coefficient on dBG (pr = 0.07 > 0.05)
insulin.coef <- as.data.frame(t(coef(insulin.fit)))
S <- -1 / insulin.coef$dBG
R <-  1 / insulin.coef$Carbs
cat('fit on insulin  : S=', S, ', R=', R, '\n')
out$BG2.insulin.fit     <- out$BG1 + S * out$Insulin + -S/R * out$Carbs
out$Insulin.insulin.fit <- (out$BG2 - out$BG1) / S + out$Carbs / R

plotspace(1,2)
out.df <- as.data.frame(out)
with(out.df, plotfit(BG2, BG2.dbg.fit, ylabel='BG2 prediction', ylimspec = range(BG2.dbg.fit, BG2.insulin.fit)))
with(out.df, addfit(BG2, BG2.insulin.fit, col='red'))
abline(0,1, col='blue', lty=2)
legend('topleft', legend=c('fit on BG', 'fit on Insulin'), col=c('black', 'red'), lty=1)

with(out.df, plotfit(Insulin, Insulin.dbg.fit, ylabel='Insulin prediction', 
                  ylimspec = range(Insulin.dbg.fit, Insulin.insulin.fit)))
with(out.df, addfit(Insulin, Insulin.insulin.fit, col='red'))
abline(0,1, col='blue', lty=2)
legend('topleft', legend=c('fit on BG', 'fit on Insulin'), col=c('black', 'red'), lty=1)


##--------------------------------------------------------------
## determine sensitivity and carb ratio from data

fitit <- function(df, date.range='all') {
    ## df = XTS object with Carbs, Insulin, BG1 and BG2
    ## daternage = date range (e.g., 'all' for all dates in df
    ##                               '2022-11-03' for a single day
    ##                               '2022-11-05/2022-11-08' for 11/5/22 through 11/8/22
    ##                               '2022-11-03/2022' for 11/3/22 through end of 2022)
    if (date.range != 'all') {
        df <- df[date.range]
    }
    fit <- lm(I(BG2 - BG1) ~ 0 + Insulin + Carbs, data=df)
    summary(fit)
    c1 <- fit$coefficients[[1]]
    c2 <- fit$coefficients[[2]]
    sensitivity <- -c1
    carb.ratio  <- sensitivity / c2
    pvalue <- as.data.frame(t(summary(fit)$coefficients[,4]))
    ## cat('\ndate.range =', daterange, '\n')
    ## cat('sensitivity =', sensitivity, '\n')
    ## cat('carb.ratio  =', carb.ratio, '\n')
    coef <- data.frame(date.range=date.range, sensitivity=sensitivity, carb.ratio=carb.ratio, 
                       sensitivity.pvalue=pvalue$Insulin, carb.ratio.pvalue=pvalue$Carbs)
    return(list(coef=coef, fit=fit, df=df))
}


##----------------------
## idea: use sliding window of fixed duration rather than "start date / 2022" in for loop below
##       look into: slider::slide_period() here: https://cran.r-project.org/web/packages/slider/vignettes/slider.html
##                  xts::apply.weekly(xts.ts, function(x) var(x))
##----------------------


days <- unique(format(zoo::index(data.xts), format = '%Y-%m-%d'))
fits <- df.init(0, c('date.range', 'sensitivity', 'carb.ratio', 'sensitivity.pvalue', 'carb.ratio.pvalue'))
for (start.date in as.character(days)) {
    date.range <- paste(start.date, '/', xts::last(days), sep='')
    fit  <- fitit(data.xts, date.range)
    fits <- rbind(fits, fit$coef)
}
fits <- droplevels.all(fits)
## print(data.xts[,1:4])
cat('\n')
print(fits)


##-----------------------------------------------------------------------------
## add model predictions to data
## select model
cat('\n')
model <- readline('Enter model number for plot from above list (or 0 to enter sensitivity and carb.ratio): ')
model <- as.numeric(model)
if (model > 0 & model < nrow(fits)) {
    model.dates <- fits[model,]$date.range
    sensitivity <- round(fits[model,]$sensitivity, 1)
    carb.ratio  <- round(fits[model,]$carb.ratio , 1)
    title <- paste('Model ', model, ': date range=', model.dates,
                   ', sensitivity=', sensitivity, ', carb.ratio=', carb.ratio, se4p='')
    model.plot <- TRUE
    print(fits[model,])
    
} else if (model == 0) {
    model.dates <- paste(days[1], '/', xts::last(days), sep='')
    sensitivity <- readline('Enter sensitivity: ')
    sensitivity <- as.numeric(sensitivity)
    carb.ratio  <- readline('Enter carb ratio : ')
    carb.ratio  <- as.numeric(carb.ratio)
    title <- paste('Not a fitted model: date range=', model.dates,
                   ', sensitivity=', sensitivity, ', carb.ratio=', carb.ratio, sep='')
    model.plot <- FALSE

} else {
    ## nothing specified or incorrect info specified
    model.dates <- paste(days[1], '/', xts::last(days), sep='')
    sensitivity <- 85
    carb.ratio  <- 6
    title <- paste('Not a fitted model: date range=', model.dates,
                   ', sensitivity=', sensitivity, ', carb.ratio=', carb.ratio, sep='')
    model.plot <- FALSE
}    
## sensitivity <- 85
## carb.ratio  <-  6
## sensitivity <- 17.5
## carb.ratio  <-  4.4
pred.xts <- data.xts
pred.xts$Insulin_model  <- (pred.xts$BG1-120)/sensitivity + pred.xts$Carbs/carb.ratio
pred.xts$BG2_model <- pred.xts$BG1 - sensitivity * pred.xts$Insulin + sensitivity/carb.ratio * pred.xts$Carbs
model.xts <- pred.xts[model.dates]

cat('\n')
cat('model data:\n')
print(model.xts)


##--------------------------------------------------------------------------
## plot results

##----------------------
## time history plot
data.xts.plot <- data.xts[,1:4]
xts::plot.xts(data.xts.plot)
plot(xts::addLegend("topright", legend.names = names(data.xts.plot), lty=1, col=1:ncol(data.xts.plot)))
## xts::plot.xts(data.xts$Insulin)


##----------------------
## convert xts objects to dataframes for plotting
pred.df <- as.data.frame(pred.xts)
model.df <- as.data.frame(model.xts)

##----------------------
## trend plots
with(pred.df, plotfit(BG1, BG2, main='Effectiveness of Bolus at Different Starting BGs'))


##----------------------
## plot BG change
with(pred.df, plotfit(BG2 - BG1, BG2_model - BG1, main=title))
if (model.plot == TRUE) addfit(model.df$BG2 - model.df$BG1, model.df$BG2_model - model.df$BG1, col='red', pch=16)
abline(0,1,col='blue', lty=2)


##----------------------
## plot BG2
with(pred.df, plotfit(BG2, BG2_model, main=title))
if (model.plot == TRUE) addfit(model.df$BG2, model.df$BG2_model, col='red', pch=16)
abline(0,1,col='blue', lty=2)


dev.off()


##----------------------
## 3D plot of model
surf.Carbs   <- seq(min(data.df$Carbs), max(data.df$Carbs), length=100)
surf.Insulin <- seq(min(data.df$Insulin), max(data.df$Insulin), length=100)
surf.BG1     <- seq(min(data.df$BG1), max(data.df$BG1), length=100)
BG_change <- df.init(100,100)
perfect   <- df.init(100,100)
for (i in 1:100) {
    for (j in 1:100) {
        BG_change[i,j] <- - sensitivity * surf.Insulin[j] + sensitivity/carb.ratio * surf.Carbs[i]
        perfect[i,j] <- 120
    }
}
BG_change <- as.matrix(BG_change)

## create plot
with(data.df, rgl::plot3d(Carbs, Insulin, BG2 - BG1, main=title))
rgl::surface3d(surf.Carbs, surf.Insulin, BG_change, alpha=0.7, col='red')
if (model.plot == TRUE) rgl::points3d(model.df$Carbs, model.df$Insulin,
                                      model.df$BG2 - model.df$BG1, col='red', pch=8)     ## this does not work
 
## create plot
with(data.df, rgl::plot3d(Carbs, BG1, BG2, main=title))
rgl::surface3d(surf.Carbs, surf.BG1, perfect, alpha=0.7, col='red')
