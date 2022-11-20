## this is a work in progress

build_real_data <- function() {
    ## identify folders with data
    folders <- list.dirs('../raw data')
    folders <- folders[-1]  # drop the first one since that is the parent
    ## initialize dataframes
    columns <- c('Date', 'Day', 'Time', 'Metric', 'Unit', 'Amount')
    bg <- df.init(rows=0, columns=columns)
    insulin <- bg
    carbs   <- bg
    for (folder in folders) {
        
        ## glucose and insulin
        df.bio <- read.csv(paste(folder, '/', 'biometrics.csv', sep=''))
        Date   <- paste(df.bio$Day, df.bio$Time, sep=" ")
        df.bio$Date <- as.POSIXct(Date, format="%Y-%m-%d %I:%M %p")
        df.bio <- subset(df.bio, select=columns)
        bg      <- rbind(bg     , df.bio[df.bio$Metric == 'Blood Glucose',])
        insulin <- rbind(insulin, df.bio[df.bio$Metric == 'Insulin',])
        
        ## carbs
        df.srv <- read.csv(paste(folder, '/', 'servings.csv', sep=''))
        df.srv$Metric <- 'Carbs'
        df.srv$Unit <- 'grams'
        df.srv$Amount <- df.srv$Carbs..g.
        Date   <- paste(df.srv$Day, df.srv$Time, sep=" ")
        df.srv$Date <- as.POSIXct(Date, format="%Y-%m-%d %I:%M %p")
        carbs <- rbind(carbs, subset(df.srv, select=columns))

    }




################################################# dlh stopped here

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

    return(df)
}
