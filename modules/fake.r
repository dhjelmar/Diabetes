fake <- function() {
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
    return(df)
}
