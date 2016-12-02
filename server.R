#Loading libraries
library(shiny)
library(ggplot2) #data visualization. install.packages("ggplot2")
library(corrplot) #correlation plotting system. install.packages("corrplot")
library(Rmisc) #tool to group many charts. install.packages("Rmisc")
library(PerformanceAnalytics) #Econometric tools for performance and risk analysis. install.packages("PerformanceAnalytics")
library(stringr) #tool to deal with strings. install.packages("stringr")
library(cblmr) #tool to conduct to the choice of the best linear model. devtools::install_github("jmcimula/cblmr")
library(dplyr)
#Loading the data
data("economics")
         
#This dataset comes with the library ggplot2
dtEcom <- economics[,-1]

#Function for CheckBoxGroup
plot_func <- function(x){
  #Space delimiter from CheckBoxGroup
  stg <- unlist(gregexpr(pattern = ' ', x))
  dlm <- length(stg)

  #Applying the delimiter length to retrieve single value
  if(dlm > 1){
            #Initializing a new data frame
            df <- data.frame()
            for(j in 1 : dlm){

                 val <- x[j]
                 val <- tolower(val)

                 for(i in 1:dim(dtEcom)[2]){
                   #Take one by one chosen items from CheckBoxGroup
                   if(identical(names(dtEcom)[i],val)){
                           dfcol <- dtEcom [,i]
                           break;
                   }
                 }
                 #Descriptive statistic
                 dfdes <- summary(dfcol)
                 mis <- length(which(is.na(dfcol)))
                 obs <- sum(dfcol)
                 dfdes <- data.frame(
                                     var=x[j],min=str_trim(str_replace_all(str_replace_all(dfdes[1],"Min.",""),":","")),
                                     max=str_trim(str_replace_all(str_replace_all(dfdes[6],"Max.",""),":","")),
                                     mean=str_trim(str_replace_all(str_replace_all(dfdes[3],"Median",""),":","")),
                                     missing=mis,
                                     observations=obs)
                 df <- rbind(df,dfdes) #Populating data frame
            }
}else{
    val <- x[1] #Single value selected
    val <- tolower(val)

    for (i in 1:dim(dtEcom)[2]){
      #Take one by one chosen items from CheckBoxGroup
      if (identical(names(dtEcom)[i], val)){
                 dfcol <- dtEcom [,i]
                 break;
      }
    }
    #Descriptive statistics
    dfdes <- summary(dfcol)
    mis <- length(which(is.na(dfcol)))
    obs <- sum(dfcol)
    df <- data.frame(var=x[1],min=str_trim(str_replace_all(str_replace_all(dfdes[1],"Min.",""),":","")),
                          max=str_trim(str_replace_all(str_replace_all(dfdes[6],"Max.",""),":","")),
                          mean=str_trim(str_replace_all(str_replace_all(dfdes[3],"Median",""),":","")),
                          missing=mis,
                          observations=obs
                     )#Simple data frame
  }
  #Return the data frame
  return(df)
}

#Function for EDA
eda <- function(x){
  
dtEcomOne <- economics
 
if(identical(tolower(x),"plot")){
    # First plot
    plotOne <- ggplot(dtEcomOne, aes(x=pce, y=psavert, colour=uempmed, group=unemploy)) + geom_line() +  ggtitle("Personal Consumption Expenditures and Savings Rate")
    # Second plot
    plotTwo <- ggplot(dtEcomOne, aes(x=date, y=pop, colour=pce)) + geom_point(alpha=.3) + geom_smooth(alpha=.2, size=1) + ggtitle("Growth of Population")
    # Third plot
    plotThree <- ggplot(subset(dtEcomOne, pop>25000), aes(x=uempmed, colour=unemploy)) + geom_density() + ggtitle("Median Duration of Unemployment for Population greater than 25,000")

    mplot <- multiplot(plotOne, plotTwo, plotThree, cols = 2)

}else if(identical(tolower(x),"cor")){
    #Correlation
    mplot <- corrplot(cor(dtEcomOne[,-1]), method="circle", is.corr = TRUE)
}else{
    #Default plot
    mplot <- chart.Correlation(dtEcomOne[,-1], histogram = TRUE, pch = 19)
}
  return(mplot)
}

#Data models
model <- function(x){
  
if(nchar(x) > 0){
        df <- blm_choice(dataframe = dtEcom, response = tolower(x))
        return(knitr::kable(df[,-c(7,8,9,10,11)]))
}else{
    return("Please make a choice from radio buttons") 
}
  
}
#Dataset
dataset <- function(){
  return(economics)
}

#Count of Unemployment from 1967 to 2015
ump <- function(){
  
  df <- economics[, c("date","unemploy")]
  df <- df %>%
    select(date, unemploy) %>%
    mutate(year = as.integer(substr(date,1,4))) %>%
    group_by(year) %>%
    summarise(s_unemploy = sum(unemploy))
  return(df)
}

# Rendering functions
shinyServer(function(input, output) {

  output$ck <- renderPrint({plot_func(input$checkGroup)})#call plot_func function
  output$rd <- renderPrint({model(input$radio)}) #call model
  output$tx <- renderPlot({eda(input$text)}) #call eda
  output$dt <- renderDataTable({dataset()}) #call dataset
  output$un <- renderTable({ump()}) #call ump
})
