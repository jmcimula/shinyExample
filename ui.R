library(shiny)
#Define UI for application that draws a histogram
shinyUI(pageWithSidebar(
  #Application title
  headerPanel("Shiny App: Data Analysis"),
  sidebarPanel(
           h3("Economics dataset"),
           helpText("This dataset was produced from US economic time series data available from http://research.stlouisfed.org/fred2...",
                    "PCE : Personal Consumption Expenditures, POP : Total Population, PSAVERT : Personal Savings Rate, UEMPMED: Median Duration of Unemployment, UNEMPLOY : Number of Unemployment in thousands  "),
           checkboxGroupInput("checkGroup",label = h3("Descriptive statistics"),
                                       choices = list("PCE" = "PCE", "POP" = "POP","PSAVERT" = "PSAVERT","UEMPMED" = "UEMPMED","UNEMPLOY"="UNEMPLOY" ),selected = "PCE"),
           submitButton("ok"),
           textInput("text", label = h3("Visualization (type Cor or Plot)")),
           submitButton("ok"),
           radioButtons("radio", label = h3("Dynamic Regression Models"),
                                 choices = list("NA"="","PCE" = "PCE", "POP" = "POP","PSAVERT" = "PSAVERT","UEMPMED" = "UEMPMED","UNEMPLOY"="UNEMPLOY" )),
           submitButton("ok")
  ),
  mainPanel(  
    h4("Descriptive statistics"),
    verbatimTextOutput("ck"),
    h4("Exploratory Analysis"),
    plotOutput("tx"),
    h4("Data models: choice of the best linear model "),
    verbatimTextOutput("rd"),
    h4("Dataset "),
    dataTableOutput("dt")
    )
  )
)
