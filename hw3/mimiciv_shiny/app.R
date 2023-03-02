#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
icu_cohort <- readRDS("icu_cohort.rds")

# Define UI for app with three tabs for three kinds of variables in MIMIC
ui <- fluidPage(
  # Application title
  titlePanel("MIMIC-IV Cohort Summaries of Demographics, Labs, and Vitals"),
  #Tabs
  tabsetPanel(
    #First tab of Demographics
    tabPanel("Demographics",
             br(),
             sidebarPanel(
               br(),
               selectInput("demo1", "Demographic one:",
                           c("Ethnicity" = "ethnicity",
                             "Language" = "language", 
                             "Insurance" = "insurance",
                             "Marital Status" = "marital_status",
                             "Gender" = "gender",
                             "Age at Hospital Admission" = "adm_age")),
               br(),
               checkboxInput("mort", "Only Thirty Day Mortality Patients")),
             mainPanel(plotOutput("plot"))),
    #Second tab of Lab Measurements
    tabPanel("Lab Measurements",
             br(),
             sidebarPanel(
               br(), 
               selectInput("meas", "Measurement Type:",
                           c("Sodium" = "Sodium",
                             "Chloride" = "Chloride", 
                             "White Blood Cells" = "WhiteBloodCells",
                             "Bicarbonate" = "Bicarbonate",
                             "Glucose" = "Glucose",
                             "Potassium" = "Potassium",
                             "Hematocrit" = "Hematocrit",
                             "Creatinine" = "Creatinine")),
               br(),
               sliderInput("bin", "Number of bins", 1, 20, 10)),
             mainPanel(plotOutput("plot2"))),
    #Third tab of Vitals
    tabPanel("Vitals",
             br(),
             sidebarPanel(
               br(), 
               selectInput("meas2", "Vital Measurement Type:",
                           c("Respiratory Rate" = "RespiratoryRate",
                             "Heart Rate" = "HeartRate", 
                             "Non-Invasive Blood Pressure (Systolic)" =
                               "NonInvasiveBloodPressuresystolic",
                             "Temperature (Fahrenheit)" =
                               "TemperatureFahrenheit",
                               "Non-Invasive Blood Pressure (mean)" =
                               "NonInvasiveBloodPressuremean")),
               br(),
               sliderInput("bins", "Number of bins", 1, 20, 10)),
             mainPanel(plotOutput("plot3")))
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot <- renderPlot({
    if (input$mort == TRUE) {
      icu_cohort %>%
        filter(thirty_day_mort == TRUE) %>%
        ggplot(aes_string(input$demo1)) + geom_bar()} 
    else {
          ggplot(icu_cohort, aes_string(input$demo1)) + geom_bar()}})
  
    output$plot2 <- renderPlot({
      icu_cohort %>%
        drop_na() %>%
        ggplot(aes_string(input$meas)) +
        geom_histogram(bins = as.numeric(input$bin))})
    
    output$plot3 <- renderPlot({
      icu_cohort %>%
        drop_na() %>%
        ggplot(aes_string(input$meas2)) +
        geom_histogram(bins = as.numeric(input$bins))})
}

# Run the application 
shinyApp(ui = ui, server = server)
