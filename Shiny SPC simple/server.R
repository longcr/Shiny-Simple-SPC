##### server.R #####

# load packages ---------------------------------------------------------------

library(shiny)
library(qcc)

# load the data ---------------------------------------------------------------

wd.datapath = paste0(getwd(),"/data")
wd.init = getwd()
setwd(wd.datapath)

d.in = read.table("data by bu fac metric.txt", sep = '\t', header = TRUE)

setwd(wd.init)


# select SPC response variable (using proportions) ----------------------------

# shiny server body -----------------------------------------------------------

shinyServer(function(input, output, session) { 
  
  output$ui <- renderUI({
    sidebarPanel(

      includeHTML("include.html"),

      selectInput(inputId = "in.bu", 
                  label = "Choose Business Unit", 
                  choices = levels(d.in$business_unit),
                  selectize = FALSE),
            
      
      selectInput(inputId = "in.facility", 
                  label = "Choose Facility", 
                  "",
                  selectize = FALSE),
            
      
      selectInput(inputId = "in.metric", 
                  label = "Choose Metric",
                  choices = c("Metric 1", "Metric 2"),
                  selectize = FALSE)
            
  ) })

  
  # this section is used to change the facility list based on the business unit
  # placed outside of the 'ui 'call but within the 'server' call
  # example is here:
  # http://stackoverflow.com/questions/21465411/r-shiny-passing-reactive-to-selectinput-choices
  
  outVar = reactive({
    levels(droplevels(d.in[d.in$business_unit == input$in.bu,]$facility))
  })
  
  observe({
    updateSelectInput(session, "in.facility",
                      choices = outVar()
    )})
  

  
# Individuals (X) chart -------------------------------------------------------

  output$plotDisplay <- renderPlot({
    
    # select subset for specific facility
    
    d.strata = subset(d.in, d.in$facility == input$in.facility)  # subset
    
    d.strata = d.strata[order(d.strata$year, d.strata$monthnum, 
                              decreasing = FALSE),]  # order by month
    
    
    # set metric

    if (input$in.metric == "Metric 1") {qccvar = "metric_1"}
    if (input$in.metric == "Metric 2") {qccvar = "metric_2"}

    
    d.strata$yearmon = as.factor(paste0(d.strata$year,"-",d.strata$monthnum))
    
    
    # create SPC chart
    
    x.chart = qcc( d.strata[,qccvar], type = "xbar.one", labels = d.strata[,'yearmon'],
                   title = paste("Individuals chart\n", input$in.facility, qccvar) )
    

  })  # end renderPlot

})    # end shinyServer


### END CODE ###