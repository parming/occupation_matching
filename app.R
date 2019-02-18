#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

occupations <<- read.table("data/offer_calculation.csv", sep=";", comment.char = "", header = TRUE)

confirmed_occupations <<- read.table("data/offer_confirmed.csv", sep=";", comment.char = "", header = TRUE)
confirmed_occupations <<- confirmed_occupations[,-1]

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Occupation matching"),
   
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      uiOutput("inputGroup"),
      uiOutput("result"),
      actionButton("answer_yes", "Yes"),
      actionButton("answer_no", "No")
    ),
      
    # Show a plot of the generated distribution
    mainPanel(
      uiOutput("job_title"),
      helpText("=="),
      uiOutput("job_suggestion"),
      uiOutput("job_tags"),
      verbatimTextOutput("occupation")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$answer_yes, {
    occupation <- occupations[occupations$id == input$id, ]
    occupation$match <- 1
    confirmed_occupations <<- rbind(confirmed_occupations, occupation)
    write.csv2(confirmed_occupations, file="data/offer_confirmed.csv", na="")
    
    occupations <<- occupations[occupations$id != input$id,]
    write.csv2(occupations[,names(occupations) != "X"], file="data/offer_calculation.csv", na="")
  })
  
  observeEvent(input$answer_no, {
    occupations <<- occupations[occupations$id != input$id,]
    write.csv2(occupations[,names(occupations) != "X"], file="data/offer_calculation.csv", na="")
  })
  
  occupation_guess <- eventReactive({
    input$answer_yes
    input$answer_no
  }, {
    
    result <- paste(nrow(confirmed_occupations), " - ", nrow(occupations))
    output$result = renderUI({
      helpText(result)
    })
    
    occupation <- sample_n(occupations, 1)
    
    output$inputGroup = renderUI({
      numericInput("id", "Ad ID", occupation$id)
    })
    output$job_title = renderUI({
      helpText(occupation$job_title)
    })
    output$job_suggestion = renderUI({
      helpText(occupation$related_skills)
    })
    output$job_tags = renderUI({
      helpText(occupation$knowledge_tags)
    })
  }, ignoreNULL = FALSE)
  
  output$occupation <- renderPrint({
    occupation_guess()
  })

}

# Run the application 
shinyApp(ui, server)

