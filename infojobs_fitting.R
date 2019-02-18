#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Naive Bayes fitting"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput("title", "Title"),
      textInput("tag1", "Tag 1"),
      textInput("tag2", "Tag 2"),
      actionButton("answer_yes", "Check"),
      actionButton("answer_suggest", "Suggest"),
      
      verbatimTextOutput("common_tags")
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      uiOutput("inputGroup"),
      verbatimTextOutput("result")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  suggestions <- eventReactive(input$answer_yes, {
    # confirmed_occupations <- read.table("data/offer_confirmed.csv", sep=";", comment.char = "", header = TRUE)
    job_title <- input$title
    tag <- input$tag1
    
    if (job_title == "") {
      index <- which(grepl(tag, jobs$knowledge_tags) & jobs$match == 0)
    } else if (tag == "") {
      index <- which(grepl(job_title, jobs$job_title) & jobs$match == 0)
    } else {
      index <- which(grepl(job_title, jobs$job_title) & grepl(tag, jobs$knowledge_tags) & jobs$match == 0)
    }
    
    output$inputGroup = renderUI({
      helpText(nrow(jobs[index,]))
    })
    
    titles <- head(jobs[index,]$job_title, 20)
    tags <- head(jobs[index,]$knowledge_tags, 20)
    unlist(mapply(paste, titles, tags))
  }, ignoreNULL = FALSE)
  
  
  suggest_tags <- eventReactive(input$answer_suggest, {
    
    common_titles <- str_word_frequence(jobs[jobs$match == 0,]$job_title, 5)
    common_combinations <- lapply(common_titles$tag, function(job_title) {
      index <- which(grepl(job_title, jobs$job_title) & jobs$match == 0)
      tags <- str_word_frequence(jobs[index,]$knowledge_tags, 5)
      tags$title <- job_title
      tags
    })
    rbind.fill(common_combinations) %>%
      select(title, tag, count)
    
  }, ignoreNULL = FALSE)
  
  
  output$result <- renderPrint({
    suggestions()
  })
  
  output$common_tags <- renderPrint({
    suggest_tags()
  })
  
}

# Run the application 
shinyApp(ui, server)

