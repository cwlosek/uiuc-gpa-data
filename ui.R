library(shiny)
library(plotly)

shinyUI(fluidPage(
  
  titlePanel("UIUC COURSE DATA"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("display", "Select Information to View",
                  choices = c("Grade Distributions", "Search by Gen Ed", "Scatter Plot (All Courses)")),
      
      conditionalPanel("input.display == \"Grade Distributions\"", 
                       
                       selectInput("Subject", "Subject", choices = c("", unique(fs1517$Subject))),
                       
                       selectInput("Course", "Course Number", choices = ""),
                       
                       selectInput("Title", "Course Name", choices = ""),
                       
                       selectInput("Professor", "Individual GPA for Professor", choices = ""),
                       
                       checkboxInput("Theme", "Use UIUC Theme")),
      
      
      conditionalPanel("input.display == \"Search by Gen Ed\"", 
                       checkboxGroupInput("geCategory", "General Education Category", 
                                          choices = c("Advanced Composition", 
                                                      "Non-Western Culture",
                                                      "Western/Comparative Culture",
                                                      "Humanities & the Arts",
                                                      "Natural Science & Technology",
                                                      "Quantitative Reasoning",
                                                      "Social & Behavioral Sciences"))),
      
      actionButton("donate", "Donate", onclick = "window.open('https://www.paypal.me/Wlosek/2', '_blank')")
      
    ),
    
    mainPanel(
      conditionalPanel("input.display == \"Grade Distributions\" && input.Course != \"\" && input.Title != \"\"", 
                       plotlyOutput("gpaPlot")),
      
      conditionalPanel("input.display == \"Scatter Plot (All Courses)\"",
                       plotlyOutput("scatterplot")),
      
      conditionalPanel("input.display == \"Search by Gen Ed\"", dataTableOutput("table")),
      
      conditionalPanel("input.display == \"Grade Distributions\" && input.Course != \"\" && input.Title != \"\"",
                       textOutput("text1"),
                       textOutput("text2")),
      
      conditionalPanel("input.display == \"Grade Distributions\" && input.Course != \"\" && input.Title != \"\"
                       && input.Professor != \"\"",
                       textOutput("text3")),
      
      tags$style("#text1{font-weight: bold;
                 top: 50%;}"),
      
      tags$style("#text3{font-weight: bold;
                 color: red;
                 top: 50%;}")
    )
   )
 )
)