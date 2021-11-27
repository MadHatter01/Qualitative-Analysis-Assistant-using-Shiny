library(shiny)
library(shinythemes)
library(DT)

ui<- fluidPage(theme= shinytheme('yeti'),
               navbarPage("My Page",
                  tabPanel("Navbar",
                  sidebarPanel(
                     h3('Enter numbers'),
                     textInput("num1", "Enter num1:","23"),
                     textInput("num2", "Enter num2:","34"),
          
                     fileInput(inputId = "filedata",
                               label = "Upload data. Choose CSV file",
                               accept = c(".csv"))
                    ),
                 mainPanel(
                   h1('Hello world'),
                   p('This is just another text!'),
                   verbatimTextOutput("addnum"),
                   h3("Display contents of CSV file"),
                   dataTableOutput("textdata")
                 )
                ),
                tabPanel('Categories','Blank for now'),
                tabPanel('Frequency','Blank for now')
               )
)


server <- function(input, output, session) {
  data <- reactive({
    inFile <- input$filedata
    if (is.null(inFile)) return(NULL)
    read.csv(inFile$datapath)
  })
  
  output$addnum <- renderText({
    paste(as.numeric(input$num1)+as.numeric(input$num2))
  })
  output$textdata<-renderDataTable(
    data()
  )
}

shinyApp(ui, server)