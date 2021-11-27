library(shiny)
library(shinythemes)

ui<- fluidPage(theme= shinytheme('yeti'),
               navbarPage("My Page",
                  tabPanel("Navbar",
                  sidebarPanel(
                     h3('Enter numbers'),
                     textInput("num1", "Enter num1:",""),
                     textInput("num2", "Enter num2:","")
                    ),
                 mainPanel(
                   h1('Hello world'),
                   p('This is just another text!'),
                   verbatimTextOutput("addnum")
                 )
                ),
                tabPanel('Categories','Blank for now'),
                tabPanel('Frequency','Blank for now')
               )
)


server <- function(input, output, session) {
  output$addnum <- renderText({
    paste(as.numeric(input$num1)+as.numeric(input$num2))
  })
}

shinyApp(ui, server)