library(shiny)
library(shinythemes)
library(DT)
library(devtools)
library(tm)
library(wordcloud)
library(dplyr)
library(RColorBrewer)
library(tidytext)
library(ggplot2)
library(tidyr)
library(stringr)
library(wordcloud2)



js <- HTML(
  "ST=function getSelectedText() {

    var text = '';
    if (typeof window.getSelection != 'undefined') {
        text = window.getSelection().toString();
    } else if (typeof document.selection != 'undefined' && document.selection.type == 'Text') {
        text = document.selection.createRange().text;
    }

}"
)
ui <- fluidPage(
  tags$head(tags$script(js)),
  tags$script(
    'let store = [];
    let store2=[];
               $(document).on("keypress", function (e) {

       if(e.which=="99"){

        text = window.getSelection().toString();
store.push(text);
Shiny.setInputValue("store", store);

    alert(store.length);
       }
       else if(e.which=="98"){
     
text = window.getSelection().toString();
store2.push(text);
console.log(store2);
Shiny.setInputValue("bcode", store2);
       }
    });'
  ),
 # tags$a(hreg = "#", onclick = "ST()", 'Click me'),
  theme = shinytheme('yeti'),
  navbarPage(
    "My Page",
    tabPanel(
      "Navbar",
      sidebarPanel(
        h3('Enter numbers'),
        textInput("num1", "Enter num1:", "23"),
        textInput("num2", "Enter num2:", "34"),
        
        fileInput(
          inputId = "filedata",
          label = "Upload data. Choose CSV file",
          accept = c(".csv")
        )
      ),
      mainPanel(
        h1('Hello world'),
        p('Data Summary'),
        verbatimTextOutput("summary_stat"),
        verbatimTextOutput("test_print"),
        h3("Datatypes available"),
        actionButton("convert", "Convert character to factor"),
        
        tableOutput("datatype.table"),
        actionButton("text_data", "Show only text"),
        h3("Display contents of CSV file"),
        dataTableOutput("textdata")
        
      )
    ),
    tabPanel('Categories',
             h3('Sample Text'),
             p('The Andromeda Galaxy (IPA: /ænˈdrɒmɪdə/), also known as Messier 31, M31, or NGC 224 and originally the Andromeda Nebula
             (see below), is a barred spiral galaxy approximately 2.5 million light-years (770 kiloparsecs) from Earth and the nearest large galaxy to the Milky Way.[6] The galaxy\'s
             name stems from the area of
             Earth\'s sky in which it appears, the constellation of Andromeda, which itself is named after the Ethiopian (or Phoenician) princess who was the wife of
             Perseus in Greek mythology.'),

           h3('Text Parsed'),
           h4("C Codes"),
               verbatimTextOutput("store"),
           h4("B Codes"),
           verbatimTextOutput("bcode")),
    tabPanel('Frequency', 
             plotOutput('wordc'),
             h3('Word Cloud'),
             wordcloud2Output("cloud"))
  )
)
jscode <- "
shinyjs.init = function() {
  $(document).keypress(function(e) { alert('Key pressed: ' + e.which); });
}"

server <- function(input, output, session) {
  counter<-list()
  data <- reactive({
    inFile <- input$filedata
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath)
    df.textfile <- as.data.frame(read.csv(inFile$datapath))
  })
  
  output$wordc <- renderPlot({
  #  data.cloud <- read.csv('APPLE_iPhone_SE.csv', header=TRUE)
    data.cloud<-data()
    data.cloud <- data.cloud %>%
      mutate(line = row_number())
    text.data <- tibble(text=data.cloud$Reviews)
    text.data$text=str_replace_all(text.data$text, "[^[:alnum:]]", " ")
    text.data$text=iconv(text.data$text, from = 'UTF-8', to = 'ASCII//TRANSLIT')
    text.data%>%
      unnest_tokens(word, text)%>%
      anti_join(stop_words) %>%
      count(word, sort=TRUE) %>%
      filter(n>450)%>%
      ggplot(aes(n, word))+
      geom_col()
    
  })
  
  output$cloud<-renderWordcloud2({
    data.cloud<-data()
    data.cloud <- data.cloud %>%
      mutate(line = row_number())
    text.data <- tibble(text=data.cloud$Reviews)
    text.data$text=str_replace_all(text.data$text, "[^[:alnum:]]", " ")
    text.data$text=iconv(text.data$text, from = 'UTF-8', to = 'ASCII//TRANSLIT')
    x<-text.data%>%
      unnest_tokens(word, text)%>%
      anti_join(stop_words) %>%
      
      count(word, sort=TRUE) %>%
      filter(n>350)
    
    wordcloud2(x)
  })
  output$store<-renderText({
   paste(input$store, collapse = '\n')
  })
  
  output$bcode<-renderText({
    paste(input$bcode, collapse = '\n')
  })
  output$summary_stat <- renderPrint({
    # paste(as.numeric(input$num1)+as.numeric(input$num2))
    paste(str(data()))
    #paste(data.type.file)
  })
  output$datatype.table <- renderTable({
    data.type.file <- lapply(data(), 'class')
    input$convert
    if (input$convert == 0) {
      data.type.file <- lapply(data(), 'class')
      return(data.type.file)
    }
    else{
      data.core = data()
      data.core[sapply(data.core, is.character)] <-
        lapply(data.core[sapply(data.core, is.character)], as.factor)
      data.type.file <- lapply(data.core, 'class')
      data.type.file
    }
  })
  output$textdata <- renderDataTable(if (input$text_data != 0) {
    data.core.val = data()
    #combine function with the factor application
    return(data.core.val[sapply(data.core.val, is.character)])
  }
  else{
    test.data = data()
    test.data[sapply(test.data, is.character)]
  })
  
  output$test_print <- renderPrint({
    test.data = data()
    lapply(test.data[sapply(test.data, is.character)], summary)
  })
  
}

shinyApp(ui, server)