library(shiny)
source("www/functions.R")


# Define UI for miles per gallon app ----
ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Simon's Spelling Bee"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    numericInput("letterN", "How Many Letters?", min = 5, max = 10,
                value = 7),
    checkboxGroupInput("excludeLetters", "Exclude These Letters",
                  choices = c("S", "Q", "Z", "X"),
                  selected = c("S")),
    actionButton("chooseLetters", "Generate Random Letters"),
    br(),
    textOutput("middleLetter"),
    textOutput("outerLetters"),
    br(),
    
    textOutput("wordCountTotal"),
    textOutput("foundWordCount")
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    
  #  tags$head(tags$script(HTML(jscode))),
    
    
    
    uiOutput("guessUI"),
    # tagAppendAttributes(
    #   textInput("guess", NULL, ""),
    #   `data-proxy-click` = "guessGo"
    # ),
    #actionButton("guessGo", "Guess"),
    br(),
    textOutput("responseText"),
    plotOutput("letterPlot"),
    br(),
    "Found Words:",
    textOutput("foundWords")
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output, session) {
  words <- reactive({
    import <- read.csv("words_alpha.txt", header = F)
    wordList <- import$V1
    print(length(wordList))
    wordList <- wordList[nchar(wordList) > 3]
    wordList <- toupper(wordList)
    print(length(wordList))
    return(wordList)})
  
  letterOptions <- reactive(setdiff(LETTERS, input$excludeLetters))
  
  chosenLetters <- eventReactive(input$chooseLetters, {
    sample(letterOptions(), input$letterN, replace = F)
  })
  
  allLetters <- reactive(chosenLetters())
  middleLetter <- reactive(allLetters()[1])
  outerLetters <- reactive(allLetters()[2:input$letterN])
  
  output$middleLetter <- renderText(middleLetter())
  output$outerLetters <- renderText(outerLetters())
  
  guess <- eventReactive(input$guessGo,{
    input$guess
  })
  
  output$guessUI <- renderUI({
    fluidPage(
      tags$head(tags$script(HTML(jscode))),
      tagAppendAttributes(
        textInput("guess", NULL, ""),
        `data-proxy-click` = "guessGo"
      ),
      actionButton("guessGo", "Guess")
    )
  })
  
  responseText <- eventReactive(input$guessGo, {
    
    print(input$guess)
    if (toupper(input$guess) %in% possibleWords()) {
      returnText <- "FOUND!"
      reactives$foundWords <- append(reactives$foundWords, toupper(input$guess))
    } else {
      returnText <- "Not in word list"
    }
    updateTextInput(session, "guess", value = "")
    return(returnText)
  })
  
  output$guessedWord <- renderText(toupper(guess()))
  
  output$test <- renderText(words()[2])
  
  possibleWords <- reactive(getAllWords(middleLetter(),
                                        outerLetters(),
                                        words()))
  output$wordCountTotal <- renderText(paste0("Total Possible Words: ", length(possibleWords())))
  output$letterPlot <- renderPlot(createWordPlot(middleLetter(),
                                                 outerLetters(),
                                                 remainingWords()))
  reactives <- reactiveValues(
    foundWords = c()
  )
  
  remainingWords <- reactive(setdiff(possibleWords(), reactives$foundWords))
  output$foundWords <- renderText(reactives$foundWords)
  output$responseText <- renderText(responseText())
  output$foundWordCount <- renderText(paste0(length(reactives$foundWords), " found words"))
}

shinyApp(ui, server)