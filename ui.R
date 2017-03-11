#--------------------------------------------------
# R UI Code for the Capstone Project Shiny App
#--------------------------------------------------

suppressWarnings(library(shiny))

shinyUI(fluidPage(
  
  # Application title
  navbarPage("DS Capstone Word Prediction Navbar",
                         tabPanel("Home"),
                          tabPanel("Ngram description", 
                                   dataTableOutput("table")),
             navbarMenu("Method",
                        tabPanel("Description", p("This app uses a ngram backoff model to predict the next word in a sentence. It is based on a subset of data prepared in advance of blogs, news and tweets. The sampled data represents ngrams of several depth level (from 1 to 6) with their frequencies, as a basis for the cascading model below. The prediction model below simplistically ranks higher predictions coming from higher rank ngrams. The model was validated against unseen date with slight above average results. Further methods to improve the prediction include extending the sampling size (currently 20%), extend the source dataset, implement generative n-gram language model (Katz backoff with discounting) and revalidate the model against new datasets.")),
                        tabPanel("Code", p('will be released soon on github, stay tuned :-)')))
  ),
  
  # Sidebar layout
  sidebarLayout(
    
    sidebarPanel(
      textInput("sentence", "Continue the sentence here below", value = "this is a result of the"),
      sliderInput("obs", "maximum predictions:",
                  min = 0, max = 30, value = 10
      ),
      radioButtons("show_table", "Show ngram result table?",
                   c("show",
                     "hide"))
    ),
    
    mainPanel(
      h4("Sentence"),
      verbatimTextOutput("text"),

      h4("Prediction"),
      verbatimTextOutput("prediction"),
      
      mainPanel(plotOutput('dataset')),
      br(),
      br(),
      dataTableOutput("output_dataset")
    )
  )
))

