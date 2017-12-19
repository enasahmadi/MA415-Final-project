library(shiny)

ui <- fluidPage(
  headerPanel("MA415 Final Project: Twitter Analytics"), 
  sidebarPanel(
    textInput("searchTerm","Enter hashtag to be searched with '#'",  value="#"),
    sliderInput("maxTweets","Number of recent tweets to use for analysis", min=100, max=1000, value=100),
    submitButton(text="Analyze")),
  mainPanel(tabsetPanel(
    tabPanel("WordCloud", HTML("<div><h3>Most used words associated with the hashtag</h3></div>"),
             plotOutput("word")),
    tabPanel("Sentiment Analysis",HTML("<div><h3> Sentiment Analysis of the Tweets! </h3></div>"),
             plotOutput("SentAnalysis1"), plotOutput("SentAnalysis2"))
  )
  )
)
