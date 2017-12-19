
# Installing package if not already installed (Stanton 2013)
EnsurePackage<-function(x)
{x <- as.character(x)
 if (!require(x,character.only=TRUE))
 {
   install.packages(pkgs=x,repos="http://cran.r-project.org")
   require(x,character.only=TRUE)
 }
}

#Identifying packages required  (Stanton 2013)
PrepareTwitter<-function()
{
  EnsurePackage("twitteR")
  EnsurePackage("stringr")
  EnsurePackage("ROAuth")
  EnsurePackage("RCurl")
  EnsurePackage("ggplot2")
  EnsurePackage("reshape")
  EnsurePackage("tm")
  EnsurePackage("RJSONIO")
  EnsurePackage("wordcloud")
  EnsurePackage("gridExtra")
  EnsurePackage("plyr")
  library(ggplot2)
  library(Rstem)
  library(sentiment)
}

PrepareTwitter()

#Function to Authenticate Access to Twitter
Authentication<-function() {
  # twitter auth vars
  CONSUMER_KEY="n0VQmi2OXrFQxoPvYvx2FIknI"
  CONSUMER_SECRET="hTkHfzJrIzQvpwAJLZAQYoyz3iryEEbX8gBlJloQedr51pNlRw"
  TOKEN="871919779426308099-Yn40D6rcq7zcC2qJowKB8aUvgSLpZVt"
  TOKEN_SECRET="aOBkoIl6wJXYcHMgstf9BuK3OapE5DNKMyr0sfycr3tQE"
  
  # twitter auth set-up
  setup_twitter_oauth(consumer_key=CONSUMER_KEY,
                      consumer_secret=CONSUMER_SECRET,
                      access_token=TOKEN,
                      access_secret=TOKEN_SECRET)
}

Authentication()

shinyServer(function(input, output) {

  # function to create a data frame from tweets
  TweetFrame<-function(twtList)
  {
      df<- do.call("rbind",lapply(twtList,as.data.frame))
      
      #removes emoticons
      df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
      df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df$text)
      df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", df$text)    # removes retweet
      df$text = gsub("@\\w+", "", df$text)      # removes @'s 
      df$text = gsub("http\\w+", "", df$text)   # removes links
      #df$text = gsub("<.*>", "", df$text)      # removes emojis
      return (df$text)
  }
  
  # function to clean the tweets more for sentimental analysis
  CleanTweetsForSentimentalAnalysis<-function(twts_txt){
    twts_txt = gsub("<.*>", "", twts_txt)      # removes emojis
    twts_txt = tolower(twts_txt)
    twts_txt = twts_txt[!is.na(twts_txt)]
    names(twts_txt) = NULL
    return (twts_txt)
  }
  
  # searches for tweets
	twtList<-reactive({
	  search_term <- paste0(paste0('@', input$searchTerm), ' OR ', paste0('to:', input$searchTerm), ' OR ', input$searchTerm)
	  twtList<-searchTwitter(search_term, n=input$maxTweets, lang="en")
	})
	tweets<-reactive({tweets<-TweetFrame(twtList())})

	#WORDCLOUD
	wordclouds<-function(text)
	{
	  # source: https://www.rdocumentation.org/packages/tm/versions/0.7-2/topics/tm_map
		library(tm)
	  
	  myCorpus<-Corpus(VectorSource(text))
		
		myCorpus <- tm_map(myCorpus, tolower)
		myCorpus <- tm_map(myCorpus, removePunctuation)
		myCorpus <- tm_map(myCorpus, removeNumbers)
		myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"))
		myCorpus <- tm_map(myCorpus, stripWhitespace)

		return (myCorpus)
	}
	
	text_word<-reactive({text_word<-wordclouds(tweets())})
	output$word<-renderPlot({
	  wordcloud(text_word(), random.order=F,max.words=Inf, rot.per=.15, colors=brewer.pal(8,"Dark2"), scale=c(4.0,1.0))
	  })
	
	#SENTIMENTAL ANALYSIS
	sentimentalTable<-reactive({
	  twts_txt = CleanTweetsForSentimentalAnalysis(tweets())
	  twts_class_emo = classify_emotion(twts_txt, algorithm="bayes", prior=1.0)
	  emotion = twts_class_emo[,7]
	  emotion[is.na(emotion)] = "unknown"
	  
	  twts_class_pol = classify_polarity(twts_txt, algorithm="bayes")
	  polarity = twts_class_pol[,4]
	  
	  sentiment_dataframe = data.frame(text=twts_txt, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)
	  sentiment_dataframe = within(sentiment_dataframe, emotion <- 
	                                 factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
	})
	
	plotSentAnalysis1<-function(sentiment_dataframe) {
	  print(ggplot(sentiment_dataframe, aes(x=polarity)) +
	          geom_bar(aes(y=..count.., fill=polarity)) +
	          scale_fill_brewer(palette="RdGy") +
	          theme(legend.position="right") + ylab("Number of Tweets") + xlab("Polarity Categories"))
	}
	
	plotSentAnalysis2<-function(sentiment_dataframe) {
	  print(ggplot(sentiment_dataframe, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) +
	          scale_fill_brewer(palette="Dark2") +
	          theme(legend.position="right") + ylab("Number of Tweets") + xlab("Emotion Categories"))
	}
	
	output$SentAnalysis1<-renderPlot({
	  sentiment_dataframe <- sentimentalTable()
	  plotSentAnalysis1(sentiment_dataframe)
	})
	
	output$SentAnalysis2<-renderPlot({
	  sentiment_dataframe <- sentimentalTable()
	  plotSentAnalysis2(sentiment_dataframe)
	})
	
})

