library(shiny)
library(rtweet)
library(dplyr)
library(glue)
library(reactable)
library(purrr)
library(tm)
library(SnowballC)
library(textclean)
library(tidytext)
library(ggmap)
library(reshape2)
library(syuzhet)
library(wordcloud)
library(shinycssloaders)
library(shinythemes)
library(remote)
library(devtools)
#devtools::install_github("hadley/emo")
library(emo)
library(tidyr)
library(textreadr)
library(rvest)
#devtools::install_github("wilkelab/ggtext")
library(ggtext)
library(forcats)
library(prismatic)
library(httr)

# createTokenNoBrowser<- function(appName, consumerKey, consumerSecret,
#                                 accessToken, accessTokenSecret) {
#   app <- httr::oauth_app(appName, consumerKey, consumerSecret)
#   params <- list(as_header = TRUE)
#   credentials <- list(oauth_token = accessToken,
#                       oauth_token_secret = accessTokenSecret)
#   token <- httr::Token1.0$new(endpoint = NULL, params = params,
#                               app = app, credentials = credentials)
#   return(token)
# }
# 
# twitter_token <- createTokenNoBrowser("ciberseminarresearch","wKQp1U9E8YXsQgaLrfpK14M53",
#                               "GrUwj5Ee4Pu8tfCCjIfLeL1Fp0cwgy7RVJZRJYmmMmC3WDRU9L",
#                               "2608928160-TSyYRwmfMjA1hG9gaA4tnSGaaHo863hi6LcwdVd",
#                               "tk8kxIGhah9YZ2rUdEhgugDvHIrzEfgTAZOTzNCj8rNt3")

# twitter_token <- create_token(app = "ciberseminarresearch",
#                               consumer_key = "wKQp1U9E8YXsQgaLrfpK14M53",
#                               consumer_secret = "GrUwj5Ee4Pu8tfCCjIfLeL1Fp0cwgy7RVJZRJYmmMmC3WDRU9L",
#                               set_renv = FALSE)

consumer_key <- "wKQp1U9E8YXsQgaLrfpK14M53"
consumer_secret <- "GrUwj5Ee4Pu8tfCCjIfLeL1Fp0cwgy7RVJZRJYmmMmC3WDRU9L"
access_token <- "2608928160-x5bfesekIYgVJKcmiT1txR94ttBHSSlVkLCIEyK"
access_secret <- "OKJTtjWz6lWXbVLlUbde8iRIFgwpJ04SxOYjYm792uaFj"

options(httr_oauth_cache = TRUE)

twitter_token <- create_token(
  app = "ciberseminarresearch",
  consumer_key,
  consumer_secret,
  access_token,
  access_secret, set_renv = FALSE)

#Set Google API Key
ggmap::register_google(key = "AIzaSyDX-h8y1k2MHCsZBjSx-UtPJiixwuvAMCk")

#### function to make URL clickable ####
make_url_html <- function(url) {
    if(length(url) < 2) {
        if(!is.na(url)) {
            as.character(glue("<a title = {url} target = '_new' href = '{url}'>{url}</a>") )
        } else {
            ""
        }
    } else {
        paste0(purrr::map_chr(url, ~ paste0("<a title = '", .x, "' target = '_new' href = '", .x, "'>", .x, "</a>", collapse = ", ")), collapse = ", ")
    }
}

emoji_to_link <- function(x) {
  paste0("https://emojipedia.org/emoji/",x) %>%
    read_html() %>%
    html_nodes("tr td a") %>%
    .[1] %>%
    html_attr("href") %>%
    paste0("https://emojipedia.org/", .) %>%
    read_html() %>%
    html_node('div[class="vendor-image"] img') %>%
    html_attr("src")
}

link_to_img <- function(x, size = 25) {
  paste0("<img src='", x, "' width='", size, "'/>")
}

mean_emoji_color <- function(x) {
  data <- png::readPNG(RCurl::getURLContent(x))
  color_freq <- names(sort(table(rgb(data[,,1], data[,,2], data[,,3])), 
                           decreasing = TRUE))
  setdiff(color_freq, c("#FFFFFF", "#000000"))[1]
}

CIBERTweets <- read.csv("./CIBERSeminarTweets.csv")
CIBERTweets <- CIBERTweets[grep("chickens",CIBERTweets$text),]
CIBERTweets$created_at <- as.POSIXct(CIBERTweets$created_at,format='%Y-%m-%d %H:%M:%OS')
locations_df <- read.csv("./locations_df.csv")
plot_data <- read.csv("./plot_data.csv")

ui <- navbarPage("CIBER Seminar Tweet Search", theme = shinytheme("cerulean"),
                 tabPanel("7-Day Search", icon = icon("twitter"),
    
    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            textInput("hashtag_to_search",
                      "Any hashtag or term to search:",
                      value = "#chickens"),
            actionButton("get_data", "Get data", class = "btn-primary"),
            br(),br(),
            downloadButton("download_data", "Download data"),
            br(),
            withSpinner(plotOutput("wordcloud"), type = 5),
            plotOutput("wordcloud2"), 
            br(),
            actionButton("see_emojis", "Emojis", class = "btn-primary", icon = icon("grin-beam")),
            br(),
            withSpinner(plotOutput("emojis"), type = 5)
        ),
        
        # Show results
        mainPanel(
          tags$h5("Use the tool on this tab to search hashtag or term. Results will display up to 2000 of the most-recent 
                  Tweets within the past week."),
          tags$h6("App made my Madison Sankovitz for the 2020 UCR CIBER Seminar."),
          br(),
        plotOutput("tweet_plot"), plotOutput("ts_sentiment"),
                               plotOutput("uniqueWords"), plotOutput("sentimentPlot"), 
                               plotOutput("map"), reactableOutput("tweet_table"))
        )
    ),
    tabPanel("30-Day Search", icon = icon("twitter"),
             # Sidebar
             sidebarLayout(
               sidebarPanel(
                 selectInput("hashtag_to_search30",
                           "Chicken adjective term to search:",
                           c("alien" = "alien",
                             "damaging" = "damaging",
                             "detrimental" = "detrimental",
                             "destructive" = "destructive",
                             "feral" = "feral",
                             "harmful" = "harmful",
                             "invasive" = "invasive",
                             "pest" = "pest",
                             "backyard" = "backyard",
                             "commercial" = "commercial",
                             "domesticated" = "domesticated",
                             "foreign" = "foreign",
                             "introduced" = "introduced",
                             "managed" = "managed",
                             "naturalized" = "naturalized",
                             "nonnative" = "nonnative",
                             "nonmanaged" = "nonmanaged",
                             "peridomestic" = "peridomestic",
                             "reared" = "reared",
                             "beneficial" = "beneficial",
                             "endemic" = "endemic",
                             "exotic" = "exotic",
                             "freerange" = "freerange",
                             "indigenous" = "indigenous",
                             "local" = "local",
                             "native" = "native",
                             "wild" = "wild"
                             )),
                 actionButton("get_data30", "Get data", class = "btn-primary"),
                 br(),br(),
                 downloadButton("download_data30", "Download data"),
                 br(),
                 withSpinner(plotOutput("wordcloud30"), type = 5),
                 plotOutput("wordcloud230"), 
                 br(),
                 actionButton("see_emojis30", "Emojis", class = "btn-primary", icon = icon("grin-beam")),
                 br(),
                 withSpinner(plotOutput("emojis30"), type = 5)
               ),

               # Show results
               mainPanel(
                 tags$h5("Use the dropdown menu to select an adjective associated with chickens. Results will display up 
to 600 Tweets containing the adjective and term 'chickens' between April 9 and May 9, 2020, grouped in six bins of 100 Tweets spread evenly 
                         across the month."),
                 tags$h6("App made my Madison Sankovitz for the 2020 UCR CIBER Seminar."),
                 br(),
                 plotOutput("tweet_plot30"), plotOutput("ts_sentiment30"),
                 plotOutput("uniqueWords30"), plotOutput("sentimentPlot30"),
                 plotOutput("map30"), reactableOutput("tweet_table30")
             )
)))

# Define server logic 
server <- function(input, output) {
    
    search_tweetsResult <- eventReactive(input$get_data, {
      search_tweets(input$hashtag_to_search, n = 2000, type = "mixed", 
                    include_rts = F, token = twitter_token)
      
    })
    
    search_tweetsResult30 <- eventReactive(input$get_data30, {
      CIBERTweets[grep(input$hashtag_to_search30,CIBERTweets$text),]

    })

    search_tweetsResult30map <- eventReactive(input$get_data30, {
      locations_df[grep(input$hashtag_to_search30,locations_df$text),]

    })
    
    emoji_results <- eventReactive(input$see_emojis, {
      req(search_tweetsResult())
      tweets <- search_tweetsResult()
      tweet_emojis <- tweets %>%
        mutate(emoji = emo::ji_extract_all(text)) %>%
        unnest(cols = c(emoji)) %>%
        count(emoji, sort = TRUE)
      top_tweets <- tweet_emojis %>%
        slice(1:10) %>%
        inner_join(plot_data)
    })
    
    emoji_results30 <- eventReactive(input$see_emojis30, {
      req(search_tweetsResult30())
      tweets <- search_tweetsResult30()
      tweet_emojis <- tweets %>%
        mutate(emoji = emo::ji_extract_all(text)) %>%
        unnest(cols = c(emoji)) %>%
        count(emoji, sort = TRUE)
      top_tweets <- tweet_emojis %>%
        slice(1:10) %>%
        inner_join(plot_data)
    })
    
    tweet_table_data <- reactive({
        req(search_tweetsResult())
        tweet_df <- search_tweetsResult()
        tweet_df %>%
            dplyr::select(user_id, status_id, created_at, screen_name, text, favorite_count, retweet_count, urls_expanded_url) %>%
            mutate(
                Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>"),
                URLs = purrr::map_chr(urls_expanded_url, make_url_html)
            )%>%
            dplyr::select(DateTime = created_at, User = screen_name, Tweet, Likes = favorite_count, RTs = retweet_count, URLs)
    })
    
    tweet_table_data30 <- reactive({
      req(search_tweetsResult30())
      tweet_df <- search_tweetsResult30()
      tweet_df %>%
        dplyr::select(user_id, status_id, created_at, screen_name, text, favorite_count, retweet_count, urls_expanded_url) %>%
        mutate(
          Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>"),
          URLs = purrr::map_chr(urls_expanded_url, make_url_html)
        )%>%
        dplyr::select(DateTime = created_at, User = screen_name, Tweet, Likes = favorite_count, RTs = retweet_count, URLs)
    })
    
output$wordcloud <- renderPlot({
  req(search_tweetsResult())
  tweets <- search_tweetsResult()
  # Data Cleaning
  # Delete Links in the Tweets
  tweets$text <- gsub("http.*", "", tweets$text)
  tweets$text <- gsub("https.*", "", tweets$text)
  tweets$text <- gsub("&", "&", tweets$text)
  # Remove punctuation, convert to lowercase, seperate all words
  tweets_clean <- tweets %>%
    dplyr::select(text) %>%
    unnest_tokens(word, text)
  # Load list of stop words - from the tidytext package
  data("stop_words")
  # Remove stop words from your list of words
  cleaned_tweet_words <- tweets_clean %>%
    anti_join(stop_words) %>%
    subset(word!="amp")
# Plot the top 15 words
  set.seed(1234)
  for_wordcloud <- cleaned_tweet_words %>%
    count(word, sort = TRUE) %>%
    top_n(100) %>%
    slice( -1:-2) %>%
    mutate(word = reorder(word, n))%>%
    filter_at(.vars= vars(word), all_vars(!grepl('0',.))) %>%
    filter_at(.vars= vars(word), all_vars(!grepl('1',.))) %>%
    filter_at(.vars= vars(word), all_vars(!grepl('2',.))) %>%
    filter_at(.vars= vars(word), all_vars(!grepl('3',.))) %>%
    filter_at(.vars= vars(word), all_vars(!grepl('4',.))) %>%
    filter_at(.vars= vars(word), all_vars(!grepl('5',.))) %>%
    filter_at(.vars= vars(word), all_vars(!grepl('6',.))) %>%
    filter_at(.vars= vars(word), all_vars(!grepl('7',.))) %>%
    filter_at(.vars= vars(word), all_vars(!grepl('8',.))) %>%
    filter_at(.vars= vars(word), all_vars(!grepl('9',.)))
    wordcloud(words = for_wordcloud$word, freq = for_wordcloud$n, min.freq = 1,
            max.words=100, random.order=FALSE, rot.per=0.35,
            colors=brewer.pal(8, "Dark2"), scale=c(3.5,0.3))
})

output$wordcloud30 <- renderPlot({
  req(search_tweetsResult30())
  tweets <- search_tweetsResult30()
  # Data Cleaning
  # Delete Links in the Tweets
  tweets$text <- gsub("http.*", "", tweets$text)
  tweets$text <- gsub("https.*", "", tweets$text)
  tweets$text <- gsub("&", "&", tweets$text)
  # Remove punctuation, convert to lowercase, seperate all words
  tweets_clean <- tweets %>%
    dplyr::select(text) %>%
    unnest_tokens(word, text)
  # Load list of stop words - from the tidytext package
  data("stop_words")
  # Remove stop words from your list of words
  cleaned_tweet_words <- tweets_clean %>%
    anti_join(stop_words) %>%
    subset(word!="amp")
  # Plot the top 15 words
  set.seed(1234)
  for_wordcloud <- cleaned_tweet_words %>%
    count(word, sort = TRUE) %>%
    top_n(100) %>%
    slice( -1:-2) %>%
    mutate(word = reorder(word, n))%>%
    filter_at(.vars= vars(word), all_vars(!grepl('0',.))) %>%
    filter_at(.vars= vars(word), all_vars(!grepl('1',.))) %>%
    filter_at(.vars= vars(word), all_vars(!grepl('2',.))) %>%
    filter_at(.vars= vars(word), all_vars(!grepl('3',.))) %>%
    filter_at(.vars= vars(word), all_vars(!grepl('4',.))) %>%
    filter_at(.vars= vars(word), all_vars(!grepl('5',.))) %>%
    filter_at(.vars= vars(word), all_vars(!grepl('6',.))) %>%
    filter_at(.vars= vars(word), all_vars(!grepl('7',.))) %>%
    filter_at(.vars= vars(word), all_vars(!grepl('8',.))) %>%
    filter_at(.vars= vars(word), all_vars(!grepl('9',.)))
  wordcloud(words = for_wordcloud$word, freq = for_wordcloud$n, min.freq = 1,
            max.words=100, random.order=FALSE, rot.per=0.35,
            colors=brewer.pal(8, "Dark2"), scale=c(3.5,0.3))
})

output$wordcloud2 <- renderPlot({
      req(search_tweetsResult())
      tweets <- search_tweetsResult()
      # Data Cleaning
      # Delete Links in the Tweets
      tweets$text <- gsub("http.*", "", tweets$text)
      tweets$text <- gsub("https.*", "", tweets$text)
      tweets$text <- gsub("&", "&", tweets$text)
      # Remove punctuation, convert to lowercase, seperate all words
      tweets_clean <- tweets %>%
        dplyr::select(text) %>%
        unnest_tokens(word, text)
      # Load list of stop words - from the tidytext package
      data("stop_words")
      # Remove stop words from your list of words
      cleaned_tweet_words <- tweets_clean %>%
        anti_join(stop_words) %>%
        subset(word!="amp")
      cleaned_tweet_words <- cleaned_tweet_words %>%
        inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE)
      # recode(x, a = "Apple", b = "Bear", c = "Car")
      cleaned_tweet_words$sentiment <- recode(cleaned_tweet_words$sentiment,positive = "1positive", negative = "2negative")
      acast(cleaned_tweet_words, word ~ sentiment, value.var = "n", fill = 0) %>%
        comparison.cloud(colors = c("#18B3B7", "#F35E5A"),
                         max.words = 100, random.order = F, rot.per = 0.35, scale=c(3.5,0.3))
    })

output$wordcloud230 <- renderPlot({
  req(search_tweetsResult30())
  tweets <- search_tweetsResult30()
  # Data Cleaning
  # Delete Links in the Tweets
  tweets$text <- gsub("http.*", "", tweets$text)
  tweets$text <- gsub("https.*", "", tweets$text)
  tweets$text <- gsub("&", "&", tweets$text)
  # Remove punctuation, convert to lowercase, seperate all words
  tweets_clean <- tweets %>%
    dplyr::select(text) %>%
    unnest_tokens(word, text)
  # Load list of stop words - from the tidytext package
  data("stop_words")
  # Remove stop words from your list of words
  cleaned_tweet_words <- tweets_clean %>%
    anti_join(stop_words) %>%
    subset(word!="amp")
  cleaned_tweet_words <- cleaned_tweet_words %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE)
  # recode(x, a = "Apple", b = "Bear", c = "Car")
  cleaned_tweet_words$sentiment <- recode(cleaned_tweet_words$sentiment,positive = "1positive", negative = "2negative")
  acast(cleaned_tweet_words, word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("#18B3B7", "#F35E5A"),
                     max.words = 100, random.order = F, rot.per = 0.35, scale=c(3.5,0.3))
})

output$emojis <- renderPlot({
  req(emoji_results())
  top_tweets <- emoji_results()
  top_tweets %>%
    ggplot(aes(fct_reorder(label, n, .desc = TRUE), 
               color = color, 
               fill = unclass(prismatic::clr_lighten(color, 0.4)), n)) +
    geom_col() +
    scale_fill_identity() +
    scale_color_identity() +
    theme_minimal() +
    theme(axis.text.x = element_markdown()) +
    labs(x = NULL, y = "Count",
         title = "Emojis used in tweets")
})

output$emojis30 <- renderPlot({
  req(emoji_results30())
  top_tweets <- emoji_results30()
  top_tweets %>%
    ggplot(aes(fct_reorder(label, n, .desc = TRUE), 
               color = color, 
               fill = unclass(prismatic::clr_lighten(color, 0.4)), n)) +
    geom_col() +
    scale_fill_identity() +
    scale_color_identity() +
    theme_minimal() +
    theme(axis.text.x = element_markdown()) +
    labs(x = NULL, y = "Count",
         title = "Emojis used in tweets")
})
    
output$tweet_plot <- renderPlot({
  req(search_tweetsResult())
  tweets <- search_tweetsResult()
  # Data plotting
  tweets %>%
    ts_plot("10 hours") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
    ggplot2::labs(
      title = "Frequency of tweets", y = "Count",
      x = "Time")
    
})

output$tweet_plot30 <- renderPlot({
  req(search_tweetsResult30())
  tweets <- search_tweetsResult30()
  # Data plotting
  tweets %>%
    ts_plot("10 hours") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
    ggplot2::labs(
      title = "Frequency of tweets", y = "Count",
      x = "Time")

})

output$ts_sentiment <- renderPlot({
  req(search_tweetsResult())
  tweets <- search_tweetsResult()
  # Data Cleaning
  # Delete Links in the Tweets
  tweets$text <- gsub("http.*", "", tweets$text)
  tweets$text <- gsub("https.*", "", tweets$text)
  tweets$text <- gsub("&", "&", tweets$text)
  tweets <- tweets %>%
    mutate(sentiment  = get_sentiment(text, "bing"))
  
  tweets_for_plotting <- tweets %>%
    group_by(interval = ceiling(seq_along(created_at)/5)) %>%
    summarize(sentiment_mean = mean(sentiment), time_mean = mean(created_at))
    
  ggplot(tweets_for_plotting, aes(x = time_mean, y = sentiment_mean)) +
    geom_smooth() + theme_bw() + theme(plot.title = element_text(face = "bold")) + labs(
      title = "Sentiment of tweets", y = "Sentiment",
      x = "Time")
  
})

output$ts_sentiment30 <- renderPlot({
  req(search_tweetsResult30())
  tweets <- search_tweetsResult30()
  # Data Cleaning
  # Delete Links in the Tweets
  tweets$text <- gsub("http.*", "", tweets$text)
  tweets$text <- gsub("https.*", "", tweets$text)
  tweets$text <- gsub("&", "&", tweets$text)
  tweets <- tweets %>%
    mutate(sentiment  = get_sentiment(text, "bing"))

  tweets_for_plotting <- tweets %>%
    group_by(interval = ceiling(seq_along(created_at)/5)) %>%
    summarize(sentiment_mean = mean(sentiment), time_mean = mean(created_at))

  ggplot(tweets_for_plotting, aes(x = time_mean, y = sentiment_mean)) +
    geom_smooth() + theme_bw() + theme(plot.title = element_text(face = "bold")) + labs(
      title = "Sentiment of tweets", y = "Sentiment",
      x = "Time")

})

output$uniqueWords <- renderPlot({
  req(search_tweetsResult())
  tweets <- search_tweetsResult()
  # Data Cleaning
  # Delete Links in the Tweets
  tweets$text <- gsub("http.*", "", tweets$text)
  tweets$text <- gsub("https.*", "", tweets$text)
  tweets$text <- gsub("&", "&", tweets$text)
  # Remove punctuation, convert to lowercase, seperate all words
  tweets_clean <- tweets %>%
    dplyr::select(text) %>%
    unnest_tokens(word, text)
  # Load list of stop words - from the tidytext package
  data("stop_words")
  # Remove stop words from your list of words
  cleaned_tweet_words <- tweets_clean %>%
    anti_join(stop_words)
  # Plot the top 15 words
  cleaned_tweet_words %>%
    count(word, sort = TRUE) %>%
    top_n(15) %>%
    slice( -1) %>%
    mutate(word = reorder(word, n)) %>%
    filter_all(any_vars(is.numeric(.))) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    labs(y = "Count",
         x = "Unique words",
         title = "Count of unique words found in tweets",
         subtitle = "Stop words removed from the list")
  ## Selecting by n
})

output$uniqueWords30 <- renderPlot({
  req(search_tweetsResult30())
  tweets <- search_tweetsResult30()
  # Data Cleaning
  # Delete Links in the Tweets
  tweets$text <- gsub("http.*", "", tweets$text)
  tweets$text <- gsub("https.*", "", tweets$text)
  tweets$text <- gsub("&", "&", tweets$text)
  # Remove punctuation, convert to lowercase, seperate all words
  tweets_clean <- tweets %>%
    dplyr::select(text) %>%
    unnest_tokens(word, text)
  # Load list of stop words - from the tidytext package
  data("stop_words")
  # Remove stop words from your list of words
  cleaned_tweet_words <- tweets_clean %>%
    anti_join(stop_words)
  # Plot the top 15 words
  cleaned_tweet_words %>%
    count(word, sort = TRUE) %>%
    top_n(15) %>%
    slice( -1) %>%
    mutate(word = reorder(word, n)) %>%
    filter_all(any_vars(is.numeric(.))) %>%
    ggplot(aes(x = word, y = n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip() +
    labs(y = "Count",
         x = "Unique words",
         title = "Count of unique words found in tweets",
         subtitle = "Stop words removed from the list")
  ## Selecting by n
})

output$sentimentPlot <- renderPlot({
  req(search_tweetsResult())
  tweets <- search_tweetsResult()
  # Data Cleaning
  # Delete Links in the Tweets
  tweets$text <- gsub("http.*", "", tweets$text)
  tweets$text <- gsub("https.*", "", tweets$text)
  tweets$text <- gsub("&", "&", tweets$text)
  # Remove punctuation, convert to lowercase, seperate all words
  tweets_clean <- tweets %>%
    dplyr::select(text) %>%
    unnest_tokens(word, text)
  # Load list of stop words - from the tidytext package
  data("stop_words")
  # Remove stop words from your list of words
  cleaned_tweet_words <- tweets_clean %>%
    anti_join(stop_words)
  # join sentiment classification to the tweet words
  bing_word_counts <- cleaned_tweet_words %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()
  bing_word_counts %>%
    group_by(sentiment) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(title = "Sentiment",
         y = "Contribution to sentiment",
         x = NULL) +
    coord_flip()
})

output$sentimentPlot30 <- renderPlot({
  req(search_tweetsResult30())
  tweets <- search_tweetsResult30()
  # Data Cleaning
  # Delete Links in the Tweets
  tweets$text <- gsub("http.*", "", tweets$text)
  tweets$text <- gsub("https.*", "", tweets$text)
  tweets$text <- gsub("&", "&", tweets$text)
  # Remove punctuation, convert to lowercase, seperate all words
  tweets_clean <- tweets %>%
    dplyr::select(text) %>%
    unnest_tokens(word, text)
  # Load list of stop words - from the tidytext package
  data("stop_words")
  # Remove stop words from your list of words
  cleaned_tweet_words <- tweets_clean %>%
    anti_join(stop_words)
  # join sentiment classification to the tweet words
  bing_word_counts <- cleaned_tweet_words %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()
  bing_word_counts %>%
    group_by(sentiment) %>%
    top_n(10) %>%
    ungroup() %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill = sentiment)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~sentiment, scales = "free_y") +
    labs(title = "Sentiment",
         y = "Contribution to sentiment",
         x = NULL) +
    coord_flip()
})
    
output$map <- renderPlot({
  req(search_tweetsResult())
  dataset <- search_tweetsResult()
  dataset <- lat_lng(dataset)
    ## plot country boundaries
  par(mar = c(0, 0, 0, 0))
  maps::map("world", lwd = .25)
  ## plot lat and lng points onto world map
  with(dataset, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))
})

output$map30 <- renderPlot({
  req(search_tweetsResult30map())
  dataset <- search_tweetsResult30map()
#  dataset <- lat_lng(dataset)
  ## plot country boundaries
  par(mar = c(0, 0, 0, 0))
  maps::map("world", lwd = .25)
  ## plot lat and lng points onto world map
  with(dataset, points(lon, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))
})

output$tweet_table <- renderReactable({
        reactable::reactable(tweet_table_data(), 
                             filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                             showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200), 
                             columns = list(
                                 DateTime = colDef(defaultSortOrder = "asc"),
                                 User = colDef(defaultSortOrder = "asc"),
                                 Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                                 Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
                                 RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE)),
                                 URLs = colDef(html = TRUE)
                             )
        )
    })

output$tweet_table30 <- renderReactable({
  reactable::reactable(tweet_table_data30(),
                       filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
                       showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200),
                       columns = list(
                         DateTime = colDef(defaultSortOrder = "asc"),
                         User = colDef(defaultSortOrder = "asc"),
                         Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
                         Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
                         RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE)),
                         URLs = colDef(html = TRUE)
                       )
  )
})

output$download_data <- downloadHandler(
    filename = function() {
        paste(input$hashtag_to_search, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
        write.csv(tweet_table_data(), file, row.names = FALSE)
    }
)

output$download_data30 <- downloadHandler(
  filename = function() {
    paste(input$hashtag_to_search30, "_", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(tweet_table_data30(), file, row.names = FALSE)
  }
)
}


# Run the application 
shinyApp(ui = ui, server = server)
