########################################################
#  Webscraping and Text Analysis with R: Part 2
########################################################
#Installing packages 
install.packages("textcat")
install.packages("readtext")
install.packages("quanteda")
install.packages("tidytext")
install.packages("sentimentr")
install.packages("textdata")
install.packages("lexicon")
install.packages("SentimentAnalysis")
install.packages("tm")
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textstats)
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(tm)
library(magrittr)
library(textcat)
library(readtext) 
library(tidytext)
library(RTextTools)
library(lubridate)
library(textstem)
library(textstat_collocations)
library(tidyverse)
library(textdata)
library(sentimentr)
library(lexicon)
library(SentimentAnalysis)
library(tm)


# Getting the data 

data1 <- read_csv("data/GR_FiftyShadesofGrey.csv", col_types = cols(book = col_skip(), reviewer = col_skip(), review = col_skip()), trim_ws = FALSE)
data <- data.table(data)

text.df <- tibble(text = str_to_lower(data$rating))
resul

#Read the csv file with readtext
kcna_csv <- readtext("GR_FiftyShadesofGrey.csv", 
                     encoding = "utf-8",
                     text_field = "rating",
                     ignore_missing_files = TRUE)
#Create the corpus
corpus_kcna <- corpus(kcna_csv)
# Pre Processing - Tokenise 
## 2. Tokenize the text
# The next step is to turn all the words into tokens. 
# The tokens() function can also remove punctuation and symbols for us.
toks <- quanteda::tokens(corpus_kcna, include_docvars = TRUE,
                         remove_punct = TRUE, 
                         remove_symbols = TRUE,)
toks <- tokens_tolower(toks) # lowercase tokens
stop_list <- stopwords("english")
toks <- tokens_remove(toks, stop_list)
toks <- tokens_remove(toks, c("i", "s", "t", "like", "just", "want", "christian", 
                              "ana", "read", "just", "one", "book", "know", "say", "get", 
                              "can", "even", "more", "likes", "commentsLike Commen"))
print(toks[2])
# i. Convert quanteda tokens object to list of tokens
toks_list <- as.list(toks) 
# ii. Apply the lemmatize_words function from textstem to the list of tokens
lemma_toks <- lapply(toks_list, lemmatize_words) 
# iii. Convert the list of lemmatized tokens back to a quanteda tokens object
lemma_toks <- as.tokens(lemma_toks) 
# i. Identify collocations
collocations <- textstat_collocations(lemma_toks, size = 2)
# ii. Choose which to keep
keep_coll_list <- collocations$collocation[1:25]
keep_coll_list
# iii. Apply to tokens object
comp_tok <- tokens_compound(lemma_toks, keep_coll_list)

# Convert to dfm...
kcna_dfm <- dfm(comp_tok)
topfeatures(kcna_dfm)


saveRDS(kcna_dfm, "~/Documents/GitHub/QTA-Project/Good_reads/kcna_dfm_grey")

#Create the dfm
kcna_dfm <- comp_tok %>%
  quanteda::tokens(remove_numbers = TRUE,
                   remove_punct = TRUE,
                   remove_symbols = TRUE,
                   remove_separators = TRUE,
                   remove_url = TRUE,
                   include_docvars = TRUE) %>%
  dfm()


topfeatures(kcna_dfm)
head(kcna_dfm, 5)


# Display the top 5 most frequent words
head(dtm_d, 5)
# sample of 10 negative words
negative_grey <- sample(data_dictionary_LSD2015$negative, 10) # sample of 10 negative words

positive_grey <- sample(data_dictionary_LSD2015$positive, 10) # sample of 10 positive words

# Looking up the features 

kcna_sent_dfm <- dfm_lookup(kcna_dfm, 
                            dictionary = data_dictionary_LSD2015[1:2])

# Sentiment Analysis 

#Let’s normalize the sentiment scores by total words and then calculate the difference between positive and negative proportion of words.

docvars(kcna_dfm, "prop_negative") <- as.numeric(kcna_sent_dfm[,1] / ntoken(kcna_dfm)) # add proportion of negative words to original dfm
docvars(kcna_dfm, "prop_positive") <- as.numeric(kcna_sent_dfm[,2] / ntoken(kcna_dfm))
docvars(kcna_dfm, "net_sentiment") <- docvars(kcna_dfm, "prop_positive") - docvars(kcna_dfm, "prop_negative") 

kcna_sent_plot <- ggplot(docvars(kcna_dfm),
                         aes(x = prop_negative,
                             y = net_sentiment)) +
  geom_smooth() + 
  theme_minimal() 
kcna_sent_plot
# poisitve 
kcna_sent_plot_p <- ggplot(docvars(kcna_dfm),
                         aes(x = prop_positive,
                             y = net_sentiment)) +
  geom_smooth() + 
  theme_minimal() 
kcna_sent_plot_p

# together
scaleFactor <- max(kcna_dfm$prop_positive) / max(kcna_dfm$prop_negative)

ggplot(docvars(kcna_dfm), 
       aes(x=net_sentiment)) +
  geom_smooth(aes(y=prop_positive), method="loess", col="blue") +
  geom_smooth(aes(y= prop_negative), method="loess", col="red") +
  scale_y_continuous(name="prop_positive", sec.axis=sec_axis(~./scaleFactor, name="prop_negative")) +
  theme(
    axis.title.y.left=element_text(color="blue"),
    axis.text.y.left=element_text(color="blue"),
    axis.title.y.right=element_text(color="red"),
    axis.text.y.right=element_text(color="red")
  )
# calculate net sentiment [tbd]

# Viewing and adding sentiment in the text not worked as expected. 
View(sentiments)

# Most positive news article 
kcna_csv$net_sentiment <- as.numeric(kcna_dfm@docvars$net_sentiment)
most_postive <- kcna_csv[which.max(kcna_csv$net_sentiment),]
most_postive$text

# Most negative news article 

most_negative <- kcna_csv[which.min(kcna_csv$net_sentiment),]
most_negative$text

# Visualization 
dtm = read_csv("GR_FiftyShadesofGrey.csv") %>%
  corpus(text_field = "rating") %>% 
  dfm()


# A different set of code to try on getting better results with the text 
# create dfm 

GI_dict = dictionary(DictionaryGI)

result = kcna_dfm %>% dfm_lookup(GI_dict) %>% 
  convert(to = "data.frame") %>% 
  as_tibble()
result = result %>% mutate(length = ntoken(kcna_dfm))
result

result = result %>% mutate(sentiment1 = (positive - negative) / (positive + negative)) 
result = result %>% mutate(sentiment2 = (positive - negative) / length) 
result = result %>% mutate(sentiment2 = (positive + negative) / length) 

result = na.omit(result)
result

# Frequency 
freqs = textstat_frequency(kcna_dfm, n = 30)
freqs %>% as_tibble() %>% filter(feature %in% GI_dict$positive)
freqs
class(freqs)

ggplot(freqs, n = 3, aes(x = feature, y = frequency)) + ggtitle("Feature frequency of Grey") +
  geom_point() + theme_minimal() 


top <- ggplot(freqs, aes(x = feature, y = frequency)) +
  ggtitle("Top tokens for Grey") + geom_point() 
+ theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

top                                                                                                                                                  


# Frequent words overview 
head(kwic(corpus, "think", window = 4))
head(kwic(corpus, "love", window = 4))
head(kwic(corpus, "fucking", window = 4))
head(kwic(corpus, "fiction", window = 4))

# Trying something else 

# "tidying" up the data (1 word per row) and adding the sentiment scores for each word
# Loading the first sentiment score lexicon

sentiment(freqs$feature)
  
freqs$sentiment <- sentiment(freqs$feature)$sentiment 

freqs
mean_sentiment <-  mean(freqs$sentiment)

# Grouping by mean for observation 
review_mean_sentiment <- freqs %>%
  group_by(frequency, docfreq) %>%
  summarize(mean_sentiment = mean(sentiment, na.rm = TRUE))


# Getting

wordcloud(words = freqs$feature, freq = freqs$frequency, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))
# Find associations 
findAssocs(corpus, terms = c("think","will","write"), corlimit = 0.25)	
