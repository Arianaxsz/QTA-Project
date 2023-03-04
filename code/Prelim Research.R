pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

lapply(c("tidyverse",
         "guardianapi", # for working with the Guardian's API
         "quanteda", # for QTA
         "quanteda.textstats", # more Quanteda!
         "quanteda.textplots", # even more Quanteda!
         "readtext", # for reading in text data
         "stringi", # for working with character strings
         "textstem", # an alternative method for lemmatizing
         "lubridate" 
), pkgTest)

gu_api_key() # run this interactive function with 
# key: b77890c3-d233-4e93-9476-03011271a477
# Ariana's key: f13c0fec-41c5-44c1-b12b-7c50132c4daa



# We want to query the API on articles querying romance novels for 2012
dat12 <- gu_content(query = "romance", from_date = "2012-01-01", to_date ="2013-01-01") # making a tibble

# We'll save this data
saveRDS(dat12, "~/Documents/GitHub/QTA-Project/code/API1")

# Or read in the data 
API1 <- readRDS("~/Documents/GitHub/QTA-Project/codeAPI1")
rom12 <- dat12
# see if you can  the object to focus on the articles we want
rom12 <- rom12[rom12$section_id == "books",]
Review <- str_extract(rom12$headline, "review")
Z <- cbind(rom12, Review)
rom12 <- Z[is.na(Z$Review) == 0, ]

#tiddying the data
tidy12 <- rom12 %>%
  select(web_title,
         byline,
         date = web_publication_date, # Rename date variable
         section_name,
         standfirst,
         body_text
  ) %>%
  mutate(date = as_datetime(date)) # parse date

which(duplicated(tidy12$web_title)) # sometimes there are duplicates...
tidy12 <- tidy12[-which(duplicated(tidy12$web_title)),]


tidy12$body_text <- str_replace(tidy12$body_text, "\u2022.+$", "") #skipping this for now. 
# make a corpus
corpus_rom <- corpus(tidy12, 
                     docid_field = "web_title", 
                     text_field = "body_text") # select the correct column here

# Summary of the corpus
as.character(corpus_rom)[1]
corpSum12 <- summary(corpus_rom, 
                     n = nrow(docvars(corpus_rom)) 
)
names(corpSum12)
head(corpSum12[,-8])
# Corpus Statistics
corpSum12 %>% 
  ggplot(aes(date)) + 
  geom_histogram()

#Tokens by date 
daily_tokens <- corpSum12 %>%
  group_by(date) %>%
  summarize(token_sum = sum(Tokens))

ggplot(data=daily_tokens, aes(x=date, y=token_sum, group = 1)) + geom_point() + geom_smooth() + theme_minimal()

## 2. Tokenize the text
# The next step is to turn all the words into tokens. 
# The tokens() function can also remove punctuation and symbols for us.
toks <- quanteda::tokens(corpus_rom, include_docvars = TRUE,
                         remove_punct = TRUE, 
                         remove_symbols = TRUE,)
toks <- tokens_tolower(toks) # lowercase tokens
stop_list <- stopwords("english")
toks <- tokens_remove(toks, stop_list)
toks <- tokens_remove(toks, c("order", "copy", "support", "guardian", "guardianbookshop.com", "charge",
                              "may", "delivery ", "observer", "story", "debut", "novel", "one", "go",
                               "write", "book", "come", "make", "year", "first", "man", "time", ""))
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
dfm_rom12 <- dfm(comp_tok)
topfeatures(dfm_rom12)
View(dfm_rom12)

saveRDS(dfm_rom, "~/Documents/GitHub/QTA-Project/code/dfm_rom12")

# Visualizing and checking for any inferences 
# Checking top features 

dfm_freq12 <- textstat_frequency(dfm_rom12, n = 20)
dfm_freq12

# sort features by frequency in descending order
dfm_freq12$feature <- with(dfm_freq12, reorder(feature, -frequency))
# plot feature frequencies
dfm_freq12 %>%
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = "feature")


################
### Corpus 2 ### Romance only - 2022-01-01 
################
gu_api_key() # run this interactive function with 
# key: b77890c3-d233-4e93-9476-03011271a477


# We want to query the API on articles featuring Ukraine since Jan 1 2023
dat22 <- gu_content(query = "romance", from_date = "2022-01-01") # making a tibble

# We'll save this data
saveRDS(dat22, "~/Documents/GitHub/QTA-Project/code/API2")

Rom22 <- dat22
# see if you can  the object to focus on the articles we want
Rom22 <- Rom22[Rom22$section_id == "books",]
Review2 <- str_extract(Rom22$headline, "review")
X <- cbind(Rom22, Review2)
Rom22 <- X[is.na(X$Review2) == 0, ]

#Parsing the date 
# First, we'll tidy up our initial dataframes.
Rom22 <- Rom22 %>%
  select(web_title,
         byline,
         date = web_publication_date, # Rename date variable
         section_name,
         standfirst,
         body_text
  ) %>%
  mutate(date = as_datetime(date)) # parse date

which(duplicated(Rom$web_title) == TRUE) # sometimes there are duplicates...
Rom22 <- Rom22[!duplicated(Rom22$web_title),] # which we can remove

#Tidy removing str
Rom22$body_text <- str_replace(Rom22$body_text, "\u2022.+$", "")
# make a corpus
corpus_rom22 <- corpus(Rom22, 
                     docid_field = "web_title", 
                     text_field = "body_text") # select the correct column here


# Summary of the corpus
as.character(corpus_rom22)[1]
corpSum22 <- summary(corpus_rom22, 
                     n = nrow(docvars(corpus_rom22)) 
)

head(corpSum22[,-8])
## 2. Tokenize the text
# The next step is to turn all the words into tokens. 
# The tokens() function can also remove punctuation and symbols for us.
toks22 <- quanteda::tokens(corpus_rom22, 
                         remove_punct = TRUE, 
                         remove_symbols = TRUE)
toks22 <- tokens_tolower(toks) # lowercase tokens
stop_list <- stopwords("english")
toks22 <- tokens_remove(toks, stop_list)
toks22 <- tokens_remove(toks, c("illustrator", "author", "order", "copy", "support", 
                              "guardian", "guardianbookshop.com", "charge", "apply",
                              "may", "delivery ", "observer", "story", "debut", 
                              "novel", "bestseller", "list", "winner", "booker",
                              "book", "write", "say", "one", "new", "read", ""))

toks22
# i. Convert quanteda tokens object to list of tokens
toks_list <- as.list(toks22) 

# ii. Apply the lemmatize_words function from textstem to the list of tokens
lemma_toks <- lapply(toks_list, lemmatize_words) 

# iii. Convert the list of lemmatized tokens back to a quanteda tokens object
lemma_toks <- as.tokens(lemma_toks) 
lemma_toks <- tokens_remove(lemma_toks, c("say", "write"))

# i. Identify collocations
collocations <- textstat_collocations(lemma_toks, size = 2)

# ii. Choose which to keep
keep_coll_list <- collocations$collocation[1:25]
keep_coll_list

# iii. Apply to tokens object
comp_tok <- tokens_compound(lemma_toks, keep_coll_list)

# Convert to dfm...
dfm_rom22 <- dfm(comp_tok)
topfeatures(dfm_rom22)
View(dfm_rom22)

#Saving 
saveRDS(dfm_rom, "~/Documents/GitHub/QTA-Project/code/dfm_rom22")

# Visualizing and checking for any inferences 
# Checking top features 

dfm_freq22 <- textstat_frequency(dfm_rom, n = 20)
dfm_freq22

# sort features by frequency in descending order
dfm_freq22$feature <- with(dfm_freq22, reorder(feature, -frequency))
# plot feature frequencies

dfm_freq22 %>%
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = "feature")


#################################################
# Visualizations on both dates 2012 and 2022 
#################################################

#Comparing both dates 

ggplot(data = NULL) +
  geom_density(aes(yday(corpSum12$date)), color = "red") +
  geom_density(aes(yday(corpSum22$date)), color = "blue") 

# We can calculate additional statistics using the summary object. 
# For example, the TTR is the ratio of types to tokens:
corpSum12$ttr <- corpSum12$Types / corpSum12$Tokens
corpSum22$ttr <- corpSum22$Types / corpSum22$Tokens

#plotting 
ggplot(data = NULL) +
  geom_point(aes(yday(corpSum12$date), corpSum12$ttr), col = "red") +
  geom_point(aes(yday(corpSum22$date), corpSum22$ttr), col = "blue") +
  geom_smooth(aes(yday(corpSum12$date), corpSum12$ttr), col = "red") +
  geom_smooth(aes(yday(corpSum22$date), corpSum22$ttr), col = "blue")

# keyness 
dfm_rom <- rbind(dfm_rom12, dfm_rom22)
head(dfm_rom)
class(dfm_rom)

# This allows us to analyse and compare keyness across years - Not able to run this, not sure why! 

set.seed(2023)

#Lubridate the date 

dfm_by_date <- dfm_group(dfm_rom, fill = TRUE) #we are grouping the data with ludridate 
dfm_by_date <- dfm_group(dfm_rom, fill = TRUE, groups = year(dfm_rom$date))

keyness <- textstat_keyness(dfm_rom)
textplot_keyness(keyness, labelsize = 2)
#
topbus <- ggplot(dfm_freq, aes(x = feature, y = frequency)) + ggtitle("Top tokens for frequency") + 
  geom_point() + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
                                                                 
# Sentiment analysis 

# Finally, let's see if sentiment analysis can detect a change in tone 
# over time. To do this, we need to use a dictionary object. We only
# want positive and negative sentiments, so we'll just use the first two 
# elements.
dfm_sentiment <- dfm_lookup(dfm_rom, data_dictionary_LSD2015[1:2])

dfm_sentiment12 <- dfm_lookup(dfm_rom12, data_dictionary_LSD2015[1:2])
sentiment12 <- head(dfm_sentiment12)
sentiment12
# Once we have the frequency for positive and negative sentiment, we can 
# use a useful feature of R - vectorisation - to calculate net sentiment 
# across each day and plot it.
prop_negative12 <- as.numeric(dfm_sentiment12[,1] / ntoken(dfm_sentiment12))
prop_positive12 <- as.numeric(dfm_sentiment12[,2] / ntoken(dfm_sentiment12))
net_sentiment12 <- docvars(dfm_sentiment12, "prop_positive") - docvars(dfm_sentiment12,"prop_negative")

docvars(dfm_sentiment$date)
docvars(dfm_rom12, "date") <- lubridate::yms_hms(dfm_rom12@docvars$date)

subset_positive_12 <- dfm_subset(dfm_sentiment12, docvars(dfm_sentiment12,
                                                               "prop_positive"), drop_docid = FALSE)

summary(prop_negative12)
summary(prop_positive12)

class(prop_negative)

subset_positive_12
#plot 
plot <- ggplot(docvars(dfm_rom12),
               aes(x = created_at,
                   y = dfm_sentiment)) +
  geom_smooth() + 
  theme_minimal()

# This plot isn't working. 
docvars(dfm_sentiment) %>%
  ggplot(aes(x = yday(date), y = dfm_sentiment, group = year(date))) +
  geom_smooth(aes(colour = as.character(year(date)))) +
  labs(title = "Sentiment over time", 
       x = "day of year", y = "net sentiment", 
       colour = "year")
