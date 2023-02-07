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
         "textstem" # an alternative method for lemmatizing
), pkgTest)

gu_api_key() # run this interactive function with 
# key: b77890c3-d233-4e93-9476-03011271a477


# We want to query the API on articles featuring Ukraine since Jan 1 2023
dat <- gu_content(query = "romance novel", from_date = "2022-01-01") # making a tibble

# We'll save this data
saveRDS(dat, "Research Project/API1")

API1 <- dat
which(duplicated(API1$web_title) == TRUE) # sometimes there are duplicates...
API1 <- API1[!duplicated(API1$web_title),] # which we can remove

# see if you can subset the object to focus on the articles we want
API1 <- API1[API1$section_id == "books",]

# make a corpus
corpus_rom <- corpus(API1, 
                     docid_field = "web_title", 
                     text_field = "body_text") # select the correct column here

corpus_rom <- stri_replace_first(corpus_rom,
                                 replacement = "",
                                 regex = "^.+?\"")

corpus_rom <- stri_replace_last(corpus_rom, 
                                replacement = "",
                                regex = "\u2022.+$")

# Let's take a look at the first article and see if we can spot any big problems
as.character(corpus_rom)[1]

## 2. Tokenize the text
# The next step is to turn all the words into tokens. 
# The tokens() function can also remove punctuation and symbols for us.
toks <- quanteda::tokens(corpus_rom, 
                         remove_punct = TRUE, 
                         remove_symbols = TRUE)
toks <- tokens_tolower(toks) # lowercase tokens
stop_list <- stopwords("english")
toks <- tokens_remove(toks, stop_list)
toks <- tokens_remove(toks, c("order", "copy", "support", "guardian", "guardianbookshop.com", "charge",
                              "may", "delivery ", "observer", "story", "debut", "novel"))
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


################
### Corpus 2 ###
################
gu_api_key() # run this interactive function with 
# key: b77890c3-d233-4e93-9476-03011271a477


# We want to query the API on articles featuring Ukraine since Jan 1 2023
dat <- gu_content(query = "romance", from_date = "2022-01-01") # making a tibble

# We'll save this data
saveRDS(dat, "Research Project/API2")
Rom <- dat
# see if you can subset the object to focus on the articles we want
Rom <- Rom[Rom$section_id == "books",]

which(duplicated(Rom$web_title) == TRUE) # sometimes there are duplicates...
Rom <- Rom[!duplicated(Rom$web_title),] # which we can remove

# make a corpus
corpus_rom <- corpus(Rom, 
                     docid_field = "web_title", 
                     text_field = "body_text") # select the correct column here

corpus_rom <- stri_replace_first(corpus_rom,
                                 replacement = "",
                                 regex = "^.+?\"")

corpus_rom <- stri_replace_last(corpus_rom, 
                                replacement = "",
                                regex = "\u2022.+$")
## 2. Tokenize the text
# The next step is to turn all the words into tokens. 
# The tokens() function can also remove punctuation and symbols for us.
toks <- quanteda::tokens(corpus_rom, 
                         remove_punct = TRUE, 
                         remove_symbols = TRUE)
toks <- tokens_tolower(toks) # lowercase tokens
stop_list <- stopwords("english")
toks <- tokens_remove(toks, stop_list)
toks <- tokens_remove(toks, c("illustrator", "author", "order", "copy", "support", 
                              "guardian", "guardianbookshop.com", "charge", "apply",
                              "may", "delivery ", "observer", "story", "debut", 
                              "novel", "bestseller", "list", "winner", "booker",
                              "book", "write", "say", "one", "new", "read"))


# i. Convert quanteda tokens object to list of tokens
toks_list <- as.list(toks) 

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
dfm_rom <- dfm(comp_tok)
topfeatures(dfm_rom)

saveRDS(dfm_rom, "Research Project/dfm_rom")


