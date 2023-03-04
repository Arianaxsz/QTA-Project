# Sentiment Analysis Fifty Shades of Grey 

data <- read_csv("GR_FiftyShadesofGrey.csv", col_types = cols(book = col_skip(), reviewer = col_skip(), review = col_skip()), trim_ws = FALSE)
data <- data.table(data)
str(data)
text.df <- tibble(text = str_to_lower(data$rating))

big_words_count <- text.df %>% unnest_tokens(output = word, input = text) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE)

# Select top 10 words 

top_ten <- big_words_count %>% 
  group_by(sentiment) %>% 
  slice_max(order_by = n, n =10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n))

top_ten

# barplot 
top_ten %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "contribution sentiment", x = NULL) + 
  coord_flip()
#######
# Trying with "loughran" 
lou_words_count <- text.df %>% unnest_tokens(output = word, input = text) %>% 
  inner_join(get_sentiments("loughran")) %>% 
  count(word, sentiment, sort = TRUE)

lou_top_ten <- lou_words_count %>% 
  group_by(sentiment) %>% 
  slice_max(order_by = n, n =10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n))

# barplot 
lou_top_ten %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") + 
  labs(y = "contribution sentiment", x = NULL) + 
  coord_flip()
