### Helper devised to ease string operations

stop_words_english <- data_frame(word = tm::stopwords("english"), lexicon = "custom")
stop_words_spanish <- data_frame(word = tm::stopwords("spanish"), lexicon = "custom")

str_to_num <- function(str) {
  as.numeric(as.character(str))  
}

str_clean <- function(str) {
  str <- gsub("[^\x20-\x7E]", "", as.character(str))
  str <- tolower(str)
}
str_clean <- function(str) {
  str <- gsub("[^\x20-\x7E]", "", as.character(str))
  str <- tolower(str)
  str <- gsub("[,/*-_]", " ", as.character(str))
}

str_to_unigram <- function(str) {
  data_frame(str) %>% 
    unnest_tokens(word, str) %>% 
    filter(!word %in% stop_words_spanish$word) %>%
    filter(!word %in% stop_words_english$word)
}

str_to_bigram <- function(str) {
  data_frame(str) %>% 
    unnest_tokens(bigram, str, token = "ngrams", n = 2) %>% 
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words_spanish$word) %>%
    filter(!word2 %in% stop_words_spanish$word) %>%
    filter(!word1 %in% stop_words_english$word) %>%
    filter(!word2 %in% stop_words_english$word) %>%
    summarize(text = str_c(word1, word2, sep=" "))
}

str_get_unigrams <- function(str) {
  str_c(list_wrap(str_to_unigram(str)$word), collapse = " ")
}

str_get_uni_bigrams <- function(str) {
  
  str <- as.character(str)
  
  unigrams <- list_wrap(str_to_unigram(str)$word)
  bigrams <- list_wrap(str_to_bigram(str)$text)
  
  c(unigrams, bigrams)
}

str_word_frequence <- function(list, cut = 0) {
  words <- data.frame(table(str_get_uni_bigrams(as.character(list))))
  words <- plyr::rename(words, c(
    "Var1"="tag", 
    "Freq"="count"
  ))
  words <- words[order(-words$count),]
  if (cut == 0) {
    return(words)
  } else {
    return(words[1:cut,])
  }
}

