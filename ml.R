### Helper devised to ease machine learning algorithms

naive_bayes_train <- function(data_frame) {
  
  data_frame$related_occupation <- unlist(lapply(data_frame$related_occupation, esco_occupation_name))
  data_frame$related_occupation <- factor(data_frame$related_occupation)
  # data_frame$words <- data_frame$knowledge_tags
  data_frame$words <- unlist(mapply(paste, data_frame$knowledge_tags, data_frame$job_title))
  
  # TODO: Change hardcoding
  corpus <- VCorpus(VectorSource(data_frame$words))
  dtm <- DocumentTermMatrix(corpus)
  freq_words <- findFreqTerms(dtm, 15)
  dtm <- DocumentTermMatrix(corpus, list(dictionary=freq_words))

  sample_size <- floor(0.75 * nrow(dtm))
  random <- sample(nrow(dtm), sample_size, replace = FALSE)
  
  nb_train <- dtm[random, ]
  nb_test <- dtm[-random, ]
  
  nb_train_bool <- apply(nb_train, MARGIN = 2, convert_counts)
  nb_test_bool <- apply(nb_test, MARGIN = 2, convert_counts)
  
  nb_train_yesno_df <- as.data.frame(as.matrix(nb_train_bool))
  nb_train_df <- as.data.frame(lapply(nb_train_yesno_df, factor))
  nb_test_yesno_df <- as.data.frame(as.matrix(nb_test_bool))
  nb_test_df <- as.data.frame(lapply(nb_test_yesno_df, factor))
  
  nb_train_labels <- data_frame[random, ]$related_occupation
  nb_test_labels <- data_frame[-random,]$related_occupation
  
  nb_classifier <- naiveBayes(nb_train_df, nb_train_labels)
  nb_pred <- predict(nb_classifier, nb_test_df)
  
  CrossTable(nb_pred, nb_test_labels, prop.chisq = FALSE, chisq = FALSE, 
             prop.t = FALSE, max.width = 1,
             dnn = c("Predicted", "Actual"))
}

naive_bayes <- function(data_frame) {
  
  data_frame$related_occupation <- unlist(lapply(data_frame$related_occupation, as.character))
  data_frame$related_occupation <- factor(data_frame$related_occupation)
  
  data_frame$words <- unlist(mapply(paste, data_frame$knowledge_tags, data_frame$job_title))
  data_frame$words <- unlist(lapply(data_frame$words, str_get_unigrams))
  
  # TODO: Change hardcoding
  corpus <- VCorpus(VectorSource(data_frame$words))
  dtm <- DocumentTermMatrix(corpus)
  freq_words <- findFreqTerms(dtm, 50)
  dtm <- DocumentTermMatrix(corpus, list(dictionary=freq_words))
  
  nb_yesno <- apply(dtm, MARGIN = 2, convert_counts)
  nb_yesno_df <- as.data.frame(as.matrix(nb_yesno))
  nb_df <- as.data.frame(lapply(nb_yesno_df, factor))
  
  nb_train <- nb_df[data_frame$related_occupation != "", ]
  nb_test <- nb_df[data_frame$related_occupation == "", ]
  
  nb_train_labels <- data_frame[data_frame$related_occupation != "", ]$related_occupation
  
  nb_classifier <- naiveBayes(nb_train, nb_train_labels)
  nb_pred <- predict(nb_classifier, nb_test)
}

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}
