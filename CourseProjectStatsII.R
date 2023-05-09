library(tm)
library(textstem)
library(lda)
library(topicmodels)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidytext)
library(ldatuning)
library(reshape)
library(wordcloud)

load("reviewDataMaster.RData")
set.seed(1)
# I need to reduce the data as it is too large computationally.
reviewData <- sample_n(reviewDataMaster, 10000)
testData <- sample_n(reviewDataMaster, 2000)


# creating corpus of Amazon Reviews
review.corpus <- Corpus(VectorSource(reviewData$Text))
test.corpus <- Corpus(VectorSource(testData$Text))

# Attaching Metadata
meta(review.corpus, tag="Review.Rating") <- reviewData$Score
meta(review.corpus, tag="Review.Title") <- reviewData$Summary
meta(review.corpus, tag="Helpfulness") <- reviewData$HelpfulnessNumerator

# Pre-processing
# transform to lower case
review.corpus <- tm_map(review.corpus, content_transformer(tolower))
test.corpus <- tm_map(test.corpus, content_transformer(tolower))
# remove numbers
review.corpus <- tm_map(review.corpus, removeNumbers) 
test.corpus <- tm_map(test.corpus, removeNumbers)  
# remove punctuation
review.corpus <- tm_map(review.corpus, removePunctuation)  
test.corpus <- tm_map(test.corpus, removePunctuation)  
# lemmatize - sort words by grouping or variant forms of the same word
test.corpus <- tm_map(test.corpus, lemmatize_strings) 
test.corpus <- tm_map(test.corpus, lemmatize_strings) 
test.corpus <- tm_map(test.corpus, removeWords, 
                        unique(c("the", "and", "'", "just", "like", "try",
                                 "find", "every", "easy", "like", "one", "take",
                                 "see", "far", "try", "even", "take", "order", "ship",
                                 "eat",
                                 stopwords("english")))) # remove stopwords
test.corpus <- tm_map(test.corpus, removeWords, 
                        unique(c("the", "and", "'", "just", "like", "try",
                                 "find", "every", "easy", "like", "one", "take",
                                 "see", "far", "try", "even", "take", "order", "ship",
                                 "eat",
                                 stopwords("english"))))
# remove whitespace
review.corpus <- tm_map(review.corpus, stripWhitespace)
test.corpus <- tm_map(test.corpus, stripWhitespace)

review.dtm <- DocumentTermMatrix(review.corpus)
test.dtm <- DocumentTermMatrix(test.corpus)



doc.lengths <- rowSums(as.matrix(review.dtm))
review.dtm <- DocumentTermMatrix(review.corpus[doc.lengths > 0])
doc.lengths <- rowSums(as.matrix(test.dtm))
test.dtm <- DocumentTermMatrix(test.corpus[doc.lengths > 0])




perplexity.df <- data.frame()
K <- c(3:20)
for (i in K){
  
  model <- LDA(review.dtm, k = i, method = "VEM",
                control = list(seed=1) )
  perplexity.score.train <- perplexity(model, newdata = review.dtm) 
  perplexity.score.test <- perplexity(model, newdata = test.dtm)
  temp <- data.frame(k=i, perplexity.score.train, perplexity.score.test)
  print(i)
  perplexity.df <- rbind(perplexity.df, temp)
}

result1 <- FindTopicsNumber(
  review.dtm,
  topics = seq(from = 3, to = 20, by = 1),
  metrics = c("CaoJuan2009", "Deveaud2014"),
  method = "VEM",
  control = list(seed = 1),
  mc.cores = NA,
  verbose = TRUE )

names(perplexity.df)[1] <- "topics"
perplexity.df.melted <- melt(perplexity.df, id="topics")
perplexity.plot <- ggplot(perplexity.df.melted, 
                          aes(x=topics, y=value, colour=variable)) +
                          geom_line()
print(perplexity.plot)




ldaOut10 <- LDA(review.dtm, k=10, control=list(seed=1))
lda.terms10 <- lda::top.topic.words(posterior(ldaOut10)$terms,10, by.score=T)

colnames(lda.terms10) <- c("junkfood", "baking", "petfood", "coffee", "dogcare", 
                           "sweets", "packages", "tea", "ingredients", "retail")

ldaOut16 <- LDA(review.dtm, k=16, control=list(seed=1))
lda.terms16 <- lda::top.topic.words(posterior(ldaOut16)$terms,5, by.score=T)
colnames(lda.terms16) <- c("junkfood", "petfood", "?", "dogcare", "packages", 
                           "??", "tea", "???", "sweets", "retail", "coffee", "????",
                           "breakfast", "baking", "?????", "condiments")


ldaOut10.topic1.words = posterior(ldaOut10)$terms[1,]
ldaOut10.topic1.topwords = head(sort(ldaOut10.topic1.words, 
                                     decreasing=T), n=20)
ldaOut10.topic1.wordcloud <- wordcloud(words=names(ldaOut10.topic1.topwords),
                                       ldaOut10.topic1.topwords)
ldaOut10.topic2.words = posterior(ldaOut10)$terms[2,]
ldaOut10.topic2.topwords = head(sort(ldaOut10.topic2.words, 
                                     decreasing=T), n=20)
ldaOut10.topic2.wordcloud <- wordcloud(words=names(ldaOut10.topic2.topwords),
                                       ldaOut10.topic2.topwords)
ldaOut10.topic3.words = posterior(ldaOut10)$terms[3,]
ldaOut10.topic3.topwords = head(sort(ldaOut10.topic3.words, 
                                     decreasing=T), n=20)
ldaOut10.topic3.wordcloud <- wordcloud(words=names(ldaOut10.topic3.topwords),
                                       ldaOut10.topic3.topwords)
ldaOut10.topic4.words = posterior(ldaOut10)$terms[4,]
ldaOut10.topic4.topwords = head(sort(ldaOut10.topic4.words, 
                                     decreasing=T), n=20)
ldaOut10.topic4.wordcloud <- wordcloud(words=names(ldaOut10.topic4.topwords),
                                       ldaOut10.topic4.topwords)
ldaOut10.topic5.words = posterior(ldaOut10)$terms[5,]
ldaOut10.topic5.topwords = head(sort(ldaOut10.topic5.words, 
                                     decreasing=T), n=20)
ldaOut10.topic5.wordcloud <- wordcloud(words=names(ldaOut10.topic5.topwords),
                                       ldaOut10.topic5.topwords)
ldaOut10.topic6.words = posterior(ldaOut10)$terms[6,]
ldaOut10.topic6.topwords = head(sort(ldaOut10.topic6.words, 
                                     decreasing=T), n=20)
ldaOut10.topic6.wordcloud <- wordcloud(words=names(ldaOut10.topic6.topwords),
                                       ldaOut10.topic6.topwords)
ldaOut10.topic7.words = posterior(ldaOut10)$terms[7,]
ldaOut10.topic7.topwords = head(sort(ldaOut10.topic7.words, 
                                     decreasing=T), n=20)
ldaOut10.topic7.wordcloud <- wordcloud(words=names(ldaOut10.topic7.topwords),
                                       ldaOut10.topic7.topwords)
ldaOut10.topic8.words = posterior(ldaOut10)$terms[8,]
ldaOut10.topic8.topwords = head(sort(ldaOut10.topic8.words, 
                                     decreasing=T), n=20)
ldaOut10.topic8.wordcloud <- wordcloud(words=names(ldaOut10.topic8.topwords),
                                       ldaOut10.topic8.topwords)
ldaOut10.topic9.words = posterior(ldaOut10)$terms[9,]
ldaOut10.topic9.topwords = head(sort(ldaOut10.topic9.words, 
                                     decreasing=T), n=20)
ldaOut10.topic9.wordcloud <- wordcloud(words=names(ldaOut10.topic9.topwords),
                                       ldaOut10.topic9.topwords)
ldaOut10.topic10.words = posterior(ldaOut10)$terms[10,]
ldaOut10.topic10.topwords = head(sort(ldaOut10.topic10.words, 
                                     decreasing=T), n=20)
ldaOut10.topic10.wordcloud <- wordcloud(words=names(ldaOut10.topic10.topwords),
                                       ldaOut10.topic10.topwords)


ldaOut10.rank <- colSums(posterior(ldaOut10)$topics) / nrow(review.dtm)
names(ldaOut10.rank) <- c("junkfood", "baking", "petfood", "coffee", "dogcare", 
                           "sweets", "packages", "tea", "ingredients", "retail")
ldaOut10.rank <- data.frame(rank = c(1:10), probability = sort(ldaOut10.rank, decreasing = T))

ldaOut10.topics <- as.matrix(topics(ldaOut10,3)) # select the top 3 topics
ldaOut10.rank$topic <- as.factor(rownames(ldaOut10.rank))


topicRankPlot <- ggplot(ldaOut10.rank, aes(x=reorder(topic, probability), 
                                           y=probability, fill=factor(probability))) +
  geom_bar(stat="identity", width=0.8) +
  geom_text(aes(label=round(probability,2)), size=3.5)+
  xlab("topic") +
  ylab("proportion") +
  coord_flip() +
  theme_classic() +
  theme(legend.position="none")




