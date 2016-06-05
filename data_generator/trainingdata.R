############################################################################################################
## Sequence		: 02
## File			: trainingdata.R
## Author		: Ambika J
## Date			: May-2016
## Notes		: This file is to create training data; ready to use it in predict algorithm.
##					First, Load the cleaned data - blog, news and twit.
##					1. Split cleaned raw data into training data and combine all 3 (data, news and twitter)
##					2. Sentencise the training data
##					3. Clean-up training data.
##						1. replace contraction
##						2. strip white space
##						3. remove repeated words
##						4. remove punctuation (this should at the end; the last step)
##					4. Tokenize
##						1 gram
##						2 grams
##						3 grams
##						4 grams
##						5 grams
##						6 grams
##						10 grams
##					5. Frequency tables for these ngrams
##					6. Sort it based on highest frequency of these ngrams
##					Then, finally save the tokenized frequency sorted data frame and train sentence.
############################################################################################################

print("###################################################################################################")
print(paste0("Start: Training data @ ",Sys.time()))

if(!exists("blog3")) load(file="data/preprocessedData_bnt3.RData")
### blog3, news3, twit3

if(!exists("blog3")) source("preprocess.R")

library(quanteda)
library(qdap)
library(stringi)

options(mc.cores=4, java.parameters = "-Xmx4g")

removeRepeatedWords <- function(x) {
    indx <- grep("\\b(\\w+)\\s+\\1\\b",x,ignore.case=T) ## gsub with this pattern, does nt work.
    x[indx] <- unlist(lapply(x[indx], function(x1) 
        paste(rle(unlist(stri_extract_all_words(x1)))$values,collapse=" ")))
    return(x)
}

#################################
## training data split 
#################################

set.seed("372")
partial_size <- 0.1
train_blog_indx <- sample(1:length(blog3), (length(blog3)*partial_size))
train_news_indx <- sample(1:length(news3), (length(news3)*partial_size))
train_twit_indx <- sample(1:length(twit3), (length(twit3)*partial_size))

train_blog <- blog3[train_blog_indx]
train_news <- news3[train_news_indx]
train_twit <- twit3[train_twit_indx]

train_corpa <- c(train_blog, train_news, train_twit)

##### Adding quiz data to the sample ###
#train_corpa <- c(train_blog, train_news, train_twit, quiz2_data, quiz3_data)


#################################
## train sentence
#################################

if(!"train_sentence" %in% ls()) train_sentence <- tokenize(train_corpa, 
                                            what = "sentence", simplify = TRUE,
                                            removeTwitter = TRUE, verbose = TRUE)

if(!"train_sentence1" %in% ls()) {
    train_sentence1 <- replace_contraction(train_sentence) 
    ##only after replace contraction, remove punctuations.
    
    train_sentence1 <- rm_white(train_sentence1) ## before removing punctuation
}

if(!"train_sentence2" %in% ls()) {
    train_sentence2 <- removeRepeatedWords(train_sentence1)
    train_sentence2 <- gsub("[[:punct:]]+", " ", train_sentence2) ## always convert it to space
    train_sentence2 <- rm_white(train_sentence2) ## after removing punctuation
    
    ### delete a few records, which has meaning less lines
    #1. haha
    #> str <- "^(ha)+$"; length(grep(str, train_sentence2, value=T, ignore.case = T))
    #[1] 1130
    indx <- grep("^(ha)+$", train_sentence2, ignore.case = T)
    if(length(indx) >= 1) train_sentence2 <- train_sentence2[-indx]
    
    ### change this - chnge and bout and bouta
    train_sentence2 <- gsub("chnge","change",train_sentence2, ignore.case = T)
    train_sentence2 <- gsub(" bouta"," about a",train_sentence2, ignore.case = T)
    train_sentence2 <- gsub(" bout "," about ",train_sentence2, ignore.case = T)
    
    ###
    indx <- grep(" ", train_sentence2, ignore.case = T, invert = T)
    if(length(indx) >= 1) train_sentence2 <- train_sentence2[-indx]
}

rm("train_sentence", "train_sentence1")

#################################
## test sentence
#################################

## Test data
baln_blog <- blog3[-train_blog_indx]
baln_news <- news3[-train_news_indx]
baln_twit <- twit3[-train_twit_indx]

test_blog <- sample(baln_blog, 150)
test_news <- sample(baln_news, 150)
test_twit <- sample(baln_twit, 200)

test_corpa <- c(test_blog, test_news, test_twit)

if(!"test_sentence" %in% ls()) test_sentence <- tokenize(test_corpa, 
                                            what = "sentence", simplify = TRUE,
                                            removeTwitter = TRUE, verbose = TRUE)

if(!"test_sentence1" %in% ls()) {
    test_sentence1 <- replace_contraction(test_sentence) 
    ##only after replace contraction, remove punctuations.
    
    test_sentence1 <- rm_white(test_sentence1) ## before removing punctuation
}

if(!"test_sentence2" %in% ls()) {
    test_sentence2 <- removeRepeatedWords(test_sentence1)
    test_sentence2 <- gsub("[[:punct:]]+", " ", test_sentence2) ## always convert it to space
    test_sentence2 <- rm_white(test_sentence2) ## after removing punctuation
    
    ### delete a few records, which has meaning less lines
    #1. haha
    #> str <- "^(ha)+$"; length(grep(str, test_sentence2, value=T, ignore.case = T))
    #[1] 1130
    #test_sentence2 <- test_sentence2[-grep("^(ha)+$", test_sentence2, ignore.case = T)]
    ## not there for test data.
	
	### delete row with single words
	#test_sentence2 <- test_sentence2[-grep("^\\w+$", test_sentence2)]
    
    ### change this - chnge and bout and bouta
    test_sentence2 <- gsub("chnge","change",test_sentence2, ignore.case = T)
    test_sentence2 <- gsub(" bouta"," about a",test_sentence2, ignore.case = T)
    test_sentence2 <- gsub(" bout "," about ",test_sentence2, ignore.case = T)
    
    ###
    indx <- grep(" ", test_sentence2, ignore.case = T, invert = T)
    if(length(indx) >= 1) test_sentence2 <- test_sentence2[-indx]
}

if(!"test_sentence3" %in% ls()) {
    #chnge --> change
    #bout --> about
    #bouta --> about a
    #test_sentence3 <- test_sentence2
}

rm("blog3","news3","twit3","train_corpa")
rm("test_sentence", "test_sentence1", "test_corpa")
save("test_sentence2", file="data/test_sentence2.RData"); save.image()

#################################
## ngrams
#################################

train_1gram <- tokenize(train_sentence2, what = "word", 
                           removeTwitter = TRUE, simplify = TRUE,
                           ngrams = 1, verbose = TRUE)

train_2grams <- tokenize(train_sentence2, what = "word", 
                           removeTwitter = TRUE, simplify = TRUE,
                           ngrams = 2, verbose = TRUE)

train_3grams <- tokenize(train_sentence2, what = "word", 
                           removeTwitter = TRUE, simplify = TRUE,
                           ngrams = 3, verbose = TRUE)

train_4grams <- tokenize(train_sentence2, what = "word", 
                           removeTwitter = TRUE, simplify = TRUE,
                           ngrams = 4, verbose = TRUE)

train_5grams <- tokenize(train_sentence2, what = "word", 
                           removeTwitter = TRUE, simplify = TRUE,
                           ngrams = 5, verbose = TRUE)

train_6grams <- tokenize(train_sentence2, what = "word", 
                           removeTwitter = TRUE, simplify = TRUE,
                           ngrams = 6, verbose = TRUE)

train_10grams <- tokenize(train_sentence2, what = "word", 
                           removeTwitter = TRUE, simplify = TRUE,
                           ngrams = 10, verbose = TRUE)


#################################
## Weka - tokenization
#################################

#token_delim <- " \\t\\r\\n.!?,;\"()"
token_delim <- " \\t\\r\\n" ## as we have removed punctuation, punctuation wud not be delimiter.
### this can be used, if the length of train_sentence1 is around 3Lakhs.
# if(!"unigram" %in% ls())  unigram  <- NGramTokenizer(train_sentence1, Weka_control(min=1,max=1))
# if(!"bigram" %in% ls())   bigram   <- NGramTokenizer(train_sentence1, Weka_control(min=2,max=2, delimiters = token_delim))
# if(!"trigram" %in% ls())  trigram  <- NGramTokenizer(train_sentence1, Weka_control(min=3,max=3, delimiters = token_delim))
# if(!"fourgram" %in% ls()) fourgram <- NGramTokenizer(train_sentence1, Weka_control(min=4,max=4, delimiters = token_delim))


#################################
## frequencies
#################################

train_1gram_df   <- as.data.frame.table(table(train_1gram),   stringsAsFactors = FALSE)
train_2grams_df  <- as.data.frame.table(table(train_2grams),  stringsAsFactors = FALSE)
train_3grams_df  <- as.data.frame.table(table(train_3grams),  stringsAsFactors = FALSE)
train_4grams_df  <- as.data.frame.table(table(train_4grams),  stringsAsFactors = FALSE)
train_5grams_df  <- as.data.frame.table(table(train_5grams),  stringsAsFactors = FALSE)
train_6grams_df  <- as.data.frame.table(table(train_6grams),  stringsAsFactors = FALSE)
train_10grams_df <- as.data.frame.table(table(train_10grams), stringsAsFactors = FALSE)

colnames(train_1gram_df)   <- c("Word", "Freq")
colnames(train_2grams_df)  <- c("Word", "Freq")
colnames(train_3grams_df)  <- c("Word", "Freq")
colnames(train_4grams_df)  <- c("Word", "Freq")
colnames(train_5grams_df)  <- c("Word", "Freq")
colnames(train_6grams_df)  <- c("Word", "Freq")
colnames(train_10grams_df) <- c("Word", "Freq")

rm("train_1gram", "train_2grams", "train_3grams", "train_4grams", "train_5grams", "train_6grams", "train_10grams")

#################################
## Probabilities (maximum-likelihood)
#################################
## as for higher degree of n-gram, the probability is almost 1 for all of it. So, we will omit 10 grams.
library(dplyr)

get_xgram_freq1 <- function (x, x_1_word) { x$Freq[grep(paste0("^", gsub("^(.*)_.*$", "\\1", x_1_word), "$"), x$Word)] }
get_xgram_freq2 <- function (x, x_1_word_list) { lapply(x_1_word_list, function(x1) get_xgram_freq1(x, x1[1])) }

# train_2grams_df_prob <- mutate(train_2grams_df, prob = round(Freq/unlist(get_xgram_freq2(train_1gram_df, train_2grams_df$Word)),2))
# train_3grams_df_prob <- mutate(train_3grams_df, prob = round(Freq/unlist(get_xgram_freq2(train_2gram_df, train_3grams_df$Word)),2))
# train_4grams_df_prob <- mutate(train_4grams_df, prob = round(Freq/unlist(get_xgram_freq2(train_3gram_df, train_4grams_df$Word)),2))
# train_5grams_df_prob <- mutate(train_5grams_df, prob = round(Freq/unlist(get_xgram_freq2(train_4gram_df, train_5grams_df$Word)),2))
# train_6grams_df_prob <- mutate(train_6grams_df, prob = round(Freq/unlist(get_xgram_freq2(train_5gram_df, train_6grams_df$Word)),2))


#################################
## sorted frequencies
#################################

train_1gram_df_srt   <-   train_1gram_df[order(train_1gram_df$Freq,   decreasing = TRUE),]
train_2grams_df_srt  <-  train_2grams_df[order(train_2grams_df$Freq,  decreasing = TRUE),]
train_3grams_df_srt  <-  train_3grams_df[order(train_3grams_df$Freq,  decreasing = TRUE),]
train_4grams_df_srt  <-  train_4grams_df[order(train_4grams_df$Freq,  decreasing = TRUE),]
train_5grams_df_srt  <-  train_5grams_df[order(train_5grams_df$Freq,  decreasing = TRUE),]
train_6grams_df_srt  <-  train_6grams_df[order(train_6grams_df$Freq,  decreasing = TRUE),]
train_10grams_df_srt <- train_10grams_df[order(train_10grams_df$Freq, decreasing = TRUE),]

# train_1gram_df_srt   <-   train_1gram_df[order(train_1gram_df$Freq,   decreasing = TRUE),]
# train_2grams_df_prob_srt  <-  train_2grams_df_prob[order(train_2grams_df_prob$Freq,  decreasing = TRUE),]
# train_3grams_df_prob_srt  <-  train_3grams_df_prob[order(train_3grams_df_prob$Freq,  decreasing = TRUE),]
# train_4grams_df_prob_srt  <-  train_4grams_df_prob[order(train_4grams_df_prob$Freq,  decreasing = TRUE),]
# train_5grams_df_prob_srt  <-  train_5grams_df_prob[order(train_5grams_df_prob$Freq,  decreasing = TRUE),]
# train_6grams_df_prob_srt  <-  train_6grams_df_prob[order(train_6grams_df_prob$Freq,  decreasing = TRUE),]
# train_10grams_df_srt <- train_10grams_df[order(train_10grams_df$Freq, decreasing = TRUE),]

rm("train_1gram_df", "train_2grams_df", "train_3grams_df", "train_4grams_df", "train_5grams_df", "train_6grams_df", "train_10grams_df")
#rm("train_2grams_df_prob", "train_3grams_df_prob", "train_4grams_df_prob", "train_5grams_df_prob", "train_6grams_df_prob")

#head(train_ngrams_df[order(train_ngrams_df$Freq,decreasing=T),])
#str="_at_the_"; grep(paste0(str,"[a-z]+",collapse=""),train_ngrams, value = T)

#################################
# train_1gram_df             <- as.data.frame.table(table(tolower(train_1gram)),   stringsAsFactors = FALSE)
# colnames(train_1gram_df)   <- c("Word", "Freq")
# train_1gram_df_srt         <-   train_1gram_df[order(train_1gram_df$Freq,   decreasing = TRUE),]
# write.csv(train_1gram_df_srt,   file="train_1gram_df_srt.csv",   row.names = FALSE)
#################################

#################################
## save files
#################################

write.csv(train_1gram_df_srt,   file="csv/train_1gram_df_srt.csv",   row.names = FALSE)
write.csv(train_2grams_df_srt,  file="csv/train_2grams_df_srt.csv",  row.names = FALSE)
write.csv(train_3grams_df_srt,  file="csv/train_3grams_df_srt.csv",  row.names = FALSE)
write.csv(train_4grams_df_srt,  file="csv/train_4grams_df_srt.csv",  row.names = FALSE)
write.csv(train_5grams_df_srt,  file="csv/train_5grams_df_srt.csv",  row.names = FALSE)
write.csv(train_6grams_df_srt,  file="csv/train_6grams_df_srt.csv",  row.names = FALSE)
write.csv(train_10grams_df_srt, file="csv/train_10grams_df_srt.csv", row.names = FALSE)

save(list = c(grep("train_[[:digit:]]+gram[s]*_df_*srt$", ls(), value=T)), 
                            file="data/ngrams_srt_df.RData"); save.image()
save("train_sentence2", file="data/train_sentence2.RData"); save.image()
save("test_sentence2", file="data/test_sentence2.RData"); save.image()

print(paste0("End: Training data @ ",Sys.time()))
print("###################################################################################################")

########################################################################