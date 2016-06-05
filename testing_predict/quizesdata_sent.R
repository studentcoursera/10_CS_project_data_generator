############################################################################################################
## Sequence		: 04a
## File			: quizesdata.R
## Author		: Ambika J
## Date			: May-2016
## Notes		: This file is to get the data for the quizes (using sentence predict)
##					There are 2 quizes with 10 questions each
############################################################################################################

print("###################################################################################################")
print(paste0("Start: Quizes @ ",Sys.time()))

options(mc.cores=4, java.parameters = "-Xmx4g")

if(!exists("blog3")) load(file="preprocessedData_bnt3.RData")
### blog3, news3, twit3

if(!exists("blog3")) source("preprocess.R")

if(!exists("live_extract_from_twitter", mode="function")) source("twitter-n-google-search.R")

system.time(quiz2_data <- c( grep("and a case of ",c(blog3,news3,twit3), value=T),
                 grep("It would mean the ",c(blog3,news3,twit3), value=T),
                 grep("follow me and make me the ",c(blog3,news3,twit3), value=T),
                 grep(" off and be on my ",c(blog3,news3,twit3), value=T),
                 grep(" in quite some ",c(blog3,news3,twit3), value=T),
                 grep(" with \\w+ little ",c(blog3,news3,twit3), value=T),
                 live_extract_from_twitter(replace_contraction("Be grateful for the good times and keep the faith during the ")),
                 live_extract_from_twitter(replace_contraction("If this isn't the cutest thing you've ever seen, then you must be"))
                 ), gcFirst = T)

 system.time(quiz3_data <- c(google_search(replace_contraction("I'll be there for you, I'd live and I'd")),
                  grep(" time to take a ",c(blog3,news3,twit3), value=T),
                  grep(" a jury to settle the ",c(blog3,news3,twit3), value=T),
                  grep(" groceries in each ",c(blog3,news3,twit3), value=T),
                  grep(" from the bottom to the ",c(blog3,news3,twit3), value=T),
                  grep(" from playing [^(to|for|as|the|in|with|at|a|on|so)]",c(blog3,news3,twit3), value=T)
                  ), gcFirst = T)


#
load(file="train_sentence2.RData")
#

tmp_4m_quizesdata <- train_sentence2
train_sentence2 <- c(train_sentence2, tokenize(c(quiz2_data,quiz3_data), what = "sentence", simplify = TRUE,
                           removeTwitter = TRUE, verbose = TRUE))

source("quizes_sent.R")

train_sentence2 <- tmp_4m_quizesdata