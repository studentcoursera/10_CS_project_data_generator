############################################################################################################
## Sequence		: 04
## File			: quizes.R
## Author		: Ambika J
## Date			: May-2016
## Notes		: This file is to get results for the quizes (using the sentence predict)
##					There are 2 quizes with 10 questions each
############################################################################################################

print("###################################################################################################")
print(paste0("Start: Quizes @ ",Sys.time()))

options(mc.cores=4, java.parameters = "-Xmx4g")

if(!exists("predict_ngram_next_word", mode="function")) source("predict.R")
#load("../predictText_work/predictText_final10_trainsentence2_4.RData")

####################################
## Quiz 2
####################################

quiz2 <- c("The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
  "You're the reason why I smile everyday. Can you follow me please? It would mean the",
  "Hey sunshine, can you follow me and make me the",
  "Very early observations on the Bills game: Offense still struggling but the",
  "Go on a romantic date at the",
  "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",
  "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",
  "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",
  "Be grateful for the good times and keep the faith during the",
  "If this isn't the cutest thing you've ever seen, then you must be")

quiz2options <- c("beer|pretzels|soda|cheese",
                  "best|world|universe|most",
                  "smelliest|saddest|bluest|happiest",
                  "players|defense|crowd|referees",
                  "grocery|mall|beach|movies",
                  "horse|phone|way|motorcycle",
                  "weeks|years|thing|time",
                  "ears|finger|eyes|toes",
                  "worse|sad|bad|hard",
                  "asleep|callous|insane|insensitive"
                  )
quiz2answers <- c("beer",
                  "world",
                  "happiest",
                  "defense",
                  "beach",
                  "way",
                  "time",
                  "finger",
                  "bad",
                  "insane")

for (i in 1:length(quiz2)) {
    print(paste(quiz2[i], predict_sent_next_word(quiz2[i]), sep=":"))
}

correct_count2 <- 0
for (i in 1:length(quiz2)) {
    ordr_tmp_results <- prediction_sent_result_set(quiz2[i])
    df <- as.data.frame.table(table(Word = ordr_tmp_results), stringsAsFactors = F)
    df <- df[order(df$Freq, decreasing = T),]
    #tmp_str <- head(ordr_tmp_results[grep(quiz2options[i], ordr_tmp_results$Word),]$Word, n=1)
    tmp_str <- head(df[grep(paste0("^",quiz2options[i],"$"), df$Word),]$Word, n=1)
    #tmp_str <- unlist(stri_split_fixed(tmp_str, pattern = "_"))
    #print(paste0(i, ". ", tmp_str[length(tmp_str)]))
    print(paste0(i, ". ", tmp_str))
    if(identical(tmp_str, quiz2answers[i])) correct_count2 <- correct_count2 + 1
}


####################################
## Quiz 3
####################################

quiz3 <- c("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd",
           "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his",
           "I'd give anything to see arctic monkeys this",
           "Talking to your mom has the same effect as a hug and helps reduce your",
           "When you were in Holland you were like 1 inch away from me but you hadn't time to take a",
           "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the",
           "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each",
           "Every inch of you is perfect from the bottom to the",
           "I’m thankful my childhood was filled with imagination and bruises from playing",
           "I like how the same people are in almost all of Adam Sandler")

quiz3options <- c("give|die|eat|sleep",
                  "marital|spiritual|financial|horticultural",
                  "weekend|morning|month|decade",
                  "hunger|sleepiness|happiness|stress",
                  "picture|look|minute|walk",
                  "incident|matter|case|account",
                  "finger|arm|toe|hand",
                  "top|center|middle|side",
                  "weekly|outside|inside|daily",
                  "novels|movies|pictures|stories"
                  )

quiz3answers <- c("die",
                  "marital",
                  "weekend",
                  "stress",
                  "picture",
                  "matter",
                  "hand",
                  "top",
                  "outside",
                  "movies")
				  
for (i in 1:length(quiz3)) {
    print(paste(i, "> ",quiz3[i], predict_sent_next_word(quiz3[i]), sep=":"))
}

correct_count3 <- 0
for (i in 1:length(quiz3)) {
    ordr_tmp_results <- prediction_sent_result_set(quiz3[i])
    df <- as.data.frame.table(table(Word = ordr_tmp_results), stringsAsFactors = F)
    df <- df[order(df$Freq, decreasing = T),]
    #tmp_str <- head(ordr_tmp_results[grep(quiz3options[i], ordr_tmp_results$Word),]$Word, n=1)
    tmp_str <- head(df[grep(paste0("^",quiz3options[i],"$"), df$Word),]$Word, n=1)
    #tmp_str <- unlist(stri_split_fixed(tmp_str, pattern = "_"))
    #print(paste0(i, ". ", tmp_str[length(tmp_str)]))
    print(paste0(i, ". ", tmp_str))
    if(identical(tmp_str, quiz3answers[i])) correct_count3 <- correct_count3 + 1
}

print(paste0("End: Quizes @ ",Sys.time()))
print("###################################################################################################")

