library(qdap)

load("data/test_sentence2.RData")

set.seed("372")
test_sentence2 <- sample(test_sentence2, 100)
predict_phrases <- gsub("(.*) (.*)$", "\\1", test_sentence2)
next_words <- gsub(".* (.*)$", "\\1", test_sentence2)

#source("predict.R")
spell_check_correct <- function(phrase) {
    
    first_part <- gsub("(.*) .*$", "\\1", phrase)
    last_word <- gsub(".* (.*)$", "\\1", phrase)
    corrected_last_word <- check_spelling(last_word)$suggestion
    
    if(is.null(corrected_last_word)) {### what do we do???
        #return(NULL)
        t <- head(check_spelling(last_word)$more.suggestions) ### only first 6 words are enough.
        for (i in 1:length(t)) { 
            corrected_last_word <- t(i)
            indx <- grep(corrected_last_word, train_sentence2); 
            if(length(indx) >= 1) {
                #return(train_sentence2[indx]) 
                ordr_tmp_results <- prediction_sent_result_set(paste0(first_part, corrected_last_word))
                if(length(ordr_tmp_results) >= 1) return(ordr_tmp_results) 
            }
        }
        return(NULL) ## when there are no sentences in train_setence, or there are no prediction result.
        
    } else { 
        ordr_tmp_results <- prediction_sent_result_set(paste0(first_part, corrected_last_word))
        if(length(ordr_tmp_results) < 1) { 
            
            t <- head(check_spelling(last_word)$more.suggestions) ### only first 6 words are enough.
            for (i in 1:length(t)) { 
                corrected_last_word <- t(i)
                indx <- grep(corrected_last_word, train_sentence2); 
                if(length(indx) >= 1) {
                    #return(train_sentence2[indx]) 
                    ordr_tmp_results <- prediction_sent_result_set(paste0(first_part, corrected_last_word))
                    if(length(ordr_tmp_results) >= 1) return(ordr_tmp_results) 
                }
            }
            return(NULL) ## when there are no sentences in train_setence, or there are no prediction result.
        } else { return(ordr_tmp_results) }
    }
}

correct_count4 <- 0
for (i in 1:length(predict_phrases)) {
    ordr_tmp_results <- prediction_sent_result_set(predict_phrases[i])
    
    if(length(ordr_tmp_results) < 1) { 
        ordr_tmp_results <- spell_check_correct(predict_phrases[i])
    }  
    
    if(length(ordr_tmp_results) >= 1) {
        df <- as.data.frame.table(table(Word = ordr_tmp_results), stringsAsFactors = F)
        df <- df[order(df$Freq, decreasing = T),]
        #tmp_str <- head(ordr_tmp_results[grep(quiz3options[i], ordr_tmp_results$Word),]$Word, n=1)
        tmp_str <- head(df$Word, n=1)
    } else { tmp_str <- "testing"}
    
    #tmp_str <- unlist(stri_split_fixed(tmp_str, pattern = "_"))
    #print(paste0(i, ". ", tmp_str[length(tmp_str)]))
    print(paste0(i, ". ", tmp_str))
    if(identical(tmp_str, next_words[i])) { correct_count4 <- correct_count4 + 1 }
}

#blank_indx <- c(37, 59, 61, 67, 73, 103, 152, 171, 172, 179, 185, 204, 207, 226, 255, 310, 319, 338, 382, 387, 414, 433, 460, 487)
blank_indx <- c(15, 45, 47)