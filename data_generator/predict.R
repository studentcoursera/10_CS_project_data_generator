############################################################################################################
## Sequence		: 03
## File			: predict.R
## Author		: Ambika J
## Date			: May-2016
## Notes		: This file is to predict next word using - blogs, news, twit offline data.
##				  All different logics related to predict will be collated here.
##					Identified the following:
##					1. Search ngrams of blogs, news and twitter
##					2. Search sentence of blogs, news and twitter
############################################################################################################

library(qdap)
library(stringi)
library(twitteR)
library(RCurl)
library(XML)
# 
# path <- getwd()
# prog_path <- paste0(getwd(),"../../scripts_programs_rmds/R/final")
# setwd(prog_path)

if(!exists("train_1_gram_df_srt")) load(file="data/ngrams_srt_df.RData")
## train_[1,2,3,4,5,6,,10]gram[s]_df_srt
if(!exists("train_sentence2")) load(file="data/train_sentence2.RData")
## train_sentence2

if(!exists("train_sentence2")) source("trainingdata.R")

#live_extract_from_twitter
if(!exists("live_extract_from_twitter", mode="function")) source("twitter-n-google-search.R")


####################################
## Search in ngrams to predict
####################################

### Logic:
# 1. Get search string
# 2. Set total number of words to 10.
# 3. search string manipulation:
# 		replace_contraction for the search string
# 		covert it to a list of words; extract only the last 10 (total_num_of_words) words
# 3a. reset the total number of words, if search string has less words than 10.
# 4. If total_search_words is >= 10; search in 10grams tokens.
#		Now, subset this 10grams. This will reduce number of records to search, considerably (at least by half).
# 			Extract only those records where the last word of search string is present. 
# 			As per the current logic to predict, if the last word do not exist, then, we cannot predict the word.
#		grep records from the subset of 10grams for the last 10 words of the search string.
#		if it returns indx, then, return those 10grams lines.
#		else continue to point 4a.
# 4a. For words 6 and greater, we need to adjust 
#		the total_num_of_words_of_search_string
#		And extract only the last 5+1 words from search string, to search
# 5. FOR Loop - words of search string - [count from 1 to total_num_of_words_in_search_string(here it is 5 tho)] 
# 		(for eliminating a word at a time and search it iteratively)
# 		Here, restricting the number of words of search to the last 5 words; as it is observed, beyond that do not yield much.
#		a. get number of words in search string; for each iteration there is a word less; 
# 			first iteration the last 5 words, next the last 4 words and so on until we have one word.
#		b. there are 5 ifs, where number of words from 5 to 1 check happens.
#			For each of those, there is specific ngram; like for 5 it is 5grams, 4 it is 4grams, etc.
#			So, whichever is applicable we assign it to a common ngram_df
#		c. Now, subset this common ngram_df. This will reduce number of records to search, considerably (at least by half).
# 			Extract only those records where the last word of search string is present. 
# 			As per the current logic to predict, if the last word do not exist, then, we cannot predict the word.
#		d. grep records from the subset of common ngram_df for the last 'n' (5 to 1) words of the search string.
#			use: function: ngram_search; parameters passed: (1)the current search string and (2)the subset of common ngram_df$Word
#				searches for the pattern, where the search it starts and ends with "search string + any one word"; in the passed ngram_df_word
#				If results are available in same case, then, take that indx;
#				else again search ignoring case.
#				return this indx.
#		e. if it returns indx, then, return those common ngram_df lines for these indexes.
#		f. else continue the for loop.
# 5a. After the for loop, if the for loop ends, that means there were no results.
# 		If there are NO results, just do nothing for now.
# 6. From 5.e. get the result-set.
# 		a. If there are results
# 			i. count the frequency of each word
# 			ii. return the word with highest frequency.
# 		b. Else
# 			i. for now, return "no_results"

ngram_search <- function (list_srch_str,train_ngram_Word) {
	#print("Started to search ...")
	tmp_str <- stri_flatten(list_srch_str, collapse = "_")
	srch <- paste0("^", tmp_str, "_[a-z]+$")
	#srch <- paste0(tmp_str,"_[a-z]+")
	
	indx <- grep(srch,train_ngram_Word) ##same case
	if ( length(indx) < 1 ) indx <- grep(srch, train_ngram_Word, ignore.case=T) ##ignore case
	
	#print("Completed search !!!")
	return(indx)
}

prediction_ngram_result_set <- function(srch_str)
{
	theword=""
	if (srch_str == "") {
		print("Please enter a string")
	} else {
		total_num_of_words <- 10
		## convert it to replace contraction; as the train data is
		srch_str1 <- gsub("[[:punct:]]", "", replace_contraction(srch_str));
		list_srch_str <- tail(unlist(stri_extract_all_words(srch_str1)), n=total_num_of_words)
		
		if (length(list_srch_str) < total_num_of_words) total_num_of_words <- length(list_srch_str)
		
		if (total_num_of_words >= 10) {
			#print(paste0("Searching 10 grams ...", Sys.time()))
			start_cnt <- (total_num_of_words-10+2)
			
			tmp_db <- train_10grams_df_srt[grep(paste0(tail(list_srch_str, n=1), "_\\w+"), train_10grams_df_srt$Word),]
			indx   <- ngram_search(list_srch_str[start_cnt:total_num_of_words], tmp_db)
			if ( length(indx) >= 1 ) return(tmp_db[indx,])
		}
		
		if (total_num_of_words > 5) {
			total_num_of_words <- 5
			list_srch_str <- tail(unlist(stri_extract_all_words(srch_str1)), n=total_num_of_words)
		}
		#print(paste0("Total # of words : ",total_num_of_words,"."))

		## Iterative process of the entire string you want to search
		for (start_cnt in 1:total_num_of_words) { 
			tmp_list_srch <- list_srch_str[start_cnt:total_num_of_words]
			num_of_words = length(tmp_list_srch) #total_num_of_words - start_cnt + 1
			#print(paste0("Searching start count ",start_cnt,"; # of words : ", num_of_words," ...",Sys.time()))
			if (num_of_words == 5) train_ngrams_df_srt <- train_6grams_df_srt
			if (num_of_words == 4) train_ngrams_df_srt <- train_5grams_df_srt
			if (num_of_words == 3) train_ngrams_df_srt <- train_4grams_df_srt
			if (num_of_words == 2) train_ngrams_df_srt <- train_3grams_df_srt
			if (num_of_words == 1) train_ngrams_df_srt <- train_2grams_df_srt
			
			tmp_db <- train_ngrams_df_srt[grep(paste0(tail(list_srch_str, n=1), "_\\w+"), train_ngrams_df_srt$Word),]
			indx   <- ngram_search(tmp_list_srch, tmp_db)
			if ( length(indx) >= 1 ) return(tmp_db[indx,])
		}
	}
}

predict_ngram_next_word <- function(srch_str)
{
	theword = ""
	### searching training blogs/news/twit
	result_set <- prediction_ngram_result_set(srch_str)
	if(length(result_set) > 1 ) {
		theword <- gsub(".*_(\\w+)$", "\\1", head(result_set$Word, n=1)) ## get the last word of this result
	} else {
		### if no results from existing blogs/news/twit, then, google search or twitter
		## for now; a constant
		theword <- "no_results"
	}
	return(theword)
}


####################################
## Search in sentences to predict
####################################

### Logic:
# 1. Get search string
# 2. search string manipulation:
# 	replace_contraction for the search string
# 	covert it to a list of words
# 3. Get total number of words in search string.
# 4. Now, subset the main sentences tokens. This will reduce number of records to search, considerably (at least by half).
# 	Extract only those records where the last word of search string is present. 
# 	As per the current logic to predict, if the last word do not exist, then, we cannot predict the word.
# 5. FOR Loop - words of search string - [count from 1 to total_num_of_words_in_search_string] 
# 	(for eliminating a word at a time and search it iteratively)
# 	Here, restricting the number of words of search to the last 5 words; as it is observed, beyond that do not yield much.
# 	a. Each ITERATIONS (Extract the search string); first iteration the last 5 words, next the last 4 words and so on until we have one word.
# 	b. pattern: extract all lines which has this search string and a word after that.
# 	c. get indexes which matches this pattern:
# 		i. grep all the lines that match this pattern (For the same case)
# 		ii. If there are no results from the same case, then, ignore.case and try.
# 	d. if there are results, then, 
# 		i. get those lines that matched.
# 		ii. extract only the next word that appears after the search string; in all these lines.
# 		iii. return this as the result-set (to predict the word).
# 	e. If there are NO results, just do nothing for now.
# 6. From 5.d.iii. get the result-set.
# 	a. If there are results
# 		i. count the frequency of each word
# 		ii. return the word with highest frequency.
# 	b. Else
# 		i. for now, return "no_results"

prediction_sent_result_set <- function(srch_str)
{
	theword=""
	if (srch_str == "") {
		print("Please enter a string")
	} else {
		## if that does not return results then after replace contraction
		srch_str1 <- gsub("[[:punct:]]", "", replace_contraction(srch_str));
        # list_srch_str <- unlist(stri_extract_all_words(srch_str1))
        # total_num_of_words <- length(list_srch_str)
            
		total_num_of_words <- 6 #length(list_srch_str) 
		#we want only last 6 words, beyond that it is not making sense
		#whenever we want this, we can change it
		list_srch_str <- tail(unlist(stri_extract_all_words(srch_str1)), n=total_num_of_words)
		
		## the last word should be present in the sentence, else, there is no point.
		## also, this being the scenario, just get only those records, which has last word.
		tmp_db <- train_sentence2[grep(paste0(tail(list_srch_str,n=1), " \\w+"),train_sentence2)]

		## Iterative process of the entire string you want to search
		for (start_cnt in 1:total_num_of_words) { 
			#print(paste0(start_cnt," : ", Sys.time()))
			
			tmp_str <- stri_flatten(list_srch_str[start_cnt:total_num_of_words], collapse=" ")
			pattern_str <- paste0(".*", tmp_str, " (\\w+).*")
			
			indx <- grep(pattern_str,tmp_db) ##same case
			if ( length(indx) < 1 ) indx <- grep(pattern_str, tmp_db, ignore.case=T) ##ignore case
			
			if ( length(indx) >= 1 ) {
				tmp <- tmp_db[indx]
				res <- gsub(pattern_str, "\\1", tmp, ignore.case = T)
				return(res)
			} 	
		}
	}
}

predict_sent_next_word <- function(srch_str)
{
	result_set <- prediction_sent_result_set(srch_str)
	if (length(result_set) >= 1) {
		words_results <- as.data.frame.table(table(result_set), stringsAsFactors = FALSE)
		return(head(words_results[order(words_results$Freq, decreasing = T),] ,n=1)$result_set)
	} else { return("no_results") }
	
}

# setwd(path)
########################################################################



