############################################################################################################
## Sequence		: 01
## File			: preprocess.R
## Author		: Ambika J
## Date			: May-2016
## Notes		: This file is to preprocess the raw data, before we "predict next word".
##				  All different logics related to preprocess will be collated here.
##					It is slotted into 3 main sections for preprocessing
##					1. First clean-up
##   					1. remove invisible characters
##   					2. remove control characters
##   					3. remove emojis
##   					4. remove all lines that has [A-Z]:
##   					5. delete rows which has URL
##   					6. remove email ids
##   					7. remove other URLs (.com|.co etc.)
##   					8. replace abbreviations
##   					9. replace contraction: this is DONE later; only for training data
##   					10. remove non words
##   					11. remove extra spaces
##   					12. remove this - \\o/ 
##					2. Bad words removal
##					3. Phase 2 clean-up (second clean-up)
##   					1. remove sentences which has only punctuation, space and numbers.
##   					2. remove multiple --- ___ === >>> <<< *** ... to only one.
##   					3. remove certain ads and spams (delete records)
##   					4. remove hash tags
##   					5. delete records with style sheets content
##   					6. delete records with nutrition and diet
##   					7. remove paranthesis and contents between them.
##   					8. remove numbers where there are 2 words and a number afer it. 
##   					9. delete records with a single word and a number.
##   					10. delete sentences related to real estate and shows
##   					11. delete records which has only one word
##   					12. numbers related clean-up
##					Then, finally save these 3 cleaned files; blog, news and twit
############################################################################################################

#source("/Users/ambikasam/Coursera/CourseraCourses/JohnsHopkins-DataAnalysis-courseraCourse/10-Capstone/scripts_programs_rmds/R/final/preprocess.R")
print("###################################################################################################")
print(paste0("Start Preprocessing @ ",Sys.time()))

#load("raw_data1.RData")
library(quanteda)
library(qdap)
library(stringi)
library(R.utils)
library(tm)

options(mc.cores=4, java.parameters = "-Xmx4g")

#################################
## variables
#################################
proj_path           <- "~/Coursera/CourseraCourses/JohnsHopkins-DataAnalysis-courseraCourse/10-Capstone/"
data_path           <- paste0(proj_path,"data/")
path_raw            <- paste0(data_path,"final/en_US/")
sample_path         <- paste0(data_path,"final-work/en_US/sample_blog_twitter_news/")
profanity_path      <- paste0(data_path,"bad-words/")

#################################
## Functions
#################################
data("regex_usa")
toSpace <- function(x, pattern) gsub(pattern, " ", x)
#removeURL <- function(x) gsub("(http|www)[^[:space:]]*", "", x)
removeURLPara <- function(x) x[-grep("(http|www)[^[:space:]]*", x)]
cleanEmail <- function(x) gsub(regex_usa$rm_email,"",x)
## call this function after calling all URLPara and email functions and 
## before hashtag and single word elimination
cleanOtherURLs <- function(x) gsub("[^\\s+]([^[:space:]]+\\.(com|net|edu|net|org|co\\.(uk|in|au)))([/?][^[:space:]]+)*", " ", x)
keepOnlyAlphaSpace <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
replaceAbbreviation <- function(x) {
    indx <- grep("([A-Za-z][\\.]\\s*){1,}([A-Za-z][\\.])",x)
    x[indx] <- replace_abbreviation(x[indx])
    return(x)
}
# replaceContraction <- function(x) {
#     indx <- grep("([A-Za-z][\\.]\\s*){1,}([A-Za-z][\\.])",x)
#     x[indx] <- replace_contraction(x[indx])
#     return(x)
# }
patternBasedCleanUp <- function(pattern, convert_to, content){
    #pattern <- "[-_][-_]+"
    indx <- grep(pattern, content)
    content[indx] <- gsub(pattern, convert_to, content[indx])
    return(content)
}

### NON-WORDS removal 
### Notice there are 2 kinds:
## 1. fraction - ¼
## 2. non-english words
## For now, just remove all these off.
###
removeNonWords <- function (content) {
    content <- iconv(content,"latin1", "ASCII", sub = "")
    non_words_content_indx <- grep("[^[:alnum:][:space:][:punct:]]",content)
    content[non_words_content_indx] <- gsub("[^[:alnum:][:space:][:punct:]]","",content[non_words_content_indx])
    content
}


#################################
## Loading the data
#################################
#setwd("~/Coursera/CourseraCourses/JohnsHopkins-DataAnalysis-courseraCourse/10-Capstone")
#if (!file.exists("data"))  dir.create("data")
if (!file.exists(paste0(proj_path,"data")))  dir.create(paste0(proj_path,"data"))

#setwd("./data")
## File download and unzip
if(!dir.exists(paste0(data_path,"final"))) {
    if(!file.exists(paste0(data_path,"final/Coursera-SwiftKey.zip"))) {
        download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", 
            destfile = paste0(data_path,"final/Coursera-SwiftKey.zip"), method = "curl")
        unzip(paste0(data_path,"final/Coursera-SwiftKey.zip"), 
            exdir = paste0(data_path,"final"))
    }
}

#################################
## Read anxillary files 
#################################
emojis <- read.delim2("../predictText_work/emojis.txt",sep=";", stringsAsFactors = FALSE)$Native
#profanity <- readLines(profanity_file)

#################################
## reading files
## first clean up
##  1. remove invisible characters
##  2. remove control characters
##  3. remove emojis
##  4. remove all lines that has [A-Z]:
##  5. delete rows which has URL
##  6. remove email ids
##  7. remove other URLs (.com|.co etc.)
##  8. replace abbreviations
##  9. replace contraction: this is DONE later; only for training data
## 10. remove non words and remove hash tags
## 11. remove extra spaces
## 12. remove this - \\o/ 

#################################
first_clean_up <- function(content) {
    print(paste0("first clean up ...",Sys.time()))
    
    print(paste0("Invisible characters ...",Sys.time()))
    ## remove invisible characters; cntrl and emoticons (use replace_contraction for emoticons)
    invisible_content_indx <- grep("[^[:print:]]",content)
    content[invisible_content_indx] <- gsub("[[:cntrl:]]","",content[invisible_content_indx])
    content[invisible_content_indx] <- replace_contraction(content[invisible_content_indx], 
                                                           contraction = emojis, replace = "")
    
    print(paste0("[A-Z]: lines delete ...",Sys.time()))
    content <- gsub(".*[A-Z]:([\\]).*","",content) ## twit: only 9 lines, delete them off.
    
    print(paste0("removeURLPara ...",Sys.time()))
    content <- removeURLPara(content)
    print(paste0("cleanEmail ...",Sys.time()))
    content <- cleanEmail(content)
    print(paste0("cleanOtherURLs ...",Sys.time()))
    content <- cleanOtherURLs(content)
    
    print(paste0("replaceAbbreviation ...",Sys.time()))
    content <- replaceAbbreviation(content)
    
    #print(paste0("replace_contraction ... this takes time ...",Sys.time()))
    #content <- replace_contraction(content) ## as this takes too lot a time, doing it only for trainign data
    # later, if we are going to use test and validate data, we will have to run this function again for that.
    print(paste0("removeNonWords ...",Sys.time()))
    content <- removeNonWords(content)
    
    print(paste0("remove hash tags ...",Sys.time()))
    content <- patternBasedCleanUp("[#@][a-zA-Z0-9]+|[#]\\S+"," ",content)
    
    print(paste0("extra spaces removal...",Sys.time()))
    ### Remove extra spaces [makes sense for :blank: than :space:]
    content <- gsub("([[:blank:]])+"," ",content)
    
    ### Removal of specific characters, that cause issue. Like this \\o/ before spell check
    print(paste0("\\o/ removal ...",Sys.time()))
    content <- patternBasedCleanUp("\\w*[\\]+\\o[/]*","",content)
    
    print(paste0("first clean up - Done!!! ...",Sys.time()))
    return(content)
}

if(!"blog" %in% ls()) blog <- readLines(paste0(path_raw,"en_US.blogs.txt"),   encoding="UTF-8")
if(!"blog1" %in% ls()) system.time(blog1 <- first_clean_up(blog))

if(!"news" %in% ls()) news <- readLines(paste0(path_raw,"en_US.news.txt"),    encoding="UTF-8")
if(!"news1" %in% ls()) system.time(news1 <- first_clean_up(news))

if(!"twit" %in% ls()) twit <- readLines(paste0(path_raw,"en_US.twitter.txt"), encoding="UTF-8", skipNul=TRUE)
if(!"twit1" %in% ls()) system.time(twit1 <- first_clean_up(twit))

#save(list = c("blog","news","twit"), file="data/raw_data.RData"); save.image()
rm("blog","news","twit")

## do hashtag clean-up before badwords removal; as many tags are profanity ones; 
## so do it along first-cleanup

###################################################################################################

#################################
## profanity 
#################################
bad_words_index <- function(content_list, bad_words_list) {
    blg_indx <- 1;
    for (i in 1:length(content_list)) {
        if (sum(content_list[[i]] %in% bad_words_list) >= 1) blg_indx = c(blg_indx,i)
    }
    blg_indx[2:length(blg_indx)]
}

##------------------------------------
### remove_bad_words_single_worded
##------------------------------------
### I m sticking to remove only single words of profanity; 
### multi words - logics are not working; tokenization for such huge set, it throws error.
profanity_path      <- paste0(data_path,"bad-words/")
profn               <- readLines(paste0(profanity_path,"bad_word_grep_uniq.txt"))
profanity_allwords  <- gsub(" ", "_", profn)
profanity_allwords1 <- sort(unique(c(profanity_allwords, gsub(" ","",grep(" ", profn, value=T)))))

remove_bad_words_single_worded <- function(content, profanity_allwords1){
    print(paste0("removing bad words ...",Sys.time()))

    content_list <- stri_extract_all_words(stri_trans_tolower(content))
    words        <- unique(unlist(content_list))
    #bad_words   <- words[words %in% profanity_allwords1] ## this wont work for words like "shitshit"
    bad_words    <- grep(paste0("^(",profanity_allwords1, ")+$",collapse="|"), words, value = T, ignore.case = T)
    ln           <- length(bad_words) ## blogs: 697; news: 370; twitter: 960
    print(ln)
    indx         <- bad_words_index(content_list, bad_words)
    
    ### finalized on stop words over removeWords and gsub. Do not change this anymore.
    print(paste("gsub ",Sys.time()))
    #content[indx] <- content[indx] %sw% bad_words ## this converts it to lower case; tho fast
    #content[indx] <- gsub(paste0("\\b", bad_words,   "\\b", collapse = "|"), "", content[indx], ignore.case = T)
                  content[indx] <- gsub(paste0("\\b", bad_words[1:370],   "\\b", collapse = "|"), "", content[indx], ignore.case = T)
    if (ln > 370) content[indx] <- gsub(paste0("\\b", bad_words[371:697], "\\b", collapse = "|"), "", content[indx], ignore.case = T)
    if (ln > 697) content[indx] <- gsub(paste0("\\b", bad_words[698:ln],  "\\b", collapse = "|"), "", content[indx], ignore.case = T)

    print(paste0("Done ...", Sys.time()))
    content
}

## remove bad words  
blog2 <- remove_bad_words_single_worded(blog1, profanity_allwords1)
news2 <- remove_bad_words_single_worded(news1, profanity_allwords1)
twit2 <- remove_bad_words_single_worded(twit1, profanity_allwords1)

### next level of clean-up of a few words
word <- "fuck"
ptrn <- paste0("^.*\\b([^[:space:]]*", word, "[^[:space:]]*)\\b.*$")

indx         <- grep(word, blog2, ignore.case = T)
blog2[indx] <- gsub(ptrn, "", blog2[indx], ignore.case = T)

indx         <- grep(word, news2, ignore.case = T)
news2[indx] <- gsub(ptrn, "", news2[indx], ignore.case = T)

indx         <- grep(word, twit2, ignore.case = T)
twit2[indx] <- gsub(ptrn, "", twit2[indx], ignore.case = T)

####
word <- "(dog|bull)shit"
ptrn <- paste0("^.*\\b([^[:space:]]*", word, "[^[:space:]]*)\\b.*$")

indx         <- grep(word, blog2, ignore.case = T)
blog2[indx] <- gsub(ptrn, "", blog2[indx], ignore.case = T)

indx         <- grep(word, news2, ignore.case = T)
news2[indx] <- gsub(ptrn, "", news2[indx], ignore.case = T)

indx         <- grep(word, twit2, ignore.case = T)
twit2[indx] <- gsub(ptrn, "", twit2[indx], ignore.case = T)

#> length(grep("fuck", c(blog2,news2,twit2), ignore.case = T))
#[1] 0

save(list = c("blog1","news1","twit1"), file="data/raw_data1.RData"); save.image()
save(list = c("blog2","news2","twit2"), file="data/raw_data2.RData"); save.image()
rm("blog1","news1","twit1")
###################################################################################################


####################################
### Phase2: Next level of clean-up
## ---------------------------------
##   1. remove sentences which has only punctuation, space and numbers.
##   2. remove multiple --- ___ === >>> <<< *** ... to only one.
##   3. remove certain ads and spams (delete records)
##   4. remove hash tags
##   5. delete records with style sheets content
##   6. delete records with nutrition and diet
##   7. remove paranthesis and contents between them.
##   8. remove numbers where there are 2 words and a number afer it. 
##   9. delete records with a single word and a number.
##   10. delete sentences related to real estate and shows
##   11. delete records which has only one word
##   12. numbers related clean-up
####################################
## There are ads, spams, just hashtags, real-estate, shows, etc.

### After a lot of checks and observations, I come to a conclusion, 
## that we need to just delete numbers and punctuations, before that some small clean-up
####################################

#load("raw_data2.RData")    
print(paste0("Start second cleanup @ ",Sys.time()))
    
### 1. remove all sentences which has only punctuation, space and numbers; 
## These are useless senteneces for prediction
indx <- grep("^[[:punct:][:space:][:digit:]]+$",blog2)
blog3 <- blog2[-indx]
indx <- grep("^[[:punct:][:space:][:digit:]]+$",news2)
news3 <- news2[-indx]
indx <- grep("^[[:punct:][:space:][:digit:]]+$",twit2)
twit3 <- twit2[-indx]

### 2. convert --+ __+ ..+ !!+ to space.
## >, <, "?!", "!?", 
ptrn <- "([-_=!><\\*\\.])[-_=!><\\*\\.]+"
blog3 <- patternBasedCleanUp(ptrn,"\\1",blog3)
news3 <- patternBasedCleanUp(ptrn,"\\1",news3)
twit3 <- patternBasedCleanUp(ptrn,"\\1",twit3)

ptrn <- "([-_=!><\\*\\.].?)+[-_=!><\\*\\.]*"
blog3 <- patternBasedCleanUp(ptrn,"\\1",blog3)
news3 <- patternBasedCleanUp(ptrn,"\\1",news3)
twit3 <- patternBasedCleanUp(ptrn,"\\1",twit3)

ptrn <- "\\s+=.=(\\s+)?|\\S+=.=\\S+"
blog3 <- patternBasedCleanUp(ptrn,"\\1",blog3)
news3 <- patternBasedCleanUp(ptrn,"\\1",news3)
twit3 <- patternBasedCleanUp(ptrn,"\\1",twit3)

# data(regex_usa)
# https://cran.r-project.org/web/packages/qdapRegex/qdapRegex.pdf - reference

### This one we will do for training data; to some extent ... later we will see; for now, only that much.
### convert numbers with punctuations (start,middle,end)

## 3. remove certain ads and spams
#1. Amazon Services LLC
## news and twitter do not have this

str <- "advertising and linking"
blog3 <- blog3[-grep(str,blog3)]

#2. G1 CERTIFIED WET TSHIRT
## blog and news do not have this

str <- "G1 CERTIFIED WET TSHIRT"
twit3 <- twit3[-grep(str,twit3)]

#3. repeadted words; will be taken care after sentencing, NOT now.

# #4. check if any sentence has only a word;
# ### remove it off; assuming most of them are meaningless (For a few sample observations made.)
# ### let us do this after cleaning, emails, urls, numbers, etc. 

#5. check if any sentence having 2 words;
#2 words are looking meaningful, so leave it as-is. Like, "Silver Lining", 
#"Orange juice", "Front Cover", "Day 28", "Hey there", etc.
#If we want we can convert the numbers in these 2 words to words instead of digits. ...

#6. 

#7. Remove hash tags ## this is done as part of first-clean up

#8. Remove meaningless words. check if those words are part of any meaningful para, or the entire para is meaningless.
## unwanted sentences; long rare sentences
blog3 <- blog3[-grep("7jtyq8epythiseurhgliasehrgtkjaeyhrtliuaeyrtiouyiortuyhilLIUYRIOUGF", blog3)]

# twit3 <- twit3[-grep("Thesis(Thesis)+", twit3)]
# twit3 <- twit3[-grep("Hbszdxjnhbdxhhbdvgfhjdxbnhbfhbfhrycbfhhhrfycbhrfycbhrfycbrfyhcbrhfycbrfyhhrbfcy", twit3)]
# twit3 <- twit3[-grep("dkfjaljfadkjfasdkjfaldjfadkjfdkfjakdfjadkfjdlfjfkljaldfjlajdfakldfjljadlkfajdl", twit3)]

# twit3 <- twit3[-grep("yourebeautifulyourebeautifulyourebeautifulyourebeautifulyourebeautifulyourebeautifulyourebeautiful", twit3, ignore.case = T)]
# twit3 <- twit3[-grep("lalalalalalalalalalalawordswordswordslalalalalalalalalaspeakingspeakingspeakinglalalalalalahaha", twit3, ignore.case = T)]
# twit3 <- twit3[-grep("yyyyyyyyyyyyyyyyyyyyyyyyyyyyeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeesssssssssssssssssssssssss", twit3, ignore.case = T)]
# twit3 <- twit3[-grep("asdfghjkljsdusbsvahavsbsbsodjsbhsgevabanvsgshsjgsgahsbjfilndndndjdnniflymdjsj", twit3, ignore.case = T)]
# twit3 <- twit3[-grep("kerrrrrrrrrrrrrriiiiiiiiiiiiiiiiiiiiiiiiiiiiiinnnnnnnnnnnnnnnnnnnnnnnnn", twit3, ignore.case = T)]

twit3 <- twit3[-grep("Thesis(Thesis)+|
                     Hbszdxjnhbdxhhbdvgfhjdxbnhbfhbfhrycbfhhhrfycbhrfycbhrfycbrfyhcbrhfycbrfyhhrbfcy|
                     dkfjaljfadkjfasdkjfaldjfadkjfdkfjakdfjadkfjdlfjfkljaldfjlajdfakldfjljadlkfajdl|
                     yourebeautiful(yourebeautiful)+|
                     (la)+(words)+(la)+(speaking)+(la)+haha|
                     y(y)+e(e)+s(s)+|
                     asdfghjkljsdusbsvahavsbsbsodjsbhsgevabanvsgshsjgsgahsbjfilndndndjdnniflymdjsj|
                     ker(r)+i(i)+n(n)+", twit3, ignore.case = T)]


#9. spaces - after (,) --> (, ); (\\.([A-Z])) --> (\\. \\1); YET TO DO

#10. some words that needs to be retained, which are part of letter count around 20 
#    (while cleaning words beyond 20 letters); YET TO DO

#institutionalization -> institutionalisation
#internationalization -> internationalisation
#uncharacteristically, characteristically

#11. some multibyte character; this is already handled above (remove non words), by doing iconv
#Con�gre�ga�tion�al
#Con�gre�ga�tion�al

#12.

#13. some style sheets kind of thing ... that needs to be deleted.
## only for blog it is seen.
blog3 <- blog3[-grep("style=",blog3)]

#14. Some calories, protein, carbohydrate related.
## blog and news have them, delte the lines off
## deleting all nutrition related stuff

blog3 <- blog3[-grep("calories.*protein.*carbohydrate",blog3,ignore.case = T)]
news3 <- news3[-grep("calories.*protein.*carbohydrate",news3,ignore.case = T)]
#twit3 <- twit3[-grep("calories.*protein.*carbohydrate",twit3,ignore.case = T)]
#as there are no records, it return 0 and thus, all records are deleted. So comment

blog3 <- blog3[-grep("mg.cholesterol",blog3,ignore.case = T)]
news3 <- news3[-grep("mg.cholesterol",news3,ignore.case = T)]
#twit3 <- twit3[-grep("mg.cholesterol",twit3,ignore.case = T)]
#as there are no records, it return 0 and thus, all records are deleted. So comment

#15. remove sentences between parenthesis

ptrn <- "\\(.*\\)"
blog3 <- patternBasedCleanUp(ptrn,"",blog3)
news3 <- patternBasedCleanUp(ptrn,"",news3)
twit3 <- patternBasedCleanUp(ptrn,"",twit3)

## 16. when there are only 2 words and then a number, remove those numbers

ptrn <- "^(\\w+ \\w+) [[:digit:]]+$"
blog3 <- patternBasedCleanUp(ptrn,"\\1",blog3)
news3 <- patternBasedCleanUp(ptrn,"\\1",news3)
twit3 <- patternBasedCleanUp(ptrn,"\\1",twit3)


### 17. single word and number, delete those records; donot make sense

str = "^\\w+ [[:digit:]]+$"
blog3 <- blog3[-grep(str,blog3)]
news3 <- news3[-grep(str,news3)]
twit3 <- twit3[-grep(str,twit3)]

### 18. most of it are real-estate and some are shows; para tht starts with num and end with num

str = "^[[:digit:]]+.+[[:digit:]]+$"
blog3 <- blog3[-grep(str,blog3)]
news3 <- news3[-grep(str,news3)]
twit3 <- twit3[-grep(str,twit3)]

### 19.  YET TO DO
#ptrn <- "^\\w+.+[[:digit:]]+$"

## 20. numbers related
blog3 <- patternBasedCleanUp("[[:digit:]]+","<num>", blog3)
news3 <- patternBasedCleanUp("[[:digit:]]+","<num>", news3)
twit3 <- patternBasedCleanUp("[[:digit:]]+","<num>", twit3)

num_manipulation <- function(content) {
    content <- gsub("<num>(.<num>)*(\\w+)*|<num>(.<num>)*(\\S+)*","<num>", content)
    ###  along with others, delete table spoon or tea spoon (tbl | tsp) - these are recipes
	indx <- grep("^<num>(.?.?.?.?.?.?$| \\w+[ ]*$|\\S+$|.*calorie)| tbl | tsp ", content)
	content <- content[-indx]
	content <- gsub("<num>(.<num>)+|<num>(<num>)+","<num>", content)
    content <- gsub("<num>.?:.?<num>.?([APap][Mm])?","<time>", content)
	content <- gsub("<num>.?[AaPp][Mm]", "<time>", content)
	content <- gsub("[$|£].?<num>","<money>", content)
	content <- gsub("<num>|<time>|<money>"," ",content)
	return(content)
}
blog3 <- num_manipulation(blog3)
news3 <- num_manipulation(news3)
twit3 <- num_manipulation(twit3)

#21. keeping this at last; as there is some issue.
ptrn <- "(ha)(ha)+"
blog3 <- gsub(ptrn, "\\1\\1 ", blog3, ignore.case = T)
news3 <- gsub(ptrn, "\\1\\1 ", news3, ignore.case = T)
twit3 <- gsub(ptrn, "\\1\\1 ", twit3, ignore.case = T)
#save(list = c("blog31","news31","twit31"), file="preprocessedData_bnt31.RData"); save.image()

##22. remove multiple white space
## remove white space before
##      comma
##      colon/s
##      semicolon/s
##      endmark/s (period, question mark, exclamation mark)
## remove white space after "[" and before "]"
## remove leading or trailing white space

blog3 <- rm_white(blog3)
news3 <- rm_white(news3)
twit3 <- rm_white(twit3)

#4. check if any sentence has only a word;
### remove it off; assuming most of them are meaningless (For a few sample observations made.)
### let us do this after cleaning, emails, urls, numbers, etc. 

str = "^(\\s+|[[:punct:]]+)*\\w+(\\s+|[[:punct:]]+)*$"
blog3 <- blog3[-grep(str,blog3)]
news3 <- news3[-grep(str,news3)]
twit3 <- twit3[-grep(str,twit3)]

blog3 <- blog3[-grep(" ", blog3, invert = TRUE)]
news3 <- news3[-grep(" ", news3, invert = TRUE)]
twit3 <- twit3[-grep(" ", twit3, invert = TRUE)]

##23. sentencize and remove one worded sentences
### sentencizing the cleaned data.
blog3_sentencized <- tokenize(blog3, what = "sentence", simplify = TRUE)
news3_sentencized <- tokenize(news3, what = "sentence", simplify = TRUE)
twit3_sentencized <- tokenize(twit3, what = "sentence", simplify = TRUE)
save(list = c("blog3_sentencized","news3_sentencized","twit3_sentencized"), file="data/preprocessedData_bnt3_sentencized.RData"); save.image()

str = "^(\\s+|[[:punct:]]+)*\\w+(\\s+|[[:punct:]]+)*$"
blog3 <- blog3_sentencized[-grep(str,blog3_sentencized)]
news3 <- news3_sentencized[-grep(str,news3_sentencized)]
twit3 <- twit3_sentencized[-grep(str,twit3_sentencized)]

blog3 <- blog3[-grep(" ", blog3, invert = TRUE)]
news3 <- news3[-grep(" ", news3, invert = TRUE)]
twit3 <- twit3[-grep(" ", twit3, invert = TRUE)]


print(paste0("End 2nd clean up @ ",Sys.time()))

rm("blog2","news2","twit2")
###################################################################################################

save(list = c("blog3","news3","twit3"), file="data/preprocessedData_bnt3.RData"); save.image()

print(paste0("End Preprocessing @ ",Sys.time()))
print("###################################################################################################")