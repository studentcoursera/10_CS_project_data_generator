############################################################################################################
## Sequence		: 01a
## File			: preprocess_checks.R
## Author		: Ambika J
## Date			: May-2016
## Notes		: This file is to check the preprocessed raw data
############################################################################################################


if(!exists("blog"))  load(file="raw_data.RData");
if(!exists("blog3")) load(file="preprocessedData_bnt3.RData")

#################################
## words and frequencies
#################################

### all words extracted and frequencied; from raw blog/news/twit data
table_blog <- as.data.frame.table(table(stri_extract_all_words(stri_flatten(tolower(blog)))), stringsAsFactors = FALSE)
table_news <- as.data.frame.table(table(stri_extract_all_words(stri_flatten(tolower(news)))), stringsAsFactors = FALSE)
table_twit <- as.data.frame.table(table(stri_extract_all_words(stri_flatten(tolower(twit)))), stringsAsFactors = FALSE)

## frequency ordered, the raw data words
table_blog_srt <- table_blog[order(table_blog$Freq, decreasing=T),]
table_news_srt <- table_news[order(table_news$Freq, decreasing=T),]
table_twit_srt <- table_twit[order(table_twit$Freq, decreasing=T),]


### cleaned up data, paragraphs are frequencied.
tb <- as.data.frame.table(table(blog3), stringsAsFactors = FALSE)
tn <- as.data.frame.table(table(news3), stringsAsFactors = FALSE)
tt <- as.data.frame.table(table(twit3), stringsAsFactors = FALSE)

## frequency ordered, the cleaned data paragraphs
tb <- tb[order(tb$Freq, decreasing=T),]
tn <- tn[order(tn$Freq, decreasing=T),]
tt <- tt[order(tt$Freq, decreasing=T),] ## only twitter has repeated sentences.

### sentencizing the cleaned data.
system.time(blog3_sentencized <- tokenize(blog3, what = "sentence", simplify = TRUE))
system.time(news3_sentencized <- tokenize(news3, what = "sentence", simplify = TRUE))
system.time(twit3_sentencized <- tokenize(twit3, what = "sentence", simplify = TRUE))

## Freq
blog3_sent_df <- as.data.frame.table(table(blog3_sentencized), stringsAsFactors = FALSE)
news3_sent_df <- as.data.frame.table(table(news3_sentencized), stringsAsFactors = FALSE)
twit3_sent_df <- as.data.frame.table(table(twit3_sentencized), stringsAsFactors = FALSE)

### words length compute - 
## BLOG
Words                <- unlist(stri_extract_all_words(stri_trans_tolower(blog3), omit_no_match = TRUE))
words_freq           <- as.data.frame.table(table(Words), stringsAsFactors = FALSE)
words_freq_charCnt   <- data.frame(words_freq, CharCount=nchar(words_freq$Words),
                               stringsAsFactors = FALSE)
words_freq_cnt_srt_b <- words_freq_charCnt[order(words_freq_charCnt$CharCount, decreasing = T),]
head(words_freq_cnt_srt_b)

## NEWS
Words                <- unlist(stri_extract_all_words(stri_trans_tolower(news3), omit_no_match = TRUE))
words_freq           <- as.data.frame.table(table(Words), stringsAsFactors = FALSE)
words_freq_charCnt   <- data.frame(words_freq, CharCount=nchar(words_freq$Words),
                               stringsAsFactors = FALSE)
words_freq_cnt_srt_n <- words_freq_charCnt[order(words_freq_charCnt$CharCount, decreasing = T),]
head(words_freq_cnt_srt_n)

## TWIT
Words                <- unlist(stri_extract_all_words(stri_trans_tolower(twit3), omit_no_match = TRUE))
words_freq           <- as.data.frame.table(table(Words), stringsAsFactors = FALSE)
words_freq_charCnt   <- data.frame(words_freq, CharCount=nchar(words_freq$Words),
                               stringsAsFactors = FALSE)
words_freq_cnt_srt_t <- words_freq_charCnt[order(words_freq_charCnt$CharCount, decreasing = T),]
head(words_freq_cnt_srt_t)

rm("Words","words_freq","words_freq_charCnt")

write.csv(words_freq_cnt_srt_b,   file="words_freq_cnt_srt_b.csv",   row.names = FALSE)
write.csv(words_freq_cnt_srt_n,   file="words_freq_cnt_srt_n.csv",   row.names = FALSE)
write.csv(words_freq_cnt_srt_t,   file="words_freq_cnt_srt_t.csv",   row.names = FALSE)