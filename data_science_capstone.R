# init
rm(list = ls())
gc()

# libs
if (!require("ggplot2"))      { install.packages("ggplot2") }
if (!require("tm"))           { install.packages("tm") }
if (!require("stringi"))      { install.packages("stringi") }
if (!require("wordcloud"))    { install.packages("wordcloud") }
if (!require("RColorBrewer")) { install.packages("RColorBrewer") }
if (!require("dplyr"))        { install.packages("dplyr") }
if (!require("slam"))         { install.packages("slam") }
if (!require("data.table"))   { install.packages("data.table") }


# download file, if not present already
if( ! file.exists("Coursera-SwiftKey.zip") ){
  src_zip_file <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  download.file(src_zip_file, destfile = "Coursera-SwiftKey.zip")
  unzip("Coursera-SwiftKey.zip")
}

# look at the source files
zip_files <- unzip("Coursera-SwiftKey.zip", list = T)
zip_files$Date <- NULL
zip_files$Language <- substr(zip_files$Name, 7, 8)
zip_files$Length_in_Mb <- zip_files$Length/(1024^2)
zip_files <- zip_files[zip_files$Length>0,]
ggplot(zip_files, aes(x = Name, y = Length_in_Mb, fill = Language)) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  theme_light() + 
  xlab("") + ylab("File size in Mb")


# only load twitter
zip_files <- zip_files[ grep("en_US", zip_files$Name),  ]

# this section reads all the files in memory 
# using dynamical variable names
# only run this part if you have enough RAM
for (file in zip_files$Name) {
  con <- file(file, "r")
  file_content <- readLines(con, encoding = "UTF-8")
  print(  paste("File:", file, "has in-memory size of:") )
  print(object.size(file_content), units="Mb")
  close(con)
  
  # dynamically create variable names
  assign(basename(file), file_content)
  file_content <- NULL
}



#   899,288 docs blogs
# 2,360,148 docs (largest collection in terms of documents) tweets
#    77,259 docs news
set.seed( 1984 ); ds.blogs  <- sample(en_US.blogs.txt,   0.2 * length(en_US.blogs.txt) ) # around 90k entries
set.seed( 1984 ); ds.tweets <- sample(en_US.twitter.txt, 0.2 * length(en_US.twitter.txt))
set.seed( 1984 ); ds.news   <- sample(en_US.news.txt,    0.2 * length(en_US.news.txt))

# blending texts together
ds <- c(ds.blogs, ds.tweets, ds.news)

hist( stri_count_words(ds), breaks=30, col=rainbow(50), main = paste("Number of words distribution for", prettyNum(length(ds), scientific=FALSE, big.mark=","), "documents" ))

# 246564
length(ds)

# word summaries
summary(stri_count_words(ds))

# summary number of characters
summary( sapply(ds, nchar) )


# calculate how much memory each object requires, and list the largest 10
tail( sort( sapply(ls(), function(x) object.size(get(x)) ) ) , 10)
rm(en_US.blogs.txt)
rm(en_US.news.txt)
rm(en_US.twitter.txt)
rm(ds.blogs)
rm(ds.news)
rm(ds.tweets)
gc()


# text mining on sampled data
corp <- VCorpus(VectorSource(ds))

# start the tm_map transformations 

# switch encoding: convert character vector from UTF-8 to ASCII
corp <- tm_map(corp, function(x)  iconv(x, 'UTF-8', 'ASCII'))

# eliminate white spaces
corp <- tm_map(corp, stripWhitespace)

# convert to lowercase
corp <- tm_map(corp, tolower)

# Remove punctuation
corp = tm_map(corp, removePunctuation)

# Remove numbers
corp = tm_map(corp, removeNumbers)

# remove stopwords
#length( stopwords("en") )
#corp <- tm_map(corp, removeWords, stopwords("en"))

# assign TEXT flag
corp <- tm_map(corp, PlainTextDocument)

# inspect outcome
#inspect( corp[1:2] ) 



######################################
# load bigrams, trigrams until 6-grams from the dataset, create word columns and regroup
for(i in 1:6) {
  print(paste0("Extracting", " ", i, "-grams from corpus"))
  tokens <- function(x) unlist(lapply(ngrams(words(x), i), paste, collapse = " "), use.names = FALSE)
  tdm <- TermDocumentMatrix(corp, control = list(tokenize = tokens))

#  tdmr <- rollup(tdm, 2, na.rm = TRUE, FUN = sum)
#  tdmr.t <- data.table(token = tdmr$dimnames$Terms, count = tdm$v) 
  # post-processing, creating dynamically word columns to simplify querying
#  tdmr.t[,  paste0("w", seq(i)) := tstrsplit(token, " ", fixed=TRUE)]
  # remove source token to save memory
#  tdmr.t$token <- NULL


  
  tdmr <- sort(slam::row_sums(tdm, na.rm = T), decreasing=TRUE)
  tdmr.t <- data.table(token = names(tdmr), count = unname(tdmr)) 
  tdmr.t[,  paste0("w", seq(i)) := tstrsplit(token, " ", fixed=TRUE)]
  # remove source token to save memory
  tdmr.t$token <- NULL
  

  print(paste0("Loaded in memory ", nrow(tdmr.t), " ", i, "-grams, taking: "))
  print(object.size(tdmr.t), units='Mb')

    #frequency distribution
  print( table(tdmr.t$count) )
  
  # dynamically create variable names
  assign(paste0("ngram",i), tdmr.t)
}
rm(ds)
rm(tdmr)
rm(tdm)
rm(tdmr.t)


# # regrouping by count
# ngram2 <- ngram2 %>% group_by(w1,w2)             %>% summarise(cnt=sum(count))
# ngram3 <- ngram3 %>% group_by(w1,w2,w3)          %>% summarise(cnt=sum(count))
# ngram4 <- ngram4 %>% group_by(w1,w2,w3,w4)       %>% summarise(cnt=sum(count))
# ngram5 <- ngram5 %>% group_by(w1,w2,w3,w4,w5)    %>% summarise(cnt=sum(count))
# ngram6 <- ngram6 %>% group_by(w1,w2,w3,w4,w5,w6) %>% summarise(cnt=sum(count))
# print(paste0("After regroping, in memory ", nrow(ngram6), " ", i, "-grams, taking: "))
# print(object.size(ngram6), units='Mb')
# 
# 

#paranoid memory cleanup
rm(cn)
rm(corp)
rm(i)

# memory size of our initial corpus
print(object.size(corp), units="Mb")

# environment size (all n-grams including corpus)
print(object.size(x=sapply(ls(), get)), units="Mb")

# save image file to disk
#rm(corp)
#gc()
# ngram1[,token:=paste(w1)]
# ngram2[,token:=paste(w1, w2)]
# ngram3[,token:=paste(w1, w2, w3)]
# ngram4[,token:=paste(w1, w2, w3, w4)]
# ngram5[,token:=paste(w1, w2, w3, w4, w5)]
# ngram6[,token:=paste(w1, w2, w3, w4, w5, w6)]

# calculate relative frequency
ngram1[,freq:=count/sum(count)]
ngram2[,freq:=count/sum(count)]
ngram3[,freq:=count/sum(count)]
ngram4[,freq:=count/sum(count)]
ngram5[,freq:=count/sum(count)]
ngram6[,freq:=count/sum(count)]

# skip sparse entities, but leave the real frequency
ngram1 <- subset(ngram1, count>1)
ngram2 <- subset(ngram2, count>1)
ngram3 <- subset(ngram3, count>1)
ngram4 <- subset(ngram4, count>1)
ngram5 <- subset(ngram5, count>1)
ngram6 <- subset(ngram6, count>1)


# how many unique words loaded (we took just splits of the full datasets)
length(unique(c(
  unlist(ngram2[,2:3, with = FALSE]), 
  unlist(ngram3[,2:4, with = FALSE]), 
  unlist(ngram4[,2:5, with = FALSE]), 
  unlist(ngram5[,2:6, with = FALSE]), 
  unlist(ngram6[,2:7, with = FALSE])
))) 

# prepare these in advance, since shiny app load-time should be smallest possible
ngram_stats <- data.frame(ngram = '', length = 0, count_min = 0, count_median = 0, count_mean = 0, count_max = 0, most_frequent_ngram='', mem = '')
for(i in 1:6) {
  # on-the-fly ngram statistics
  s <- summary( eval(parse(text = paste0('ngram',i,'$count'))) )
  # extract the most frequent word
  w <- eval(parse(text = paste0('ngram',i,'[1,seq(',i,')+1, with=F]')))
  most_frequent_ngram <- paste(unlist(w), sep=" ", collapse = " ")
  m <- paste(round(object.size(eval(parse(text = paste0('ngram',i))))/1024^2,1),'Mb')
  ngram_stats <- rbind(ngram_stats, 
                      data.frame(
                        ngram = paste0('ngram',i), 
                        length = nrow(eval(parse(text = paste0('ngram',i)))), 
                        count_min = s[1], 
                        count_median = s[3], 
                        count_mean = round(s[4],1),
                        count_max = s[6],
                        most_frequent_ngram = most_frequent_ngram,
                        mem = m
                        )
                 )
}
rm(s); rm(w);rm(m);rm(most_frequent_ngram);rm(i);
ngram_stats <- ngram_stats[-1,]

# save data for the Shiny app
save.image("20th_corpus_freq.Rdata")


#
# predict function
# simple back-off algorithm working its way from large 6-grams to tri/bi/unigrams
#
pred_words <- function(sentence, n = 10){
  
  # follow a similar preparation path as the large corpus
  sentence <- removeNumbers(sentence)
  sentence <- removePunctuation(sentence)
  sentence <- tolower(sentence)
  
  # split into words
  words <- unlist(strsplit(sentence, split = " " ))
  
  # only focus on last 5 words
  words <- tail(words, 5)
  
  word1 <- words[1];word2 <- words[2];word3 <- words[3];word4 <- words[4];word5 <- words[5];
  datasub <- data.table()
  
  if (nrow(datasub)==0 & !is.na(word5)) {
    if(nrow(datasub) == 0) datasub <- subset(ngram6, w1==word1 & w2==word2 & w3==word3 & w4==word4 & w5==word5)
    if(nrow(datasub) == 0) datasub <- subset(ngram5, w1==word2 & w2==word3 & w3==word4 & w4==word5)
    if(nrow(datasub) == 0) datasub <- subset(ngram4, w1==word3 & w2==word4 & w3==word5)
    if(nrow(datasub) == 0) datasub <- subset(ngram3, w1==word4 & w2==word5)
    if(nrow(datasub) == 0) datasub <- subset(ngram2, w1==word5)
  }
  
  if (nrow(datasub)==0 & !is.na(word4)) {
    if(nrow(datasub) == 0) datasub <- subset(ngram5, w1==word1 & w2==word2 & w3==word3 & w4==word4)
    if(nrow(datasub) == 0) datasub <- subset(ngram4, w1==word2 & w2==word3 & w3==word4)
    if(nrow(datasub) == 0) datasub <- subset(ngram3, w1==word3 & w2==word4)
    if(nrow(datasub) == 0) datasub <- subset(ngram2, w1==word4)
  }
  
  if (nrow(datasub)==0 & !is.na(word3)) {
    if(nrow(datasub) == 0) datasub <- subset(ngram4, w1==word1 & w2==word2 & w3==word3)
    if(nrow(datasub) == 0) datasub <- subset(ngram3, w1==word2 & w2==word3)
    if(nrow(datasub) == 0) datasub <- subset(ngram2, w1==word3)
  }
  
  if (nrow(datasub)==0 & !is.na(word2)) {
    if(nrow(datasub) == 0) datasub <- subset(ngram3, w1==word1 & w2==word2)
    if(nrow(datasub) == 0) datasub <- subset(ngram2, w1==word2)
  }
  
  if (nrow(datasub)==0 & !is.na(word1)) {
    if(nrow(datasub) == 0) datasub <- subset(ngram2, w1==word1)
    if(nrow(datasub) == 0) datasub <- head(ngram1)
  }
  
  if(nrow(datasub) > 0){
    datasub$freq <- datasub$count / sum(datasub$count)
    as.data.frame(head(datasub[order(-freq)], min(n, nrow(datasub))))
  }
  
}



pred_words('i want to go')
pred_words('each')
pred_words('thanks for all the')
pred_words('according to')
pred_words('i have decided')
pred_words('at the top')
pred_words('it is by design')
pred_words('instead of saying this')
pred_words('can you focus on the')


# For each of the sentence fragments below use your natural language processing algorithm to predict the next word in the sentence.
# 
# When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd
# 
# die
# 
# sleep
# 
# give
# 
# eat
# 1
# point
# 2. 
# Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his
# 
# spiritual
# 
# financial
# 
# horticultural
# 
# marital
# 1
# point
# 3. 
# I'd give anything to see arctic monkeys this
# 
# morning
# 
# decade
# 
# month
# 
# weekend
# 1
# point
# 4. 
# Talking to your mom has the same effect as a hug and helps reduce your
# 
# sleepiness
# 
# hunger
# 
# happiness
# 
# stress
# 1
# point
# 5. 
# When you were in Holland you were like 1 inch away from me but you hadn't time to take a
# 
# look
# 
# walk
# 
# picture
# 
# minute
# 1
# point
# 6. 
# I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the
# 
# incident
# 
# account
# 
# case
# 
# matter
# 1
# point
# 7. 
# I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each
# 
# toe
# 
# arm
# 
# hand
# 
# finger
# 1
# point
# 8. 
# Every inch of you is perfect from the bottom to the
# 
# side
# 
# center
# 
# top
# 
# middle
# 1
# point
# 9. 
# I’m thankful my childhood was filled with imagination and bruises from playing
# 
# inside
# 
# daily
# 
# outside
# 
# weekly
# 1
# point
# 10. 
# I like how the same people are in almost all of Adam Sandler's
# 
# pictures
# 
# movies
# 
# novels
# 
# stories





# For each of the sentence fragments below use your natural language processing algorithm to predict the next word in the sentence.
# 
# The guy in front of me just bought a pound of bacon, a bouquet, and a case of
# 
# beer
# 
# cheese
# 
# soda
# 
# pretzels
# 1
# point
# 2. 
# You're the reason why I smile everyday. Can you follow me please? It would mean the
# 
# most
# 
# best
# 
# world
# 
# universe
# 1
# point
# 3. 
# Hey sunshine, can you follow me and make me the
# 
# bluest
# 
# smelliest
# 
# saddest
# 
# happiest
# 1
# point
# 4. 
# Very early observations on the Bills game: Offense still struggling but the
# 
# players
# 
# referees
# 
# crowd
# 
# defense
# 1
# point
# 5. 
# Go on a romantic date at the
# 
# beach
# 
# movies
# 
# mall
# 
# grocery
# 1
# point
# 6. 
# Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my
# 
# motorcycle
# 
# phone
# 
# way
# 
# horse
# 1
# point
# 7. 
# Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some
# 
# weeks
# 
# time
# 
# years
# 
# thing
# 1
# point
# 8. 
# After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little
# 
# eyes
# 
# fingers
# 
# toes
# 
# ears
# 1
# point
# 9. 
# Be grateful for the good times and keep the faith during the
# 
# bad
# 
# hard
# 
# sad
# 
# worse
# 1
# point
# 10. 
# If this isn't the cutest thing you've ever seen, then you must be
# 
# asleep
# 
# insensitive
# 
# insane
# 
# callous
