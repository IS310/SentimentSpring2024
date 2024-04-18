---
title: "Sentiment1"
output: html_document
date: "2024-04-18"
---

```{r}
library(tidytext)
library(textdata)

# Create a function for looking up AFINN sentiment values from words
Sents <- get_sentiments("afinn")
SVal <- Sents$value
names(SVal) <- Sents$word
SVF <- function(x){return(as.integer(SVal[x]))}


# Read Jane Eyre into chapter-size chunks of text
JaneSentimentByChapter <- c() # start with an empty vector 
text.v <- scan("https://www.gutenberg.org/files/1260/1260-0.txt", what="character", sep="\n")
start.v <- (grep("\\*\\*\\*\\W*START OF TH",text.v) + 1)
end.v <- (grep("\\*\\*\\*\\W*END OF TH",text.v) - 1)
novel.lines.v <- text.v[start.v:end.v]                              # 1 element per line
novel.v <- paste(novel.lines.v, collapse="<br>")                    # linebreaks become tags
novel.lower.v <- tolower(novel.v)
jane.chap.l <- strsplit(novel.lower.v, "<br>chapter [ivx]+(—conclusion)?<br>")    # split at chapter breaks
jane.chap.v <- unlist(jane.chap.l)                                  # 1 element per chapter


# Loop over the chapters, split them into words, and sum up the AFINN values for each chapter
for(i in 1:length(jane.chap.v)) {    
  chapteri <- jane.chap.v[i]
  chapterlist <- strsplit(chapteri,"\\W")
  chapterv <- unlist(chapterlist)
  baz <- SVF(chapterv)            # sentiment values for every word
  buz <- baz[which(baz!="NA")]    # but many words aren't in AFINN, so eliminate null values
  JaneSentimentByChapter[i] = sum(buz)
}

# Create a graph plotting the sentiment totals for each chapter of the novel
plot(JaneSentimentByChapter)


```