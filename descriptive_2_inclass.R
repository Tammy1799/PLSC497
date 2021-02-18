# Descriptive practice


#1. Write two sentences. Save each as a seperate object in R. 

require(quanteda)
s1 <- "I have a dog named Crosby."
s2 <- "Crosby's best friend is named Reuben."

#2. Combine them into a corpus
c <- c(s1, s2)
c
#3. Make this corpus into a dfm with all pre-processing options at their defaults.
dfm_txt<-dfm(c)
dfm_txt
#4. Now save a second dfm, this time with stopwords removed.
?dfm
toks <- tokens(c)
txt <- tokens_remove(toks, stopwords("english"))
txt2 <- tokens(txt, remove_punct = TRUE)
dfm_txt2<-dfm(txt2)
dfm_txt2
#5. Calculate the TTR for each of these dfms (use textstat_lexdiv). Which is higher?
newDfm_txt <- textstat_lexdiv(dfm_txt)
newDfm_txt
newDfm_txt2 <- textstat_lexdiv(dfm_txt2)
newDfm_txt2
#6. Calculate the Manhattan distance between the two sentences you've constructed (by hand!)
#summation|yi-yj|. 
#|1| + |0| + |1| + |-1| + |-1| + |-1| + |-1| = 1 + 1 + 1 + 1 + 1 + 1 = 6