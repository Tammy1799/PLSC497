# Descriptive Analysis of Texts

#quateda has a number of descriptive statistics available for reporting on texts.  
#The **simplest of these** is through the `summary()` method:
install.packages("quanteda")
#quanteda is text to data, package
require(quanteda) #to load into session
#txt is a named character vector
txt <- c(sent1 = "This is an example of the summary method for character objects.",
         sent2 = "The cat in the hat swung the bat.")
summary(txt)

#This also works for corpus objects:
#corpus is main function within quanteda. Q contains a couple of corpus.
#corpus in this case is data_char_ukimmig2010
summary(corpus(data_char_ukimmig2010, notes = "Created as a demo."))

#To access the **syllables** of a text, we use `syllables()`:
#to access number of syllables
nsyllable(c("Superman.", "supercalifragilisticexpialidocious", "The cat in the hat."))

#We can even compute the **Scabble value** of English words, using `scrabble()`:
#counting up values of these words in scrabble
nscrabble(c("cat", "quixotry", "zoo"))

#We can analyze the **lexical diversity** of texts, using `lexdiv()` on a dfm:
#take corpus subset of data_corpus_inaugural where year is greater than 1980
myDfm <- dfm(corpus_subset(data_corpus_inaugural, Year > 1980))
textstat_lexdiv(myDfm, "R") #myDfm is document feature matrix

#We can analyze the **readability** of texts, using `readability()` on a vector of texts or a corpus:

readab <- textstat_readability(corpus_subset(data_corpus_inaugural, Year > 1980), 
                               measure = "Flesch.Kincaid")

#We can **identify documents and terms that are similar to one another**, using `similarity()`:
#how many of same words appear in each document
## Presidential Inaugural Address Corpus
presDfm <- dfm(data_corpus_inaugural, remove = stopwords("english")) #remove the stop word english
# compute some document similarities
textstat_simil(presDfm, "1985-Reagan",  margin = "documents") #will get an error so solve by: ?textstat_simil and run it
    #and then put in textstat_simil(presDfm, presDfm["1985-Reagan"],  margin = "documents")

x<- c("2009-Obama", "2013-Obama")
textstat_simil(presDfm, presDfm[x,], margin = "documents", method = "cosine")

#And this can be used for **clustering documents**:

presDfm <- dfm(corpus_subset(data_corpus_inaugural, Year > 1900), stem = TRUE,
               remove = stopwords("english"))
presDfm <- dfm_trim(presDfm, min_count = 5, min_docfreq = 3)
# hierarchical clustering - get distances on normalized dfm
presDistMat <- dist(as.matrix(dfm_weight(presDfm, "relFreq")))
# hiarchical clustering the distance object
presCluster <- hclust(presDistMat)
# label with document names
presCluster$labels <- docnames(presDfm)
# plot as a dendrogram
dev.new()
plot(presCluster)

