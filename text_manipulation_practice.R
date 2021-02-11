# Manupulating text in R

#1. Find a sentence online. Save it as a string. 
require(quanteda)
x <- "Penn State to expand student engagement activities, this spring."

#2. Select only the third word of the sentence. Save it as a new string.
x2 <- strsplit(x, " ")
x3 <- x2[[1]]
new_string <- x3[3]

#3. Choose a letter that appears in your sentence. Use the gsub command to replace all instances of that letter with a period. 
gsub('a', '.', "Penn State to expand student engagement activities, this spring.")
