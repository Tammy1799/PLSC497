##### R Task
library(rtweet)
get_token() 
## Pick your favorite celebrity who has a Twitter account. 
## Answer: I chose Tiffany O'Malley.

## find the most recent tweet the celebrited liked
tsf <- get_favorites("TiffSwish_Flick", n = 1)
View(tsf)

##Download their 500 most recent tweets.
tmls <- get_timelines(c("TiffSwish_Flick"), n = 500)
View(tmls)

#Calculate which one got the most ``likes"
library(dplyr)
tmls2 <-
  tmls %>%
  select(text, favorite_count)

most <- max(tmls2$favorite_count)

tmls2 %>%
  filter(favorite_count == most)

### Create a DFM from the text of these tweets
library(quanteda)

data <- dfm(tmls$text)
View(data)

### After removing stopwords, what word did the celebrity tweet most often?
data2 <- dfm(tmls$text, remove = stopwords("english"))
max(data2)
View(data2)
#Answer: The "word" the celebrity used the most was a !.