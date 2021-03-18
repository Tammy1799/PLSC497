
#----------------------------------------
# Set up environment                     ---
#----------------------------------------
# clear global environment
rm(list = ls())
# load required libraries
install.packages("dplyr")
library(dplyr)
install.packages("caret")
library(caret)

# set working directory
setwd("/Users/tammystugan/Documents/GitHub/TAD_2021/TAD_2021/TAD_2021/R lessons")


#----------------------------------------
# 1. Load, clean and inspect data        ---
#----------------------------------------
news_data <- readRDS("news_data.rds")
table(news_data$category)

# let's work with 2 categories
#2 labels weird news and good news
news_samp <- news_data %>% filter(category %in% c("WEIRD NEWS", "GOOD NEWS")) %>% select(headline, category) %>% setNames(c("text", "class"))

# get a sense of how the text looks
dim(news_samp)
head(news_samp$text[news_samp$class == "WEIRD NEWS"]) #with brackets selecting only where news_samp is equal to weird news
head(news_samp$text[news_samp$class == "GOOD NEWS"])
#word weird is gonna predict as something in weird news and something heartworming is gonna appear in good news

# some pre-processing (the rest will let dfm do)
news_samp$text <- gsub(pattern = "'", "", news_samp$text)  # replace apostrophes
news_samp$class <- dplyr::recode(news_samp$class,  "WEIRD NEWS" = "weird", "GOOD NEWS" = "good") #replacing weird news with weird and good news with good

# what's the distribution of classes?
prop.table(table(news_samp$class))

# randomize order (notice how we split below)
#set seed allows us to use the same random sample that we used
set.seed(1984)
news_samp <- news_samp %>% sample_n(nrow(news_samp)) #saying creating sample_n (randomized sample) and nrow new_samp means getting whole dataset
rownames(news_samp) <- NULL


#----------------------------------------
# 4. Support Vector Machine (SVM) using Caret ---
#----------------------------------------
#library(caret)
install.packages("quanteda")
library(quanteda)


# create document feature matrix
news_dfm <- dfm(news_samp$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english")) %>% convert("matrix") #convert dfm into matrix
#dfm is a matrix. dfm is a special kind of object. when converting it to a matrix it is stripped of some of special featurees of dfm

# A. the caret package has it's own partitioning function
set.seed(1984)
#split data into training and test set
ids_train <- createDataPartition(1:nrow(news_dfm), p = 0.8, list = FALSE, times = 1) #gonna look at whole dataset and then take 80% of that
#dataset and run one time
train_x <- news_dfm[ids_train, ] %>% as.data.frame() # train set data
train_y <- news_samp$class[ids_train] %>% as.factor()  # train set labels
test_x <- news_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- news_samp$class[-ids_train] %>% as.factor() # test set labels

# baseline
#prediction needs to be at least this good
baseline_acc <- max(prop.table(table(test_y)))
baseline_acc

# B. define training options (we've done this manually above)
trctrl <- trainControl(method = "none")
trctrl <- trainControl(method = "LOOCV", p = 0.8)
?trainControl
# C. train model (caret gives us access to even more options)
# see: https://topepo.github.io/caret/available-models.html
# svm - linear

install.packages("e1071")
library(e1071)
svm_mod_linear <- train(x = train_x,
                            y = train_y,
                            method = "svmLinear",
                            trControl = trctrl)



####Be sure to save it as a new file, with a new filename!


# 1. Re-run the analysis with a smaller training set and larger test set. Does the accuracy go up or down?

#Answer: The accuracy goes up.

news_dfm <- dfm(news_samp$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english")) %>% convert("matrix") #convert dfm into matrix
#dfm is a matrix. dfm is a special kind of object. when converting it to a matrix it is stripped of some of special featurees of dfm

# A. the caret package has it's own partitioning function
set.seed(1984)
#split data into training and test set
ids_train <- createDataPartition(1:nrow(news_dfm), p = 0.4, list = FALSE, times = 1) #gonna look at whole dataset and then take 80% of that
#dataset and run one time
train_x <- news_dfm[ids_train, ] %>% as.data.frame() # train set data
train_y <- news_samp$class[ids_train] %>% as.factor()  # train set labels
test_x <- news_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- news_samp$class[-ids_train] %>% as.factor() # test set labels

# baseline
#prediction needs to be at least this good
baseline_acc <- max(prop.table(table(test_y)))
baseline_acc







