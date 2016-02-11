# SYS 6018 Final Project
# Predicting Collapsed Answers on Quora
# Shannon Mitchell, Mason Montgomery, Akshat Verma, Mike Voltmer
# Due 10 Dec 2015


setwd("/Users/bi523mv/Downloads")

library(caret)
library(entropy)
library(httr)
library(kernlab)
library(koRpus)
library(randomForest)
library(RCurl)
library(rvest)
library(stringr)
library(tm)
library(urltools)

# read in data
dat <- read.delim("quora.csv", header = FALSE, sep = "|", quote = "", stringsAsFactors = FALSE, na.strings = c("Anon", "NA", ""))

# create column headers
colnames(dat) <- c("Class", "Topic", "Question", "Answer", "Upvotes", "Views", "Questions.by.Answerer", 
                   "Answers.by.Answerer", "Posts.by.Answerer", "Followers.of.Answerer")

# remove rows that are not being read in properly
dat <- dat[grepl("[A-Z]", dat$Class), ]

# remove rows that have character strings in any of these 5 columns
dat <- dat[!grepl("[A-Z]", dat$Upvotes), ]
dat <- dat[!grepl("[A-Z]", dat$Questions.by.Answerer), ]
dat <- dat[!grepl("[A-Z]", dat$Answers.by.Answerer), ]
dat <- dat[!grepl("[A-Z]", dat$Posts.by.Answerer), ]
dat <- dat[!grepl("[A-Z]", dat$Followers.of.Answerer), ]

# remove rows that have empty answers
dat <- dat[dat$Answer!="\"\"",]

# remove commas from last 4 columns & convert to numeric
dat <- data.frame(dat[1:6], apply(dat[7:10], 2, function(x) as.numeric(gsub(",","", x))))

# convert Class column to factor
dat$Class <- as.factor(dat$Class)

# convert Views column to numeric (has "k" to represent 000's)
k <- which(grepl("k", dat$View))
dat$Views <- as.numeric(gsub("k","", dat$Views))
dat$Views[k] <- ifelse(k %in% dat$Views, dat$Views[k] * 1000, dat$Views[k])

# convert Upvotes to numeric (this also has "k" to represent 000's)
k2 <- which(grepl("k", dat$Upvotes))
dat$Upvotes <- as.numeric(gsub("k","", dat$Upvotes))
dat$Upvotes[k2] <- ifelse(k2 %in% dat$Upvotes, dat$Upvotes[k] * 1000, dat$Upvotes[k])


####
# feature engineering
####

# character and word stats
dat$n.char <- str_count(dat$Answer, boundary("character", skip_word_none = FALSE)) # includes spaces
dat$n.words <- str_count(dat$Answer, boundary("word", skip_word_none = TRUE))      # does not include spaces
dat$n.sentences <- str_count(dat$Answer, boundary("sentence", skip_word_none = FALSE))
dat$n.spaces <- str_count(dat$Answer, " ")
dat$percen.upper <- (sapply(regmatches(dat$Answer, gregexpr("[A-Z]", dat$Answer, perl=TRUE)),length))/
  (sapply(regmatches(dat$Answer, gregexpr("[A-Za-z]", dat$Answer, perl=TRUE)),length))
dat$percen.lower <- rep(1,nrow(dat)) - dat$percen.upper
dat$len.ratio <- str_count(dat$Question, boundary("character", skip_word_none = FALSE))/dat$n.char

# what type of question? (Who, What, Where, etc)

questionWord <- function(text, Qword) {
  if (str_detect(tolower(text), tolower(Qword)))
    return(as.factor(1))
  else return(as.factor(0))
}

# 0/1 for each question word
dat$who <- apply(as.data.frame(dat$Question), 1, questionWord, "who")      # includes who and whose
dat$what <- apply(as.data.frame(dat$Question), 1, questionWord, "what")
dat$when <- apply(as.data.frame(dat$Question), 1, questionWord, "when")
dat$where <- apply(as.data.frame(dat$Question), 1, questionWord, "where")
dat$why <- apply(as.data.frame(dat$Question), 1, questionWord, "why")
dat$how <- apply(as.data.frame(dat$Question), 1, questionWord, "how")
dat$which <- apply(as.data.frame(dat$Question), 1, questionWord, "which")


# readability indices

# add check for answer < 100 tokens, if not return NA?
getReadability <- function(text, index.name) {
#   # return NULL for empty answer (breaks readability index function)
#   if (text == "\"\"") return(NULL)
#   else {
  tokens <- tokenize(text, lang="en", format="obj")
  read.summary <- summary(readability(tokens, index=index.name))
  return(read.summary[1,"grade"])
#  }
}

dat$coleman.liau <- apply(as.data.frame(dat$Answer), 1, getReadability, "Coleman.Liau")
dat$fog <- apply(as.data.frame(dat$Answer), 1, getReadability, "FOG")

# convert to right type
dat$coleman.liau <- as.numeric(dat$coleman.liau)
dat$fog <- as.numeric(dat$fog)

getEntropy <- function(text) {
  words <- str_split(tolower(text), boundary("word", skip_word_none = TRUE))
  freq <- table(words)/length(words)
  entropy <- entropy(freq)
  return(entropy)
}

dat$entropy <- apply(as.data.frame(dat$Answer), 1, getEntropy)

######
# get percentage of slang and stopwords (denominator = total words)
#####

# get text slang from netlingo.com list
url <- "http://www.netlingo.com/acronyms.php"
source.page <- read_html(url)
slang.list <- source.page %>%
  html_nodes("li > span") %>%
  html_text()

# clean slang list to remove terms with punctuation
slang.list <- tolower(slang.list)
slang.list <- slang.list[!grepl("[[:punct:]]", slang.list)]

countWords <- function(text, wordList) {
  wordCount <- 0
  for (word in wordList) {
    wordAndSpace <- paste0(" ", word, " ")
    if (grepl(wordAndSpace,text)) wordCount <- wordCount + 1
  }
  return(wordCount)
}

stopword.list <- stopwords(kind = "en")
stopword.count <- apply(as.data.frame(dat$Answer), 1, countWords, stopword.list)
slang.count <- apply(as.data.frame(dat$Answer), 1, countWords, slang.list)

dat$percen.stopwords <- stopword.count/dat$n.words
dat$percen.slang <- slang.count/dat$n.words


######  Semantic Similarity ########
get_semantic_similarity <- function(dat){
  phrase1 = gsub("[^[:alnum:] ]", "", dat[3])
  phrase2 = gsub("[^[:alnum:] ]", "", dat[4])
  url = param_set('http://swoogle.umbc.edu/StsService/GetStsSim?operation=api', 'phrase1', curlEscape(phrase1))
  url = param_set(url, 'phrase2', curlEscape(phrase2))
  r <- POST(as.character(url))
  return (str_trim(as.character(r)))
}
semantic_similarity_index = vector(mode="character",length = 4554)
semantic_similarity_index = append(semantic_similarity_index,unlist(apply(dat,1,get_semantic_similarity)))
semantic_similarity_index = unname(semantic_similarity_index)
dat$semantic_similarity_index = semantic_similarity_index


# remove rows that have characters in the semantic index column
dat <- dat[!grepl("[a-z]", dat$semantic_similarity_index), ]

dat$semantic_similarity_index <- as.numeric(dat$semantic_similarity_index)
dat$Topic <- as.factor(dat$Topic)

# remove rows that are NA in the "...Answerer, percen, and index" columns
dat <- dat[complete.cases(dat[, c("Questions.by.Answerer", "Answers.by.Answerer", "Posts.by.Answerer",
                                  "Followers.of.Answerer", "percen.upper", "percen.lower", "semantic_similarity_index")]), ]

####################################
#
# Modeling starts here
# 
####################################

# split into train test
set.seed(12345)
trainIndex <- createDataPartition(dat$Class, p = .8,
                                  list = FALSE,
                                  times = 1)
train <- dat[trainIndex, ]
test <- dat[-trainIndex,]

# dimensions:
# train: 3420   30
# test: 854  30

# had to remove Question, Answer (charaacters) and percen.lower (linear dependence w/ percen.upper)
# and Views (NA's for collapsed answers)
form <- as.formula(Class ~ Topic + Upvotes + Questions.by.Answerer + Answers.by.Answerer + Posts.by.Answerer +
                     Followers.of.Answerer + n.char + n.words + n.sentences + n.spaces + percen.upper + 
                     len.ratio + who + what + when + where + why + how + which + coleman.liau + fog + entropy + percen.stopwords +
                     percen.slang +	semantic_similarity_index)

# run logistic regression - "does not converge warning"
mod <- glm(form, data = train, family = 'binomial')

summary(mod)

# make predictions - had to do na.omit because some of the test variables containted NA's
preds <- predict(mod, test, type = 'response', na.action = na.omit)
predsBinary <- ifelse(preds > .5, 'C', 'A')

# confusion matrix
confusionMatrix(predsBinary, test$Class)


########## SVM ######################

svm.fit <- ksvm(form, data = train, type="C-svc", kernel="rbfdot", C=1)
preds.SVM <- predict(svm.fit, test, type = 'response')
confusionMatrix(preds.SVM, test$Class)

########## Random Forst #################

variables <- c("Topic","Upvotes","Questions.by.Answerer","Answers.by.Answerer","Posts.by.Answerer","Followers.of.Answerer","n.char","n.words","n.sentences","n.spaces","percen.upper","percen.lower","len.ratio","who","what","when","where","why","how","which","coleman.liau","fog","entropy","percen.stopwords","percen.slang","semantic_similarity_index")
rf.fit <- randomForest(x=train[variables],y = train$Class, ntree = 500,mtry = 10,do.trace = TRUE,replace = TRUE) #norm.votes

#Validation
pred.rf <- predict(rf.fit,test[variables])
confusionMatrix(pred.rf, test$Class)

#Variable Importance
feat = importance(rf.fit)
sort(feat,decreasing=T)

#Feature Selection
sel_variables <- c("Topic","Upvotes","Answers.by.Answerer","Followers.of.Answerer","n.char","n.words","n.spaces","len.ratio","coleman.liau","fog","entropy","percen.stopwords","semantic_similarity_index")

rf.fit2 <- randomForest(x=train[sel_variables],y = train$Class, ntree = 500,mtry = 10,do.trace = TRUE,replace = TRUE) #norm.votes
pred.rf2 <- predict(rf.fit2,test[variables])
confusionMatrix(pred.rf2, test$Class)
