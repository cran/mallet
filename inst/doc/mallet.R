## ---- echo=FALSE--------------------------------------------------------------
options(rmarkdown.html_vignette.check_title = FALSE)

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("mallet")

## ---- eval=FALSE--------------------------------------------------------------
#  options(java.parameters = "-Xmx4g")

## -----------------------------------------------------------------------------
library(mallet)

## -----------------------------------------------------------------------------
# Note this is the path to the folder where the stoplists are stored in the R package.
# Change this path to another directory to read other txt files into R.
directory <- system.file("stoplists", package = "mallet")

files_in_directory <- list.files(directory, full.names = TRUE)

txt_file_content <- character(length(files_in_directory))
for(i in seq_along(files_in_directory)){
  txt_file_content[i] <- paste(readLines(files_in_directory[i]), collapse = "\n")
}
# We can check the content with str()
str(txt_file_content)

## -----------------------------------------------------------------------------
library(dplyr)
data(sotu)
sotu[["text"]][1:2]

## -----------------------------------------------------------------------------
mallet_supported_stoplists()
stopwords_en_file_path <- mallet_stoplist_file_path("en")

## -----------------------------------------------------------------------------
sotu.instances <- 
  mallet.import(id.array = row.names(sotu), 
                text.array = sotu[["text"]], 
                stoplist = stopwords_en_file_path,
                token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

## -----------------------------------------------------------------------------
sotu.instances.short <- 
  mallet.import(text.array = sotu[["text"]])

## -----------------------------------------------------------------------------
stop_vector <- readLines(stopwords_en_file_path)
sotu.instances.short <- 
  mallet.import(text.array = sotu[["text"]], 
                stoplist = stop_vector)

## -----------------------------------------------------------------------------
topic.model <- MalletLDA(num.topics=10, alpha.sum = 1, beta = 0.1)

## -----------------------------------------------------------------------------
topic.model$loadDocuments(sotu.instances)

## -----------------------------------------------------------------------------
vocabulary <- topic.model$getVocabulary()
head(vocabulary)

## -----------------------------------------------------------------------------
word_freqs <- mallet.word.freqs(topic.model)
head(word_freqs)

## -----------------------------------------------------------------------------
topic.model$setAlphaOptimization(20, 50)

## -----------------------------------------------------------------------------
topic.model$train(200)

## -----------------------------------------------------------------------------
topic.model$maximize(10)

## -----------------------------------------------------------------------------
doc.topics <- mallet.doc.topics(topic.model, smoothed=TRUE, normalized=TRUE)
topic.words <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)

## -----------------------------------------------------------------------------
mallet.top.words(topic.model, word.weights = topic.words[2,], num.top.words = 5)

## -----------------------------------------------------------------------------
docs <- which(doc.topics[,2] > 0.50)
doc_size <- nchar(sotu[["text"]])[docs]
idx <- docs[order(doc_size, decreasing = TRUE)[1]]
sotu[["text"]][idx]


## -----------------------------------------------------------------------------
post1975_topic_words <- mallet.subset.topic.words(topic.model, sotu[["year"]] > 1975)
mallet.top.words(topic.model, word.weights = post1975_topic_words[2,], num.top.words = 5)

## ---- fig.height=5, fig.width=5-----------------------------------------------
topic_labels <- mallet.topic.labels(topic.model, num.top.words = 2)
topic_clusters <- mallet.topic.hclust(doc.topics, topic.words, balance = 0.5)
plot(topic_clusters, labels=topic_labels, xlab = "", )

## -----------------------------------------------------------------------------
state_file <- file.path(tempdir(), "temp_mallet_state.gz")
save.mallet.state(topic.model = topic.model, state.file = state_file)

## -----------------------------------------------------------------------------
doc.topics.counts <- mallet.doc.topics(topic.model, smoothed=FALSE, normalized=FALSE)

rm(topic.model)

## -----------------------------------------------------------------------------
new.topic.model <- MalletLDA(num.topics=10, alpha.sum = 1, beta = 0.1)
new.topic.model$loadDocuments(sotu.instances)
load.mallet.state(topic.model = new.topic.model, state.file = state_file)

doc.topics.counts[1:3, 1:6]
mallet.doc.topics(new.topic.model, smoothed=FALSE, normalized=FALSE)[1:3, 1:6]

## -----------------------------------------------------------------------------
model_file <- file.path(tempdir(), "temp_mallet.model")
mallet.topic.model.save(new.topic.model, model_file)
read.topic.model <- mallet.topic.model.read(model_file)

doc.topics.counts[1:3, 1:6]
mallet.doc.topics(read.topic.model, smoothed=FALSE, normalized=FALSE)[1:3, 1:6]

