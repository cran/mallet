#' @title
#' An R Wrapper for the Java Mallet Topic Modeling Toolkit
#'
#' @description
#' An R interface for the Java Machine Learning for Language Toolkit (mallet)
#' <http://mallet.cs.umass.edu/> to estimate probabilistic topic models, such
#' as Latent Dirichlet Allocation. We can use the R package to read textual data into mallet from R objects,
#' run the Java implementation of mallet directly in R, and extract results
#' as R objects. The Mallet toolkit  has many functions, this wrapper focuses
#' on the topic modeling sub-package written by David Mimno. The package uses
#' the rJava package to connect to a JVM.
#'
#' @references
#' The model, Latent Dirichlet allocation (LDA):
#' \cite{David M Blei, Andrew Ng, Michael Jordan. Latent Dirichlet Allocation. J. of Machine Learning Research, 2003.}
#'
#' The Java toolkit:
#' \cite{Andrew Kachites McCallum. The Mallet Toolkit. 2002.}
#'
#' Details of the fast sparse Gibbs sampling algorithm:
#' \cite{Limin Yao, David Mimno, Andrew McCallum. Streaming Inference for Latent Dirichlet Allocation. KDD, 2009.}
#'
#' Hyperparameter optimization:
#' \cite{Hanna Wallach, David Mimno, Andrew McCallum. Rethinking LDA: Why Priors Matter. NIPS, 2010.}
#'
#' @name mallet-package
#' @docType package
#' @import rJava
NULL

#' State of the Union Adresses.
#'
#' A dataset containing State of the Union Adresses by paragraph from 1946 to 2000.
#'
#' @format A \code{\link[tibble]{tibble}} \code{data.frame} with 6816 rows and 3 variables:
#' \describe{
#'   \item{year}{Year of the adress.}
#'   \item{paragraph}{The paragraph of the address.}
#'   \item{text}{The address content.}
#' }
#' @source \url{https://en.wikipedia.org/wiki/State_of_the_Union}
"sotu"



#' @title
#' Create a Mallet topic model trainer
#'
#' @description
#' This function creates a java cc.mallet.topics.RTopicModel object that wraps a
#' Mallet topic model trainer java object, cc.mallet.topics.ParallelTopicModel.
#' Note that you can call any of the methods of this java object as properties.
#' In the example below, I make a call directly to the
#' \code{topic.model$setAlphaOptimization(20, 50)} java method,
#' which passes this update to the model itself.
#'
#' @param num.topics
#' The number of topics to use. If not specified, this defaults to 10.
#' @param alpha.sum
#' This is the magnitude of the Dirichlet prior over the topic distribution of a document.
#' The default value is 5.0. With 10 topics, this setting leads to a Dirichlet with
#' parameter \eqn{\alpha_k = 0.5}. You can intuitively think of this parameter as a
#' number of "pseudo-words", divided evenly between all topics, that are present in
#' every document no matter how the other words are allocated to topics. This is an
#' initial value, which may be changed during training if hyperparameter
#' optimization is active.
#' @param beta
#' This is the per-word weight of the Dirichlet prior over topic-word distributions.
#' The magnitude of the distribution (the sum over all words of this parameter) is
#' determined by the number of words in the vocabulary. Again, this value may change
#' due to hyperparameter optimization.
#'
#' @returns a \code{cc.mallet.topics.RTopicModel} object
#'
#' @examples
#' \dontrun{
#' # Read in sotu example data
#' data(sotu)
#' sotu.instances <-
#'    mallet.import(id.array = row.names(sotu),
#'                  text.array = sotu[["text"]],
#'                  stoplist = mallet_stoplist_file_path("en"),
#'                  token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
#'
#' # Create topic model
#' topic.model <- MalletLDA(num.topics=10, alpha.sum = 1, beta = 0.1)
#' topic.model$loadDocuments(sotu.instances)
#'
#' # Train topic model
#' topic.model$train(200)
#'
#' # Extract results
#' doc_topics <- mallet.doc.topics(topic.model, smoothed=TRUE, normalized=TRUE)
#' topic_words <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)
#' top_words <- mallet.top.words(topic.model, word.weights = topic_words[2,], num.top.words = 5)
#' }
#'
#' @export
MalletLDA <- function(num.topics = 10, alpha.sum = 5.0, beta = 0.01) {
  checkmate::assert_int(num.topics, lower = 2)
  checkmate::assert_number(alpha.sum, lower = 0)
  checkmate::assert_number(beta, lower = 0)

  rJava::.jnew("cc/mallet/topics/RTopicModel", num.topics, alpha.sum, beta)
}



#' @title
#' Retrieve a matrix of words weights for topics
#'
#' @description
#' This function returns a matrix with one row for every topic
#' and one column for every word in the vocabulary.
#'
#' @param topic.model
#' A \code{cc.mallet.topics.RTopicModel} object created by \code{\link{MalletLDA}}.
#' @param normalized
#' If \code{TRUE}, normalize the rows so that each topic sums to one. If \code{FALSE},
#' values will be integers (possibly plus the smoothing constant) representing the
#' actual number of words of each type in the topics.
#' @param smoothed
#' If \code{TRUE}, add the smoothing parameter for the model (initial value specified as
#' \code{beta} in \code{MalletLDA}). If \code{FALSE}, many values will be zero.
#'
#' @returns a number of topics by vocabulary size matrix.
#'
#' @examples
#' \dontrun{
#' # Read in sotu example data
#' data(sotu)
#' sotu.instances <-
#'    mallet.import(id.array = row.names(sotu),
#'                  text.array = sotu[["text"]],
#'                  stoplist = mallet_stoplist_file_path("en"),
#'                  token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
#'
#' # Create topic model
#' topic.model <- MalletLDA(num.topics=10, alpha.sum = 1, beta = 0.1)
#' topic.model$loadDocuments(sotu.instances)
#'
#' # Train topic model
#' topic.model$train(200)
#'
#' # Extract results
#' doc_topics <- mallet.doc.topics(topic.model, smoothed=TRUE, normalized=TRUE)
#' topic_words <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)
#' top_words <- mallet.top.words(topic.model, word.weights = topic_words[2,], num.top.words = 5)
#' }
#'
#' @export
mallet.topic.words <- function(topic.model, normalized=FALSE, smoothed=FALSE) {
  rJava::.jevalArray(topic.model$getTopicWords(normalized, smoothed), simplify=T)
}


#' @title
#' Retrieve a matrix of topic weights for every document
#'
#' @description
#' This function returns a matrix with one row for every document and one
#' column for every topic.
#'
#' @param topic.model
#' A \code{cc.mallet.topics.RTopicModel} object created by \code{\link{MalletLDA}}.
#' @param normalized
#' If \code{TRUE}, normalize the rows so that each document sums to one. If \code{FALSE},
#' values will be integers (possibly plus the smoothing constant) representing the
#' actual number of words of each topic in the documents.
#' @param smoothed
#' If \code{TRUE}, add the smoothing parameter for the model (initial value specified as
#' \code{alpha.sum} in \code{MalletLDA}). If \code{FALSE}, many values will be zero.
#'
#' @returns a number of documents by number of topics matrix.
#'
#' @examples
#' \dontrun{
#' # Read in sotu example data
#' data(sotu)
#' sotu.instances <-
#'    mallet.import(id.array = row.names(sotu),
#'                  text.array = sotu[["text"]],
#'                  stoplist = mallet_stoplist_file_path("en"),
#'                  token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
#'
#' # Create topic model
#' topic.model <- MalletLDA(num.topics=10, alpha.sum = 1, beta = 0.1)
#' topic.model$loadDocuments(sotu.instances)
#'
#' # Train topic model
#' topic.model$train(200)
#'
#' # Extract results
#' doc_topics <- mallet.doc.topics(topic.model, smoothed=TRUE, normalized=TRUE)
#' topic_words <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)
#' top_words <- mallet.top.words(topic.model, word.weights = topic_words[2,], num.top.words = 5)
#' }
#'
#'
#' @export
mallet.doc.topics <- function(topic.model, normalized=FALSE, smoothed=FALSE) {
  rJava::.jevalArray(topic.model$getDocumentTopics(normalized, smoothed), simplify=T)
}


#' @title
#' Descriptive statistics of word frequencies
#'
#' @description
#' This method returns a data frame with one row for each unique vocabulary word,
#' and three columns: the word as a \code{character} value, the total number of
#' tokens of that word type, and the total number of documents that contain that
#' word at least once. This information can be useful in identifying candidate
#' stopwords.
#'
#' @param topic.model
#' A \code{cc.mallet.topics.RTopicModel} object created by \code{\link{MalletLDA}}.
#'
#' @seealso
#' \code{\link{MalletLDA}}
#'
#' @returns a \code{data.frame} with the word type (\code{word}), the word frequency (\code{word.freq}), and the document frequency (\code{doc.freq})
#'
#' @examples
#' \dontrun{
#' # Read in sotu example data
#' data(sotu)
#' sotu.instances <-
#'    mallet.import(id.array = row.names(sotu),
#'                  text.array = sotu[["text"]],
#'                  stoplist = mallet_stoplist_file_path("en"),
#'                  token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
#'
#' # Create topic model
#' topic.model <- MalletLDA(num.topics=10, alpha.sum = 1, beta = 0.1)
#' topic.model$loadDocuments(sotu.instances)
#'
#' # Get word frequencies
#' word_freqs <- mallet.word.freqs(topic.model)
#'
#' }
#'
#' @export
mallet.word.freqs <- function(topic.model) {
  word.freqs <- rJava::.jevalArray(topic.model$getWordFrequencies(), simplify=T)
  data.frame(word = topic.model$getVocabulary(), word.freq = word.freqs[,1], doc.freq = word.freqs[,2])
}


#' @title
#' Estimate topic-word distributions from a sub-corpus
#'
#' @description
#' This function returns a matrix of word probabilities for each topic similar to
#' \code{\link{mallet.topic.words}}, but estimated from a subset of the documents
#' in the corpus. The model assumes that topics are the same no matter where they
#' are used, but we know this is often not the case. This function lets us test
#' whether some words are used more or less than we expect in a particular set
#' of documents.
#'
#' @param topic.model
#' A \code{cc.mallet.topics.RTopicModel} object created by \code{\link{MalletLDA}}.
#' @param subset.docs
#' A logical vector of \code{TRUE}/\code{FALSE} values specifying which documents should
#' be used/included and which should be ignored.
#' @param normalized
#' If \code{TRUE}, normalize the rows so that each topic sums to one. If \code{FALSE},
#' values will be integers (possibly plus the smoothing constant) representing
#' the actual number of words of each type in the topics.
#' @param smoothed
#' If \code{TRUE}, add the smoothing parameter for the model (initial value specified
#' as \code{beta} in \code{MalletLDA}). If \code{FALSE}, many values will be zero.
#'
#' @seealso
#' \code{\link{mallet.topic.words}}
#'
#' @returns a number of topics by vocabulary size matrix for the the included documents.
#'
#' @examples
#' \dontrun{
#' # Read in sotu example data
#' data(sotu)
#' sotu.instances <-
#'    mallet.import(id.array = row.names(sotu),
#'                  text.array = sotu[["text"]],
#'                  stoplist = mallet_stoplist_file_path("en"),
#'                  token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
#'
#' # Create topic model
#' topic.model <- MalletLDA(num.topics=10, alpha.sum = 1, beta = 0.1)
#' topic.model$loadDocuments(sotu.instances)
#'
#' # Train topic model
#' topic.model$train(200)
#'
#' # Extract subcorpus topic word matrix
#' post1975_topic_words <- mallet.subset.topic.words(topic.model, sotu[["year"]] > 1975)
#' mallet.top.words(topic.model, word.weights = post1975_topic_words[2,], num.top.words = 5)
#' }
#'
#' @export
mallet.subset.topic.words <- function(topic.model, subset.docs, normalized=FALSE, smoothed=FALSE) {
  rJava::.jevalArray(topic.model$getSubCorpusTopicWords(subset.docs, normalized, smoothed), simplify=T)
}


#' @title
#' Get the most probable words and their probabilities for one topic
#'
#' @description
#' This function returns a data frame with two columns, one containing the most
#' probable words as character values, the second containing the weight assigned
#' to that word in the word weights vector you supplied.
#'
#' @param topic.model
#' A \code{cc.mallet.topics.RTopicModel} object created by \code{\link{MalletLDA}}.
#'
#' @param word.weights
#' A vector of word weights for one topic, usually a row from the \code{topic.words}
#' matrix from \code{mallet.topic.words}.
#' @param num.top.words
#' The number of most probable words to return. If not specified, defaults to 10.
#'
#' @returns a \code{data.frame} with the top terms (\code{term}) and their weights/probability (\code{weight}).
#'
#' @examples
#' \dontrun{
#' # Read in sotu example data
#' data(sotu)
#' sotu.instances <-
#'    mallet.import(id.array = row.names(sotu),
#'                  text.array = sotu[["text"]],
#'                  stoplist = mallet_stoplist_file_path("en"),
#'                  token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
#'
#' # Create topic model
#' topic.model <- MalletLDA(num.topics=10, alpha.sum = 1, beta = 0.1)
#' topic.model$loadDocuments(sotu.instances)
#'
#' # Train topic model
#' topic.model$train(200)
#'
#' # Extract top words
#' top_words <- mallet.top.words(topic.model, word.weights = topic_words[2,], num.top.words = 5)
#' }
#'
#' @export
mallet.top.words <- function(topic.model, word.weights, num.top.words=10) {
  top.indices <- order(word.weights, decreasing=T)[1:num.top.words]
  data.frame(term = topic.model$getVocabulary()[top.indices], weight = word.weights[top.indices], stringsAsFactors=F)
}

#' @title
#' Import text documents into Mallet format
#'
#' @description
#' This function takes an array of document IDs and text files (as character strings)
#' and converts them into a Mallet instance list.
#'
#' @param id.array
#' An array of document IDs. Default is \code{text.array} index.
#' @param text.array
#' A character vector with each element containing a document.
#' @param stoplist
#' The name of a file containing stopwords (words to ignore), one per line, or a character vector containing stop words.
#' If the file is not in the current working directory, you may need to include a full path.
#' Default is no stoplist.
#' @param preserve.case
#' By default, the input text is converted to all lowercase.
#' @param token.regexp
#' A quoted string representing a regular expression that defines a token. The default
#' is one or more unicode letter: "[\\\\p\{L\}]+". Note that special characters must
#' have double backslashes.
#'
#' @seealso
#' \code{\link{mallet.word.freqs}} returns term and document frequencies, which may be useful in selecting stopwords.
#'
#' @returns a \code{cc/mallet/types/InstanceList} object.
#'
#' @examples
#' \dontrun{
#' # Read in sotu example data
#' data(sotu)
#' sotu.instances <-
#'    mallet.import(id.array = row.names(sotu),
#'                  text.array = sotu[["text"]],
#'                  stoplist = mallet_stoplist_file_path("en"),
#'                  token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
#'
#' }
#'
#' @export
mallet.import <- function(id.array = NULL, text.array, stoplist = "", preserve.case=FALSE, token.regexp="[\\p{L}]+") {
  checkmate::assert_character(id.array, null.ok = TRUE, len = length(text.array))
  checkmate::assert_character(text.array, any.missing = FALSE)
  checkmate::assert(checkmate::check_character(stoplist, any.missing = FALSE),
                    checkmate::check_file_exists(stoplist))
  checkmate::assert_flag(preserve.case)
  checkmate::assert_string(token.regexp)

  if(checkmate::test_file_exists(stoplist)) {
    stoplist.file <- normalizePath(stoplist)
  } else {
    tmp_file <- tempfile(fileext = ".txt")
    writeLines(text = stoplist, tmp_file)
    stoplist.file <- tmp_file
  }
  if(is.null(id.array)){
    id.array <- as.character(1:length(text.array))
  }

  token.pattern <- rJava::J("java/util/regex/Pattern")$compile(token.regexp)
  pipe.list <- rJava::.jnew("java/util/ArrayList")
  pipe.list$add(rJava::.jnew("cc/mallet/pipe/CharSequence2TokenSequence", token.pattern))
  if (! preserve.case) { pipe.list$add(rJava::.jnew("cc/mallet/pipe/TokenSequenceLowercase")) }
  pipe.list$add(rJava::.jnew("cc/mallet/pipe/TokenSequenceRemoveStopwords", rJava::.jnew("java/io/File", stoplist.file), "UTF-8", FALSE, FALSE, FALSE))
  pipe.list$add(rJava::.jnew("cc/mallet/pipe/TokenSequence2FeatureSequence"))
  #pipe.list$add(rJava::.jnew("cc/mallet/pipe/PrintInputAndTarget"))

  pipe <- rJava::.jnew("cc/mallet/pipe/SerialPipes", rJava::.jcast(pipe.list, "java/util/Collection"))

  instances <- rJava::.jnew("cc/mallet/types/InstanceList", rJava::.jcast(pipe, "cc/mallet/pipe/Pipe"))

  rJava::J("cc/mallet/topics/RTopicModel")$addInstances(instances, id.array, text.array)

  return(instances)
}

# mallet.read.dir() function, created by Dan Bowen
# This function takes a directory path as its only argument
# ... and returns a data.frame() with 2 columns: <id> & <text>.
# ... This data.frame() has as many rows as there are files in the Dir.
# The form of this functions return attempts to conform to that
# ... used by the mallet.import() function, available in the 'mallet' R package

#' @title
#' Import documents from a directory into Mallet format
#'
#' @author Dan Bowen
#'
#' @description
#'  This function takes a directory path as its only argument and returns a
#'  \code{data.frame} with two columns: <id> & <text>,
#'  which can be passed to the \code{mallet.import} function.
#'  This \code{data.frame} has as many rows as there are files in the \code{Dir}.
#'
#' @param Dir
#' The path to a directory containing one document per file.
#'
#' @note
#' This function was contributed to RMallet by Dan Bowen.
#'
#' @seealso
#' \code{\link{mallet.import}}
#'
#' @returns a \code{data.frame} with file \code{id} and \code{text} content.
#'
#' @examples
#' \dontrun{
#' directory <- system.file("stoplists", package = "mallet")
#' stoplists <- mallet.read.dir(directory)
#' }
#'
#' @export
mallet.read.dir <- function(Dir) {
  .Deprecated()
  # get Dir Files (filepaths)
  Files <- file.path(Dir, list.files(Dir))
  # for each File:
  mallet.read.file <- function(File) {
    # read File, per line
    Lines <- scan(File, what='character', sep='\n', quote='')
    # paste Lines back together with '\n'
    string <- paste(Lines, collapse='\n')
    # return data.frame
    data.frame(id=File, text=string, stringsAsFactors=F)
  }
  # apply the above function to the Files in the dir
  # ... rbind the resulting list of data.frames together
  do.call(rbind, lapply(Files, mallet.read.file))
}

## Get a vector containing short names for all the topics

#' @title
#' Get strings containing the most probable words for each topic
#'
#' @description
#' This function returns a vector of strings, one for each topic, with the
#' most probable words in that topic separated by spaces.
#'
#' @param topic.model
#' A \code{cc.mallet.topics.RTopicModel} object created by \code{\link{MalletLDA}}.
#' @param topic.words
#' The matrix of topic-word weights returned by \code{\link{mallet.topic.words}}
#' Default (NULL) is to use the \code{topic.model} to extract the \code{topic.words}.
#' @param num.top.words
#' The number of words to include for each topic. Defaults to 3.
#' @param ...
#' Further arguments supplied to \code{\link{mallet.topic.words}}.
#'
#' @returns a character vector with one element per topic
#'
#' @seealso
#' \code{\link{mallet.topic.words}} produces topic-word weights.
#' \code{\link{mallet.top.words}} produces a data frame for a single topic.
#'
#' @examples
#' \dontrun{
#' # Read in sotu example data
#' data(sotu)
#' sotu.instances <-
#'    mallet.import(id.array = row.names(sotu),
#'                  text.array = sotu[["text"]],
#'                  stoplist = mallet_stoplist_file_path("en"),
#'                  token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
#'
#' # Create topic model
#' topic.model <- MalletLDA(num.topics=10, alpha.sum = 1, beta = 0.1)
#' topic.model$loadDocuments(sotu.instances)
#'
#' # Train topic model
#' topic.model$train(200)
#'
#' # Create hiearchical clusters of topics
#' doc_topics <- mallet.doc.topics(topic.model, smoothed=TRUE, normalized=TRUE)
#' topic_words <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)
#' topic_labels <- mallet.topic.labels(topic.model)
#' plot(mallet.topic.hclust(doc_topics, topic_words, balance = 0.3), labels=topic_labels)
#' }
#'
#' @export
mallet.topic.labels <- function(topic.model, topic.words = NULL, num.top.words=3, ...) {
  if(is.null(topic.words)){
    topic.words <- mallet.topic.words(topic.model, ...)
  }
  topic.model
  n.topics <- dim(topic.words)[1]
  topics.labels <- rep("", n.topics)
  for (topic in 1:n.topics) topics.labels[topic] <- paste(mallet.top.words(topic.model, topic.words[topic,], num.top.words)$term, collapse=" ")
  topics.labels
}

## Return a hierarchical clustering of topics.

#' @title
#' Return a hierarchical clustering of topics
#'
#' @description
#' Returns a hierarchical clustering of topics that can be plotted as a dendrogram.
#' There are two ways of measuring topic similarity: topics may contain the some of
#' the same words, or the may appear in some of the same documents. The \code{balance} parameter allows you to interpolate between the similarities determined by these two methods.
#'
#' @param doc.topics
#' A documents by topics matrix of topic probabilities (see \code{\link{mallet.doc.topics}}).
#' @param topic.words
#' A topics by words matrix of word probabilities (see \code{\link{mallet.topic.words}}) .
#' @param balance
#' A value between 0.0 (use only document-level similarity)
#' and 1.0 (use only word-level similarity).
#' @param method method to use in \code{\link[stats]{dist}} to compute distance between topics.
#' Defaults to \code{euclidian}.
#' @param ...
#' Further arguments for \code{\link[stats]{hclust}}.
#'
#' @seealso
#' This function uses data matrices from \code{\link{mallet.doc.topics}}
#' and \code{\link{mallet.topic.words}} using the \code{\link{hclust}} function.
#'
#' @returns An object of class \code{\link{hclust}} which describes the tree produced by the clustering process.
#'
#' @examples
#' \dontrun{
#' # Read in sotu example data
#' data(sotu)
#' sotu.instances <-
#'    mallet.import(id.array = row.names(sotu),
#'                  text.array = sotu[["text"]],
#'                  stoplist = mallet_stoplist_file_path("en"),
#'                  token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
#'
#' # Create topic model
#' topic.model <- MalletLDA(num.topics=10, alpha.sum = 1, beta = 0.1)
#' topic.model$loadDocuments(sotu.instances)
#'
#' # Train topic model
#' topic.model$train(200)
#'
#' # Create hiearchical clusters of topics
#' doc_topics <- mallet.doc.topics(topic.model, smoothed=TRUE, normalized=TRUE)
#' topic_words <- mallet.topic.words(topic.model, smoothed=TRUE, normalized=TRUE)
#' topic_labels <- mallet.topic.labels(topic.model)
#' plot(mallet.topic.hclust(doc_topics, topic_words, balance = 0.3), labels=topic_labels)
#' }
#'
#' @export
mallet.topic.hclust <- function(doc.topics, topic.words, balance = 0.3, method = "euclidean", ...) {
  checkmate::assert_matrix(doc.topics, ncols = nrow(topic.words))
  checkmate::assert_matrix(topic.words, nrow = ncol(doc.topics))
  checkmate::assert_number(balance, lower = 0, upper = 1)
  checkmate::assert_string(method)

  ## transpose and normalize the doc topics
  topic.docs <- t(doc.topics)
  topic.docs <- topic.docs / rowSums(topic.docs)

  stats::hclust(balance * stats::dist(topic.words, method = method) + (1.0 - balance) * stats::dist(topic.docs, method = method), ...)
}

#' @title
#' Load (read) and save (write) a topic from a file
#'
#' @description
#' This function returns the topic model loaded from a file or stores a topic model to file.
#'
#' @param filename The mallet topic model file
#' @param topic.model
#' A \code{cc.mallet.topics.RTopicModel} object created by \code{\link{MalletLDA}}.
#'
#' @export
mallet.topic.model.read <- function(filename) {
  rJava::J("cc/mallet/topics/RTopicModel")$read(rJava::.jnew("java/io/File", filename))
}

#' @rdname mallet.topic.model.read
#' @export
mallet.topic.model.load <- mallet.topic.model.read

#' @rdname mallet.topic.model.read
#' @export
mallet.topic.model.write <- function(topic.model, filename) {
  topic.model$write(rJava::.jnew("java/io/File", filename))
}

#' @rdname mallet.topic.model.read
#' @export
mallet.topic.model.save <- mallet.topic.model.write



#' @title
#' Load and save mallet instances from/to file
#'
#' @description
#' This function returns the topic model loaded from a file.
#'
#' @param filename The filename to save to or load from.
#' @param instances An \code{cc/mallet/types/InstanceList} \code{instanceList} object to save/write to file.
#'
#' @export
save.mallet.instances <- function(instances, filename) {
  instances$save(rJava::.jnew("java/io/File", filename))
}

#' @rdname save.mallet.instances
#' @export
load.mallet.instances <- function(filename) {
  rJava::J("cc.mallet.types.InstanceList")$load(rJava::.jnew("java/io/File", filename))
}
