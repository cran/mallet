% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mallet.R
\name{mallet.top.words}
\alias{mallet.top.words}
\title{Get the most probable words and their probabilities for one topic}
\usage{
mallet.top.words(topic.model, word.weights, num.top.words = 10)
}
\arguments{
\item{topic.model}{A \code{cc.mallet.topics.RTopicModel} object created by \code{\link{MalletLDA}}.}

\item{word.weights}{A vector of word weights for one topic, usually a row from the \code{topic.words}
matrix from \code{mallet.topic.words}.}

\item{num.top.words}{The number of most probable words to return. If not specified, defaults to 10.}
}
\value{
a \code{data.frame} with the top terms (\code{term}) and their weights/probability (\code{weight}).
}
\description{
This function returns a data frame with two columns, one containing the most
probable words as character values, the second containing the weight assigned
to that word in the word weights vector you supplied.
}
\examples{
\dontrun{
# Read in sotu example data
data(sotu)
sotu.instances <-
   mallet.import(id.array = row.names(sotu),
                 text.array = sotu[["text"]],
                 stoplist = mallet_stoplist_file_path("en"),
                 token.regexp = "\\\\p{L}[\\\\p{L}\\\\p{P}]+\\\\p{L}")

# Create topic model
topic.model <- MalletLDA(num.topics=10, alpha.sum = 1, beta = 0.1)
topic.model$loadDocuments(sotu.instances)

# Train topic model
topic.model$train(200)

# Extract top words
top_words <- mallet.top.words(topic.model, word.weights = topic_words[2,], num.top.words = 5)
}

}
