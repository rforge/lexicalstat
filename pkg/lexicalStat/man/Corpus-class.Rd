\name{Corpus}
\alias{Corpus}
\alias{Corpus-class}
\title{The root hierarchy of all classes available for representing
linguistic corpora.}
\description{
  Here is an overview of the class you may use for
  representing corpora with their main associated methods.
}
\details{

 \itemize{
   \item \bold{\code{Corpus}}.
   \itemize{
     \item \bold{\code{\link{CorpusAsFrequencies}}}.
       \itemize{
         \item \bold{\code{\link{FrequencyList}}}. The list of total frequencies of types.
         \item \bold{\code{\link{LexicalTable}}}. A table of sub-frequencies of the types in the parts.
     }
     \item \bold{\code{\link{CorpusAsTokens}}}.
       \itemize{
         \item \bold{\code{\link{FullText}}}. A list of vectors (part) of character elements (tokens).
         \item \bold{\code{\link{Tabulated}}}. Tokens represented in several forms (lemma, pos...) with several possible grouping of tokens (these groupings are not parts: some tokens may not belong to any group).
     }
   }
 }

  Each class is more suited for some task and more suited
  for reading a corpus from a text file: \code{\link{FullText}}
  make easy to read corpus as plain text files;
  \code{\link{Tabulated}} is more suited for reading a tabulated file
  such as produced by a part-of-speech tagger.

  The virtual class \code{\link{CorpusAsFrequencies}}
  groups classes where the actual exhaustive list of tokens
  is not kept, but where frequencies are given instead. The
  virtual class \code{\link{CorpusAsTokens}} groups classes
  where actual tokens are available in their textual order.

  The \code{\link{FrequencyList}} is the less rich
  representation. It does not contains any partition of of
  the corpus in part. Both \code{\link{LexicalTable}} and
  \code{\link{FullText}} contains a partition of the
  corpus. \code{\link{Tabulated}} is the most rich
  representation, it may give several representation for
  each token and several types of grouping tokens into
  sequences (sentence, chapter, text...).

The methods available are as follows:

 \itemize{
   \item \bold{\code{Corpus}}: \code{\link{N}}.
   \itemize{
     \item \bold{\code{\link{CorpusAsFrequencies}}}: \code{\link{ntype}}, \code{\link{contains.types}} and \code{\link{types}}.
       \itemize{
         \item \bold{\code{\link{FrequencyList}}}: \code{\link{contains.types}}, \code{\link{freq}} and \code{\link{hapax}}
         \item \bold{\code{\link{LexicalTable}}}.
     }
     \item \bold{\code{\link{CorpusAsTokens}}}: \code{\link{ngram}}, \code{\link{kwic}}, \code{\link{tactique.subcorpus}} and \code{\link{slice}}.
       \itemize{
         \item \bold{\code{\link{FullText}}}.
         \item \bold{\code{\link{Tabulated}}}.
     }
   }
 }



  A set of \code{\link{as.*}} functions allows for
  conversion between classes:
  \code{\link{as.FrequencyList}},
  \code{\link{as.LexicalTable}}, \code{\link{as.Tabulated}}
  and \code{\link{as.FullText}}. Of course, conversion may
  only be done from a "rich" toward a less “rich"
  representation. You can convert \code{\link{FullText}}
  and \code{\link{Tabulated}} back of forth. You can
  convert both of them into \code{\link{LexicalTable}}. You
  can convert a \code{\link{LexicalTable}} only into a
\code{\link{FrequencyList}} and a \code{\link{FrequencyList}} cannot be
converted into another class.

  (\code{\link{FullText}} <-> \code{\link{Tabulated}}) ->
  \code{\link{LexicalTable}} -> \code{\link{FrequencyList}}
}
\author{
  Sylvain Loiseau
}
\seealso{
  The direct virtual subclasses
  \code{\link{CorpusAsFrequencies}} and
  \code{\link{CorpusAsTokens}}
}

