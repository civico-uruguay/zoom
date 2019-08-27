library(quanteda)
library(readtext)
library(topicmodels)

fa <- readtext("data/programas/FA - Bases Programaticas 2020-2025.txt", encoding = "utf-8")
fa_corpus <- corpus(fa)
ca_corpus <- corpus(ca)

summary(ca_corpus)

docvars(fa_corpus, "partido") <- "fa"
docvars(ca_corpus, "partido") <- "ca"

kwic(ca_corpus, pattern = "eduacion")
dfmat2=dfm(ca_palabras)
dfmat <- dfm(ca_corpus,
                  remove_punct = TRUE, remove_numbers = TRUE, remove = stopwords("spanish"))

dfmat3 <- dfm_trim(dfmat2, min_termfreq = 4, max_docfreq = 10)
dfmat3

## Document-feature matrix of: 14 documents, 1,263 features (64.5% sparse).

set.seed(100)
my_lda_fit20 <- LDA(convert(dfmat2, to = "topicmodels"), k = 15)

