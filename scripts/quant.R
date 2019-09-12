library(quanteda)
library(readtext)
library(topicmodels)

fa <- readtext("data/programas/FA - Bases Programaticas 2020-2025.txt", encoding = "utf-8")
fa_corpus <- corpus(oraciones(fa))
ca_corpus <- corpus(ca)

summary(fa_corpus)

docvars(fa_corpus, "partido") <- "fa"
docvars(ca_corpus, "partido") <- "ca"

kwic(ca_corpus, pattern = "eduacion")
dfmat2=dfm(fa_palabras)
dfmat <- dfm(ca_corpus,
                  remove_punct = TRUE, remove_numbers = TRUE, remove = stopwords("spanish"))

dfmat3 <- dfm_trim(dfmat2, min_termfreq = 4, max_docfreq = 10)
dfmat3

## Document-feature matrix of: 14 documents, 1,263 features (64.5% sparse).

set.seed(100)
my_lda_fit20 <- LDA(convert(dfmat2, to = "topicmodels"), k = 3, control = list(seed = 1234))


ap_topics <- tidy(my_lda_fit20, matrix = "beta")
ap_topics

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread %>% 
  filter(abs(log_ratio) > 10) %>% 
ggplot(aes(reorder(term, log_ratio),log_ratio))+geom_bar(stat="identity")+
  coord_flip()


beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic3 > .001) %>%
  mutate(log_ratio = log2(topic3 / topic1))

beta_spread %>% 
  filter(abs(log_ratio) > 10) %>% 
  ggplot(aes(reorder(term, log_ratio),log_ratio))+geom_bar(stat="identity")+
  coord_flip()
