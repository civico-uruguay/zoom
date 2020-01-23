library(pdftools)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

ley<-pdf_text("../Downloads/anteproyectoLUC.pdf") %>% 
  readr::read_lines()

ley <- ley %>% 
  str_squish() %>% 
  str_replace_all(",","") %>%
  str_replace_all("-","") %>%
  tolower() %>% 
  strsplit( split= " ")

head(ley)

ley_w <- ley %>% 
  unlist()

text_df <- tibble(line = 1:length(ley_w), text = ley_w)

text_df <- text_df %>%
  mutate(text=sub("[[:punct:]]", "", text)) %>% 
  mutate(text=sub("[[:digit:]]", "", text)) %>%
  mutate(text=sub("–" , "", text)) %>%
  mutate(text=sub("'" , "", text)) %>%
  mutate(text=sub("\"", "", text)) %>%
  mutate(text=sub("“" , "", text)) %>%
  mutate(text=sub("”" , "", text)) %>%
  mutate(text=sub("‘" , "", text)) %>%
  filter(text!="") %>% 
  filter(text!="–") 
   
  
stop_words_es <-read.csv("../Documents/GitHub/zoom/data/stop-words-spanish.csv", stringsAsFactors = FALSE, fileEncoding = "utf-8")

luc <- text_df %>%
  rename(word=text) %>% 
  anti_join(stop_words_es) %>%
  filter(!word %in% c("aa","","as"))


obvias = c("ley","artículo","capítulo","capitulo", "presente", "forma", "quedará", "redactado",
           "sustitúyese","efectos","caso","diciembre","octubre","nacional")
luc %>% 
count(word, sort = TRUE) %>%
  filter(!word %in% obvias) %>% 
  filter(n > 50) %>%
#  filter(n > max(n)-max(n)*0.5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()


library(tidytext)
library(tidyr)

bigrams <- aux %>%
  mutate(text=gsub("[[:digit:]]+", "", text)) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2, to_lower = TRUE)

bigrams_sep <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_sep %>%
  filter(!word1 %in% stop_words_es$word) %>%
  filter(!word2 %in% stop_words_es$word) %>% 
  filter(!word1 %in% obvias) %>% 
  filter(!word2 %in% obvias)  

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)


library(igraph)
library(ggraph)

bigram_graph <- bigrams_filtered %>%
    count(word1, word2, sort = TRUE) %>% 
    filter(n>5) %>% 
    graph_from_data_frame()
  
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
ggraph(bigram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                   arrow = a, end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()


