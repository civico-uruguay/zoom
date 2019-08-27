

oraciones <- function(raw){
  nov_text <- gsub("[[:cntrl:]]", " ", raw)
  nov_text <- tolower(nov_text)
  nov_text <- removeNumbers(nov_text)
  nov_text <- stripWhitespace(nov_text)
  nov_text=nov_text[!nov_text %in% c(" ","")]
  texto <- paste(nov_text,collapse=" ")
  oraciones=unlist(strsplit(texto, "\\."))
  oraciones = oraciones[which(oraciones!="")]
  oraciones = as.array(oraciones[which(oraciones!=" ")])
  return(oraciones)
}
limpiar <- function(nov_raw){ 
  diez <- rep(1:ceiling(length(nov_raw)/10), each = 10)
  diez <- diez[1:length(nov_raw)]
  
  nov_text <-
    cbind(
      rep(1:ceiling(length(nov_raw)/10), each = 10) %>%
        .[1:length(nov_raw)],
      nov_raw
    ) %>%
    data.frame %>%
    aggregate(
      nov_raw ~ V1,
      data = .,
      FUN = paste,
      collapse=" ") %>%
    select(nov_raw) %>%
    as.matrix
  
  dim(nov_text)
  
  nov_text <- gsub("[[:cntrl:]]", " ", nov_text)
  nov_text <- gsub("[[:punct:]]", " ", nov_text)
  nov_text <- gsub("[^[:alnum:] ]", " ", nov_text)
  nov_text <- gsub("•", " ", nov_text)
  nov_text <- gsub("", " ", nov_text)
  nov_text <- tolower(nov_text)
  nov_text <- removeWords(nov_text, words = stopwords("spanish"))
  nov_text <- removePunctuation(nov_text)
  nov_text <- removeNumbers(nov_text)
  nov_text <- stripWhitespace(nov_text)
  
  nov_text <- removeWords(nov_text, words = c("tit","ps","mag","ello","través","as","vez","dra",
                                              "usted", "pues", "tal", "tales","tan", "as?", "dijo",
                                              "cómo", "sino", "entonces", "aunque", "don", "doña",
                                              "todas","fin","prof",
                                              "toda","deben","debe","ser","cada","hacia","forma","indice"))
  
  return(nov_text)
}
tdm <- function(nov_text){
  nov_corpus <- nov_text %>% VectorSource() %>% Corpus()
  nov_ptd <- nov_corpus %>% tm_map(PlainTextDocument)
  
  nov_tdm <- TermDocumentMatrix(nov_corpus)
  return(nov_tdm)
}
matriz <- function(nov_text){
  nov_tdm=tdm(nov_text)
  
  nov_mat <- as.matrix(nov_tdm)
  
  nov_mat <- nov_mat %>% rowSums() %>% sort(decreasing = TRUE)
  nov_mat <- data.frame(palabra = names(nov_mat), frec = nov_mat)
  return(nov_mat)
}

source('scripts/classify_polarity.R')
source('scripts/create_matrix.R')
source('scripts/classify_emotion.R')


