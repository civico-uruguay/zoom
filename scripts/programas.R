source("scripts/librerias.R")
source("scripts/funciones.R")

# Levantar Programas ------------------------------------------------------

pn <- read_lines("data/programas/Partido Nacional.txt")
ca <- read_lines("data/programas/Cabildo Abierto.txt")
pc <- read_lines("data/programas/Partido Colorado.txt")
pi <- read_lines("data/programas/Partido Independiente.txt")
up <- read_lines("data/programas/Unidad Popular.txt")
fa <- read_lines("data/programas/FA - Bases Programaticas 2020-2025.txt")

# Ordenar y Limpiar en Oraciones - Matrices y TDM -----------------------------------

pn=oraciones(pn)
ca=oraciones(ca)
pc=oraciones(pc)
pi=oraciones(pi)
up=oraciones(up)
fa=oraciones(fa)

partidos = c("ca","fa","pc","pi","pn","up")

for (i in 1:length(partidos)){
  assign(paste0(partidos[i],"_palabras"), limpiar(get(partidos[i])))
  assign(paste0(partidos[i],"_tdm"), tdm(get(paste0(partidos[i],"_palabras"))))
  assign(paste0(partidos[i],"_matrix"), matrix(get(paste0(partidos[i],"_palabras"))))
}


# Palabras más frecuentes -------------------------------------------------

for (i in 1:length(partidos)){
  aux=get(paste0(partidos[i],"_matrix"))  
  aux= aux[1:10, ] #mumero de palabras ej 10
  long=length(aux$palabra) 
  partido=rep(partidos[i], long)  
  aux=cbind(aux,partido)
  if (i==1){frecuencias_10=aux}
  else{
    frecuencias_10 = rbind(frecuencias_10, aux)
  }
}

frecuencias_10 %>%
  ggplot(aes(reorder(palabra, -frec), frec, fill=partido)) +
  geom_bar(stat = "identity", color = "black",alpha=0.7) +
  geom_text(aes(hjust = 1.3, label = frec)) + 
  coord_flip() + 
  labs(title = "Diez palabras más frecuentes",  x = "Palabras", y = "Número de usos")#+
facet_grid(~partido, scales="free")



# Asociacion entre 2 palabras ---------------------------------------------

pn_tdm
findAssocs(pn_tdm, terms = c("ley","años","política"),
           corlimit = .3)



# Diagrama de Asociación de palabras --------------------------------------

freq.terms <- findFreqTerms(pn_tdm, lowfreq=15)
plot(pn_tdm,freq.terms, corThreshold = 0.10, weighting = T)

findFreqTerms(pn_tdm, lowfreq = 0, highfreq = Inf)

nov_new <- removeSparseTerms(pn_tdm, sparse = .95)

nov_new <- nov_new %>% as.matrix()
nov_new <- nov_new / rowSums(nov_new)
nov_dist <- dist(nov_new, method = "euclidian")
nov_hclust <-  hclust(nov_dist, method = "ward.D")
plot(nov_hclust, main = "P. pn - hclust",
     hang = -1, cex = 0.6, sub = "", xlab = "")

rect.hclust(nov_hclust, k = 10, border="blue")

nov_agnes <- agnes(nov_dist, method = "average")
plot(nov_agnes, which.plots = 2, main = "P. pn - Agnes",
     sub = "", xlab = "")
rect.hclust(nov_agnes, k = 10, border = "blue")



# Emociones ---------------------------------------------------------------

# classify emotion
class_emo = classify_emotion(pn, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(pn, algorithm="bayes", minWordLength = 1)


# get polarity best fit
polarity = class_pol[,4]

#PROBANDO C?DIGO DE NAIVE BAYES
sent_df = data.frame(text=pn, emotion=emotion,polarity=polarity, stringsAsFactors=FALSE)

#sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))


# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="Categorias", y="Comentarios") +
  
  labs(title = "Analisis de Emociones",
       plot.title = element_text(size=12))


# Diversidad --------------------------------------------------------------
qdap::diversity(pn)
library(qdap)