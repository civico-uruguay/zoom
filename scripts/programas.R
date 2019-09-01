source("scripts/librerias.R")
source("scripts/funciones.R")

# Levantar Programas ------------------------------------------------------

pn <- read_lines("data/programas/Partido Nacional.txt")
pg <- read_lines("data/programas/Partido de la Gente.txt")
ca <- read_lines("data/programas/Cabildo Abierto.txt")
pc <- read_lines("data/programas/Partido Colorado.txt")
pi <- read_lines("data/programas/Partido Independiente.txt")
up <- read_lines("data/programas/Unidad Popular.txt")
fa <- read_lines("data/programas/FA - Bases Programaticas 2020-2025.txt")


partidos = c("ca","fa","pc","pg","pi","pn","up")

# Ordenar y Limpiar en Oraciones - Matrices y TDM -----------------------------------

pn=oraciones(pn)
pg=oraciones(pg)
ca=oraciones(ca)
pc=oraciones(pc)
pi=oraciones(pi)
up=oraciones(up)
fa=oraciones(fa)

#partidos = c("ca")
for (i in 1:length(partidos)){
  assign(paste0(partidos[i],"_palabras"), limpiar(get(partidos[i])))
  assign(paste0(partidos[i],"_tdm"), tdm(get(paste0(partidos[i],"_palabras"))))
  assign(paste0(partidos[i],"_matrix"), matriz(get(paste0(partidos[i],"_palabras"))))
}

# Tabla Palabras - Oraciones   --------------------------------------------
npalabras <- data.frame(matrix(ncol=length(partidos), nrow=0))
colnames(npalabras) <- partidos
lista_palabras=c("educación", "trabajo","seguridad", "economía","pobreza")

for (i in 1:length(partidos)){
  aux_npalabras=wordcount(get(partidos[i]), sep = " ", count.function = sum)
  npalabras["oraciones", i] =length(get(partidos[i]))
  npalabras["palabras", i] = aux_npalabras
  aux=get(paste0(partidos[i],"_matrix"))
  npalabras["palabras_unicas", i] = length(aux$palabra)
    for (j in lista_palabras){
      frecuencia=aux[aux$palabra==j,]$frec
      if(!identical(frecuencia, numeric(0))){
        npalabras[j, i] = frecuencia
        #h=paste0(j,"_porc")
        #npalabras[h, i] = round((frecuencia/aux_npalabras)*100,2)
      }
    }
  }

# Palabras más frecuentes -------------------------------------------------

rm(frecuencias_10)
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
  labs(title = "Diez palabras más frecuentes",  x = "Palabras", y = "Número de usos")+
facet_grid(~partido, scales="free")

frecuencia_10w = frecuencias_10 %>% 
  spread(.,key="partido", frec)

frec_palabras <- data.frame(matrix(ncol=length(partidos), nrow=0))
colnames(frec_palabras) <- partidos
lista_palabras=as.vector(frecuencia_10w$palabra)

for (i in 1:length(partidos)){
  aux=get(paste0(partidos[i],"_matrix"))
  frec_palabras["total_sinvacias", i] = length(aux$palabra)
  for (j in lista_palabras){
    frecuencia=aux[aux$palabra==j,]$frec
    if(!identical(frecuencia, numeric(0))){
      frec_palabras[j, i] = frecuencia
      #h=paste0(j,"_porc")
      #npalabras[h, i] = round((frecuencia/aux_npalabras)*100,2)
    }
  }
}

write.csv(frec_palabras,"data/frec10_pal.csv", fileEncoding = "utf-8")

frec_l=frec_palabras %>%
  mutate(palabra=row.names(.)) %>% 
  melt(id.vars="palabra", variable.name="partido",value.name = "frec" )%>% 
  filter(palabra!="total_sinvacias") %>% 
  arrange(partido, desc(frec)) %>% 
  group_by(partido) %>% mutate(id = row_number())

frec_l %>% 
  ggplot(aes(reorder(palabra,-frec), frec, fill=partido)) +
  geom_bar(stat = "identity",  position="dodge",color = "black",alpha=0.7) +
  geom_text(aes(hjust = 1.3, label = frec)) + 
  coord_flip() + 
  labs(title = "Diez palabras más frecuentes",  x = "Palabras", y = "Número de usos")+
  facet_grid(~partido, scales="free")+theme(legend.position ="none")


# distribucion de palabras ------------------------------------------------
rm(frecuencias)
for (i in 1:length(partidos)){
  aux=get(paste0(partidos[i],"_matrix"))  
#  aux= aux[1:10, ] #mumero de palabras ej 10
  long=length(aux$palabra) 
  partido=rep(partidos[i], long)  
  aux=cbind(aux,partido)
  if (i==1){frecuencias=aux}
  else{
    frecuencias = rbind(frecuencias, aux)
  }
}

for (i in partidos) { 
  extremost=frecuencias %>% 
    filter(partido==i) %>% 
    top_n(1, frec) 
  extremosb=frecuencias %>% 
    filter(partido==i) %>% 
    top_n(-10, frec)
  
extremos = c(as.character(sample(extremosb$palabra,3)),
             as.character(extremost$palabra))  
  
fig=frecuencias %>% 
  filter(partido==i) %>%
  ggplot(aes(reorder(palabra,frec), frec, fill=partido))+
  geom_bar(stat = "identity",  position="dodge",color = "black",alpha=0.7) +
#  geom_text(aes(hjust = 1.3, label = frec)) + 
  theme(axis.text.y = element_text(angle = 45))+
  labs(title = paste("Distribucion palabras ",i ),  x = "Palabras", y = "Número de usos")+
  theme(legend.position ="none")+
  scale_x_discrete(breaks = {extremos})+
  coord_flip() 
print(fig)
  }

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
