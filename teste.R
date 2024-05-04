banco1<-read.csv("banco_final.csv")
library(tidyverse)

# projeto fantasma - estat#
# Stephanye Oliveira Rizza#

# análises#----

#' 1)Número de lançamentos a cada década por formato de lançamento;
#' 2)Variação da nota IMBD por temporada dos episódios;
#' 3)Top 3 terrenos mais frequentes pela ativação da armadilha;
#' 4)Relação entre as notas IMBD e engajamento;
#' 5)Variação da nota de engajamento pelo personagem que conseguiu capturar o monstro;

# pacotes ----

library(tidyverse)

# carregamento do banco ----

banco1<-read.csv("banco_final.csv")
banco1

# 1) ----

(banco$format)
tab <- table(banco$format)
tab

tab2 <- banco1$date_aired
banco1 %>% distinct(date_aired)

# limpando coluna date_aired#----

banco1$date_aired <- as.Date(banco1$date_aired, "%Y-%m-%d")
class(banco1$date_aired)
banco1$date_aired <- year(banco1$date_aired)

#separando em décadas 

tentativa<-banco1$date_aired
decadas<-cut(tentativa, breaks= seq(1960, 2030, by=10), labels= seq(1960, 2020,by=10))
anosxdecadas <- data.frame(ano=tentativa, década= as.numeric(as.character(decadas)))
anosxdecadas

#tentando agrupar date_aired com format 
formato<-banco1$format
tentativatab <- banco1%>%
  group_by(anosxdecadas, formato)
frequência <- table(tentativatab)
frequência
# 2) ----

banco1 %>% distinct(banco1$season)
season<- c(1,2,3,4)
dados2<-banco1[banco1$season %in% season,]
class(banco1$season)
banco1$season <- as.character(banco1$season)
banco1 %>% distinct(season)
banco1$imdb %>% distinct(imdb)
bancoteste<- read.csv("banco_final-Copia.csv")

read(banco1)
banco1$imdb
banco1$season
class(banco1$imdb)
class(banco1$season)

# filtrar season #

banco1 %>%
  filter(season == "1" | season == "2" | season == "3" | season == "4")
banco1$season
filtro <- filter(banco1, season %in% c("1","2","3","4"))
filtro1<- filter(banco1, imdb %in% c(nrow= 51, 53))
filtro1

tb <- table(banco1$season, banco1$imdb)%>%prop.table(1)
tb  
class(filtro1)
rm(dados2)
