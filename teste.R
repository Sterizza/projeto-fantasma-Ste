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

data.lançamento <-banco1$date_aired
décadas <-cut(tentativa, breaks= seq(1960, 2030, by=10), labels= seq(1960, 2020,by=10))
lançamento.década <- data.frame(ano=data.lançamento, décadas = as.numeric(as.character(decadas)))

#tentando agrupar date_aired com format 

formato <- banco1$format
lançamento.formato <- banco1%>%
  group_by(lançamento.década, formato)


frequência <- table(tentativatab)
frequência.lançamento <- data.frame(tentativatab)
frequência.lançamento <- seq()
formato <- as.data.frame(formato)
lançamentos <- tentativatab %>%
  group_by(lançamento.década)

#gráfico análise 1 

estat_colors <- c(
  "#A11D21", "#003366", "#CC9900",
  "#663333", "#FF6600", "#CC9966",
  "#999966", "#006606", "#008091", 
  "#041835", "#666666" )
theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = estat_colors),
      scale_colour_manual(values = estat_colors)
    )
  )
}

ggplot(data= dados, mapping= aes(x = década, y= Freq, fill = Formato ))+
  geom_col(position="dodge")+
  labs(x="Décadas", y= "N° de lançamentos")+
  theme_estat()
ggsave("análise1.pdf", width = 158, height = 93, units = "mm")

dados<-dados%>%rename(Formato=banco1.format)



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

# análise 3 
banco1<- banco
banco1$setting_terrain
terreno <- banco1$setting_terrain
terreno <- as.data.frame(banco1$setting_terrain)
terreno <- frequency(n())
terreno <- terreno%>%
  arrange(desc(Freq))
terreno

banco1$trap_work_first

terreno.armadilha <- banco1%>%
  group_by(setting_terrain, trap_work_first)
terreno.armadilha
rm(terreno.armadilha)
armadilha <- banco1$trap_work_first
terreno<- group_by(armadilha)
armadilha <- as.data.frame(armadilha)
terreno <- frequency(n())
terreno.armadilha <- table(banco1$setting_terrain, banco1$trap_work_first)
terreno.armadilha <- count(terreno.armadilha$banco1.setting_terrain)%>%
  arrange(desc(n))
  
terreno.armadilha
terreno <- as.table(terreno)
terreno1 <-table(banco1$setting_terrain)
terreno1<- as.data.frame(terreno1)
terreno1 <-  terreno1%>%
  arrange(desc(Freq))
terreno.armadilha
terreno1 <- data.frame(terreno1$Freq/sum(terreno1$Freq))
tabela.terreno <- table(banco1$setting_terrain)
tabela1 <- terreno1%>%
  mutate(frequencia.relativa = 100*(Freq/sum(Freq)))
tabela.terreno
sum(tabela1$frequencia.relativa)

# gráfico pra terreno mais frequente 

ggplot(data= tabela1, mapping= aes(x = reorder(terreno, frequencia.relativa), y= frequencia.relativa))+
  geom_col()+
  labs(x="Tipo de Terreno", y= "Frequência relativa (%)")
           
# limpando df (tipo de terreno x ativação da armadilha)
terreno
teste <- terreno.armadilha 
teste <- as.data.frame(teste)
teste<- slice(teste, c(5, 11, 15, 20, 26, 30, 35, 41, 45))
teste<- slice(teste, 4:9)

#gráfico tipo de terreno x ativação da armadilha 

teste <- rename(teste, ativação= Var2)
ggplot(data= teste, mapping= aes(x = Var1, y= Freq, fill = ativação ))+
  geom_col(position="dodge")+
  labs(x="Tipo de terreno", y= "N° de ativações")+
  theme_estat()


