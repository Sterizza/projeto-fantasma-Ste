banco1 <-read.csv("banco_final.csv")
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

tentativatab

# 2) análise 2 

season.imdb <- table(banco1$season, banco1$imdb)
season.imdb <-as.data.frame(season.imdb)
class(banco1$season)
class(banco1$imdb)

banco1 %>% distinct(banco1$season)
season<- c(1,2,3,4)
dados2<-banco1[banco1$season %in% season,]
class(banco1$season)
banco1$season <- as.character(banco1$season)
banco1 %>% distinct(season)
banco1$imdb %>% distinct(imdb)


# filtrar season #

season.imdb <- rename(season.imdb, imdb = Var2)
season.imdb <- rename(season.imdb, temporada = Var1)
season <- banco1$season 
imdb <- banco$imdb
imdb <- as.data.frame(imdb)
season <- slice(season, -c(153,336,337,339,340,342,357,362,373,375,378,390,392,406,420,421,422,423,433,438,452,453,470,472,484,485,488,490,491,493,514,517,518,524,540,554,555,557,562,573,600,601,51,53,62,335,343,553,556,572,301,308,313,334,338,341,454,471,486,487,489,492,507))
imdb <- slice(imdb, -c(153,336,337,339,340,342,357,362,373,375,378,390,392,406,420,421,422,423,433,438,452,453,470,472,484,485,488,490,491,493,514,517,518,524,540,554,555,557,562,573,600,601,51,53,62,335,343,553,556,572,301,308,313,334,338,341,454,471,486,487,489,492,507))
seasonximdb <- data.frame(season, imdb)
rm(seasonximdb)
season <- as.data.frame(season)
season <- rename(season, temporada=V1)
season1 <- banco$season
season1 <- as.data.frame(season1)
season1 <- as.numeric(season1)
season1 <- which(season1$season1, Season)
sen <- as.data.frame(season)
season.imdb <- slice(season.imdb, -c(5, 12, 19, 26, 33,40,47,54,61,68,75,82,89,96,103,110,117,124,131,138,145,152,159,166,173,180,187,194,201,208,215,222,229,236,243,250,257,264,271,278,285,292,299,306,313,320,327,334,341,348,355,362,369, 6, 13,20,27,34,41,48,55,62,69,76,83,90,97,104,111,118,125,132,139,146,153,160,167,174,181,188,195,202,209,216,223,230,237,244,251,258,265,272,279,286,293,300,307,314,321,328,335,342,349,356,363,370, 7, 14,21,28,35,42,49,56,63,70,77,84,91,98,105,112,119,126,133,140,147,154,161,168,175,182,189,196,203,210,217,224,231,238,245,252,259,266,273,280,287,294,301,308,315,322,329,336,343,350,357,364,371))
season.imdb1 <- 
season.imbd1 <- slice(season.imdb1, c(1,5,9,13,17,21,25,29,33,37,41,45,49,53,57,61,65,69,73,77,81,85,89,93,97,101,105,109,113,117,121,125,129,133,137,141,145,149,153,157,161,165,169,173,177,181,185,189,193,197,201, 205, 209))
?filter
ggplot(mpg) +
  aes(x=factor(""), y= ) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="
white")+
  labs(x="", y="Consumo em Cidade (milhas/galão)")+
  theme_estat()
18


banco1 %>%
  filter(season == "1" | season == "2" | season == "3" | season == "4")
banco1$season
season.imdb <- filter(season.imdb, season %in% c("1","2","3","4"))
filtro1<- filter(banco1, imdb %in% c(nrow= 51, 53))
filtro1

tb <- table(banco1$season, banco1$imdb)%>%prop.table(1)
tb  
class(filtro1)
rm(dados2)

ggplot(seasonximdb) +
  aes(x = season, y = imdb) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Temporada", y = "Nota IMDB") +
  theme_estat()


ggplot(seasonximdb) +
  aes(x = reorder(season, imdb), y = imdb) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Temporada", y = "Nota IMDB") +
  theme_estat()
ggsave("box2.pdf", width = 158, height = 93, units = "mm")

class(banco1$imbd)
season.imdb$imdb <- as.numeric(season.imdb$imdb)
season.imdb$temporada <- as.numeric(season.imdb$temporada)

T1 <- filter(seasonximdb, seasonximdb$season == "1")
T1
T2 <- filter(seasonximdb, seasonximdb$season == "2")
T2
T3 <- filter(seasonximdb, seasonximdb$season == "3")
T4<- filter(seasonximdb, seasonximdb$season == "4")

summary(T1) 
sd(T1$imdb)
var(T1$imdb)
summary(T2)
sd(T2$imdb)
var(T2$imdb)
summary(T3)
sd(T3$imdb)
var(T3$imdb)
summary(T4)
sd(T4$imdb)
var(T4$imdb)

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

ggplot(data= tabela1, mapping= aes(x = reorder(Var1, frequencia.relativa), y= frequencia.relativa))+
  geom_col(fill = "#A11D21")+
  labs(x="Tipo de Terreno", y= "Frequência relativa (%)")+
  theme_estat()
ggsave("barr.pdf", width = 158, height = 93, units = "mm")

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
ggsave("barr2.pdf", width = 158, height = 93, units = "mm")

# análise 4 ----

imbdxengajamento <- data.frame(banco1$imdb, banco1$engagement)

ggplot(imbdxengajamento) +
  aes(x = banco1.imdb, y = banco1.engagement) +
  geom_point(colour = "#A11D21", size = 3, alpha= .68) +
  labs(
    x = "Nota IMBD",
    y = "Engajamento"
  ) +
  theme_estat()
ggsave("disp.pdf", width = 158, height = 93, units = "mm")


ggplot(imbdxengajamento) +
  aes(x = banco1.imdb, y = frequency(imbdxengajamento, banco1.imdb)) +
  geom_col(colour = "#A11D21", size = 3) +
  labs(
    x = "E",
    y = "f"
     ) +
  theme_estat()
mean(imbdxengajamento$banco1.imdb)
median(imbdxengajamento$banco1.imdb)
sd(imbdxengajamento$banco1.imdb)

mean(imbdxengajamento$banco1.engagement)
median(imbdxengajamento$banco1.engagement)
sd(imbdxengajamento$banco1.engagement)

imbdxengajamento
cor.test(imbdxengajamento$banco1.imdb, imbdxengajamento$banco1.engagement, method="pearson")

summary(imbdxengajamento) 
sd(imbdxengajamento$banco1.imdb)
var(imbdxengajamento$banco1.imdb)
sd(imbdxengajamento$banco1.engagement)
var(imbdxengajamento$banco1.engagement)

# análise 5 -----

personagensxengajamento <- data.frame(banco$unmask_fred, banco$unmask_daphnie, banco$unmask_velma, banco$unmask_shaggy, banco$unmask_scooby, banco$unmask_other, banco$engagement)
fred <- data.frame(banco$unmask_fred, banco$engagement)
fred <- as.logical(fred$banco.unmask_fred)
fred <- filter(fred, fred$banco.unmask_fred == "True")
fredf<- data.frame(banco$unmask_fred, banco$engagement)
fredf <- filter(fredf, fredf$banco.unmask_fred == "False")
Fred <- bind_rows(fred, fredf)
Fred

daphnie <- data.frame(banco$unmask_daphnie, banco$engagement)
daphT <- filter(daphnie, daphnie$banco.unmask_daphnie == "True")
daphF <- filter(daphnie, daphnie$banco.unmask_daphnie == "False")
Daphnie <- bind_rows(daphT, daphF)

velma <- data.frame(banco$unmask_velma, banco$engagement)
velmaT <- filter(velma, velma$banco.unmask_velma == "True")
velmaF <- filter(velma, velma$banco.unmask_velma == "False")
Velma <- bind_rows(velmaT, velmaF)

dados

shaggy <- data.frame(banco$unmask_shaggy, banco$engagement)
shaggyT <- filter(shaggy, shaggy$banco.unmask_shaggy == "True")
shaggyF <- filter(shaggy, shaggy$banco.unmask_shaggy == "False")
Shaggy <- bind_rows(shaggyT, shaggyF)

scooby <- data.frame(banco$unmask_scooby, banco$engagement)
scoobyT <- filter(scooby, scooby$banco.unmask_scooby == "True")
scoobyF <- filter(scooby, scooby$banco.unmask_scooby == "False")
Scooby <- bind_rows(scoobyT, scoobyF)

other <- data.frame(banco$unmask_other, banco$engagement)
otherT <- filter(other, other$banco.unmask_other == "True")
otherF <- filter(other, other$banco.unmask_other == "False")
Other <- bind_rows(otherT, otherF)

ggplot(Scooby) +
  aes(x = banco.unmask_scooby, y = banco.engagement) +
  geom_boxplot(fill = c("#A11D21", "#003366") , width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Capturou ou não capturou", y = "Engajamento") +
  theme_estat()

ggplot(Other) +
  aes(x = banco.unmask_other, y = banco.engagement) +
  geom_boxplot(fill = c("#A11D21", "#003366") , width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Capturou ou não capturou", y = "Engajamento") +
  theme_estat()

ggplot(Shaggy) +
  aes(x = banco.unmask_shaggy, y = banco.engagement) +
  geom_boxplot(fill = c("#A11D21", "#003366") , width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Capturou ou não capturou", y = "Engajamento") +
  theme_estat()

ggplot(Velma) +
  aes(x = banco.unmask_velma, y = banco.engagement) +
  geom_boxplot(fill = c("#A11D21", "#003366") , width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Capturou ou não capturou", y = "Engajamento") +
  theme_estat()

ggplot(Daphnie) +
  aes(x = banco.unmask_daphnie, y = banco.engagement) +
  geom_boxplot(fill = c("#A11D21", "#003366") , width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Capturou ou não capturou", y = "Engajamento") +
  theme_estat()

ggplot(Fred) +
  aes(x = banco.unmask_fred, y = banco.engagement) +
  geom_boxplot(fill = c("#A11D21", "#003366") , width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Capturou ou não capturou", y = "Engajamento") +
  theme_estat()


summary(fredf) 
sd(fredf$banco.engagement)
var(fredf$banco.engagement)

summary(fred) 
sd(fred$banco.engagement)
var(fred$banco.engagement)

summary(velmaF) 
sd(velmaF$banco.engagement)
var(velmaF$banco.engagement)

summary(velmaT) 
sd(velmaT$banco.engagement)
var(velmaT$banco.engagement)

summary(daphT)
sd(daphT$banco.engagement)
var(daphT$banco.engagement)

summary(daphF)
sd(daphF$banco.engagement)
var(daphF$banco.engagement)

summary(shaggyF)
sd(shaggyF$banco.engagement)
var(shaggyF$banco.engagement)

summary(shaggyT)
sd(shaggyT$banco.engagement)
var(shaggyT$banco.engagement)


summary(scoobyF)
sd(scoobyF$banco.engagement)
var(scoobyF$banco.engagement)

summary(scoobyT)
sd(scoobyT$banco.engagement)
var(scoobyT$banco.engagement)

summary(otherF)
sd(otherF$banco.engagement)
var(otherF$banco.engagement)

summary(otherT)
sd(otherT$banco.engagement)
var(otherT$banco.engagement)

tentativa <- bind_rows(daphT, velmaT)
tentativa
velmaT
velmaT$banco.engagement <- NULL
daphT$banco.engagement <- NULL
daphT <- c(daphT, rep(NA, length(velmaT)- length(daphT)))
nova <- c(daphT, velmaT)
boxplots <- na.omit(boxplots$banco.unmask_fred)

boxplots <- bind_cols(fred, daphT, velmaT, shaggyT, scoobyT, otherT)

ggplot(boxplots) +
  aes(x = , y = banco.engagement) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "p", y = "e") +
  theme_estat()
ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")

tentativa$novacoluna <- ifelse(is.na(tentativa$banco.unmask_daphnie), tentativa$banco.unmask_daphnie, tentativa$banco.unmask_velma)

solução <- data.frame(banco$engagement)                
