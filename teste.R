banco <-read.csv("banco_final.csv")
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
tab <- data.frame(banco$format, banco$date_aired)
tab

# limpando coluna date_aired#----

tab$banco.date_aired <- as.Date(tab$banco.date_aired, "%Y-%m-%d")
class(tab$banco.date_aired)
tab$banco.date_aired <- year(tab$banco.date_aired)

#separando em décadas 

tab$decadas <- cut(tab$banco.date_aired, breaks = seq(1959,2029, 10), labels = seq(1960, 2020, 10))

frequencia <- tab %>%
  group_by(banco.format, decadas)%>%
  summarise(freq= n())%>%
mutate(
  freq_relativa = round(freq / sum(freq) * 100,1))

frequencia<-frequencia%>%rename(Formato =banco.format) 

frequencia$Formato <- frequencia$Formato %>% str_replace('Movie', 'Filme')
frequencia$Formato <- frequencia$Formato %>% str_replace('Serie', 'Série')

#gráfico análise 1 


ggplot(frequencia) +
  aes(x = decadas, y = freq, group = Formato, colour = Formato) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Décadas", y = "Número de lançamentos") +
  theme_estat()
ggsave("series_grupoC.pdf", width = 158, height = 93, units = "mm")


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
  mutate(frequencia.relativa = round(Freq / sum(Freq) * 100,1))
tabelat <- data.frame(banco$setting_terrain)
tabelat <- tabelat%>%
  group_by(banco.setting_terrain) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100,1)
  )

tabelaC <- tabelat %>%
  count(banco.setting_terrain)%>%
  mutate(
    freq = n,
    relative_freq = round((freq / sum(freq)) * 100, 1),
    freq = gsub("\\.", ",", relative_freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )
porcentagens2 <- str_c(tabelat$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas2 <- str_squish(str_c(tabelat$freq, " (", porcentagens2, ")"))
tabela.terreno
sum(tabela1$frequencia.relativa)

tabelat$banco.setting_terrain <- tabelat$banco.setting_terrain %>% str_replace('Urban', 'Urbano')
tabelat$banco.setting_terrain <- tabelat$banco.setting_terrain %>% str_replace('Forest', 'Floresta')
tabelat$banco.setting_terrain <- tabelat$banco.setting_terrain %>% str_replace('Desert', 'Deserto')
tabelat$banco.setting_terrain <- tabelat$banco.setting_terrain %>%str_replace('Island', 'Ilha')
tabelat$banco.setting_terrain <- tabelat$banco.setting_terrain %>%str_replace('Snow', 'Neve')
tabelat$banco.setting_terrain <- tabelat$banco.setting_terrain %>%str_replace('Swamp', 'Pântano')
tabelat$banco.setting_terrain <- tabelat$banco.setting_terrain %>%str_replace('Coast', 'Costa')
tabelat$banco.setting_terrain <- tabelat$banco.setting_terrain %>%str_replace('Jungle', 'Selva')
tabelat$banco.setting_terrain <- tabelat$banco.setting_terrain %>%str_replace('Ocean', 'Oceano')
tabelat$banco.setting_terrain <- tabelat$banco.setting_terrain %>%str_replace('Cave', 'Caverna')
tabelat$banco.setting_terrain <- tabelat$banco.setting_terrain %>%str_replace('Space', 'Espaço')
tabelat$banco.setting_terrain <- tabelat$banco.setting_terrain %>%str_replace('Air', 'Ar')
tabelat$banco.setting_terrain <- tabelat$banco.setting_terrain %>%str_replace('Moon', 'Lua')
tabelat$banco.setting_terrain <- tabelat$banco.setting_terrain %>%str_replace('Mountain', 'Montanha')
tabelat$banco.trap_work_first <- tabelat$banco.trap_work_first %>% str_replace('False', 'Não Funcionou' )
tabelat$banco.trap_work_first <- tabelat$banco.trap_work_first %>% str_replace('True', 'Funcionou' )
# gráfico pra terreno mais frequente 

ggplot(data= tabela1, mapping= aes(x = reorder(Var1, frequencia.relativa), y= frequencia.relativa))+
  geom_col(fill = "#A11D21")+
  coord_flip()+
  labs(x="Tipo de Terreno", y= "Frequência relativa (%)")+
  theme_estat()
ggsave("barr.pdf", width = 158, height = 93, units = "mm")

ggplot(tabelat) +
  aes(
    x = fct_reorder(banco.setting_terrain, freq, .desc = T), y = freq,
    fill = "#A11D21", label = legendas2
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  coord_flip()+
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Terreno", y = "Frequência") +
  theme_estat()
ggsave("colunas-freq.pdf", width = 158, height = 93, units = "mm")

ggplot(tabelat) +
  aes(x = fct_reorder(banco.setting_terrain, freq, .desc=T), y = freq, label = legendas2) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  coord_flip()+
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Terrenos", y = "Frequência") +
  theme_estat()
ggsave("colunas-uni-freqv.pdf", width = 158, height = 93, units = "mm")

ggplot(tabelaC) +
  aes(x = fct_reorder(banco.setting_terrain, n, .desc=T), y = n, label = label) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  coord_flip()+
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Terrenos", y = "Frequência") +
  theme_estat()
ggsave("colunas-uni-freqC.pdf", width = 158, height = 93, units = "mm")

# limpando df (tipo de terreno x ativação da armadilha)
terreno
teste <- data.frame(banco$setting_terrain, banco$trap_work_first)
teste <- slice( teste, -c(5,7,26,34,44,79,81,91,96,99,105,125,148,161,164,171,185,196,247,258,280,291,334,339,357,360,361,372,377,380,390,404,417,452,462,505,518,543,583,602))
teste <- slice(teste, -c(3,14,30,38,67,91,94,100,101,115,126,153,180,227,234,252,257,318,328,349,378,433,449,473,481,145,184,367,498,547,474,120,15,31,50,92,93,99,104,141,164,165,225,255,259,266,267,320,329,341,344,363,477,499,506,536,555,11,33,49,55,57,71,145,158,167,184,261,279,286,291,313,367,392,419,476,498,520,524,544,547,2,4,12,39,60,68,80,83,110,112,133,136,149,150,154,169,172,182,186,202,211,226,251,271,289,359,361,380,394,430,446,447,460,461,465,487,488,314,21,87,88,105,118,119,156,163,178,230,233,321,327,368,445,12,60,80,112,149,154,182,202,271,361,380,430,461,4,110,172,186,446,447,460,465,514,65,70,86,148,287,374,429,419,556))
teste<- slice(teste, c(11,12,13,27,28,29,38,39,40))
teste<- slice(teste, -c(340))
teste<- slice(teste, c(3, 407, 2,9,10,12,15,19,21,24,42,50,54,74,79,81,198,200,202,205,231,237,241,245,251,285,287,289,290,292,299,300,301,302,303,305,306,307,312,314,315,316,318,320,321,324,325,326,329,331,337,338,339,344,345,352,353,354,355,357,359,360,361,365,366,368,369,370,372,374,376,377,378,381,385,394,396,397,403,405,408,409,410,411,412,414,417,418,420,421,3,5,6,11,13,14,32,33,34,35,36,37,40,43,44,45,66,70,85,91,95,97,113,143,173,197,206,207,208,209,210,211,217,218,219,223,229,230,232,233,235,243,244,250,254,255,256,260,267,279,280,281,282,284,286,288,291,293,308,311,313,322,323,328,333,334,335,336,340,342,343,346,347,356,362,363,364,367,371,382,383,388,390,391,392,393,395,398,399,402,406,413,415,416,419))
teste <- slice(teste, c(22,24,52,56,118,267,376,384,402,470,486,519,528,553,559, 564,567,575,589,597, 21,78,124,134,337,364,474,479,490,501,516,520,530,537,539,546,555,576,580,588,6,19,25,32,39,64,136,304,306,310,315,348,358,369,379,426,429,432,434,441,442,443,444,445,447,448,449,458,459,460,467,469,472,473,477,489,491,499,511,514,523,531,533,534,541,545,547,550,566,569,578,581,584,585,587,590,593,596,598,599,8,20,50,51,53,61,66,67,68,141,150,155,157,191,228,303,316,317,318,319,320,321,327,328,329,333,346,347,349,350,355,366,367,383,391,419,420,421,422,424,428,430,435,450,455,457,471,476,485,487,493,497,498,502,503,529,532,538,551,561,563,565,570,571,579,591,10,11,57,109,385,433,488,527,592,18,30,84,431,456,463,515,522,526,568))
teste <- filter(teste$banco.setting_terrain == "Urbano", "Rural", "Floresta")
class(teste$banco.setting_terrain)
teste <- teste%>%
  group_by(banco.setting_terrain, banco.trap_work_first) %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100,1)
  )
teste <- slice(teste, c(2,9,10,12,15,19,21,24,42,50,54,74,79,81,198,200,202,205,231,237,241,245,251,285,287,289,290,292,299,300,301,302,303,305,306,307,312,314,315,316,318,320,321,324,325,326,329,331,337,338,339,344,345,352,353,354,355,357,359,360,361,365,366,368,369,370,372,374,376,377,378,381,385,394,396,397,403,405,407,408,409,410,411,412,414,417,418,420,421,3,5,6,11,13,14,32,33,34,35,36,37,40,43,44,45,66,70,85,91,95,97,113,143,173,197,206,207,208,209,210,211,217,218,219,223,229,230,232,233,235,243,244,250,254,255,256,260,267,279,280,281,282,284,286,288,291,293,308,311,313,322,323,328,333,334,335,336,340,342,343,346,347,356,362,363,364,367,371,382,383,388,390,391,392,393,395,398,399,402,406,413,415,416,418))
teste$banco.trap_work_first <- NULL
#gráfico tipo de terreno x ativação da armadilha 

teste <- rename(teste, Ativação = ativação)
ggplot(data= teste, mapping= aes(x = Var1, y= Freq, fill = ativação ))+
  geom_col(position="dodge")+
  labs(x="Tipo de terreno", y= "N° de ativações")+
  theme_estat()
ggsave("barr2.pdf", width = 158, height = 93, units = "mm")


porcentagens3 <- str_c(teste$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas3 <- str_squish(str_c(teste$freq, " (", porcentagens3, ")"))

ggplot(teste) +
  aes(
    x = fct_reorder(banco.setting_terrain, freq, .desc = T), y = freq,
    fill = Ativação, label = legendas3
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Terrenos", y = "Número de ativações") +
  theme_estat()
ggsave("colunas-bi-freq2.pdf", width = 158, height = 93, units = "mm")

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
testeF <- fred
testeF$banco.unmask_fred <- NULL

TesteD <- daphT
TesteD$banco.unmask_daphnie <- NULL

testeV <- velmaT
testeV$banco.unmask_velma <- NULL

testeSh <- shaggyT
testeSh$banco.unmask_shaggy <- NULL

testeS <- scoobyT
testeS$banco.unmask_scooby <- NULL

testeO <- otherT
testeO$banco.unmask_other <-NULL

engajamento <- c( 199.58, 173.24, 178.03, 162.79, 205.93, 162.40, 212.23, 119.51, 177.33, 168.91, 170.22, 149.76, 229.42, 160.10, 204.37, 158.66, 212.00, 172.45, 134.81, 182.58, 181.53, 228.27, 130.57, 132.24, 221.34, 176.53, 140.01, 244.07, 149.75, 208.30, 153.25, 135.19, 123.47, 153.76, 220.36 , 170.24, 125.29, 196.52, 218.35, 233.00, 216.71, 187.67 ,154.76 ,162.74, 196.26 ,161.89 ,190.30 , 162.26 ,195.84 ,162.76 ,176.83 , 191.17 ,104.88 ,221.64 , 132.81, 144.82, 173.19 ,213.97 , 167.24 ,169.44,
  161.47, 
  173.32 ,
  200.12,
  258.83 ,
  153.15 ,
  177.76 ,
  138.36 ,
  189.54 ,
  148.32 ,
  145.03 ,
  214.00 ,
  135.96 ,
  177.05 ,
  168.31 ,
  205.63 ,
  190.47, 
  201.20,
  175.93 ,
  185.16 ,
  200.25 ,
  147.95 ,
  195.78 ,
  146.59 ,
  166.33 ,
  245.06 ,
  161.71 ,
  159.64 ,
  168.00 ,
  173.76 ,
  201.36 ,
  154.45 ,
  213.32 ,
  167.49 ,
  180.09 ,
  152.74 ,
  160.01 ,
  204.88 ,
  177.40 ,
  180.85 ,
  162.67 ,
  179.22 ,
  173.72 ,
  212.58 ,
  212.23 ,
  169.74 ,
  173.67 ,
  187.72 ,
  211.46 ,
  184.72 ,
  214.99 ,
  202.76 ,
  188.97 ,
  153.37 ,
  161.23 ,
  169.39 ,
  176.34 ,
  180.48 ,
  158.05 ,
  169.97 ,
  144.05 ,
  175.65 ,
  130.72 ,
  159.89 ,
  175.48 ,
  160.18 ,
  190.30 ,
  162.26 ,
  208.02 ,
  216.32 ,
  204.07 ,
  191.04 ,
  197.17 ,
  160.84 ,
  203.68 ,
  188.28 ,
  157.37 ,
  159.64 ,
  200.36 ,
  160.01 ,
  151.86 ,
  191.48 ,
  212.23 ,
  212.00 ,
  169.74 ,
  176.53 ,
  202.75 ,
  208.30 ,
  169.22 ,
  193.19 ,
  193.76 ,
  188.19 ,
  170.09 ,
  214.71 ,
  129.78 ,
  183.26 ,
  184.02 ,
  159.20 ,
  128.99 ,
  201.92 ,
  213.50 ,
  168.21 ,
  143.04 ,
  194.07 ,
  186.47 ,
  194.57 ,
  211.70 ,
  178.53 ,
  165.00 ,
  156.12 ,
  164.55 ,
  167.83 ,
  220.17 ,
  154.90 ,
  164.60 ,
  205.76 ,
  178.20 ,
  160.18 ,
  190.30 ,
  191.00 ,
  196.84 ,
  165.98 ,
  206.54 ,
  201.56 ,
  175.59 ,
  216.32 ,
  173.47 ,
  162.50 ,
  184.68 ,
  207.53 ,
  163.37 ,
  144.93 ,
  169.02 ,
  194.70 ,
  210.61 ,
  181.87 ,
  192.23 ,
  124.40 ,
  226.21 ,
  195.72 ,
  186.27 ,
  146.53 ,
  173.41 ,
  203.68 ,
  159.29 ,
  175.48 ,
  249.06 ,
  143.84 ,
  157.37 ,
  187.32 ,
  192.15 ,
  214.00 ,
  198.39 ,
  156.86 ,
  171.46 ,
  164.91 ,
  205.65 ,
  204.35 ,
  182.64 ,
  175.36 ,
  159.64 ,
  187.88 ,
  136.39 ,
  169.28 ,
  133.40 ,
  152.74 ,
  213.72 ,
  180.64 ,
  160.01 ,
  215.79 ,
  174.16 ,
  191.29 ,
  192.69 ,
  156.69 ,
  161.62 ,
  212.23 ,
  181.47 ,
  177.56 ,
  132.24 ,
  176.53 ,
  208.30 ,
  202.76 ,
  180.39 ,
  124.10,
  233.00 ,
  127.18 ,
  169.70 ,
  188.45 ,
  127.86 ,
  212.23 ,
  225.27 ,
  181.47 ,
  160.10 ,
  166.26 ,
  188.29 ,
  193.99 ,
  149.66 ,
  180.08 ,
  229.69 ,
  173.45 ,
  180.38 ,
  163.37 ,
  178.41 ,
  167.15 ,
  149.24 ,
  184.25 ,
  196.12 ,
  169.61 ,
  188.28,
  140.70 ,
  164.31 ,
  227.91 ,
  178.24 ,
  190.81 ,
  204.37 ,
  199.88 ,
  208.39 ,
  170.24 ,
  180.55 ,
  207.81 ,
  194.14 ,
  148.19 ,
  173.50 ,
  194.04 ,
  179.43 ,
  163.63 ,
  175.36 ,
  169.28 ,
  190.48 ,
  201.31 ,
  147.13 ,
  156.41 ,
  144.49 ,
  202.19 ,
  169.22 ,
  153.93 ,
  160.11 ,
  155.32 ,
  199.10 ,
  204.63 ,
  196.36 ,
  145.08 ,
  185.42 ,
  182.90,
  193.47)

Personagens.cap <- c("Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred","Fred" ,"Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred","Fred","Fred","Fred", "Fred" , "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred", "Fred" , "Fred", "Fred" ,"Fred" , "Fred" , "Fred" , "Fred" , "Fred" , "Fred" , "Fred" , "Fred" , "Fred", "Fred" , "Fred" , "Fred" , "Fred" , "Fred" , "Fred" , "Fred", "Fred" , "Fred" ,"Fred", "Fred" ,"Fred" , "Fred" , "Fred", "Fred" , "Fred" , "Fred" , "Fred" , "Fred" , "Fred" , "Fred" , "Fred" ,"Fred" ,"Fred" , "Fred" , "Fred" , "Fred",  "Fred" , "Fred" , "Fred" ,"Fred" , "Fred" , "Fred", "Fred", "Fred" , "Fred", "Fred", "Fred", "Fred", 
"Daphnie", "Daphnie", "Daphnie","Daphnie", "Daphnie", "Daphnie", "Daphnie" , "Daphnie" , "Daphnie" , "Daphnie" , 
"Daphnie", 
"Daphnie" , 
"Daphnie" , 
"Daphnie" , "Daphnie" , 
"Daphnie" , 
"Daphnie" , 
"Daphnie" , 
"Daphnie" , "Daphnie" , 
"Daphnie", "Daphnie" , "Daphnie" , "Daphnie" , "Daphnie" , "Daphnie" , 
"Daphnie" , "Daphnie" , "Daphnie" , "Daphnie" , "Daphnie",  "Daphnie" ,"Daphnie" , "Daphnie", "Daphnie", 
"Daphnie", "Daphnie", 
"Velma", 
"Velma", "Velma", "Velma", "Velma","Velma", "Velma", "Velma", 
"Velma", "Velma", "Velma", "Velma", "Velma", "Velma", "Velma","Velma","Velma","Velma", 
"Velma", 
"Velma", "Velma", 
"Velma", 
"Velma", 
"Velma", 
"Velma", "Velma", "Velma", 
"Velma", 
"Velma", 
"Velma", 
"Velma", "Velma", 
"Velma", "Velma", "Velma", "Velma", 
"Velma", "Velma","Velma","Velma","Velma", "Velma", "Velma","Velma", "Velma", "Velma", "Velma","Velma", "Velma", "Velma", "Velma", "Velma", "Velma", "Velma", 
"Velma", "Velma","Velma","Velma", 
"Velma", 
"Velma", 
"Velma", "Velma","Velma", "Velma", "Velma","Velma", "Velma", "Velma", "Velma", "Velma", "Velma", 
"Velma", 
"Velma", 
"Velma", "Velma","Velma", "Velma", 
"Velma", 
"Velma", "Velma", "Velma", "Velma", "Velma", "Velma", "Velma", "Velma", "Velma","Velma", 
"Velma", 
"Velma", 
"Velma", 
"Velma", "Velma", "Velma", 
"Shaggy",  
"Shaggy", "Shaggy", 
"Shaggy", "Shaggy", "Shaggy", 
"Shaggy", 
"Shaggy", "Shaggy", 
"Shaggy", "Shaggy", 
"Shaggy", 
"Shaggy", 
"Scooby","Scooby", 
"Scooby", 
"Scooby", "Scooby", "Scooby", 
"Scooby","Scooby", "Scooby", 
"Scooby", "Scooby", "Scooby", "Scooby", "Scooby", 
"Scooby", "Scooby", "Scooby", 
"Scooby", 
"Scooby", "Scooby", "Scooby","Scooby", 
"Scooby", "Other", 
"Other","Other","Other", "Other","Other", "Other", "Other","Other", "Other", 
"Other", 
"Other","Other", 
"Other", 
"Other", "Other", "Other", 
"Other", "Other", "Other", 
"Other", "Other", "Other", 
"Other", "Other","Other", 
"Other", 
"Other", 
"Other", "Other", "Other", "Other", "Other", "Other","Other") 

sla$Personagens.cap <- sla$Personagens.cap %>% str_replace('Fred', 'Fred')
sla$Personagens.cap <- sla$Personagens.cap %>% str_replace('Shaggy', 'Salsicha')
sla$Personagens.cap <- sla$Personagens.cap %>% str_replace('Other', 'Outro')
sla <- data.frame(engajamento, Personagens.cap)  

ggplot(sla) +
  aes(x = Personagens.cap, y = engajamento) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Personagens", y = "Engajamento") +
  theme_estat()
ggsave("box_bi.pdf", width = 158, height = 93, units = "mm")

ggplot(sla) +
  aes(x = reorder(Personagens.cap, engajamento, FUN = median), y = engajamento) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Personagem", y = "Engajamento") +
  theme_estat()
ggsave("box_biC.pdf", width = 158, height = 93, units = "mm")
  
tentativa2 <- sla 
  
ggplot(tentativa2) +
  aes(x = reorder(Personagens.cap, engajamento, FUN = median), y = engajamento) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Engajamento", y = "Personagem") +
  theme_estat()  
  
  
# correção análise 5 

correção <- data.frame(banco$caught_fred, banco$caught_daphnie, banco$caught_velma, banco$caught_shaggy, banco$caught_scooby, banco$caught_other, banco$caught_not, banco$engagement)
Fred <- data.frame(banco$caught_fred, banco$engagement)
Fred <- filter(Fred, banco.caught_fred %in% c('True'))

Daphnie <- data.frame(banco$caught_daphnie, banco$engagement)
Daphnie <- filter(Daphnie, banco.caught_daphnie %in% c('True'))

Velma <- data.frame(banco$caught_velma, banco$engagement)
Velma <- filter(Velma, banco.caught_velma %in% c('True'))

Salsicha <- data.frame(banco$caught_shaggy, banco$engagement)
Salsicha <- filter(Salsicha, banco.caught_shaggy %in% c('True'))

Scooby <- data.frame(banco$caught_scooby, banco$engagement)
Scooby <- filter(Scooby, banco.caught_scooby %in% c('True'))

Outro <- data.frame(banco$caught_other, banco$engagement)
Outro <- filter(Outro, banco.caught_other %in% c('True'))

Não <- data.frame(banco$caught_not, banco$engagement)
Não <- filter(Não, banco.caught_not %in% c('True'))

engajamento <- c(
                 178.03,
                 162.79,
                 161.62,
                 151.86,
                 162.40,
                 212.58,
                 151.85,
                 177.33,
                 195.57,
                 168.91,
                 145.70,
                 188.39,
                 181.95,
                 229.42,
                 204.37,
                 194.24,
                 155.97,
                 183.95,
                 181.53,
                 199.88,
                 130.57,
                 149.75,
                 190.66,
                 208.30,
                 153.25,
                 169.22,
                 170.09,
                 118.82,
                 186.48,
                 153.32,
                 213.50,
                 184.02,
                 161.77,
                 178.20,
                 216.71,
                 160.18,
                 196.26,
                 200.78,
                 146.76,
                 190.30,
                 196.84,
                 208.02,
                 189.52,
                 175.59,
                 173.47,
                 162.50,
                 163.37,
                 144.93,
                 176.83,
                 198.78,
                 204.21,
                 221.64,
                 199.90,
                 196.12,
                 144.82,
                 173.19,
                 213.97,
                 210.61,
                 167.24,
                 191.04,
                 124.40,
                 183.56,
                 226.21,
                 197.17,
                 176.37,
                 169.44,
                 246.92,
                 186.27,
                 161.47,
                 190.81,
                 146.53,
                 160.88,
                 173.32,
                 142.18,
                 173.41,
                 203.68,
                 200.12,
                 199.89,
                 153.15,
                 138.36,
                 175.48,
                 141.21,
                 249.06,
                 153.80,
                 155.89,
                 127.18,
                 199.81,
                 143.84,
                 157.37,
                 187.32,
                 214.00,
                 198.39,
                 156.86,
                 135.96,
                 177.05,
                 205.63,
                 201.20,
                 175.93,
                 185.16,
                 204.35,
                 200.25,
                 194.16,
                 146.59,
                 131.93,
                 162.48,
                 159.64,
                 179.76,
                 173.76,
                 181.19,
                 205.93,
                 163.66,
                 136.39,
                 188.72,
                 167.23,
                 169.28,
                 158.01,
                 190.48,
                 248.14,
                 160.01,
                 201.31,
                 188.10,
                 156.41,
                 144.49,
                 140.70,
                 202.19,
                 153.93,
                 160.11,
                 140.78,
                 174.16,
                 191.29,
                 168.79,
                 173.72,
                 194.24,
                 118.82,
                 205.61,
                 200.03,
                 184.02,
                 161.77,
                 157.42,
                 196.26,
                 200.78,
                 165.98,
                 173.47,
                 144.79,
                 144.93,
                 160.84,
                 138.36,
                 190.47,
                 205.65,
                 185.16,
                 204.35,
                 147.95,
                 195.78,
                 245.06,
                 181.19,
                 189.72,
                 167.49,
                 200.36,
                 160.01,
                 199.10,
                 182.90,
                 194.24,
                 129.78,
                 180.55,
                 183.26,
                 159.20,
                 180.38,
                 184.02,
                 165.00,
                 216.71,
                 187.67,
                 184.66,
                 179.05,
                 200.78,
                 208.02,
                 199.08,
                 201.56,
                 173.47,
                 204.07,
                 238.05,
                 180.04,
                 160.84,
                 159.98,
                 138.36,
                 168.31,
                 212.71,
                 164.91,
                 185.16,
                 204.35,
                 176.74,
                 161.71,
                 187.88,
                 201.36,
                 181.19,
                 153.30,
                 205.19,
                 164.09,
                 160.01,
                 131.98,
                 176.07,
                 156.69,
                 225.09,
                 188.45,
                 199.58,
                 210.07,
                 196.80,
                 191.48,
                 162.64,
                 170.86,
                 212.23,
                 119.51,
                 188.38,
                 181.47,
                 148.58,
                 145.70,
                 211.43,
                 190.81,
                 205.03,
                 194.24,
                 172.45,
                 134.81,
                 183.95,
                 132.24,
                 140.01,
                 244.07,
                 149.75,
                 202.75,
                 123.47,
                 170.24,
                 177.38,
                 207.86,
                 218.35,
                 205.46,
                 152.40,
                 195.06,
                 160.44,
                 188.45,
                 176.45,
                 207.81,
                 188.97,
                 200.03,
                 180.39,
                 175.48,
                 128.99,
                 184.02,
                 156.12,
                 163.37,
                 189.66,
                 200.78,
                 161.89,
                 178.41,
                 162.26,
                 165.98,
                 173.47,
                 162.50,
                 184.68,
                 173.50,
                 176.83,
                 169.02,
                 191.17,
                 104.88,
                 159.98,
                 177.76,
                 189.54,
                 198.06,
                 143.84,
                 192.15,
                 179.47,
                 185.16,
                 200.25,
                 182.64,
                 181.19,
                 154.45,
                 180.09,
                 171.16,
                 180.86,
                 160.01,
                 196.41,
                 180.29,
                 188.45,
                 176.45,
                 149.66,
                 207.81,
                 173.67,
                 211.46,
                 171.24,
                 202.76,
                 193.07,
                 153.37,
                 200.03,
                 203.62,
                 161.23,
                 160.52,
                 186.22,
                 169.37,
                 169.39,
                 180.48,
                 180.39,
                 148.19,
                 169.57,
                 169.97,
                 121.99,
                 130.72,
                 159.89,
                 175.48,
                 160.41,
                 216.10,
                 232.26,
                 132.23,
                 183.26,
                 147.88,
                 201.92,
                 228.92,
                 166.60,
                 168.21,
                 173.45,
                 143.04,
                 194.07,
                 186.47,
                 194.57,
                 211.70,
                 178.53,
                 165.00,
                 156.12,
                 164.55,
                 167.83,
                 220.17,
                 154.90,
                 164.60,
                 163.37,
                 189.66,
                 205.76,
                 178.20,
                 195.27,
                 157.42,
                 154.76,
                 160.18,
                 200.78,
                 190.30,
                 172.75,
                 162.26,
                 165.98,
                 206.54,
                 162.76,
                 216.32,
                 173.47,
                 184.68,
                 173.50,
                 176.83,
                 169.02,
                 149.24,
                 196.59,
                 184.25,
                 191.17,
                 104.88,
                 181.87,
                 167.24,
                 194.04,
                 152.62,
                 191.04,
                 195.72,
                 173.41,
                 159.29,
                 177.76,
                 138.36,
                 189.54,
                 198.06,
                 183.39,
                 143.84,
                 148.32,
                 145.03,
                 179.47,
                 185.16,
                 200.25,
                 176.74,
                 131.93,
                 168.00,
                 214.38,
                 154.45,
                 180.09,
                 133.40,
                 160.01,
                 159.50,
                 196.41,
                 180.29,
                 192.69,
                 193.47,
                 188.45,
                 173.24,
                 127.86,
                 196.80,
                 190.13,
                 205.93,
                 191.48,
                 162.64,
                 212.23,
                 225.27,
                 205.67,
                 119.51,
                 188.38,
                 181.47,
                 148.58,
                 170.22,
                 149.76,
                 141.60,
                 190.81,
                 204.37,
                 205.03,
                 212.00,
                 172.45,
                 134.81,
                 183.95,
                 228.27,
                 132.24,
                 221.34,
                 176.53,
                 244.07,
                 166.26,
                 188.29,
                 135.19,
                 123.47,
                 153.76,
                 193.19,
                 220.36,
                 193.99,
                 208.39,
                 188.19,
                 203.69,
                 170.24,
                 177.38,
                 129.12,
                 214.71,
                 207.86,
                 218.35,
                 164.02,
                 205.46,
                 172.53,
                 195.06,
                 160.44,
                 164.31,
                 227.91,
                 178.24,
                 204.37,
                 168.79,
                 158.66,
                 207.85,
                 184.22,
                 177.56,
                 163.54,
                 199.88,
                 129.12,
                 125.29,
                 207.86,
                 135.08,
                 168.46,
                 163.34,
                 141.09,
                 190.79,
                 176.45,
                 191.15,
                 187.72,
                 211.46,
                 184.72,
                 208.48,
                 194.14,
                 176.34,
                 158.05,
                 122.77,
                 175.65,
                 166.97,
                 181.26,
                 139.95,
                 194.39,
                 168.87,
                 160.58,
                 245.91,
                 130.75,
                 124.10,
                 228.92,
                 233.00,
                 161.77,
                 164.30,
                 191.00,
                 207.53,
                 183.72,
                 192.23,
                 169.61,
                 163.63,
                 188.28,
                 189.54,
                 148.32,
                 171.46,
                 204.35,
                 166.33,
                 131.93,
                 214.38,
                 213.32,
                 169.28,
                 169.70,
                 134.89,
                 120.07,
                 152.74,
                 213.72,
                 180.64,
                 204.88,
                 177.40,
                 203.91,
                 180.85,
                 215.79,
                 147.13,
                 180.29,
                 176.19,
                 144.49,
                 169.22,
                 144.98,
                 155.32,
                 196.36,
                 168.79,
                 179.22,
                 145.08,
                 185.42,
                 140.62,
                 176.07,
                 138.14,
                 181.25,
                 143.39,
                 192.92,
                 217.69,
                 157.46,
                 166.56,
                 147.30,
                 154.11,
                 126.69,
                 194.70,
                 207.93,
                 211.07,
                 199.51,
                 127.76,
                 221.51,
                 248.02,
                 153.05,
                 191.77,
                 200.27,
                 180.23,
                 192.95,
                 166.68,
                 137.04,
                 191.20,
                 159.29,
                 189.33,
                 190.25,
                 199.81,
                 184.77,
                 158.89)

personagens <- c(
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Fred",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Daphnie",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Velma",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Salsicha",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Scooby",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Outro",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado",
                 "Não capturado")

tabela <- data.frame(personagens, engajamento)

ggplot(tabela) +
  aes(x = reorder(personagens, engajamento, FUN = median), y = engajamento) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Personagem", y = "Engajamento") +
  theme_estat()
ggsave("box_biD.pdf", width = 158, height = 93, units = "mm")

summary(Fred)
sd(Fred$banco.engagement)
var(Fred$banco.engagement)

summary(Outro)
sd(Outro$banco.engagement)
var(Outro$banco.engagement)

summary(Scooby)
sd(Scooby$banco.engagement)
var(Scooby$banco.engagement)

summary(Salsicha)
sd(Salsicha$banco.engagement)
var(Salsicha$banco.engagement)

summary(Velma)
sd(Velma$banco.engagement)
var(Velma$banco.engagement)

summary(Não)
sd(Não$banco.engagement)
var(Não$banco.engagement)

summary(Daphnie)
sd(Daphnie$banco.engagement)
var(Daphnie$banco.engagement)
