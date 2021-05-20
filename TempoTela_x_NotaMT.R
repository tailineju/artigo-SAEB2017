library(tidyverse)
library(sqldf)
library(dplyr)

#Ler csv
todosdadosAmostra <- read.csv("C:\\Users\\SONY\\Desktop\\unb\\Atividade2.2\\amostra_180111558.csv")

set.seed(123)
df <- read_csv(
  "C:\\Users\\SONY\\Desktop\\unb\\TrabalhoFinal\\amostra_180111558.csv",
  col_names = TRUE,
  col_types = NULL,
  locale = default_locale(),
  na = c("", "NA"),
  quoted_na = TRUE,
  quote = "\"",
  comment = "",
  trim_ws = TRUE,
  skip = 0,
  progress = show_progress(),
  skip_empty_rows = TRUE
)

amostra <- sample_n(df, 100)

######## dataset filtrado
tempotela_Todos <- amostra %>% 
  filter(!is.na(USO_TEMPO_TELAS)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(USO_TEMPO_TELAS=='A'|USO_TEMPO_TELAS=='B'|USO_TEMPO_TELAS=='C'|USO_TEMPO_TELAS=='D') %>%
  select(USO_TEMPO_TELAS,NOTA_MT) 


tempotela <- amostra %>% 
  filter(!is.na(USO_TEMPO_TELAS)) %>%
  filter(!is.na(NOTA_MT)) %>%
  group_by(USO_TEMPO_TELAS) %>% 
  summarize(
    qtd = n()
  )
tempotela

A <- amostra %>% 
  filter(!is.na(USO_TEMPO_TELAS)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(USO_TEMPO_TELAS=='A') %>%
  select(NOTA_MT) 
B <- amostra %>% 
  filter(!is.na(USO_TEMPO_TELAS)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(USO_TEMPO_TELAS=='B') %>%
  select(NOTA_MT) 
C <- amostra %>% 
  filter(!is.na(USO_TEMPO_TELAS)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(USO_TEMPO_TELAS=='C') %>%
  select(NOTA_MT) 
D <- amostra %>% 
  filter(!is.na(USO_TEMPO_TELAS)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(USO_TEMPO_TELAS=='D') %>%
  select(NOTA_MT) 
E <- amostra %>% 
  filter(!is.na(USO_TEMPO_TELAS)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(USO_TEMPO_TELAS=='E') %>%
  select(NOTA_MT) 


############ mediana
mediana<-median(c(a,b,c,d)) #mediana geral
a<-c(sum(a>mediana),sum(a<=mediana))
b<-c(sum(b>mediana),sum(b<=mediana))
c<-c(sum(c>mediana),sum(c<=mediana))
d<-c(sum(d>mediana),sum(d<=mediana))
dados<-as.table(cbind(a,b,c,d))
chisq.test(dados)

x <- c(A,B,C,D,E)

mediana<-median(c(A$NOTA_MT,B$NOTA_MT,C$NOTA_MT,D$NOTA_MT,E$NOTA_MT)) #mediana geral
a<-c(sum(A$NOTA_MT>mediana),sum(A$NOTA_MT<=mediana))
b<-c(sum(B$NOTA_MT>mediana),sum(B$NOTA_MT<=mediana))
c<-c(sum(C$NOTA_MT>mediana),sum(C$NOTA_MT<=mediana))
d<-c(sum(D$NOTA_MT>mediana),sum(D$NOTA_MT<=mediana))
e<-c(sum(E$NOTA_MT>mediana),sum(E$NOTA_MT<=mediana))
dados<-as.table(cbind(a,b,c,d,e))
chisq.test(dados)

#############Kruskal-Wallis
kruskal.test(tempotela_Todos$NOTA_MT, tempotela_Todos$USO_TEMPO_TELAS)

ggplot(tempotela_Todos, aes(x=USO_TEMPO_TELAS, y=NOTA_MT)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Tempo de tela", y="Nota de Matemática") +
  theme_bw()


