library(tidyverse)
#library(sqldf)
library(dplyr)

#Ler csv
#todosdadosAmostra <- read.csv("C:\\Users\\SONY\\Desktop\\unb\\Atividade2.2\\amostra_180111558.csv")

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

amostra <- sample_n(df, 200)


tempotela <- amostra %>% 
  filter(!is.na(USO_TEMPO_TELAS)) %>%
  filter(!is.na(NOTA_MT)) %>%
  group_by(USO_TEMPO_TELAS) %>% 
  summarize(
    qtd = n()
  )
tempotela

AE <- amostra %>% 
  filter(!is.na(USO_TEMPO_TELAS)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(USO_TEMPO_TELAS=='A'|USO_TEMPO_TELAS=='E') %>%
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


############ mediana

mediana<-median(c(AE$NOTA_MT,B$NOTA_MT,C$NOTA_MT,D$NOTA_MT)) #mediana geral
ae<-c(sum(AE$NOTA_MT>mediana),sum(AE$NOTA_MT<=mediana))
b<-c(sum(B$NOTA_MT>mediana),sum(B$NOTA_MT<=mediana))
c<-c(sum(C$NOTA_MT>mediana),sum(C$NOTA_MT<=mediana))
d<-c(sum(D$NOTA_MT>mediana),sum(D$NOTA_MT<=mediana))
dados<-as.table(cbind(ae,b,c,d))
chisq.test(dados)

#############Kruskal-Wallis
kruskal.test(tempotela_Todos$NOTA_MT, tempotela_Todos$USO_TEMPO_TELAS)

######## dataset filtrado
tempotela_Todos <- amostra %>% 
  filter(!is.na(USO_TEMPO_TELAS)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(USO_TEMPO_TELAS=='A'|USO_TEMPO_TELAS=='B'|USO_TEMPO_TELAS=='C'|USO_TEMPO_TELAS=='D'|USO_TEMPO_TELAS=='E') %>%
  select(USO_TEMPO_TELAS,NOTA_MT) 


tempotela_Todos$USO_TEMPO_TELAS %<>%
  str_replace_all("^A$", "AE") %>%
  str_replace_all("^E$", "AE")


ggplot(tempotela_Todos, aes(x=USO_TEMPO_TELAS, y=NOTA_MT)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Tempo de tela", y="Nota de Matemática") +
  theme_bw()

# testando normalidade para
shapiro.test(tempotela_Todos$NOTA_MT)
shapiro.test(c(AE$NOTA_MT,B$NOTA_MT,C$NOTA_MT,D$NOTA_MT))
ad.test(c(AE$NOTA_MT,B$NOTA_MT,C$NOTA_MT,D$NOTA_MT))
ad.test(tempotela_Todos$NOTA_MT)

#teste de variância
#### NOTA_LP e USO_TEMPO_TELAS
LeveneTest(tempotela_Todos$NOTA_MT, tempotela_Todos$USO_TEMPO_TELAS, center = mean)
LeveneTest(tempotela_Todos$NOTA_MT, tempotela_Todos$USO_TEMPO_TELAS, center = median)
bartlett.test(tempotela_Todos$NOTA_MT, tempotela_Todos$USO_TEMPO_TELAS)

#Anova com um fator
aov_res <- aov(tempotela_Todos$NOTA_MT ~ tempotela_Todos$USO_TEMPO_TELAS)
summary (aov_res)

pairwise.t.test(tempotela_Todos$NOTA_MT, tempotela_Todos$USO_TEMPO_TELAS, p.adjust.method = "bonferroni" )
pairwise.wilcox.test(tempotela_Todos$NOTA_MT, tempotela_Todos$USO_TEMPO_TELAS, p.adjust.method="bonferroni")
