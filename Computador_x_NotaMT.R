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
  filter(!is.na(COMPUTADOR)) %>%
  filter(!is.na(NOTA_MT)) %>%
  group_by(COMPUTADOR) %>% 
  summarize(
    qtd = n()
  )
tempotela

A <- amostra %>% 
  filter(!is.na(COMPUTADOR)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(COMPUTADOR=='A') %>%
  select(NOTA_MT) 
B <- amostra %>% 
  filter(!is.na(COMPUTADOR)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(COMPUTADOR=='B') %>%
  select(NOTA_MT) 
C <- amostra %>% 
  filter(!is.na(COMPUTADOR)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(COMPUTADOR=='C') %>%
  select(NOTA_MT) 
DE <- amostra %>% 
  filter(!is.na(COMPUTADOR)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(COMPUTADOR=='D'|COMPUTADOR=='E') %>%
  select(NOTA_MT) 


############ mediana

mediana<-median(c(A$NOTA_MT,B$NOTA_MT,C$NOTA_MT,D$NOTA_MT,E$NOTA_MT)) #mediana geral
a<-c(sum(A$NOTA_MT>mediana),sum(A$NOTA_MT<=mediana))
b<-c(sum(B$NOTA_MT>mediana),sum(B$NOTA_MT<=mediana))
c<-c(sum(C$NOTA_MT>mediana),sum(C$NOTA_MT<=mediana))
de<-c(sum(DE$NOTA_MT>mediana),sum(DE$NOTA_MT<=mediana))
dados<-as.table(cbind(a,b,c,d,e))
chisq.test(dados)

######## dataset filtrado
computador_Todos <- amostra %>% 
  filter(!is.na(COMPUTADOR)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(COMPUTADOR=='A'|COMPUTADOR=='B'|COMPUTADOR=='C'|COMPUTADOR=='D'|COMPUTADOR=='E') %>%
  select(COMPUTADOR,NOTA_MT) 

#############Kruskal-Wallis
kruskal.test(computador_Todos$NOTA_MT, computador_Todos$COMPUTADOR)



computador_Todos$COMPUTADOR  %<>%
  str_replace("^A$", "Não tem")%>% 
  str_replace("^B$", "Sim, um")%>% 
  str_replace("^C$", "Sim, dois")%>% 
  str_replace("^D$", "Sim, três ou mais")%>% 
  str_replace("^E$", "Sim, três ou mais")

ordem_comp <- c("Não tem","Sim, um","Sim, dois","Sim, três ou mais")

ggplot(computador_Todos, aes(x=factor(COMPUTADOR,levels = ordem_comp), y=NOTA_MT)) +
  geom_boxplot(fill=c("#7AA3CC"), width = 0.5) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Tem computador em casa?", y="Nota de Matemática") +
  theme.t()+
  ggsave("imagens/mt-comp.png", width = 158, height = 93, units = "mm")

# testando normalidade para
shapiro.test(computador_Todos$NOTA_MT)
shapiro.test(c(AE$NOTA_MT,B$NOTA_MT,C$NOTA_MT,D$NOTA_MT))
ad.test(c(AE$NOTA_MT,B$NOTA_MT,C$NOTA_MT,D$NOTA_MT))
ad.test(computador_Todos$NOTA_MT)

#teste de vari?ncia
#### NOTA_LP e COMPUTADOR
LeveneTest(computador_Todos$NOTA_MT, computador_Todos$COMPUTADOR, center = mean)
LeveneTest(computador_Todos$NOTA_MT, computador_Todos$COMPUTADOR, center = median)
bartlett.test(computador_Todos$NOTA_MT, computador_Todos$COMPUTADOR)

#Anova com um fator
aov_res <- aov(computador_Todos$NOTA_MT ~ computador_Todos$COMPUTADOR)
summary (aov_res)

pairwise.t.test(computador_Todos$NOTA_MT, computador_Todos$COMPUTADOR, p.adjust.method = "bonferroni" )
pairwise.wilcox.test(computador_Todos$NOTA_MT, computador_Todos$COMPUTADOR, p.adjust.method="bonferroni")
