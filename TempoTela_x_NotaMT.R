#Carregando pacotes ----

if (!require(pacman)) {
  install.package("pacman")
  library(pacman)}

pacman::p_load(tidyverse,nortest) #verificar qual pacote tem Levene

theme.t <- function(position_legend = "top"){
  return(list(
    theme_bw(),
    theme(axis.title.y=element_text(colour="black", size=12),
          axis.title.x = element_text(colour="black", size=12),
          axis.text = element_text(colour = "black", size=9.5),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black")),
    theme(legend.position=position_legend)))}

#Dados ----
set.seed(123)
df <- read_csv("amostra_180111558.csv",
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

#Limpeza dos dados ----

tempotela <- amostra %>% 
  filter(!is.na(USO_TEMPO_TELAS)) %>%
  filter(!is.na(NOTA_MT)) %>%
  group_by(USO_TEMPO_TELAS) %>% 
  summarize(
    qtd = n()
  )

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


#Teste da Mediana (Qui-Quadrado) ----
mediana<-median(c(AE$NOTA_MT,B$NOTA_MT,C$NOTA_MT,D$NOTA_MT)) #mediana geral
ae<-c(sum(AE$NOTA_MT>mediana),sum(AE$NOTA_MT<=mediana))
b<-c(sum(B$NOTA_MT>mediana),sum(B$NOTA_MT<=mediana))
c<-c(sum(C$NOTA_MT>mediana),sum(C$NOTA_MT<=mediana))
d<-c(sum(D$NOTA_MT>mediana),sum(D$NOTA_MT<=mediana))
dados<-as.table(cbind(ae,b,c,d))
chisq.test(dados)


#Limpeza dos dados II ----
tempotela_Todos <- amostra %>% 
  filter(!is.na(USO_TEMPO_TELAS)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(USO_TEMPO_TELAS=='A'|USO_TEMPO_TELAS=='B'|USO_TEMPO_TELAS=='C'|USO_TEMPO_TELAS=='D'|USO_TEMPO_TELAS=='E') %>%
  select(USO_TEMPO_TELAS,NOTA_MT) 


tempotela_Todos$USO_TEMPO_TELAS %<>%
  str_replace("^A$", "Menos de 1h ou não vê")%>% 
  str_replace("^B$", "Entre 1h e 2h")%>% 
  str_replace("^C$", "Mais de 2h")%>% 
  str_replace("^D$", "Mais de 3h")%>% 
  str_replace("^E$", "Menos de 1h ou não vê")

ordem_telas <- rev(c("Menos de 1h ou não vê","Entre 1h e 2h","Mais de 2h","Mais de 3h"))

#Análise gráfica ----
ggplot(tempotela_Todos, aes(x=factor(USO_TEMPO_TELAS,levels = ordem_telas), y=NOTA_MT)) +
  geom_boxplot(fill=c("#7AA3CC"), width = 0.5) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Tempo de uso de telas", y="Nota de Matemática") +
  theme.t()+
  coord_flip()+
  ggsave("imagens/mt-telas.png", width = 158, height = 93, units = "mm")

#Testes para normalidade ----
shapiro.test(tempotela_Todos$NOTA_MT)
shapiro.test(c(AE$NOTA_MT,B$NOTA_MT,C$NOTA_MT,D$NOTA_MT))
ad.test(c(AE$NOTA_MT,B$NOTA_MT,C$NOTA_MT,D$NOTA_MT))
ad.test(tempotela_Todos$NOTA_MT)

#Testes para igualdade de variâncias ----
LeveneTest(tempotela_Todos$NOTA_MT, tempotela_Todos$USO_TEMPO_TELAS, center = mean)
LeveneTest(tempotela_Todos$NOTA_MT, tempotela_Todos$USO_TEMPO_TELAS, center = median)
bartlett.test(tempotela_Todos$NOTA_MT, tempotela_Todos$USO_TEMPO_TELAS)

#Anova com um fator ----
aov_res <- aov(tempotela_Todos$NOTA_MT ~ tempotela_Todos$USO_TEMPO_TELAS)
summary (aov_res)

#Comparação de médias ----
pairwise.t.test(tempotela_Todos$NOTA_MT, tempotela_Todos$USO_TEMPO_TELAS, p.adjust.method = "bonferroni" )


#########################################################################
#Testes não paramétricos (caso não fosse assumida normalidade) ----

#Kruskal-Wallis
kruskal.test(tempotela_Todos$NOTA_MT, tempotela_Todos$USO_TEMPO_TELAS)
#Comparação de médias 
pairwise.wilcox.test(tempotela_Todos$NOTA_MT, tempotela_Todos$USO_TEMPO_TELAS, p.adjust.method="bonferroni")
