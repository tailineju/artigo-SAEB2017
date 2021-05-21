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
df <- read_csv("amostra.csv",
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

NORTE <- amostra %>% 
  filter(!is.na(REGIAO)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(REGIAO==1) %>%
  select(NOTA_MT) 
NORDESTE <- amostra %>% 
  filter(!is.na(REGIAO)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(REGIAO==2) %>%
  select(NOTA_MT) 
SUDESTE <- amostra %>% 
  filter(!is.na(REGIAO)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(REGIAO==3) %>%
  select(NOTA_MT) 
SUL <- amostra %>% 
  filter(!is.na(REGIAO)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(REGIAO==4) %>%
  select(NOTA_MT) 
CENTRO <- amostra %>% 
  filter(!is.na(REGIAO)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(REGIAO==5) %>%
  select(NOTA_MT) 

#Teste da Mediana (Qui-Quadrado) ----

mediana<-median(c(NORTE$NOTA_MT,NORDESTE$NOTA_MT,SUL$NOTA_MT,SUDESTE$NOTA_MT,CENTRO$NOTA_MT)) #mediana geral
norte<-c(sum(NORTE$NOTA_MT>mediana),sum(NORTE$NOTA_MT<=mediana))
nordeste<-c(sum(NORDESTE$NOTA_MT>mediana),sum(NORDESTE$NOTA_MT<=mediana))
sul<-c(sum(SUL$NOTA_MT>mediana),sum(SUL$NOTA_MT<=mediana))
sudeste<-c(sum(SUDESTE$NOTA_MT>mediana),sum(SUDESTE$NOTA_MT<=mediana))
centro<-c(sum(NORTE$NOTA_MT>mediana),sum(NORTE$NOTA_MT<=mediana))
dados<-as.table(cbind(norte,nordeste,sul,sudeste,centro))
chisq.test(dados)

#Limpeza de dados II ----
regiao_Todos <- amostra %>% 
  filter(!is.na(REGIAO)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(REGIAO==1|REGIAO==2|REGIAO==3|REGIAO==4|REGIAO==5) %>%
  select(REGIAO,NOTA_MT) 


regiao_Todos$REGIAO <- regiao_Todos$REGIAO%>%
  str_replace("1", "Norte")%>%
  str_replace("2", "Nordeste")%>%
  str_replace("3", "Sudeste")%>%
  str_replace("4", "Sul")%>%
  str_replace("5", "Centro-Oeste")

ordem_regiao <- c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste")

#Análise gráfica ----

ggplot(regiao_Todos, aes(x=factor(REGIAO,levels = ordem_regiao), y=NOTA_MT)) +
  geom_boxplot(fill=c("#7AA3CC"), width = 0.5) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Região", y="Nota de Matemática") +
  theme.t()+
  ggsave("imagens/mt-regiao.png", width = 158, height = 93, units = "mm")

#Testes para normalidade ----
shapiro.test(regiao_Todos$NOTA_MT)
ad.test(regiao_Todos$NOTA_MT)

#Testes para igualdade de variâncias ----
LeveneTest(regiao_Todos$NOTA_MT, regiao_Todos$REGIAO, center = mean)
LeveneTest(regiao_Todos$NOTA_MT, regiao_Todos$REGIAO, center = median)
bartlett.test(regiao_Todos$NOTA_MT, regiao_Todos$REGIAO)

#Anova com um fator ----
aov_res <- aov(regiao_Todos$NOTA_MT ~ regiao_Todos$REGIAO)
summary (aov_res)

#Comparação de médias ----
pairwise.t.test(regiao_Todos$NOTA_MT, regiao_Todos$REGIAO, p.adjust.method = "bonferroni" )

#########################################################################
#Testes não paramétricos (caso não fosse assumida normalidade) ----

#Kruskal-Wallis
kruskal.test(regiao_Todos$NOTA_MT, regiao_Todos$REGIAO)
#Comparação de médias 
pairwise.wilcox.test(regiao_Todos$NOTA_MT, regiao_Todos$REGIAO, p.adjust.method="bonferroni")
