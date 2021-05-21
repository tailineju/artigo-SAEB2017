#Carregando pacotes ----

if (!require(pacman)) {
  install.package("pacman")
  library(pacman)}

pacman::p_load(tidyverse,dplyr,RColorBrewer)

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

amostra$REGIAO <- amostra$REGIAO%>%
  str_replace("1", "Norte")%>%
  str_replace("2", "Nordeste")%>%
  str_replace("3", "Sudeste")%>%
  str_replace("4", "Sul")%>%
  str_replace("5", "Centro-Oeste")

amostra <- amostra %>% 
  filter(!is.na(AFAZERES_DOM)) %>%
  filter(!is.na(REGIAO))

#Categorização ----

A <- amostra %>%
  group_by(AFAZERES_DOM, REGIAO) %>%
  filter(AFAZERES_DOM=="A")%>%
  summarise(Ni = n()) 
total <- sum(A$Ni)
A <- A %>%
  mutate(Fi = round((Ni/total)*100,4))

B <- amostra %>%
  group_by(AFAZERES_DOM, REGIAO) %>%
  filter(AFAZERES_DOM=="B")%>%
  summarise(Ni= n())%>%
  na.omit()
total <- sum(B$Ni)
B <- B %>%
  mutate(Fi = round((Ni/total)*100,4))

C <- amostra %>%
  group_by(AFAZERES_DOM, REGIAO) %>%
  filter(AFAZERES_DOM=="C")%>%
  summarise(Ni= n())%>%
  na.omit()
total <- sum(C$Ni)
C <- C %>%
  mutate(Fi = round((Ni/total)*100,4))

D <- amostra %>%
  group_by(AFAZERES_DOM, REGIAO) %>%
  filter(AFAZERES_DOM=="D")%>%
  summarise(Ni= n())%>%
  na.omit()
total <- sum(D$Ni)
D <- D %>%
  mutate(Fi = round((Ni/total)*100,4))

E <- amostra %>%
  group_by(AFAZERES_DOM, REGIAO) %>%
  filter(AFAZERES_DOM=="E")%>%
  summarise(Ni= n())%>%
  na.omit()
total <- sum(E$Ni)
E <- E %>%
  mutate(Fi = round((Ni/total)*100,4))

#Teste de independência ----
M <- as.table(rbind(A$Ni, B$Ni, C$Ni, D$Ni, E$Ni))
dimnames(M) <- list(afazeres = c("A","B", "C","D","E"),
                    regiao =c("Centro-Oeste", "Nordeste","Norte", "Sudeste","Sul") )
M
(R<-chisq.test(M))

#Tabela de contingência dos valores esperados
ME = rbind(R$expected, 
           total=apply(R$expected,2,sum))
ME

# conclusão que não há independência entre afazeres domésticos e regiões
# há uma associação entre afazeres  domésticos e regiões


#Dados completos e tratados ----
regioes <- rbind(A, B, C, D, E)

regioes$AFAZERES_DOM <- regioes$AFAZERES_DOM%>%
  str_replace("^A$", "Menos de 1h")%>%
  str_replace("^B$", "Entre 1h e 2h")%>%
  str_replace("^C$", "Mais de 2h")%>%
  str_replace("^D$", "Mais de 3h")%>%
  str_replace("^E$", "Não faz")

ordem_ad <- c("Menos de 1h", "Entre 1h e 2h", "Mais de 2h", "Mais de 3h", "Não faz")
ordem_regiao <- c("Norte","Nordeste","Sudeste","Sul","Centro-Oeste")


#Análise gráfica ----

ggplot(data=regioes,aes(x=factor(AFAZERES_DOM,levels = ordem_ad), y=Fi,
                        fill=factor(REGIAO,levels = ordem_regiao))) + 
  geom_bar(stat="identity",position="stack")+
  labs(x="Tempo gasto em afazeres domésticos", y="Frequência relativa",fill="Região")+
  scale_fill_brewer(palette="Blues")+
  theme.t()+
  ggsave("imagens/ad-regiao.png", width = 158, height = 93, units = "mm")

