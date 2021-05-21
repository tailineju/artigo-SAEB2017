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

amostra <- amostra %>% 
  filter(!is.na(AFAZERES_DOM)) %>%
  filter(!is.na(USO_TEMPO_TELAS))

#Categorização ----
A <- amostra %>%
  group_by(AFAZERES_DOM, USO_TEMPO_TELAS) %>%
  filter(AFAZERES_DOM=="A")%>%
  summarise(Ni = n()) 
total <- sum(A$Ni)
A <- A %>% mutate(Fi = round((Ni/total)*100,4))

B <- amostra %>%
  group_by(AFAZERES_DOM, USO_TEMPO_TELAS) %>%
  filter(AFAZERES_DOM=="B")%>%
  summarise(Ni= n())%>%
  na.omit()
total <- sum(B$Ni)
B <- B %>%  mutate(Fi = round((Ni/total)*100,4))

C <- amostra %>%   group_by(AFAZERES_DOM, USO_TEMPO_TELAS) %>%
  filter(AFAZERES_DOM=="C")%>%
  summarise(Ni= n())%>%
  na.omit()
total <- sum(C$Ni)
C <- C %>%  mutate(Fi = round((Ni/total)*100,4))

D <- amostra %>%  group_by(AFAZERES_DOM, USO_TEMPO_TELAS) %>%
  filter(AFAZERES_DOM=="D")%>%
  summarise(Ni= n())%>%
  na.omit()
total <- sum(D$Ni)
D <- D %>%  mutate(Fi = round((Ni/total)*100,4))

E <- amostra %>%
  group_by(AFAZERES_DOM, USO_TEMPO_TELAS) %>%
  filter(AFAZERES_DOM=="E")%>%
  summarise(Ni= n())%>%
  na.omit()
total <- sum(E$Ni)
E <- E %>%  mutate(Fi = round((Ni/total)*100,4))

#Teste de independência ----

M <- as.table(rbind(A$Ni, B$Ni, C$Ni, D$Ni, E$Ni))
dimnames(M) <- list(afazeres = c("A", "B","C", "D","E"),
                    tempotela = c("A","B", "C","D","E"))
M
(R <-chisq.test(M))

#Tabela de contingência ----
ME = rbind(R$expected, 
           total=apply(R$expected,2,sum))
ME

# Conclusão que há indepedência entre afazeres domésticos e tempo de tela
# Não há associação entre entre afazeres domésticos e tempo de tela

tempotela <- rbind(A, B, C, D, E)

#Limpeza dos dados ----

tempotela$USO_TEMPO_TELAS %<>%
  str_replace("^A$", "Menos de 1h/não vê")%>% 
  str_replace("^B$", "Entre 1h e 2h")%>% 
  str_replace("^C$", "Mais de 2h")%>% 
  str_replace("^D$", "Mais de 3h")%>% 
  str_replace("^E$", "Menos de 1h/não vê")

tempotela$AFAZERES_DOM <- tempotela$AFAZERES_DOM%>%
  str_replace("^A$", "Menos de 1h")%>%
  str_replace("^B$", "Entre 1h e 2h")%>%
  str_replace("^C$", "Mais de 2h")%>%
  str_replace("^D$", "Mais de 3h")%>%
  str_replace("^E$", "Não faz")

ordem_ad <- c("Menos de 1h", "Entre 1h e 2h", "Mais de 2h", "Mais de 3h", "Não faz")
ordem_telas <- rev(c("Menos de 1h/não vê","Entre 1h e 2h","Mais de 2h","Mais de 3h"))


#Análise gráfica ----

ggplot(data=tempotela,aes(x=factor(AFAZERES_DOM,levels = ordem_ad), y=Fi,
                          fill=factor(USO_TEMPO_TELAS,levels = ordem_telas))) + 
  geom_bar(stat="identity",position="stack")+
  labs(x="Tempo gasto em afazeres domésticos", y="Frequência relativa", fill="Tempo de telas")+
  scale_fill_brewer(palette="Blues")+
  theme.t()+
  ggsave("imagens/ad-tela.png", width = 158, height = 93, units = "mm")

