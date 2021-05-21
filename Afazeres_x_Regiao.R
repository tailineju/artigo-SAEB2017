library(tidyverse)
#library(sqldf)
library(dplyr)
library(RColorBrewer)

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


amostra$REGIAO <- amostra$REGIAO%>%
  str_replace("1", "Norte")%>%
  str_replace("2", "Nordeste")%>%
  str_replace("3", "Sudeste")%>%
  str_replace("4", "Sul")%>%
  str_replace("5", "Centro-Oeste")



amostra <- amostra %>% 
  filter(!is.na(AFAZERES_DOM)) %>%
  filter(!is.na(REGIAO))


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


M <- as.table(rbind(A$Ni, B$Ni, C$Ni, D$Ni, E$Ni))
dimnames(M) <- list(afazeres = c("A","B", "C","D","E"),
                    regiao =c("Centro-Oeste", "Nordeste","Norte", "Sudeste","Sul") )
M
(R<-chisq.test(M))

# tabela de conting?ncia dos valores esperados
ME = rbind(R$expected, 
           total=apply(R$expected,2,sum))
ME

# conclus?o que n?o h? independ?ncia entre afazeres dom?sticos e regi?es
# h? uma associa??o entre afazeres dom?sticos e regi?o

regioes <- rbind(A, B, C, D, E)

regioes$AFAZERES_DOM <- regioes$AFAZERES_DOM%>%
  str_replace("^A$", "Menos de 1 hora")%>%
  str_replace("^B$", "Entre 1 e 2 horas")%>%
  str_replace("^C$", "Mais de 2 horas")%>%
  str_replace("^D$", "Mais de 3 horas")%>%
  str_replace("^E$", "Não faz")

ordem_ad <- c("Menos de 1 hora", "Entre 1 e 2 horas", "Mais de 2 horas", "Mais de 3 horas", "Não faz")
  
ggplot(data=regioes,aes(x=factor(AFAZERES_DOM,levels = ordem_ad), y=Fi,fill=REGIAO)) + 
  geom_bar(stat="identity",position="stack")+
  labs(x="Tempo gasto em afazeres domésticos", y="Frequência relativa")+
  scale_fill_brewer(palette="Blues")+
  theme.t()+
  ggsave("imagens/ad-regiao.png", width = 158, height = 93, units = "mm")

