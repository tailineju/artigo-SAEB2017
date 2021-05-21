library(tidyverse)
#library(sqldf)
library(dplyr)

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

# tabela de contingência dos valores esperados
ME = rbind(R$expected, 
           total=apply(R$expected,2,sum))
ME

# conclusão que não há independência entre afazeres domésticos e regiões
# há uma associação entre afazeres domésticos e região

regioes <- rbind(A, B, C, D, E)
  
ggplot(data=regioes,aes(x=AFAZERES_DOM, y=Fi,fill=REGIAO)) + 
  geom_bar(
    stat="identity",position="stack"
#    position="dodge"
  )+
  labs(x="Afazeres domésticos por região", y="") +
  theme_bw()

