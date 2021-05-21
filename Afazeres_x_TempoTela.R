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




amostra <- amostra %>% 
  filter(!is.na(AFAZERES_DOM)) %>%
  filter(!is.na(USO_TEMPO_TELAS))

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


M <- as.table(rbind(A$Ni, B$Ni, C$Ni, D$Ni, E$Ni))
dimnames(M) <- list(afazeres = c("A", "B","C", "D","E"),
                    tempotela = c("A","B", "C","D","E"))
M
(R <-chisq.test(M))

# tabela de conting?ncia dos valores esperados
ME = rbind(R$expected, 
           total=apply(R$expected,2,sum))
ME

#conclus?o que h? independ?ncia entre afazeres dom?sticos e tempo de tela
# n?o h? associa??o entre entre afazeres dom?sticos e tempo de tela

tempotela <- rbind(A, B, C, D, E)

ggplot(data=tempotela,aes(x=AFAZERES_DOM, y=Fi,fill=USO_TEMPO_TELAS)) + 
  geom_bar(stat="identity",position="stack")+
  labs(x="Tempo gasto em afazeres domésticos", y="Frequência relativa")+
  scale_fill_brewer(palette="Blues")+
  theme.t()+
  ggsave("imagens/ad-regiao.png", width = 158, height = 93, units = "mm")

