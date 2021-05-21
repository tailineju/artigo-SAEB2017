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

amostra$SEXO <- amostra$SEXO%>%
  str_replace("A", "Masculino")%>%
  str_replace("B", "Feminino")


amostra <- amostra %>% 
  filter(!is.na(AFAZERES_DOM)) %>%
  filter(!is.na(SEXO))


fem <- amostra %>%
  group_by(AFAZERES_DOM, SEXO) %>%
  filter(SEXO=="Feminino")%>%
  summarise(Ni= n())%>%
  na.omit()

masc <- amostra %>%
  group_by(AFAZERES_DOM, SEXO) %>%
  filter(SEXO=="Masculino")%>%
  summarise(Ni= n())%>%
  na.omit()

# https://rpubs.com/EstatBasica/Cap14
M <- as.table(rbind(fem$Ni, masc$Ni))
dimnames(M) <- list(sexo = c("F", "M"),afazeres = c("A","B", "C","D","E"))
M
(R <- chisq.test(M))


# tabela de contingência dos valores esperados
ME = rbind(R$expected, 
                total=apply(R$expected,2,sum))
ME


#conclusão que não há independência entre o sexo e os afazeres domésticos
#problema está na categoria B, olha os valores na tabela M e na ME
# há associação entre o sexo e afazeres domésticos


#teste de homogeneidade uma das marginais dee estar fixada
#https://www.youtube.com/watch?v=MixF1KzoJao
#https://www.youtube.com/watch?v=tR60jzlGKHg

ggplot(data=amostra) + 
  geom_bar(
    mapping=aes(x=AFAZERES_DOM, fill=SEXO),
    position="dodge"
  )+
  labs(x="Afazeres domésticos por sexo", y="Ni") +
  theme_bw()

ggplot(data=amostra) + 
  geom_bar(
    mapping=aes(x=SEXO, fill=AFAZERES_DOM),
    position="dodge"
  )+
  labs(x="Afazeres domésticos por sexo", y="Ni") +
  theme_bw()
