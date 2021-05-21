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

amostra$AFAZERES_DOM <- amostra$AFAZERES_DOM%>%
  str_replace("^A$", "Menos de 1 hora")%>%
  str_replace("^B$", "Entre 1 e 2 horas")%>%
  str_replace("^C$", "Mais de 2 horas")%>%
  str_replace("^D$", "Mais de 3 horas")%>%
  str_replace("^E$", "N?o faz")

amostra$SEXO <- amostra$SEXO%>%
  str_replace("A", "Masculino")%>%
  str_replace("B", "Feminino")


amostra <- amostra %>% 
  filter(!is.na(AFAZERES_DOM)) %>%
  filter(!is.na(SEXO))

#Categoriza??o ----

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


#Teste de independ?ncia ----
# https://rpubs.com/EstatBasica/Cap14
M <- as.table(rbind(fem$Ni, masc$Ni))
dimnames(M) <- list(sexo = c("F", "M"),afazeres = c("A","B", "C","D","E"))
M
(R <- chisq.test(M))


# tabela de conting?ncia dos valores esperados
ME = rbind(R$expected, 
                total=apply(R$expected,2,sum))
ME


#conclus?o que n?o h? independ?ncia entre o sexo e os afazeres dom?sticos
#problema est? na categoria B, olha os valores na tabela M e na ME
# h? associa??o entre o sexo e afazeres dom?sticos


#teste de homogeneidade uma das marginais dee estar fixada
#https://www.youtube.com/watch?v=MixF1KzoJao
#https://www.youtube.com/watch?v=tR60jzlGKHg

#An?lise gr?fica ----

ordem_ad <- c("Menos de 1 hora", "Entre 1 e 2 horas", "Mais de 2 horas", "Mais de 3 horas", "N?o faz")

ggplot(data=amostra) + 
  geom_bar(mapping=aes(x=factor(AFAZERES_DOM,levels = ordem_ad), fill=SEXO),
           position="dodge")+
  labs(x="Afazeres dom?sticos por sexo", y="Frequ?ncia absoluta") +
  scale_fill_brewer(palette="Paired")+
  theme.t()+
  ggsave("imagens/ad-sexo.png", width = 158, height = 93, units = "mm")

ggplot(data=amostra) + 
  geom_bar(
    mapping=aes(x=SEXO, fill=AFAZERES_DOM),
    position="dodge"
  )+
  labs(x="Afazeres dom?sticos por sexo", y="Ni") +
  theme_bw()
