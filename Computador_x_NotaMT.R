library(tidyverse)
library(sqldf)
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

amostra <- sample_n(df, 100)


computador <- amostra %>% 
  filter(!is.na(COMPUTADOR)) %>%
  filter(!is.na(NOTA_MT)) %>%
  group_by(COMPUTADOR) %>% 
  summarize(
    qtd = n()
  )
computador
computador_A <- amostra %>% 
  filter(!is.na(COMPUTADOR)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(COMPUTADOR=='A') %>%
  select(NOTA_MT) 
computador_A
computador_B <- amostra %>% 
  filter(!is.na(COMPUTADOR)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(COMPUTADOR=='B') %>%
  select(NOTA_MT) 
computador_C <- amostra %>% 
  filter(!is.na(COMPUTADOR)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(COMPUTADOR=='C') %>%
  select(NOTA_MT) 
computador_D <- amostra %>% 
  filter(!is.na(COMPUTADOR)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(COMPUTADOR=='D') %>%
  select(NOTA_MT) 


write_csv(
  tempotela_A,
  "C:\\Users\\SONY\\Desktop\\unb\\TrabalhoFinal\\computador_a.csv",
  na = "NA",
  append = FALSE,
  quote_escape = "double",
  eol = "\n",
)

computador_Todos <- amostra %>% 
  filter(!is.na(COMPUTADOR)) %>%
  filter(!is.na(NOTA_MT)) %>%
  filter(COMPUTADOR=='A'|COMPUTADOR=='B'|COMPUTADOR=='C'|COMPUTADOR=='D') %>%
  select(COMPUTADOR,NOTA_MT) 

ggplot(computador_Todos, aes(x=COMPUTADOR, y=NOTA_MT)) +
  geom_boxplot(fill=c("#A11D21"), width = 0.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Computador", y="Nota de Matemática") +
  theme_bw()


