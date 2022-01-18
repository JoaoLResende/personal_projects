library(rtweet)
library(tidytext)
library(tidyverse)
library(here)
library(googlesheets4)
library(googledrive)
library(widyr)
library(janitor)
library(lexiconPT)
library(ggthemes)


setwd(here())

token <- read_sheet(as_id("seu_token_aqui"))

ultimos_dados <- read.csv(file = "BBB/2022-01-15_dados_bbb.CSV" )

lista_bb <- googlesheets4::read_sheet(ss = as_id("1fw24KE2Wb9-GiW1pQ4PSQE_dJepUJ-ke-Gl7vkUeuD0")) %>%
  filter(!is.na(twitter))


token <- create_token(
  app = "joao",
  consumer_key = token$api_key,
  consumer_secret = token$api_secret_key,
  access_token = token$access_token,
  access_secret = token$access_secret)

rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  pattern <- unique(pattern)
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  accentTypes <- c("´","`","^","~","¨","ç")
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  return(str)
}




bbb <- c()

for (linha in 1:(nrow(lista_bb))) {
   rodada <- lista_bb[linha,]

  bbb[[linha]]<- search_tweets(rodada$twitter,n=1000,include_rts = FALSE,retryonratelimit=F) %>% select(user_id, text) %>% mutate(bbb = paste0(rodada$Nome))


}

todos_os_bbb = do.call(rbind, bbb)


sentimentos <- oplexicon_v3.0 %>%
  mutate(term = rm_accent(term)) %>%
  select(- type, - polarity_revision)


dados_atuais_bbb <- todos_os_bbb %>%
  mutate(text = iconv( text ,from = "UTF-8", to="LATIN1"),  ## removendo acentos e caracteries especiais
         text = rm_accent(text)) %>%
  unnest_tokens(term, text, to_lower = TRUE, token = "ngrams", n = 1) %>%
  inner_join(sentimentos, by = "term") %>%
  group_by(bbb) %>%
  summarise(total = sum(polarity)) %>%
  ungroup()

polaridade_bbb <- dados_atuais_bbb %>%
  mutate(bbb = fct_reorder(bbb, total )) %>%
  ggplot(aes(bbb, total, color = bbb)) +
  geom_segment(aes(x = bbb, xend = bbb, y = 0, yend = total),
               color = "gray", lwd = 1.5) +
  geom_point(size = 4, show.legend = FALSE) +
  coord_flip()+
  theme_fivethirtyeight()+
  labs(title = "Sentimento em relação a cada Brother no Twitter",
       subtitle = "Análise se as palavras utilzadas nos tweets citando os bothers é negativa ou positiva",
       x = "Bother",
       y = "Pontuação",
       caption = paste0("João Resede, ", Sys.Date()))



ggsave(paste0(Sys.Date(), sep = "_", "polaridade_bbb.jpeg"),polaridade_bbb, device = "jpeg",path =file.path(getwd(), "BBB"))

write.csv(x = todos_os_bbb, file = paste0(Sys.Date(), sep = "_", "dados_bbb.csv"))


calculado_ultimos_dados <- ultimos_dados %>%
  mutate(text = iconv( text ,from = "UTF-8", to="LATIN1"),  ## removendo acentos e caracteries especiais
         text = rm_accent(text)) %>%
  unnest_tokens(term, text, to_lower = TRUE, token = "ngrams", n = 1) %>%
  inner_join(sentimentos, by = "term") %>%
  group_by(bbb) %>%
  summarise(total = sum(polarity)) %>%
  ungroup() %>%
  rename("ultimo_total" = total)

combinado_ultimos_dois <- calculado_ultimos_dados %>%
  left_join(dados_atuais_bbb) %>%
  pivot_longer(names_to = "totais", values_to = "indice", cols = -1) %>%
  left_join(lista_bb, by = c("bbb" = "Nome"))


comparacao_duas_datas <- combinado_ultimos_dois %>%
  mutate(bbb = fct_reorder(bbb, indice )) %>%
  ggplot(aes(bbb,indice , color = totais)) +
  geom_line(col = "gray", size = 0.7, arrow = arrow())+
  geom_point(size = 4) +
  coord_flip()+
  theme_fivethirtyeight()+
  scale_color_discrete(labels = c("Pontuação Atual", "Pontuação da última avaliação"), name = "")+
  labs(title = "Sentimento em relação a cada Brother no Twitter",
       subtitle = "Diferença entre a última análise (15/01) e a atual(17/01)",
       x = "Bother",
       y = "Pontuação",
       color = "",
       caption = paste0("João Resede, ", Sys.Date()))

ggsave(paste0(Sys.Date(), sep = "_", "comparacao_duas_datas.jpeg"),comparacao_duas_datas, device = "jpeg",path =file.path(getwd(), "BBB"))


