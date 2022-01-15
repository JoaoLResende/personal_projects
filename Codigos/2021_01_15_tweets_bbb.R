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


token <- create_token(
  app = "joao",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_secret)

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



lista_bb <- googlesheets4::read_sheet(ss = as_id("1fw24KE2Wb9-GiW1pQ4PSQE_dJepUJ-ke-Gl7vkUeuD0")) %>%
  filter(!is.na(twitter))

bbb <- c()

for (linha in 1:(nrow(lista_bb))) {
   rodada <- lista_bb[linha,]

  bbb[[linha]]<- search_tweets(rodada$twitter,n=1000,include_rts = FALSE,retryonratelimit=F) %>% select(user_id, text) %>% mutate(bbb = paste0(rodada$Nome))


}

todos_os_bbb = do.call(rbind, bbb)


sentimentos <- oplexicon_v3.0 %>%
  mutate(term = rm_accent(term)) %>%
  select(- type, - polarity_revision)

polaridade_bbb <- todos_os_bbb %>%
  mutate(text = iconv( text ,from = "UTF-8", to="LATIN1"),  ## removendo acentos e caracteries especiais
         text = rm_accent(text)) %>%
  unnest_tokens(term, text, to_lower = TRUE, token = "ngrams", n = 1) %>%
  inner_join(sentimentos, by = "term") %>%
  group_by(bbb) %>%
  summarise(total = sum(polarity)) %>%
  ungroup() %>%
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
       caption = "João Resede, 15/01/2020")



ggsave(paste0(Sys.Date(), sep = "_", "polaridade_bbb.jpeg"),polaridade_bbb, device = "jpeg",path =file.path(getwd(), "BBB"))

