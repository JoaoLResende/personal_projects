require(tidyverse)
require(googledrive)
require(googlesheets4)
require(basedosdados)
require(scales)

#fazendo download dos dados pelo sheets.
#Esses dados foram baixados pela API do basedosdados no R.

dados_futebol <-  read_sheet(as_id("1pkjIzk_BuhfugTFvevtM58-ywKTEhZGFn-wqd-oeCgQ"))

#olhar quantas rodadas tem em cada ano
dados_futebol %>%
  distinct(rodada, ano_campeonato) %>%
  group_by(ano_campeonato) %>%
  summarise("ultima_rodada" = max(rodada))


#notou-se quem em todos os anos tem os dados de 38 rodadas, porém até 2006 o campeonato brasileiro tinha mais de 38 rodadas.
#limpar a base de dados e adicionar quantos pontos cada time ganhou e remover campeonatos anteriores a 2006, pois não se tem a base completa
#além disso, uma variavel binaria em caso de vitoria do manda,te visitante, empata.

dados_com_pontos_tidy <-  dados_futebol %>%
  filter(ano_campeonato>=2006) %>%
  select(ano_campeonato,rodada,estadio,time_man,time_vis,gols_man,gols_vis) %>%
  mutate(pts_vis = case_when(gols_man >gols_vis ~ 0,
                             gols_man <gols_vis ~ 3,
                             gols_man  == gols_vis ~ 1),
         pts_man = case_when(gols_man >gols_vis ~ 3,
                             gols_man <gols_vis ~ 0,
                             gols_man  == gols_vis ~ 1)) %>%
   pivot_longer(names_to = "mando", values_to = "time", cols = 4:5) %>%
  mutate(pontos_rodada = case_when(mando == "time_man" ~ pts_man ,
                                   mando == "time_vis" ~ pts_vis),
         gols = case_when(mando == "time_man" ~ gols_man,
                          TRUE ~ gols_vis )) %>%
  select(-c(pts_vis, pts_man,gols_vis, gols_man)) %>%
  mutate(turno = case_when(rodada <= 19 ~ "Primeiro",
                           TRUE ~ "Segundo"))%>%
  mutate("resultado" = case_when(pontos_rodada == 3 & mando == "time_man" ~ "vitoria_mandante",
                                 pontos_rodada == 0 & mando == "time_man" ~ "derrota_mandante",
                                 pontos_rodada == 3 & mando == "time_vis" ~ "vitoria_visitante",
                                 pontos_rodada == 0 & mando == "time_vis" ~ "derrota_visitante",
                                 pontos_rodada == 1 ~ "empate"))

#Descobrir influencia mando de campo

#Gráfico que mostra a concentração de vitórias entre mandante e vistivntes por ano

dados_com_pontos_tidy  %>%
  filter(resultado %in% c("vitoria_mandante", "vitoria_visitante")) %>%
  group_by(ano_campeonato, resultado) %>%
  count(resultado) %>%
  ungroup() %>%
  group_by(ano_campeonato ) %>%
  mutate(pct = n/sum(n),
         n= case_when(resultado == "vitoria_visitante" ~ -n,
                      TRUE ~ n)) %>%
  ungroup() %>%
  ggplot(aes(ano_campeonato, n, fill = resultado )) +
  geom_col() +
  coord_flip()+
  geom_hline(yintercept = 0)+
  geom_text(aes(label = percent(round(pct,2))),hjust=0)+
  # geom_text(aes(x = ano_campeonato, y = resultado,label = n)) +
  scale_y_continuous(breaks=seq(-300, 300, 50), labels=abs(seq(-300, 300, 50)))+
  labs( title = "Vitórias em Campeonatos Brasileiros",
      subtitle = "Divídidas entre mandantes e visitantes",
      x = "Ano",
      y = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_fill_manual(values = c("#047BAE", "#B1DCE3"), labels= c("Mandante", "Visitante"))

#Total de pontos por ano, divididos entre visitantes e mandantes.
#Com a falta dpúblico em 2020, podia se imaginar que o mando de campo não era tão importante.
dados_com_pontos_tidy %>%
  group_by(ano_campeonato, mando) %>%
  summarise(pontos_totais= sum(pontos_rodada, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(ano_campeonato ) %>%
  mutate(pct = pontos_totais/sum(pontos_totais),
         pontos_totais= case_when(mando== "time_vis" ~ -pontos_totais,
                      TRUE ~ pontos_totais)) %>%
  ungroup()%>%
  ggplot(aes(ano_campeonato, pontos_totais, fill = mando )) +
  geom_col() +
  coord_flip()+
  geom_hline(yintercept = 0)+
  geom_text(aes(label = percent(round(pct,2))),hjust=0)+
  # geom_text(aes(x = ano_campeonato, y = resultado,label = n)) +
  scale_y_continuous(breaks=seq(-800, 800, 50), labels=abs(seq(-800, 800, 50)))+
  labs( title = "Pontos em Campeonatos Brasileiros",
        subtitle = "Divídidas entre mandantes e visitantes",
        x = "Ano",
        y = "Número total de Pontos",
        fill = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
   scale_fill_manual(values = c("#047BAE", "#B1DCE3"), labels= c("Mandante", "Visitante"))

#Aparentemnte a ausencia de público não mudou a quantidade de pontos feitos por visitantes.


#Tabela com o consolidado de pontos totais de cada clube em cada ano.
total_pontos_time_ano <-dados_com_pontos_tidy %>%
  group_by(ano_campeonato, time) %>%
  summarise(pontos_por_campeonato = sum(pontos_rodada)) %>%
  ungroup() %>%
  mutate(ano_campeonato = as.double(ano_campeonato))






