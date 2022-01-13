require(tidyverse)
require(basedosdados)
require(lubridate)
library(ggthemes)


#Para esse código ser reprodutível, colocar a ID do projeto do Big Querry na variável abaixo
set_billing_id("sua_chave_aqui")

#Query para a coleta dos daos, foi feito um Join entre duas tabelas diferentes, para termos as informações completas,
#Além disso foi feito o valor acumulaod mensal por estação, pois os dados eram diários.
#Uma alteração foi a extarção dos dados de mes e ano de cada um dos dias de medição

query <- ("WITH local AS (SELECT id_estacao, id_municipio, estacao FROM `basedosdados.br_inmet_bdmep.estacao`
where id_municipio = '3145901'
 OR id_municipio = '3106200'
 OR id_municipio =  '3162500'
),precipatacao AS (
SELECT
id_estacao ,
extract(YEAR FROM data) as ano,
extract(MONTH FROM data) as mes,
sum(precipitacao_total) as chuva
FROM `basedosdados.br_inmet_bdmep.microdados`
GROUP BY ano, id_estacao, mes
)
SELECT *
FROM local AS t
LEFT OUTER JOIN precipatacao AS c
ON c.id_estacao = t.id_estacao")

#Criação de uma variável mes, uma vez que na query foi necessário dismembrar as datas.
chuva_regiao <- read_sql(query) %>%
  mutate_if(bit64::is.integer64, as.integer) %>%
  mutate(data = ymd(paste0(ano, sep = "/" ,mes, sep = "/","01"))) %>%
  select(-c(ano, mes))


#Gráfico de time series, removendo a medição de cercadinho que tem dados incompletos

historico_chuva_ano <- chuva_regiao %>%
  filter( estacao != "Belo Horizonte - Cercadinho") %>%
  ggplot(aes(data, chuva, color = estacao)) +
  geom_line(size = 1)+
  facet_wrap(~ estacao)+
  labs(title = "Total de precipitação mensal",
       subtitle = "Quantidade de precipitação mensal em 5 locais diferentes",
       x = "Data",
       y = "Precipitação (mm)",
       color = "Região")+
  scale_x_date(breaks=date_breaks("12 months"),
               date_labels = "%b %Y")+
  theme_fivethirtyeight()+
  theme(axis.text.x = element_text(angle = 45, hjust=1))


#Recorte dos últimos 5 anos, com os dados apenas do Mês de janeiro, que historicamente é o mais chuvoso, conforme o gráfico acima
cinco_anos_chuva <-chuva_regiao %>%
  filter(str_detect(data, "-01-01"),
         data > "2016-01-01") %>%
  ggplot(aes(data, chuva, fill = estacao)) +
  geom_col(position = "dodge")+
  geom_hline(aes(yintercept = 344.1), linetype = 'longdash', col = 'red') +
  scale_x_date(breaks=date_breaks("1 year"),
               date_labels = "%b %Y")+
  labs(title = "Total de precipitação mês de janeiro nos últimos 5 anos",
       subtitle = "Linha vermelha é a quantidade de chuva entre o dia 01/01/2022 e 09/01/2022",
       x = "Data",
       y = "Precipitação (mm)",
       fill = "Região")


  ggsave("historico_chuva_ano.jpeg",historico_chuva_ano, device = "jpeg",path = getwd())
  ggsave("cinco_anos_chuva.jpeg",cinco_anos_chuva, device = "jpeg",path = getwd())


