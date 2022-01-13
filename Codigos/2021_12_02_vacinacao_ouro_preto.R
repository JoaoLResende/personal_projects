require(tidyverse)
require(googledrive)
require(googlesheets4)
require(basedosdados)
require(scales)
require(ggfx)
require(gridExtra)
require(grid)

#Função para encontrar o que não esta presente
'%ni%'  <- Negate('%in%')


#query pada download dos dados (vacinação), em caso de reprodução,rodar set_billing_id(), com o código do projeto bigquerry
query_vacinacao <- ("SELECT  idade_paciente, sexo_paciente, dose_vacina, COUNT(dose_vacina) AS TOTAL
FROM `basedosdados.br_ms_vacinacao_covid19.microdados`
WHERE id_municipio_estabelecimento = '3146107'
GROUP BY sexo_paciente, idade_paciente, dose_vacina	")


#query pada download dos dados (população), em caso de reprodução,rodar set_billing_id(), com o código do projeto bigquerry

query_populacao <- "SELECT *
FROM basedosdados-staging.br_ms_populacao_staging.municipio
WHERE id_municipio = '3146107'
and ano = 2020"

#Download dos dados de vacinação e população
vacinacao_op <- read_sql(query_vacinacao)
populacao_op <- read_sql(query_populacao) %>% select(-c(id_municipio, ano))


#Tratando os dados da vacinação e juntando com os dados da população da cidade
vacinacao_op_tratado <- vacinacao_op %>%
  mutate(dose_vacina = case_when(dose_vacina %in% c("Dose Adicional", "Reforço") ~ "Dose Adicional",
                                 TRUE ~ dose_vacina)) %>%
  mutate(idade_paciente = case_when(idade_paciente >80 ~ "80_mais",
                                  idade_paciente >75 ~ "75-79 anos",
                                  idade_paciente >70 ~ "70-74 anos",
                                  idade_paciente >65 ~ "65-69 anos",
                                  idade_paciente >60 ~ "60-64 anos",
                                  idade_paciente >55 ~ "55-59 anos",
                                  idade_paciente >50 ~ "50-54 anos",
                                  idade_paciente >45 ~ "45-49 anos",
                                  idade_paciente >40 ~ "40-44 anos",
                                  idade_paciente >35 ~ "35-39 anos",
                                  idade_paciente >30 ~ "30-34 anos",
                                  idade_paciente >25 ~ "25-29 anos",
                                  idade_paciente >20 ~ "20-24 anos",
                                  idade_paciente >15 ~ "15-20 anos",
                                  idade_paciente >10 ~ "10-14 anos",
                                  idade_paciente > 5 ~ "5-9 anos",
                                  TRUE ~ "0-4 anos")) %>%
  group_by(sexo_paciente, dose_vacina, idade_paciente) %>%
  summarise(TOTAL = sum(TOTAL)) %>%
  ungroup() %>%
  mutate(sexo_paciente = case_when(sexo_paciente == "F" ~ "feminino",
                                  sexo_paciente == "M" ~ "masculino")) %>%
  rename("sexo" = sexo_paciente,
         "grupo_idade" = idade_paciente) %>%
  left_join(populacao_op) %>%
  mutate(TOTAL = as.double(TOTAL),
         populacao = as.double(populacao)) %>%
  pivot_wider(values_from = TOTAL, names_from = dose_vacina) %>%
  mutate(nao_imunizada_2_doses = populacao - `2ª Dose`  ) %>%
  pivot_longer(names_to = "identificador", values_to = "TOTAL", cols = 3:8) %>%
  filter(!is.na(TOTAL)) %>%
  mutate(TOTAL = case_when(TOTAL < 0 ~ 0,
                           TRUE ~ TOTAL))

#Gráfico da vacinação por sexo e faixa etária, junto com a população. O gráfico mostra a proporção por faixa etária da vacinação
plot_vacina_op_faixa_etaria <- vacinacao_op_tratado %>%
  filter(identificador %in% c( "nao_imunizada_2_doses", "2ª Dose")) %>%
  filter(grupo_idade %ni% c("0-4 anos", "5-9 anos")) %>%
  mutate(TOTAL = case_when(sexo == "feminino"  ~ -TOTAL,
                           TRUE  ~TOTAL)) %>%
  mutate(identificador = fct_reorder(identificador, abs(TOTAL))) %>%
  ggplot(aes(grupo_idade, TOTAL, fill = sexo, alpha =identificador ))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(values = c("#B1DCE3", "#047BAE"), labels= c("Feminimo", "Masculino"))+
  scale_alpha_manual(values=c(0.6,1),guide = 'none')+
  scale_y_continuous(breaks=seq(-3500, 3500, 500), labels=abs(seq(-3500,3500,by=500)))+
  labs( title = "Vacinação por faixa etária em Ouro Preto - Minas Gerais",
        subtitle = "Cor escura imunizados com 2 doses",
        x = "Faixa Etária",
        y = "",
        fill ="",
        alpha = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())



grid.arrange(plot_vacina_op_faixa_etaria + labs(caption="Fontes: basedosdados.org"),
             bottom = textGrob("Autor:João Lúcio Resende", x = 1,
                               hjust = 1, gp = gpar(fontface = 3L, fontsize = 9)))

