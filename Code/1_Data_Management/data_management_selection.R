library(tidyverse)

arquivo_SRAG = "Input/INFLUD-23-11-2020.csv"

nome_arquivo_saida = "srag_filtrado_08_08_modelagem"

srag = vroom::vroom(arquivo_SRAG)

fim = as.Date("2020-08-08")

srag = srag %>% 
  filter(between(SEM_NOT, 8, 32))

texto = paste0("Filtros executados em: ", Sys.time(), "\n","Arquivo: ", arquivo_SRAG, "\n")
texto = paste0(texto, "Total: ", nrow(srag), "\n")


# Filtra pacientes internados
srag_internados = srag %>% 
  filter(HOSPITAL == 1)

texto = paste0(texto, "Hospitalizations: ", nrow(srag_internados), "\n")

texto = paste0(texto, "(-) Other respitatory viruses (influenza + others): ", nrow(srag_internados %>%
                                                                   filter(CLASSI_FIN == 1 | CLASSI_FIN == 2)), "\n")
texto = paste0(texto, "(-) Other causes (e.g, tuberculosis, bacterial pneumonia, etc): ", nrow(srag_internados %>%
                                                                   filter(CLASSI_FIN == 3)), "\n")
texto = paste0(texto, "(-) Without etiologic definition: ", nrow(srag_internados %>%
                                                                   filter(CLASSI_FIN == 4)), "\n")
texto = paste0(texto, "(-) Without classification: ", nrow(srag_internados %>%
                                                               filter(is.na(CLASSI_FIN))), "\n")

# Filtra pacientes com COVID
srag_covid = srag_internados %>%
  filter(CLASSI_FIN == 5)

texto = paste0(texto,"SARS-CoV-2 Classification: ", nrow(srag_covid), "\n")

#Filtra pacientes com PCR positivo para COVID
srag_pcr = srag_covid %>% 
  filter((PCR_SARS2 == 1 |
            (PCR_RESUL == 1 & str_detect(DS_PCR_OUT, "SARS|COVID|COV|CORONA")) |
            (PCR_RESUL == 1 & CRITERIO == 1)) &
           !(DS_PCR_OUT %in% c("CORONAVIRUS NL63",
                             "CORONAVIRUS SUBTIPO 229E",
                             "ENTEROVIRUS / PARECOVIRUS",
                             "CORONAVIRUS SUBTIPO 229E",
                             "CORONAVIRUS NL63",
                             "MYCOPLASMA PNEUMONIAE",
                             "OUTRO VIRUS RESPIRATORIO",
                             "OUTRO VIRUS REPIRATORIO",
                             "OUTRO VIRUS RESPIRAORIO",
                             "RINOVIRUS/ENTEROVIRUS",
                             "ENTEROVIRUS",
                             "H1N1 TESTE RAPIDO",
                             "VIRUS SINDICAL RESPIRATORIO",
                             "VIRUS SINCICIAL REPIRATORIO",
                             "VIRUS SINCICIAL RESPIRATORIO")))

texto = paste0(texto, "SARS-CoV-2 Positive: ", nrow(srag_pcr), "\n")

# Filtra pacientes adultos (>= 20 anos)
srag_adultos = srag_pcr %>%
  filter(NU_IDADE_N >= 20 & TP_IDADE == 3)

texto = paste0(texto, "(-) Age < 20 years: ", nrow(srag_pcr) - nrow(srag_adultos), "\n")
texto = paste0(texto, "Adults: ", nrow(srag_adultos), "\n")

srag_desfecho = srag_adultos %>% 
  filter(EVOLUCAO == 1| EVOLUCAO == 2| EVOLUCAO == 3)
texto = paste0(texto, "Final Sample: ", nrow(srag_desfecho), "\n")


srag_filtrado = srag_desfecho %>% 
  mutate(date_int = as.Date(DT_INTERNA, format = "%d/%m/%Y"),
         date_not = as.Date(DT_NOTIFIC, format = "%d/%m/%Y"),
         date_sint = as.Date(DT_SIN_PRI, format = "%d/%m/%Y"),
         date_desf = as.Date(DT_EVOLUCA, format = "%d/%m/%Y"),
         date_enc = as.Date(DT_ENCERRA, format = "%d/%m/%Y"),
         date_uti = as.Date(DT_ENTUTI, format = "%d/%m/%Y"),
         date_said_uti = as.Date(DT_SAIDUTI, format = "%d/%m/%Y"),
         date_pcr = as.Date(DT_PCR, format = "%d/%m/%Y")) %>% 
  mutate(date_int = as.Date(case_when(
    date_int > fim & lubridate::year(date_int) > lubridate::year(fim) ~ paste0(str_sub(DT_INTERNA,1, 6), "2020"),
    date_int > fim & lubridate::month(date_int) > lubridate::month(fim) ~ "00000",
    date_int > fim & lubridate::day(date_int) > lubridate::day(fim) ~ "00000",
    TRUE ~ as.character(DT_INTERNA)
  ), format = "%d/%m/%Y"))%>% 
  mutate(date_not = as.Date(case_when(
    date_not > fim & lubridate::year(date_not) > lubridate::year(fim) ~ paste0(str_sub(DT_NOTIFIC,1, 6), "2020"),
    date_not > fim & lubridate::month(date_not) > lubridate::month(fim) ~ "00000",
    date_not > fim & lubridate::day(date_not) > lubridate::day(fim) ~ "00000",
    TRUE ~ as.character(DT_NOTIFIC)
  ), format = "%d/%m/%Y"))%>% 
  mutate(date_sint = as.Date(case_when(
    date_sint > fim & lubridate::year(date_sint) > lubridate::year(fim) ~ paste0(str_sub(DT_SIN_PRI,1, 6), "2020"),
    date_sint > fim & lubridate::month(date_sint) > lubridate::month(fim) ~ "00000",
    date_sint > fim & lubridate::day(date_sint) > lubridate::day(fim) ~ "00000",
    TRUE ~ as.character(DT_SIN_PRI)
  ), format = "%d/%m/%Y"))%>% 
  mutate(date_enc = as.Date(case_when(
    date_enc > fim & lubridate::year(date_enc) > lubridate::year(fim) ~ paste0(str_sub(DT_ENCERRA,1, 6), "2020"),
    date_enc > fim & lubridate::month(date_enc) > lubridate::month(fim) ~ "00000",
    date_enc > fim & lubridate::day(date_enc) > lubridate::day(fim) ~ "00000",
    TRUE ~ as.character(DT_ENCERRA)
  ), format = "%d/%m/%Y"))%>% 
  mutate(date_uti = as.Date(case_when(
    date_uti > fim & lubridate::year(date_uti) > lubridate::year(fim) ~ paste0(str_sub(DT_ENTUTI,1, 6), "2020"),
    date_uti > fim & lubridate::month(date_uti) > lubridate::month(fim) ~ "00000",
    date_uti > fim & lubridate::day(date_uti) > lubridate::day(fim) ~ "00000",
    TRUE ~ as.character(DT_ENTUTI)
  ), format = "%d/%m/%Y"))%>% 
  mutate(date_said_uti = as.Date(case_when(
    date_said_uti > fim & lubridate::year(date_said_uti) > lubridate::year(fim) ~ paste0(str_sub(DT_SAIDUTI,1, 6), "2020"),
    date_said_uti > fim & lubridate::month(date_said_uti) > lubridate::month(fim) ~ "00000",
    date_said_uti > fim & lubridate::day(date_said_uti) > lubridate::day(fim) ~ "00000",
    TRUE ~ as.character(DT_SAIDUTI)
  ), format = "%d/%m/%Y"))%>% 
  mutate(date_pcr = as.Date(case_when(
    date_pcr > fim & lubridate::year(date_pcr) > lubridate::year(fim) ~ paste0(str_sub(DT_PCR,1, 6), "2020"),
    date_pcr > fim & lubridate::month(date_pcr) > lubridate::month(fim) ~ "00000",
    date_pcr > fim & lubridate::day(date_pcr) > lubridate::day(fim) ~ "00000",
    TRUE ~ as.character(DT_PCR)
  ), format = "%d/%m/%Y"))

# Colunas com valores "Sim", "Nao", "Ignorado" e NA
colunas = c("FEBRE", "TOSSE", "GARGANTA", "DISPNEIA", "DESC_RESP",
            "SATURACAO", "DIARREIA", "VOMITO", "OUTRO_SIN", "PUERPERA",
            "CARDIOPATI", "HEMATOLOGI", "SIND_DOWN", "HEPATICA", "ASMA",
            "DIABETES", "NEUROLOGIC", "PNEUMOPATI", "IMUNODEPRE",
            "RENAL", "OBESIDADE", "OUT_MORBI")

# Cria variaveis de faixas de idade, regioes e tempos.
# Traduz dados para o formato desejado na analise descritiva
srag_filtros = srag_filtrado %>%
  mutate(CS_SEXO = case_when(CS_SEXO == "M" ~ "Male",
                             CS_SEXO == "F" ~ "Female")) %>% 
  mutate(FAIXA_IDADE = case_when(NU_IDADE_N <= 39 ~ "20-39",
                                 NU_IDADE_N <= 49 ~ "40-49",
                                 NU_IDADE_N <= 59 ~ "50-59",
                                 NU_IDADE_N <= 69 ~ "60-69",
                                 NU_IDADE_N <= 79 ~ "70-79",
                                 TRUE ~ "80+")) %>%
  mutate(CS_RACA = case_when(CS_RACA == "1" ~ "White",
                             CS_RACA == "2" ~ "Black/Brown",
                             CS_RACA == "3" ~ "Asian",
                             CS_RACA == "4" ~ "Black/Brown",
                             CS_RACA == "5" ~ "Indigenous")) %>% 
  mutate(CS_ESCOL_N = case_when(CS_ESCOL_N == "0" ~ "Illiterate",
                                CS_ESCOL_N == "1" ~ "Up to high school",
                                CS_ESCOL_N == "2" ~ "Up to high school",
                                CS_ESCOL_N == "3" ~ "High school",
                                CS_ESCOL_N == "4" ~ "College/University")) %>% 
  mutate(REGIAO = case_when(SG_UF %in% c("SP", "RJ", "ES", "MG") ~ "Southeast",
                            SG_UF %in% c("SC", "RS", "PR") ~ "South",
                            SG_UF %in% c("MT", "MS", "GO", "DF") ~ "Central-West",
                            SG_UF %in% c("AM", "AP", "TO", "PA", "RO", "RR", "AC") ~ "North",
                            SG_UF %in% c("BA", "AL", "SE", "PE", "MA", "RN", "PB", "CE", "PI") ~ "Northeast")) %>% 
  mutate(EVOLUCAO = case_when(EVOLUCAO == 1 ~ "Discharge",
                              EVOLUCAO == 2 ~ "Death",
                              EVOLUCAO == 3 ~ "Death")) %>%
  mutate(SUPORT_VEN = case_when(SUPORT_VEN == 1 ~ "Yes, invasive",
                                SUPORT_VEN == 2 ~ "Yes, non-invasive",
                                SUPORT_VEN == 3 ~ "No")) %>% 
  mutate(UTI = case_when(UTI == 1 ~ "Yes",
                         UTI == 2 ~ "No")) %>% 
  mutate(PUERPERA = case_when(CS_SEXO == "Male" ~ NA_real_, TRUE ~ PUERPERA), # few missing considered as entered for puerpera
         CS_SEXO  = case_when(PUERPERA == 1 ~ "Female", TRUE ~ CS_SEXO)) %>%  # 1 case puerpera with missing sex
  mutate_at(all_of(colunas), function(x){case_when(x == 1 ~ 1,
                                                   x == 2 ~ 0)}) %>%
  mutate(
    CONT_COMORB = CARDIOPATI + HEMATOLOGI +  HEPATICA + DIABETES +
      NEUROLOGIC + PNEUMOPATI + IMUNODEPRE + RENAL +
      OBESIDADE,
    n_comorb = case_when(
      CONT_COMORB == 0 ~ 0,
      CONT_COMORB == 1 ~ 1,
      CONT_COMORB == 2 ~ 1,
      CONT_COMORB >  2 ~ 2)
  ) %>% 
  mutate(CONT_COMORB_mreal = case_when(!is.na(CARDIOPATI) & !is.na(HEMATOLOGI) & !is.na(HEPATICA) &
                                         !is.na(DIABETES) & !is.na(NEUROLOGIC) & !is.na(PNEUMOPATI) &
                                         !is.na(IMUNODEPRE) & !is.na(RENAL) & !is.na(OBESIDADE) ~ CONT_COMORB,
                                       TRUE ~ NA_real_
  ),
  n_comorb_mreal = case_when(
    CONT_COMORB_mreal == 0 ~ 0,
    CONT_COMORB_mreal == 1 ~ 1,
    CONT_COMORB_mreal == 2 ~ 1,
    CONT_COMORB_mreal >  2 ~ 2)) %>% 
    mutate_at(vars(colunas), function(x) {case_when(x == 1 ~ "Yes",
                                                    x == 0 ~ "No")})
    

srag_filtros <- cbind(ID = rownames(srag_filtros), srag_filtros)

# Exporta arquivos
texto = paste0(texto, "Salvo em: ", nome_arquivo_saida, "\n")

write_csv(srag_filtros, paste0("Input/",nome_arquivo_saida,".csv"))
write.table(texto, paste0("Output/filtros_aplicados_", nome_arquivo_saida ,".txt"), row.names = F)


#   finished