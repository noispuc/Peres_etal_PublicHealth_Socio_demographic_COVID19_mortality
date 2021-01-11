
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(gtsummary)


# Data input --------------------------------------------------------------
srag_outcome <-  vroom::vroom("Input/srag_filtrado_08_08_modelagem.csv")



colunas1 = c("CS_SEXO", "FAIXA_IDADE", "CS_RACA", "CS_ESCOL_N", "REGIAO", "SUPORT_VEN", "n_comorb_mreal")
colunas2 = c("UTI")

srag_descritiva <- 
    srag_outcome %>%
    mutate(LOS = as.numeric(date_desf - date_int)) %>% 
    select(HOSPITAL, CS_SEXO, NU_IDADE_N, FAIXA_IDADE, CS_RACA, CS_ESCOL_N,
           REGIAO, n_comorb_mreal, UTI, SUPORT_VEN, LOS, EVOLUCAO) %>% 
    mutate(EVOLUCAO = ifelse(EVOLUCAO == "Death", 1, 0)) %>% 
    mutate_at(vars(colunas1), function(x){ifelse(is.na(x), "Not reported", x)}) %>% 
    mutate(CS_SEXO = factor(CS_SEXO, levels = c("Male", "Female", "Not reported"))) %>% 
    mutate(CS_RACA = factor(CS_RACA, levels = c("White", "Black/Brown", "Asian", "Indigenous", "Not reported"))) %>% 
    mutate(CS_ESCOL_N = factor(CS_ESCOL_N, levels = c("College/University", "High school", "Up to high school", "Illiterate", "Not reported"))) %>% 
    mutate(REGIAO = factor(REGIAO, levels = c("South", "Southeast", "Central-West", "Northeast", "North", "Not reported"))) %>% 
    mutate(SUPORT_VEN = factor(SUPORT_VEN, levels = c("No", "Yes, non-invasive","Yes, invasive", "Not reported"))) %>%
    mutate(n_comorb_mreal = ifelse(is.na(n_comorb_mreal), "Not reported", n_comorb_mreal)) %>% 
    mutate(n_comorb_mreal = factor(n_comorb_mreal, levels = c("0", "1", "2", "Not reported"), labels = c("No comorbidites", "1-2",">=3", "Not reported"))) %>%
    mutate_at(vars(colunas2), function(x){ifelse(is.na(x), "Not reported", x)}) %>% 
    mutate_at(vars(colunas2), function(x){factor(x, levels = c("No", "Yes", "Not reported"))}) %>% 
    mutate(
        IMV_ICU = case_when(
            UTI == "Yes" & SUPORT_VEN == "Yes, invasive" ~ "In ICU",
            UTI == "No" & SUPORT_VEN == "Yes, invasive" ~ "Outside ICU",
            UTI == "Not reported" & SUPORT_VEN == "Yes, invasive" ~ "Not reported"
        )) %>% 
    mutate(IMV_ICU = factor(IMV_ICU, levels = c("In ICU", "Outside ICU", "Not reported")))




# Data Analysis -----------------------------------------------------------

## Descriptive - self-reported race
lista_labels_raca <-  list(
    HOSPITAL ~ "Total, No.",
    CS_SEXO ~ "Gender, No. (%)",
    NU_IDADE_N ~ "Age, median (IQR)",
    FAIXA_IDADE ~ "Age group, No. (%)",
    EVOLUCAO ~ "In-hospital deaths, No. (%)",
    CS_ESCOL_N ~ "Level of education, No. (%)",
    REGIAO ~ "Region, No. (%)",
    UTI ~ "ICU hospitalization, No. (%)",
    SUPORT_VEN ~ "Ventilatory support, No. (%)",
    n_comorb_mreal ~ "Number of Comorbidities, No. (%)",
    IMV_ICU ~ "Place of Invasive Ventilatory support, No. (%)",
    LOS ~ "Hospital Length of Stay (days), median (IQR)"
    )


tb_desc_raca <- 
    srag_descritiva %>% 
    tbl_summary(
        by = CS_RACA, 
        missing = "no", 
        label = lista_labels_raca,
        statistic = list(HOSPITAL ~ "{n}")
    ) %>% 
    add_overall() %>% 
    add_n()
tb_desc_raca

writexl::write_xlsx(tb_desc_raca$table_body %>% 
                        select(-c(variable, row_type)) %>% 
                        rename("Characteristics" = "label",
                               "Total" = "stat_0",
                               "White" = "stat_1",
                               "Black/Brown" = "stat_2",
                               "Asian" = "stat_3",
                               "Indigenous" = "stat_4",
                               "Not reported" = "stat_5")
                    , "Output/Tables/table1_descriptive.xlsx")





# Supplementary analyis - Descriptive table Not reported  -----------------


## Descriptive - CS_RACA
lista_labels_raca <-  list(
    HOSPITAL ~ "Total, No.",
    CS_SEXO ~ "Gender, No. (%)",
    NU_IDADE_N ~ "Age, median (IQR)",
    FAIXA_IDADE ~ "Age group, No. (%)",
    EVOLUCAO ~ "In-hospital deaths, No. (%)",
    CS_ESCOL_N ~ "Level of education, No. (%)",
    REGIAO ~ "Region, No. (%)",
    UTI ~ "ICU hospitalization, No. (%)",
    SUPORT_VEN ~ "Ventilatory support, No. (%)",
    n_comorb_mreal ~ "Number of Comorbidities, No. (%)",
    IMV_ICU ~ "Place of Invasive Ventilatory support, No. (%)",
    LOS ~ "Hospital Length of Stay (days), median (IQR)"
)


tb_desc_raca <- 
    srag_descritiva %>% 
    mutate(
        is_reported = if_else(CS_RACA != "Not reported", "Reported", "Missing")
    ) %>% 
    tbl_summary(
        by = is_reported, 
        missing = "no", 
        label = lista_labels_raca,
        statistic = list(HOSPITAL ~ "{n}")
    ) %>% 
    add_overall() %>% 
    add_n()
tb_desc_raca

writexl::write_xlsx(tb_desc_raca$table_body %>% 
                        select(-c(variable, row_type)) %>% 
                        rename("Characteristics" = "label",
                               "Total" = "stat_0",
                               "Not reported" = "stat_1",
                               "Reported" = "stat_2")
                    , "Output/Supplementary/table_descriptive_not_reported.xlsx")
