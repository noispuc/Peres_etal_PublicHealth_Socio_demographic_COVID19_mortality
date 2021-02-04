
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(gtsummary)


# Data input --------------------------------------------------------------
srag_outcome <-  vroom::vroom("Input/srag_filtrado_08_08_modelagem.csv.gz")


srag_descritiva <- 
    srag_outcome %>%
    select(HOSPITAL, CS_SEXO, NU_IDADE_N, FAIXA_IDADE, CS_RACA, CS_ESCOL_N, REGIAO,
           UTI, SUPORT_VEN, EVOLUCAO, CARDIOPATI, HEMATOLOGI, HEPATICA,  DIABETES, 
           NEUROLOGIC, PNEUMOPATI, IMUNODEPRE, RENAL,  OBESIDADE, n_comorb_mreal, date_not) %>% 
    # mutate_at(vars(colunas1), function(x){ifelse(is.na(x), "Not reported", x)}) %>% 
    mutate(CS_SEXO = factor(CS_SEXO, levels = c("Male", "Female"))) %>% 
    mutate(CS_RACA = factor(CS_RACA, levels = c("White", "Black/Brown", "Asian", "Indigenous"))) %>% 
    mutate(CS_ESCOL_N = factor(CS_ESCOL_N, levels = c("College/University", "High school", "Up to high school", "Illiterate"))) %>% 
    mutate(REGIAO = factor(REGIAO, levels = c("South", "Southeast", "Central-West", "Northeast", "North"))) %>% 
    mutate(EVOLUCAO = ifelse(EVOLUCAO == "Death", 1, 0)) %>% 
    mutate(FAIXA_IDADE = as.factor(FAIXA_IDADE)) %>% 
    mutate(n_comorb_mreal = factor(n_comorb_mreal, levels = c(0, 1, 2), labels = c("No comorbidity", "1-2", ">= 2"))) %>% 
    mutate(SUPORT_VEN = factor(SUPORT_VEN, levels = c("No", "Yes, non-invasive", "Yes, invasive")))





# In-hospital mortality combined plot (Figure 2) --------------------------

## IHM per self-reported race
df_ihm_race <- 
    left_join(
        srag_descritiva %>%
            count(CS_RACA) %>%
            rename(total = n),
        srag_descritiva %>%
            filter(EVOLUCAO == 1) %>%
            count(CS_RACA) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), "%)")
    ) %>% 
    filter(!is.na(CS_RACA)) 

writexl::write_xlsx(df_ihm_race, "Output/Supplementary/IHM_self_reported_race.xlsx")

## Plot - IHM per self-reported race
plot_ihm_race <-
    df_ihm_race %>%
    ggplot() +
    geom_col(aes(x = CS_RACA, y = in_hosp_mortal, fill = CS_RACA),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_discrete(name = "") +
    labs(x = "Self-reported race", y = "In-hospital mortality") +
    theme_bw() 




### IHM per self-reported race and age
df_ihm_race_age <- 
    left_join(
        srag_descritiva %>%
            count(CS_RACA, FAIXA_IDADE) %>%
            rename(total = n),
        srag_descritiva %>%
            filter(EVOLUCAO == 1) %>%
            count(CS_RACA, FAIXA_IDADE) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), "%)")
    ) %>% 
    filter(!is.na(CS_RACA)) 

writexl::write_xlsx(df_ihm_race_age, "Output/Supplementary/IHM_self_reported_race_age.xlsx")

## Plot - IHM per self-reported race and age
plot_ihm_race_age <-
    df_ihm_race_age %>%
    ggplot() +
    geom_col(aes(x = FAIXA_IDADE, y = in_hosp_mortal, fill = CS_RACA),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_discrete(name = "") +
    labs(x = "Age (years)", y = "In-hospital mortality") +
    theme_bw() 





### IHM per self-reported race and number of comorbidities
df_ihm_race_comorb <- 
    left_join(
        srag_descritiva %>%
            count(CS_RACA, n_comorb_mreal) %>%
            rename(total = n),
        srag_descritiva %>%
            filter(EVOLUCAO == 1) %>%
            count(CS_RACA, n_comorb_mreal) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), "%)")
    ) %>% 
    filter(!is.na(CS_RACA), !is.na(n_comorb_mreal))

writexl::write_xlsx(df_ihm_race_comorb, "Output/Supplementary/IHM_self_reported_race_comorb.xlsx")



## Plot - IHM self-reported race and number of comorbidities
plot_ihm_race_comorb <-
    df_ihm_race_comorb %>%
    ggplot() +
    geom_col(aes(x = n_comorb_mreal, y = in_hosp_mortal, fill = CS_RACA),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_discrete(name = "") +
    labs(x = "Number of comorbidities", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom") 





### IHM self-reported race and level of education
df_ihm_race_escol <- 
    left_join(
        srag_descritiva %>%
            count(CS_RACA, CS_ESCOL_N) %>%
            rename(total = n),
        srag_descritiva %>%
            filter(EVOLUCAO == 1) %>%
            count(CS_RACA, CS_ESCOL_N) %>%
            rename(deaths = n)
    ) %>%
    mutate(
        in_hosp_mortal = deaths / total,
        ihm_ratio = paste0(deaths, "/", total, " (", round(100 * in_hosp_mortal, 0), "%)")
    ) %>% 
    filter(!is.na(CS_RACA), !is.na(CS_ESCOL_N)) %>% 
    mutate(CS_ESCOL_N = factor(CS_ESCOL_N, 
                               levels = c("Illiterate", 
                                          "Up to high school",
                                          "High school",
                                          "College/University")
                               )
           )


writexl::write_xlsx(df_ihm_race_escol, "Output/Supplementary/IHM_self_reported_education.xlsx")


## Plot - IHM self-reported race and level of education
plot_ihm_race_escol <-
    df_ihm_race_escol %>%
    ggplot() +
    geom_col(aes(x = CS_ESCOL_N, y = in_hosp_mortal, fill = CS_RACA),
             position = "dodge") +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0, 1)) +
    scale_fill_discrete(name = "") +
    labs(x = "Level of education", y = "In-hospital mortality") +
    theme_bw() +
    theme(legend.position = "bottom") 




#### Combining plots
combined_plot_ihm_skin_color <- 
    ggpubr::ggarrange(plot_ihm_race,
                      plot_ihm_race_age,
                      plot_ihm_race_escol,
                      plot_ihm_race_comorb,
                      nrow = 2,
                      ncol = 2,
                      align = "hv",
                      common.legend = TRUE, 
                      legend = "bottom", 
                      font.label = list(size = 8))


ggsave("Output/Figures/figure2_IHM_selfreported_race.pdf",
       combined_plot_ihm_skin_color,
       units = "in", dpi = 900, height = 8, width = 10)






# Supplementary Analysis - IHM per self-reported race over time -----------

df_ihm_race_epi_week <- 
    srag_descritiva %>%
    filter(!is.na(CS_RACA)) %>% 
    bind_rows(
        srag_descritiva %>% 
            mutate(
                CS_RACA = "Total"
            )
    ) %>% 
    mutate(
        epi_week = lubridate::epiweek(date_not),
        epi_week = ifelse(epi_week <= 8,  8, epi_week),
        epi_week_group = case_when(
            epi_week <= 14 ~ 1,
            epi_week <= 17 ~ 2,
            epi_week <= 20 ~ 3,
            epi_week <= 23 ~ 4,
            epi_week <= 26 ~ 5,
            epi_week <= 29 ~ 6,
            epi_week <= 32 ~ 7,
        )
    ) %>% 
    group_by(CS_RACA, epi_week_group) %>% 
    summarise(
        total = n(),
        deaths = sum(EVOLUCAO)
    ) %>% 
    ungroup() %>% 
    mutate(
        in_hosp_mortal = deaths / total
    ) %>% 
    mutate(
        CS_RACA = factor(CS_RACA, levels = c("Total", "White", "Black/Brown",
                                             "Asian", "Indigenous"))
    )




## Plot - IHM per self-reported race over time
plot_ihm_race_epi_week <-
    df_ihm_race_epi_week %>%
    filter(CS_RACA != "Total") %>%
    ggplot() +
    geom_point(aes(x = epi_week_group,
                   y = in_hosp_mortal, color = CS_RACA)) +
    geom_line(aes(x = epi_week_group, 
                  y = in_hosp_mortal, color = CS_RACA, linetype = CS_RACA)) +
    scale_linetype_manual(values = c("dashed", "solid", "solid", "solid", "solid"),
                          labels = c("Total", "White", "Black/Brown", "Asian", "Indigenous"),
                          name = "", guide = F) +
    scale_color_manual(values = c("black", "#F8766D", "#7CAE00", "#00BFC4", "#C77CFF"), 
                       labels = c("Total", "White", "Black/Brown", "Asian", "Indigenous"),
                       name = "", guide = F) +
    scale_x_continuous(breaks = 1:7,
                       labels = c("8-14", "15-17", "18-20", 
                                  "21-23", "24-26", "27-29",
                                  "30-32")
    ) +
    scale_y_continuous(labels = scales::percent_format(),
                       limits = c(0.25, 0.75)
    ) +
    labs(x = "Epidemiological Week", y = "In-hospital mortality") +
    theme_bw() +
    facet_wrap(. ~ CS_RACA, nrow = 5, ncol = 1) +
    theme(legend.position = "bottom")


ggsave("Output/Supplementary/plot_ihm_per_epi_week.pdf", 
       plot_ihm_race_epi_week, units = "in", dpi = 900,
       width = 5, height = 10)



