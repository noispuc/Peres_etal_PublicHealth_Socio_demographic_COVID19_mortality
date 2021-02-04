
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(lme4)
library(broom.mixed)
library(gtsummary)


# Data input --------------------------------------------------------------
srag_outcome <-  vroom::vroom("Input/srag_filtrado_08_08_modelagem.csv.gz")


srag_descritiva <- 
    srag_outcome %>%
    select(CS_SEXO, FAIXA_IDADE, CS_RACA, CS_ESCOL_N, REGIAO, 
           EVOLUCAO, CARDIOPATI, HEMATOLOGI, HEPATICA,  DIABETES, 
           NEUROLOGIC, PNEUMOPATI, IMUNODEPRE, RENAL,  OBESIDADE) %>% 
    mutate_at(
        c("CARDIOPATI", "HEMATOLOGI", "HEPATICA",  "DIABETES", 
          "NEUROLOGIC", "PNEUMOPATI", "IMUNODEPRE", "RENAL",  "OBESIDADE", 
          "EVOLUCAO"),
        as.factor
    ) %>% 
    mutate(CS_SEXO = factor(CS_SEXO, levels = c("Male", "Female"))) %>% 
    mutate(CS_RACA = factor(CS_RACA, levels = c("White", "Black/Brown", "Asian", "Indigenous"))) %>% 
    mutate(CS_ESCOL_N = factor(CS_ESCOL_N, levels = c("College/University", "High school", "Up to high school", "Illiterate"))) %>% 
    mutate(REGIAO = factor(REGIAO, levels = c("South", "Southeast", "Central-West", "Northeast", "North"))) %>% 
    mutate(EVOLUCAO = ifelse(EVOLUCAO == "Death", 1, 0)) %>% 
    mutate(FAIXA_IDADE = as.factor(FAIXA_IDADE))


srag_descritiva_missing <- 
    srag_outcome %>%
    filter(!is.na(REGIAO)) %>% 
    select(CS_SEXO, FAIXA_IDADE, CS_RACA, CS_ESCOL_N, REGIAO, 
           EVOLUCAO, CARDIOPATI, HEMATOLOGI, HEPATICA,  DIABETES, 
           NEUROLOGIC, PNEUMOPATI, IMUNODEPRE, RENAL,  OBESIDADE) %>% 
    mutate_at(
        c("CARDIOPATI", "HEMATOLOGI", "HEPATICA",  "DIABETES", 
          "NEUROLOGIC", "PNEUMOPATI", "IMUNODEPRE", "RENAL",  "OBESIDADE", 
          "EVOLUCAO"),
        as.factor
    ) %>% 
    # mutate_all(function(x){ifelse(is.na(x), "Not reported", x)}) %>% 
    mutate(CS_SEXO = factor(CS_SEXO, levels = c("Male", "Female"))) %>% 
    mutate(CS_RACA = factor(CS_RACA, levels = c("White", "Black/Brown", "Asian", "Indigenous"))) %>% 
    mutate(CS_ESCOL_N = factor(CS_ESCOL_N, levels = c("College/University", "High school", "Up to high school", "Illiterate"))) %>% 
    mutate(REGIAO = factor(REGIAO, levels = c("South", "Southeast", "Central-West", "Northeast", "North"))) %>% 
    mutate(EVOLUCAO = ifelse(EVOLUCAO == "Death", 1, 0)) %>% 
    mutate(FAIXA_IDADE = as.factor(FAIXA_IDADE)) %>% 
    mutate_at(
        c("CS_SEXO", "CS_RACA", "CS_ESCOL_N", "CARDIOPATI", "HEMATOLOGI", 
          "HEPATICA",  "DIABETES",  "NEUROLOGIC", "PNEUMOPATI", 
          "IMUNODEPRE", "RENAL",  "OBESIDADE"),
        ~fct_explicit_na(., na_level = "Not Reported")
        ) %>% 
    mutate(
        EVOLUCAO = factor(EVOLUCAO)
    )


# Proportion of ICU and invasive ventilation ------------------------------
## IHM by Race group


model_complete <- 
    glm(EVOLUCAO ~ ., family = "binomial", 
        data = srag_descritiva
        ) %>% 
    broom::tidy(., exponentiate = TRUE, conf.int = TRUE)

writexl::write_xlsx(model_complete, "Output/Supplementary/model_complete.xlsx")



# model_complete <- 
#     rms::lrm(EVOLUCAO ~ ., 
#         data = srag_descritiva
#     ) 


model_not_rep <- 
    glm(EVOLUCAO ~ ., family = "binomial", 
        data = srag_descritiva_missing 
        ) %>% 
    broom::tidy(., exponentiate = TRUE, conf.int = TRUE)

writexl::write_xlsx(model_not_rep, "Output/Supplementary/model_not_rep.xlsx")





# Plot - Model OR estimantes (Figure 3) -----------------------------------
df_plot_estimates <-
    srag_descritiva %>% 
    select(
        CS_SEXO,
        FAIXA_IDADE,
        CS_RACA,
        CS_ESCOL_N,
        REGIAO
    ) %>% 
    distinct() %>% 
    pivot_longer(CS_SEXO:REGIAO, names_to = "variable", values_to = "type") %>% 
    distinct() %>% 
    arrange(variable, type) %>% 
    mutate(
        type = ifelse(is.na(type), "Not Reported", as.character(type))
    ) %>% 
    filter(type != "Not Reported") %>% 
    mutate(var_comp = paste0(variable, type)) %>% 
    left_join(
        model_complete %>% 
            select(
                term, comp_estimate = estimate, 
                comp_conf.low = conf.low, comp_conf.high = conf.high
            )
        , by = c("var_comp" = "term")
    ) %>% 
    mutate(
        comp_estimate = ifelse(is.na(comp_estimate), 1, comp_estimate)
    ) %>% 
    mutate(
        index = 1:n(),
        index_group = case_when(
            variable == "CS_SEXO" ~ 100,
            variable == "FAIXA_IDADE" ~ 200,
            variable == "CS_ESCOL_N" ~ 300,
            variable == "REGIAO" ~ 400,
            variable == "CS_RACA" ~ 500
        ),
        order = index + index_group
    )

plot_model_estimates <- 
    df_plot_estimates %>% 
    mutate(
        variable = factor(variable, 
                          levels = c("CS_SEXO", "FAIXA_IDADE", "CS_ESCOL_N", "CS_RACA", "REGIAO"),
                          labels = c("Sex", "Age", "Level of Education", "Self-reported race", "Region"))
    ) %>% 
    arrange(variable) %>% 
    ggplot() +
    geom_point(aes(y = fct_reorder(type, -order), x = comp_estimate, color = variable)) +
    geom_errorbarh(aes(y = fct_reorder(type, -order), xmin = comp_conf.low, xmax = comp_conf.high, color = variable)) +
    geom_vline(aes(xintercept = 1), linetype = "dashed", size = 0.2) +
    scale_x_continuous(trans = "log10", breaks = c(0.1, 0.25, 0.5, 1, 2, 5, 10)) +
    labs(x = "Odds Ratio (95% Confidence Interval)", y = "") + 
    scale_color_discrete(name = "") +
    theme_bw() +
    theme(legend.position = "right")


ggsave("Output/Figures/figure3_model_estimates.pdf", plot_model_estimates, 
       units = "in", width = 7, height = 5, dpi = 900)
