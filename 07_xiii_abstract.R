# Author: Víctor Suárez-Lledó
# Date: Fri Mar  8 10:57:48 2019
# Summary: final script for the XIII FES
# --------------


# Data Loading ------------------------------------------------------------
source("https://raw.githubusercontent.com/dgrtwo/drlib/master/R/reorder_within.R")

library(tidyverse)
theme_set(theme_light())

d_raw <- haven::read_dta("./data/health_Inequalities_ess7_basicdata.dta") %>% 
  select(-pais, -age2, -age6, -obesity, -wellbeing, -stflife_2, -happiness,
         -eduyrs2, -vstdoc, -srh, -depressed) %>% 
  mutate_if(sjlabelled::is_labelled, sjlabelled::as_label) %>% 
  sjlabelled::var_labels(
    alcfrq = "Frequency alcohol consumption",
    alternattr = "Use of alternative threatmen",
    bmi = "Body mass index",
    cigday = "Cigarettes smoke on typical day",
    depression = "Level of depression",
    etnmin = "Member of an ethnic minory",
    freshdiet = "Consumption of fresh food",
    income = "Total income",
    occhaz = "Occupation hazard",
    physact = "Physical activity",
    soccap = "Social capital",
    uemp3m2 = "Unmets needs when was a child")
  

sjPlot::view_df(d_raw) 

summarytools::view(summarytools::dfSummary(d_raw))

d <- d_raw %>%
  mutate_at(vars(multidisc, childpr, isced, famfinpr), as.ordered) %>% 
  mutate(female = factor(female, labels = c("Male", "Female"))) %>% 
  na.omit() 


# Exploring groups --------------------------------------------------------
library(gt)

d %>% 
  count(isced, female, multimorbidity) %>% 
  mutate(percent = n/sum(n)) %>% 
  mutate(isced = str_to_title(isced)) %>% 
  mutate(isced_multi = paste0(isced, " (", multimorbidity, ")")) %>% 
  select(-isced, -multimorbidity) %>% 
  gt(groupname_col = "female",
     rowname_col = "isced_multi") %>% 
  tab_stubhead_label("Level of education (Multimorbidity)") %>% 
  summary_rows(groups = c("Male", "Female"),
               columns = vars(n, percent),
               fns = list(Total = "sum"),
               formatter = fmt_number,
               drop_trailing_zeros = T) %>%   
  fmt_percent(vars(percent)) %>% 
  fmt_number(vars(n), drop_trailing_zeros = T) %>% 
  cols_label(n = "Obs.",
             percent = "(%)") %>% 
  gtsave(here::here("tables", paste0(Sys.Date(), "-groups-stats.rtf")))
  
  


# Descriptive statistics --------------------------------------------------

d %>% 
  select(-female, -isced, -multimorbidity) %>% 
  mutate_if(is.factor, as.double) %>%  
  summarise_all(list(mean = mean, sd = sd, min = min, max = max)) %>% 
  gather(key, value) %>% 
  separate(key, into = c("variable", "stat")) %>% 
  spread(key = "stat", value = "value") %>% 
  left_join(tibble(variable = colnames(d),
                   labels = sjlabelled::get_label(d_raw, def.value = "", 
                                                  case = "parsed"))) %>% 
  select(labels, variable, mean, sd, min, max) %>% 
  gt() %>%  
  fmt_number(columns = vars(mean, sd, min, max), drop_trailing_zeros = T) %>% 
  gtsave(here::here("tables", paste0(Sys.Date(), "-descr-stats.rtf")))
  


# Exploring correlation between variables ---------------------------------
library(tidymodels)

my_recipe_foo <- recipe(depression ~ ., data = d %>% 
                      select(-isced, -female, -multimorbidity)) %>%
  step_ordinalscore(multidisc, childpr, famfinpr) %>% 
  step_dummy(all_predictors(), -all_numeric()) %>% 
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_corr(all_predictors(), threshold = .8) %>% 
  check_missing(all_predictors()) %>% 
  prep(d, retain = TRUE)

m_corr <- juice(my_recipe_foo) %>%
  correlate() %>%
  rearrange() %>% 
  shave()

rplot(m_corr)

juice(my_recipe_foo) %>% 
  correlate() %>% 
  network_plot(min_cor = .2)

pdf(file = here::here("outcome", paste0(Sys.Date(),"_upper_plot.pdf")),
    height = 4, width = 4)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
cor(juice(my_recipe_foo)) %>% 
  corrplot::corrplot(method = "color",
                     tl.col = "indianred4",
                     outline = T, addgrid.col = "gray",
                     addCoef.col="black", order = "AOE", 
                     cl.cex = .6,
                     number.cex = .3, tl.cex = .6, type = "full", 
                     diag=F)
dev.off()

# library(ggraph)
# library(igraph)
# 
# juice(my_recipe_foo) %>% 
#   correlate() %>%
#   stretch() %>% 
#   igraph::graph_from_data_frame(directed = F) %>% 
#   ggraph() +
#   geom_edge_link() +
#   geom_node_point()

# library(gt)
# m_corr %>% 
#   gt() %>% 
#   tab_header(title = "Correlation between variables")
  


# Recipe ------------------------------------------------------------------
library(tidymodels)
my_recipe <- recipe(depression ~ ., data = d %>% 
                      select(-isced, -female, -multimorbidity)) %>%
  step_ordinalscore(multidisc, childpr, famfinpr) %>% 
  step_dummy(all_predictors(), -all_numeric()) %>% 
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_corr(all_predictors(), threshold = .8) %>% 
  check_missing(all_predictors()) %>% 
  prep()


# Using caret instead of tidymodels ---------------------------------------

rf_model <- function(x) {
  rf <- caret::train(depression ~.,
                     data = x,
                     method = "ranger",
                     importance = "impurity",
                     trControl = caret::trainControl(method = "cv",
                                                     number = 3,
                                                     savePredictions = "final",
                                                     verboseIter = TRUE),
                     tuneLength = 10)
  
}

set.seed(123)
rf_results <- d %>% 
  nest(-isced, -female, -multimorbidity) %>% 
  mutate(data = map(data, bake, object = my_recipe)) %>% 
  mutate(rf_mod_result = map(data, rf_model)) %>% 
  mutate(var_imp = map(rf_mod_result, caret::varImp)) %>% 
  group_by(isced, female, multimorbidity) %>% 
  mutate(df_var_imp = map(var_imp, "importance"),
         df_var_imp = map(df_var_imp, rownames_to_column, var = "variables")) %>% 
  unnest(df_var_imp) %>% 
  ungroup()


# Plotting ----------------------------------------------------------------

### Primary education
rf_results %>%
  filter(isced == "primary education or less") %>% 
  select(-isced) %>% 
  rename("Gender" = female,
         "varimportance" = Overall) %>% 
  group_by(multimorbidity, variables) %>% 
  mutate(Mean = mean(varimportance),
         aux_mean = Mean) %>% 
  ungroup() %>% 
  spread(Gender, varimportance) %>% 
  gather(Gender, varimportance, -multimorbidity, -variables, -aux_mean) %>%
  mutate(multimorbidity = as.character(multimorbidity)) %>% 
  mutate(type = if_else(Gender %in% c("Male", "Female"), "Gender", "Mean")) -> fee

ggplot(fee %>% filter(type != "Mean")) +
  geom_point(aes(x = reorder_within(variables, aux_mean, multimorbidity), 
                 y = varimportance, color = Gender)) +
  geom_point(data = fee %>% filter(type == "Mean"), 
             aes(reorder_within(variables, aux_mean, multimorbidity), 
             y = varimportance, shape = type)) +
  scale_shape_manual(values = c("Mean" = 24, "Gender" = 21),
                     limits = c("Mean", "Gender"),
                     labels = c("Mean", "Gender"),
                     name = "Value") +
  scale_x_reordered() +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  ylab("Gini Importance (%)") +
  xlab("") +
  coord_flip() +
  facet_wrap(multimorbidity ~., scales = "free")
ggsave(here::here("outcome", 
                  paste0(Sys.Date(), "-primary-education.pdf")),
       width = 7.31, height = 3)


### Lower secondary
rf_results %>%
  filter(isced == "lower secondary") %>% 
  select(-isced) %>% 
  rename("Gender" = female,
         "varimportance" = Overall) %>% 
  group_by(multimorbidity, variables) %>% 
  mutate(Mean = mean(varimportance),
         aux_mean = Mean) %>% 
  ungroup() %>% 
  spread(Gender, varimportance) %>% 
  gather(Gender, varimportance, -multimorbidity, -variables, -aux_mean) %>%
  mutate(multimorbidity = as.character(multimorbidity)) %>% 
  mutate(type = if_else(Gender %in% c("Male", "Female"), "Gender", "Mean")) -> fee

ggplot(fee %>% filter(type != "Mean")) +
  geom_point(aes(x = reorder_within(variables, aux_mean, multimorbidity), 
                 y = varimportance, color = Gender)) +
  geom_point(data = fee %>% filter(type == "Mean"), 
             aes(reorder_within(variables, aux_mean, multimorbidity), 
                 y = varimportance, shape = type)) +
  scale_shape_manual(values = c("Mean" = 24, "Gender" = 21),
                     limits = c("Mean", "Gender"),
                     labels = c("Mean", "Gender"),
                     name = "Value") +
  scale_x_reordered() +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  ylab("Gini Importance (%)") +
  xlab("") +
  coord_flip() +
  facet_wrap(multimorbidity ~., scales = "free")
ggsave(here::here("outcome", 
                  paste0(Sys.Date(), "-lower-secondary.pdf")),
       width = 7.31, height = 3)


### upper secondary

rf_results %>%
  filter(isced == "upper secondary") %>% 
  select(-isced) %>% 
  rename("Gender" = female,
         "varimportance" = Overall) %>% 
  group_by(multimorbidity, variables) %>% 
  mutate(Mean = mean(varimportance),
         aux_mean = Mean) %>% 
  ungroup() %>% 
  spread(Gender, varimportance) %>% 
  gather(Gender, varimportance, -multimorbidity, -variables, -aux_mean) %>%
  mutate(multimorbidity = as.character(multimorbidity)) %>% 
  mutate(type = if_else(Gender %in% c("Male", "Female"), "Gender", "Mean")) -> fee

ggplot(fee %>% filter(type != "Mean")) +
  geom_point(aes(x = reorder_within(variables, aux_mean, multimorbidity), 
                 y = varimportance, color = Gender)) +
  geom_point(data = fee %>% filter(type == "Mean"), 
             aes(reorder_within(variables, aux_mean, multimorbidity), 
                 y = varimportance, shape = type)) +
  scale_shape_manual(values = c("Mean" = 24, "Gender" = 21),
                     limits = c("Mean", "Gender"),
                     labels = c("Mean", "Gender"),
                     name = "Value") +
  scale_x_reordered() +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  ylab("Gini Importance (%)") +
  xlab("") +
  coord_flip() +
  facet_wrap(multimorbidity ~., scales = "free")
ggsave(here::here("outcome", 
                  paste0(Sys.Date(), "-upper-secondary.pdf")),
       width = 7.31, height = 3)

### post-secondary

rf_results %>%
  filter(isced == "post-secondary") %>% 
  select(-isced) %>% 
  rename("Gender" = female,
         "varimportance" = Overall) %>% 
  group_by(multimorbidity, variables) %>% 
  mutate(Mean = mean(varimportance),
         aux_mean = Mean) %>% 
  ungroup() %>% 
  spread(Gender, varimportance) %>% 
  gather(Gender, varimportance, -multimorbidity, -variables, -aux_mean) %>%
  mutate(multimorbidity = as.character(multimorbidity)) %>% 
  mutate(type = if_else(Gender %in% c("Male", "Female"), "Gender", "Mean")) -> fee

ggplot(fee %>% filter(type != "Mean")) +
  geom_point(aes(x = reorder_within(variables, aux_mean, multimorbidity), 
                 y = varimportance, color = Gender)) +
  geom_point(data = fee %>% filter(type == "Mean"), 
             aes(reorder_within(variables, aux_mean, multimorbidity), 
                 y = varimportance, shape = type)) +
  scale_shape_manual(values = c("Mean" = 24, "Gender" = 21),
                     limits = c("Mean", "Gender"),
                     labels = c("Mean", "Gender"),
                     name = "Value") +
  scale_x_reordered() +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  ylab("Gini Importance (%)") +
  xlab("") +
  coord_flip() +
  facet_wrap(multimorbidity ~., scales = "free")
ggsave(here::here("outcome", 
                  paste0(Sys.Date(), "-post-secondary.pdf")),
       width = 7.31, height = 3)


### tertiary education

rf_results %>%
  filter(isced == "tertiary education") %>% 
  select(-isced) %>% 
  rename("Gender" = female,
         "varimportance" = Overall) %>% 
  group_by(multimorbidity, variables) %>% 
  mutate(Mean = mean(varimportance),
         aux_mean = Mean) %>% 
  ungroup() %>% 
  spread(Gender, varimportance) %>% 
  gather(Gender, varimportance, -multimorbidity, -variables, -aux_mean) %>%
  mutate(multimorbidity = as.character(multimorbidity)) %>% 
  mutate(type = if_else(Gender %in% c("Male", "Female"), "Gender", "Mean")) -> fee

ggplot(fee %>% filter(type != "Mean")) +
  geom_point(aes(x = reorder_within(variables, aux_mean, multimorbidity), 
                 y = varimportance, color = Gender)) +
  geom_point(data = fee %>% filter(type == "Mean"), 
             aes(reorder_within(variables, aux_mean, multimorbidity), 
                 y = varimportance, shape = type)) +
  scale_shape_manual(values = c("Mean" = 24, "Gender" = 21),
                     limits = c("Mean", "Gender"),
                     labels = c("Mean", "Gender"),
                     name = "Value") +
  scale_x_reordered() +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  ylab("Gini Importance (%)") +
  xlab("") +
  coord_flip() +
  facet_wrap(multimorbidity ~., scales = "free")
ggsave(here::here("outcome", 
                  paste0(Sys.Date(), "-tertiary-education.pdf")),
       width = 7.31, height = 3)


# PCA ---------------------------------------------------------------------

### PCA Analysis
pca_results <- rf_results %>% 
  mutate(female = as.character(female)) %>% 
  spread(variables, Overall) %>% 
  unite("individual", c("isced", "female", "multimorbidity"), 
        sep = " ", remove = F) %>%
  nest() %>% 
  mutate(pca = map(data, ~ prcomp(.x %>% 
                                    select(-individual, -isced, 
                                           -female, -multimorbidity, -soccap, -praccom_Yes,
                                           -etnmin, -multidisc), 
                                  center = TRUE, retx = T)),
         pca_aug = map2(pca, data, ~augment(.x, data = .y)))

### PCA Axis Composition

pca_results %>% 
  transmute(rotation = map(pca, "rotation"),
            rotation = map(rotation, as_tibble, 
                           rownames = "id")) %>% 
  unnest() %>% 
  select(id:PC2) %>% 
  arrange(desc(abs(PC1))) %>% 
  mutate_if(is.numeric, ~if_else(abs(.) > .14, ., NA_real_)) %>%
  gt() %>% 
  fmt_number(vars(PC1, PC2), drop_trailing_zeros = T) %>% 
  fmt_missing(vars(PC1, PC2)) %>% 
  gtsave(here::here("tables", paste0(Sys.Date(), "-pca-loadings.rtf")))

### PCA Plot  
library(ggfortify)

bind_rows(
  pca_results %>%
    unnest(pca_aug) %>% 
    select(-individual, -`.rownames`) %>% 
    group_by(female) %>% 
    summarise_if(is.numeric, mean) %>% 
    rename("type" = female),
  pca_results %>%
    unnest(pca_aug) %>% 
    select(-individual, -`.rownames`) %>% 
    group_by(isced) %>% 
    summarise_if(is.numeric, mean) %>% 
    rename("type" = isced),
  pca_results %>%
    unnest(pca_aug) %>% 
    select(-individual, -`.rownames`) %>% 
    group_by(multimorbidity) %>% 
    summarise_if(is.numeric, mean) %>% 
    rename("type" = multimorbidity)
) -> pca_mean_result

pca_results %>% 
  mutate(
    pca_graph = map2(
      .x = pca,
      .y = data,
      ~ autoplot(.x, loadings = TRUE, 
                 loadings.colour = "grey",
                 scale = 0,
                 loadings.label = TRUE,
                 loadings.label.repel = TRUE,
                 data = .y, label = FALSE,
                 label.label = "",
                 label.repel = FALSE,
                 colour = "female",
                 shape = "multimorbidity",
                 size = "isced") +
        theme_bw() +
        scale_color_brewer(type = "qual", palette = "Dark2") +
        labs(title = "First two principal components of PCA on RF results") +
        geom_label(data = pca_mean_result %>% 
                     filter(type %in% c("Female", "Male", "Yes", "No")), 
                   aes(x = `.fittedPC1`, y = `.fittedPC2`, label = type))
    )
  ) %>%
  pull(pca_graph)

ggsave(here::here("outcome", 
                  paste0(Sys.Date(), "-pca-plot.pdf")),
       width = 7.31, height = 5)

# Model -------------------------------------------------------------------
# 
# my_rf <- function(dataset) {
#   rand_forest(mode = "regression") %>% 
#     set_engine("randomForest", localImp = TRUE,
#                importance = T) %>%
#     fit(depression~.,
#         data = juice(dataset))
# }
# 
# assess_predictions <- function(split, recipe, model) {
#   raw_assessment <- assessment(split)
#   processed <- bake(recipe, new_data = raw_assessment)
#   model %>%
#     predict(new_data = processed) %>%
#     bind_cols(processed) %>%
#     mutate(
#       # Sale_Price is already logged by the recipe
#       .resid = depression - .pred,
#       # Save the original row number of the data
#       .row = as.integer(split, data = "assessment")
#     )
# }
# 
# 
# Preparing training data -------------------------------------------------
# 
# library(tidymodels)
# 
# foo <- d %>% 
#   nest(-isced, -female, -multimorbidity) %>% 
#   mutate(train_cv = map(data, mc_cv, times = 5)) %>% 
#   select(-data) %>% 
#   unnest(train_cv) %>% 
#   group_by(female, multimorbidity, isced) %>% 
#   mutate(prepared = map(splits, prepper, recipe = my_recipe),
#          resul = map(prepared, my_rf)) %>% 
#   mutate(pred = pmap(list(split = splits,
#                           recipe = prepared,
#                           model = resul), 
#                      assess_predictions))  
# 
# foo %>% 
#   ungroup() %>%  
#   group_by(female, multimorbidity, isced, id) %>% 
#   mutate(rmse = map(pred, rmse, truth = depression, estimate = .pred),
#          rmse = map_dbl(rmse, ".estimate")) %>% 
#   ungroup() %>% 
#   group_by(female, multimorbidity, isced) %>% 
#   filter(rmse == min(rmse)) %>% 
#   select(-splits, -id, -prepared, -pred, -rmse) %>%
#   mutate(fit = map(resul, "fit"),
#          importance = map(fit, randomForest::importance),
#          importance = map(importance, as_tibble, rownames = "id")) %>% 
#   unnest(importance) %>% 
#   rename(IncMSE = `%IncMSE`) -> aaa
# 
# aaa %>% 
#   ungroup() %>%
#   select(-IncNodePurity) %>% 
#   spread(multimorbidity, IncMSE) %>% 
#   mutate(differences = Yes - No,
#          id = fct_reorder(id, differences)) %>%
#   select(-differences) %>% 
#   gather()
#   
#   ggplot(aes(x = id, y = IncMSE, color = multimorbidity)) +
#   geom_point(size = 2) +
#   coord_flip() +
#   facet_grid(isced ~ female, scales = "free", space = "free")
#   