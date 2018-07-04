rm(list = ls())
gc() #garbage collector

library(tidyverse)
library(xgboost)
library(magrittr)
library(data.table)
library(caret)
library(mltools)

#devtools::install_github("Microsoft/LightGBM", ref = "1b7643b", subdir = "R-package")
library(lightgbm)

#detach(lightgbm)
#unloadNamespace("lightgbm")

set.seed(0)

data_dir = "C:\\Users\\Eovil\\Desktop\\Kaggle_Homecredit competition"

#---------------------------
cat("Loading data...\n")
tr <- read_csv(file.path(data_dir, "application_train.csv"))
te <- read_csv(file.path(data_dir, "application_test.csv"))

bureau <- read_csv(file.path(data_dir, "bureau.csv")) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer())) 

cred_card_bal <-  read_csv(file.path(data_dir, "credit_card_balance.csv")) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer())) 

pos_cash_bal <- read_csv(file.path(data_dir, "POS_CASH_balance.csv")) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer())) 

prev <- read_csv(file.path(data_dir, "previous_application.csv")) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer()))

#---------------------------
cat("Preprocessing...\n")

to_exclude = c('SK_ID_CURR', 'SK_ID_BUREAU')

# AVG metrics calculation
avg_bureau <- bureau %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>%
  rename_at(vars(-one_of(to_exclude)), ~ paste0(., '_avg')) %>%
  mutate(buro_count = bureau %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

avg_cred_card_bal <- cred_card_bal %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_avg')) %>%
  mutate(card_count = cred_card_bal %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

avg_pos_cash_bal <- pos_cash_bal %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_avg')) %>%
  mutate(pos_count = pos_cash_bal %>%  
           group_by(SK_ID_PREV, SK_ID_CURR) %>% 
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

avg_prev <- prev %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_avg')) %>%
  mutate(nb_app = prev %>%  
           group_by(SK_ID_CURR) %>% 
           count() %$% n)

# MAX metrics calculation

max_bureau <- bureau %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) %>%
  rename_at(vars(-one_of(to_exclude)), ~ paste0(., '_max'))

max_cred_card_bal <- cred_card_bal %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_max'))

max_pos_cash_bal <- pos_cash_bal %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_max'))

max_prev <- prev %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(max(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_max'))

# MIN metrics calculation

min_bureau <- bureau %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(min(., na.rm = TRUE))) %>%
  rename_at(vars(-one_of(to_exclude)), ~ paste0(., '_min'))

min_cred_card_bal <- cred_card_bal %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(min(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_min'))

min_pos_cash_bal <- pos_cash_bal %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(min(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_min'))

min_prev <- prev %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(min(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_min'))

# SUM metrics calcualtion

sum_bureau <- bureau %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(sum(., na.rm = TRUE))) %>%
  rename_at(vars(-one_of(to_exclude)), ~ paste0(., '_sum'))

sum_cred_card_bal <- cred_card_bal %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(sum(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_sum'))

sum_pos_cash_bal <- pos_cash_bal %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(sum(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_sum'))

sum_prev <- prev %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(sum(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_sum'))

# STD.DEV metrics calcualtion

sd_bureau <- bureau %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(sd(., na.rm = TRUE))) %>%
  rename_at(vars(-one_of(to_exclude)), ~ paste0(., '_sd'))

sd_cred_card_bal <- cred_card_bal %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(sd(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_sd'))

sd_pos_cash_bal <- pos_cash_bal %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(sd(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_sd'))

sd_prev <- prev %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(funs(sd(., na.rm = TRUE))) %>% 
  rename_at(vars(-one_of(to_exclude[1])), ~ paste0(., '_sd'))

tri <- 1:nrow(tr)
y <- tr$TARGET
save(tri, file = paste0(data_dir, "//Calculation//input_tri.RData"), version = NULL)
save(y, file = paste0(data_dir, "//Calculation//input_y.Rdata"), version = NULL)

tr_te <- tr %>% 
  select(-TARGET) %>% 
  bind_rows(te) %>%
  left_join(avg_bureau, by = "SK_ID_CURR") %>% 
  left_join(avg_cred_card_bal, by = "SK_ID_CURR") %>% 
  left_join(avg_pos_cash_bal, by = "SK_ID_CURR") %>% 
  left_join(avg_prev, by = "SK_ID_CURR") %>%
  left_join(max_bureau, by = "SK_ID_CURR") %>% 
  left_join(max_cred_card_bal, by = "SK_ID_CURR") %>% 
  left_join(max_pos_cash_bal, by = "SK_ID_CURR") %>% 
  left_join(max_prev, by = "SK_ID_CURR") %>%
  left_join(min_bureau, by = "SK_ID_CURR") %>% 
  left_join(min_cred_card_bal, by = "SK_ID_CURR") %>% 
  left_join(min_pos_cash_bal, by = "SK_ID_CURR") %>% 
  left_join(min_prev, by = "SK_ID_CURR") %>%
  left_join(sum_bureau, by = "SK_ID_CURR") %>% 
  left_join(sum_cred_card_bal, by = "SK_ID_CURR") %>% 
  left_join(sum_pos_cash_bal, by = "SK_ID_CURR") %>% 
  left_join(sum_prev, by = "SK_ID_CURR") %>%
  left_join(sd_bureau, by = "SK_ID_CURR") %>% 
  left_join(sd_cred_card_bal, by = "SK_ID_CURR") %>% 
  left_join(sd_pos_cash_bal, by = "SK_ID_CURR") %>% 
  left_join(sd_prev, by = "SK_ID_CURR") %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer())) %>% #convert factor to integer to save space
  data.matrix()

rm(tr, te, prev,  bureau, cred_card_bal, pos_cash_bal,
   avg_pos_cash_bal, avg_prev, avg_bureau, avg_cred_card_bal,
   max_pos_cash_bal, max_prev, max_bureau, max_cred_card_bal,
   min_pos_cash_bal, min_prev, min_bureau, min_cred_card_bal,
   sd_pos_cash_bal, sd_prev, sd_bureau, sd_cred_card_bal,
   sum_pos_cash_bal, sum_prev, sum_bureau, sum_cred_card_bal); gc()

save(tr_te, file = paste0(data_dir, "//Calculation//input_bigmatrix_short.RData"), version = NULL)

tr_te = as.data.table(tr_te)

# create vars for NaN observations
col_num = ncol(tr_te)
for (i in 3:col_num) {
  colname = names(tr_te)[i]
  tr_te[is.na(eval(as.name(colname)))|is.nan(eval(as.name(colname)))|is.null(eval(as.name(colname)))|is.infinite(eval(as.name(colname))), 
        paste0(colname, '_nulls') := 1]
  tr_te[is.na(eval(as.name(paste0(colname, '_nulls')))), paste0(colname, '_nulls') := 0]
}

# outliers marking
outliers_remove = function(dt,col_from,col_to) {
  for (i in col_from:col_to) {
    colname = names(dt)[i]
    qnt <- quantile(dt[,eval(as.name(colname))], probs=c(.25, .75), na.rm = T)
    H <- 1.5 * (qnt[2]-qnt[1])
    dt[eval(as.name(colname)) < (qnt[1] - H), paste0(colname, '_outliers') := -1]
    dt[eval(as.name(colname)) > (qnt[2] + H), paste0(colname, '_outliers') := 1]
    dt[is.na(eval(as.name(paste0(colname, '_outliers')))), paste0(colname, '_outliers') := 0]
  }
  return(as.data.table(dt))
}

tr_te = outliers_remove(tr_te, col_from = 3, col_to = col_num)

# create experiment variables
tr_te[, `:=` (exp_var_1 = 1.1*DAYS_BIRTH - 0.9*NAME_CONTRACT_STATUS_avg
              , exp_var_2 = 1.1*DAYS_LAST_PHONE_CHANGE + 1.1*AMT_CREDIT
              , exp_var_3 = 0.1*FLAG_LAST_APPL_PER_CONTRACT_avg - 1.1*DAYS_EMPLOYED
              , exp_var_4 = 1.9*DAYS_FIRST_DRAWING_min * 0.3*AMT_GOODS_PRICE
              , exp_var_5 = 1.2*CNT_PAYMENT_min - 1.5*DAYS_CREDIT_max
              , exp_var_6 = 0.2*CODE_GENDER^3.2 / 1.3*SK_ID_PREV_max.y^3.3 + 2*DAYS_ENDDATE_FACT_avg
              , exp_var_7 = 0.4*SK_DPD_DEF_min.y + 1.1*FLAG_PHONE + 1*DAYS_CREDIT_avg
              , exp_var_8 = 0.6*DAYS_ID_PUBLISH + 0.8*CODE_REJECT_REASON_max
              , exp_var_9 = 0.7*CODE_GENDER / 1.2*OWN_CAR_AGE + 1.7*NAME_PRODUCT_TYPE_sd
              , exp_var_10 = 0.6*AMT_CREDIT_SUM_DEBT_avg + 1.4*FLAG_EMAIL
              , exp_var_11 = 0.9*SELLERPLACE_AREA_min + 0.7*CNT_PAYMENT_sd^3.9
              , exp_var_12 = 0.8*DAYS_REGISTRATION - 0.3*AMT_CREDIT
              , exp_var_13 = 0.5*NAME_YIELD_GROUP_sd + 0.8*CNT_PAYMENT_max
              )]

#tr_te = sparsify(tr_te, sparsifyNAs = FALSE, naCols = "none")
# create matrix from dt without RAM issues
temp_names = colnames(tr_te)
write_csv(as.data.frame(temp_names), path = paste0(data_dir, "//Calculation//input_colnames.csv"), col_names = TRUE)
write_csv(tr_te, path = paste0(data_dir, "//Calculation//input_bigmatrix.csv"), col_names = FALSE)
rm(tr_te)

temp_names = read.csv(file = paste0(data_dir, "//Calculation//input_colnames.csv"))
matr_scan = scan(file = paste0(data_dir, "//Calculation//input_bigmatrix.csv"), sep = ',')
tr_te = matrix(matr_scan, ncol = length(temp_names$temp_names), byrow = TRUE, dimnames = list(NULL, temp_names$temp_names))
rm(matr_scan)

save(tr_te, file = paste0(data_dir, "//Calculation//input_matrix_long.RData"), version = NULL)

load(file = paste0(data_dir, "//Calculation//input_bigmatrix_short.RData"), .GlobalEnv)
load(file = paste0(data_dir, "//Calculation//input_bigmatrix_long.RData"), .GlobalEnv)

# create random vars
#input_random_vars = get_random_feature(tr_te[,c(1:531)], 500)

#---------------------------
load(file = paste0(data_dir, "//Calculation//input_tri.RData"), .GlobalEnv)
load(file = paste0(data_dir, "//Calculation//input_y.Rdata"), .GlobalEnv)

cat("Preparing data...\n")
dtest <- lgb.Dataset(data = tr_te[-tri, ])
tr_te <- tr_te[tri, ]
tri <- caret::createDataPartition(y, p = 0.9, list = F) %>% c()

dtrain = lgb.Dataset(data = tr_te[tri, ], label = y[tri])
dval = lgb.Dataset(data = tr_te[-tri, ], label = y[-tri])
cols <- colnames(tr_te)

rm(tr_te, y, tri); gc()

#---------------------------
cat("Training model...\n")

lgb.grid = list(objective = "binary",
                metric = "auc",
                min_sum_hessian_in_leaf = 1, #comented to make possible additional splits
                feature_fraction = 0.8, #subset for columns per iteration
                bagging_fraction = 0.8,
                
                #subsample=0.8,
                #max_depth=8, #it seems that this parameter have a bad impact for model. Max depth set to static value.
                #reg_alpha=.1,
                #reg_lambda=.1,
                #min_split_gain=.01,
                #min_child_weight=100,
                
                #min_data = 100,
                #max_bin = 50,
                #lambda_l1 = 8,
                #lambda_l2 = 1.3,
                #min_data_in_bin=100,
                #min_gain_to_split = 10,
                #min_data_in_leaf = 30,
                is_unbalance = TRUE)

# m_gbm_cv = lgb.cv(params = lgb.grid,
#                       data = dtrain,
#                       num_threads = 8,
#                       nrounds = 10,
#                       learning_rate = 0.02, #this parameter could be set to 0.01
#                       eval_freq = 20,
#                       num_leaves = 225, # typical: 255, usually {15, 31, 63, 127, 255, 511, 1023, 2047, 4095}.
#                       #eval = {"binary_error"}, #can place own validation function here
#                       #categorical_feature = categoricals.vec,
#                       num_iterations = 50,
#                       early_stopping_round = 200,
#                       use_missing = FALSE,
#                       valid_sets = list(train = dval),
#                       nfold = 5,
#                       stratified = TRUE,
#                       verbose = -1)

m_gbm_cv = lgb.train(params = lgb.grid,
                     data = dtrain,
                     num_threads = 10,
                     nrounds = 10,
                     learning_rate = 0.02, #this parameter could be set to 0.01
                     eval_freq = 20,
                     num_leaves = 255, # typical: 255, usually {15, 31, 63, 127, 255, 511, 1023, 2047, 4095}.
                     #eval = "binary_error", #can place own validation function here #unknown parameter
                     #categorical_feature = categoricals.vec,
                     num_iterations = 3000, #2000
                     early_stopping_round = 200,
                     boosting = "dart", #dropout
                     use_missing = FALSE,
                     valids = list(train = dval),
                     #nfold = 5, #unknown parameter
                     #stratified = TRUE, #unknown parameter
                     verbose = 2)

#lgb.model.cv = lgb.cv(params = lgb.grid, data = lgb.train, learning_rate = 0.02, num_leaves = 25,
#                   num_threads = 2 , nrounds = 7000, early_stopping_rounds = 50,
#                   eval_freq = 20, eval = lgb.normalizedgini,
#                   categorical_feature = categoricals.vec, nfold = 5, stratified = TRUE)

# try to implement grid search

grid_search <- expand.grid(#max_depth = c(8, 10),
                           #min_data_in_leaf = c(2, 4, 6),
                           min_sum_hessian_in_leaf = c(1),
                           feature_fraction = c(0.5, 0.8, 1),
                           bagging_fraction = c(0.5, 0.8, 1),
                           bagging_freq = c(0, 1),
                           #lambda_l1 = c(0.2, 0.4),
                           #lambda_l2 = c(0.2, 0.4),
                           #min_gain_to_split = c(0.1, 0.2, 0.4, 0.8),
                           num_leaves = c(127, 255, 511),
                           #learning_rate = c(0.05, 0.01, 0.02, 0.03),
                           use_missing = c(TRUE, FALSE),
                           is_unbalance = c(TRUE, FALSE))

perf <- numeric(nrow(grid_search))

for (i in 1:nrow(grid_search)) {
  model <- lgb.train(params = list(objective = "binary",
                          metric = "auc",
                          #max_depth = grid_search[i, "max_depth"],
                          #min_data_in_leaf = grid_search[i,"min_data_in_leaf"],
                          min_sum_hessian_in_leaf = grid_search[i, "min_sum_hessian_in_leaf"],
                          feature_fraction =  grid_search[i, "feature_fraction"],
                          bagging_fraction =  grid_search[i, "bagging_fraction"],
                          bagging_freq =  grid_search[i, "bagging_freq"],
                          #lambda_l1 =  grid_search[i, "lambda_l1"],
                          #lambda_l2 =  grid_search[i, "lambda_l2"],
                          #min_gain_to_split =  grid_search[i, "min_gain_to_split"],
                          is_unbalance = grid_search[i, "is_unbalance"]),
                     data = dtrain,
                     num_threads = 10,
                     nrounds = 3,
                     learning_rate = 0.02, #grid_search[i, "learning_rate"], #this parameter could be set to 0.01
                     eval_freq = 20,
                     num_leaves = grid_search[i, "num_leaves"], # typical: 255, usually {15, 31, 63, 127, 255, 511, 1023, 2047, 4095}.
                     #eval = "binary_error", #can place own validation function here #unknown parameter
                     #categorical_feature = categoricals.vec,
                     num_iterations = 2000, #set 2000 iterations
                     early_stopping_round = 200,
                     use_missing = grid_search[i, "use_missing"],
                     valids = list(train = dval),
                     #nfold = 5, #unknown parameter
                     #stratified = TRUE, #unknown parameter
                     verbose = 2)
  perf[i] <- max(rbindlist(model$record_evals$train$auc))
  gc(verbose = FALSE)
}
View(perf)
# grid_search
perf
cat("Model ", which.max(perf), " is max auc: ", max(perf), sep = "","\n")
print(grid_search[which.max(perf), ])

grid_search

traceback()

#---------------------------
read_csv(file.path(data_dir, "//Models//sample_submission.csv")) %>%  
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
         TARGET = predict(m_gbm_cv, dtest)) %>%
  write_csv(file.path(data_dir, paste0("//Models//exp_cols_", round(m_gbm_cv$best_score, 5), ".csv")))

# write file with characteristic parameters
write_csv(lgb.importance(m_gbm_cv, percentage = TRUE), file.path(data_dir, paste0("//Results//exp_cols_", round(m_gbm_cv$best_score, 5), "_importance.csv")))
