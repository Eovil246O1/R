library(magrittr)
library(lightgbm)
library(moments)
library(data.table)
library(recommenderlab)
library(tidyverse)

#---------------------------
cat("Loading data...\n")

data_dir = "C:\\Users\\Viacheslav_Pyrohov\\Desktop\\Kaggle_Homecredit competition"

tr <- read_csv(file.path(data_dir, "application_train.csv"))
te <- read_csv(file.path(data_dir, "application_test.csv"))

#---------------------------
cat("Preprocessing...\n")
fn <- funs(mean, sd, min, max, sum, n_distinct, .args = list(na.rm = TRUE))
numbers_of_applications = c(1, 3, 5)

#---------------------------
cat("Preprocessing bureau_balance.csv...\n")
bbalance <- read_csv(file.path(data_dir, "bureau_balance.csv"))

sum_bbalance <- bbalance %>%
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>%
  group_by(SK_ID_BUREAU) %>%
  summarise_all(fn)

rm(bbalance); gc()

#---------------------------
cat("Preprocessing bureau.csv...\n")
bureau <- read_csv(file.path(data_dir, "bureau.csv"))

sum_bureau <- bureau %>% 
  left_join(sum_bbalance, by = "SK_ID_BUREAU") %>% 
  select(-SK_ID_BUREAU) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)

rm(bureau, sum_bbalance); gc()

#---------------------------
cat("Preprocessing credit_card_balance.csv...\n")
cc_balance <- read_csv(file.path(data_dir, "credit_card_balance.csv"))

sum_cc_balance <- cc_balance %>% 
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)

rm(cc_balance); gc()

#---------------------------
cat("Preprocessing installments_payments.csv...\n")
payments <- read_csv(file.path(data_dir, "installments_payments.csv"))

sum_payments <- payments %>% 
  select(-SK_ID_PREV) %>% 
  mutate(PAYMENT_PERC = AMT_PAYMENT / AMT_INSTALMENT,
         PAYMENT_DIFF = AMT_INSTALMENT - AMT_PAYMENT,
         DPD = DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT,
         DBD = DAYS_INSTALMENT - DAYS_ENTRY_PAYMENT,
         DPD = ifelse(DPD > 0, DPD, 0),
         DBD = ifelse(DBD > 0, DBD, 0)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn) 

rm(payments); gc()

#---------------------------
cat("Preprocessing POS_CASH_balance.csv...\n")
pc_balance <- read_csv(file.path(data_dir, "POS_CASH_balance.csv"))

sum_pc_balance <- pc_balance %>% 
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)

rm(pc_balance); gc()

#---------------------------
cat("Preprocessing previous_application.csv...\n")
prev <- read_csv(file.path(data_dir, "previous_application.csv"))

sum_prev <- prev %>%
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  mutate(DAYS_FIRST_DRAWING = ifelse(DAYS_FIRST_DRAWING == 365243, NA, DAYS_FIRST_DRAWING),
         DAYS_FIRST_DUE = ifelse(DAYS_FIRST_DUE == 365243, NA, DAYS_FIRST_DUE),
         DAYS_LAST_DUE_1ST_VERSION = ifelse(DAYS_LAST_DUE_1ST_VERSION == 365243, NA, DAYS_LAST_DUE_1ST_VERSION),
         DAYS_LAST_DUE = ifelse(DAYS_LAST_DUE == 365243, NA, DAYS_LAST_DUE),
         DAYS_TERMINATION = ifelse(DAYS_TERMINATION == 365243, NA, DAYS_TERMINATION),
         APP_CREDIT_PERC = AMT_APPLICATION / AMT_CREDIT) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)

prev
? summarize

#neptune prev applications
neptune_prev = prev %>%
  group_by(SK_ID_CURR) %>%
  mutate(previous_application_number_of_prev_application = n_distinct(SK_ID_PREV, na.rm = FALSE),
         previous_application_prev_was_approved = if_else(first(NAME_CONTRACT_STATUS, SK_ID_PREV) == 'Approved', 1, 0),
         previous_application_prev_was_refused = if_else(first(NAME_CONTRACT_STATUS, SK_ID_PREV) == 'Refused', 1, 0),
         id_ind = row_number(SK_ID_PREV)) %>%
  select(c(SK_ID_CURR, id_ind, previous_application_number_of_prev_application, previous_application_prev_was_approved, previous_application_prev_was_refused)) %>%
  distinct()



# group_object = prev_applications_tail.groupby(by=['SK_ID_CURR'])['CNT_PAYMENT'].mean().reset_index()
# group_object.rename(index=str, columns={
#   'CNT_PAYMENT': 'previous_application_term_of_last_{}_credits_mean'.format(number)},
#   inplace=True)
# features = features.merge(group_object, on=['SK_ID_CURR'], how='left')
# 
# group_object = prev_applications_tail.groupby(by=['SK_ID_CURR'])['DAYS_DECISION'].mean().reset_index()
# group_object.rename(index=str, columns={
#   'DAYS_DECISION': 'previous_application_days_decision_about_last_{}_credits_mean'.format(number)},
#   inplace=True)
# features = features.merge(group_object, on=['SK_ID_CURR'], how='left')
# 
# group_object = prev_applications_tail.groupby(by=['SK_ID_CURR'])['DAYS_FIRST_DRAWING'].mean().reset_index()
# group_object.rename(index=str, columns={
#   'DAYS_FIRST_DRAWING': 'previous_application_days_first_drawing_last_{}_credits_mean'.format(number)},
#   inplace=True)
# features = features.merge(group_object, on=['SK_ID_CURR'], how='left')

rm(prev); gc()

tri <- 1:nrow(tr)
y <- tr$TARGET

tr_te <- tr %>% 
  select(-TARGET) %>% 
  bind_rows(te) %>%
  left_join(sum_bureau, by = "SK_ID_CURR") %>% 
  left_join(sum_cc_balance, by = "SK_ID_CURR") %>% 
  left_join(sum_payments, by = "SK_ID_CURR") %>% 
  left_join(sum_pc_balance, by = "SK_ID_CURR") %>% 
  left_join(sum_prev, by = "SK_ID_CURR") %>% 
  select(-SK_ID_CURR) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  mutate(na = apply(., 1, function(x) sum(is.na(x))),
         DAYS_EMPLOYED = ifelse(DAYS_EMPLOYED == 365243, NA, DAYS_EMPLOYED),
         DAYS_EMPLOYED_PERC = sqrt(DAYS_EMPLOYED / DAYS_BIRTH),
         INCOME_CREDIT_PERC = AMT_INCOME_TOTAL / AMT_CREDIT,
         INCOME_PER_PERSON = log1p(AMT_INCOME_TOTAL / CNT_FAM_MEMBERS),
         ANNUITY_INCOME_PERC = sqrt(AMT_ANNUITY / (1 + AMT_INCOME_TOTAL)),
         LOAN_INCOME_RATIO = AMT_CREDIT / AMT_INCOME_TOTAL,
         ANNUITY_LENGTH = AMT_CREDIT / AMT_ANNUITY,
         CHILDREN_RATIO = CNT_CHILDREN / CNT_FAM_MEMBERS,
         CREDIT_TO_GOODS_RATIO = AMT_CREDIT / AMT_GOODS_PRICE,
         INC_PER_CHLD = AMT_INCOME_TOTAL / (1 + CNT_CHILDREN),
         SOURCES_PROD = EXT_SOURCE_1 * EXT_SOURCE_2 * EXT_SOURCE_3,
         CAR_TO_BIRTH_RATIO = OWN_CAR_AGE / DAYS_BIRTH,
         CAR_TO_EMPLOY_RATIO = OWN_CAR_AGE / DAYS_EMPLOYED,
         PHONE_TO_BIRTH_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_BIRTH,
         PHONE_TO_EMPLOY_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_EMPLOYED)

docs <- str_subset(names(tr), "FLAG_DOC")
live <- str_subset(names(tr), "(?!NFLAG_)(?!FLAG_DOC)(?!_FLAG_)FLAG_")
inc_by_org <- tr_te %>% 
  group_by(ORGANIZATION_TYPE) %>% 
  summarise(m = median(AMT_INCOME_TOTAL)) %$% 
  setNames(as.list(m), ORGANIZATION_TYPE)

rm(tr, te, fn, sum_bureau, sum_cc_balance, 
   sum_payments, sum_pc_balance, sum_prev); gc()
tr_te %<>% 
  mutate(DOC_IND_KURT = apply(tr_te[, docs], 1, moments::kurtosis),
         LIVE_IND_SUM = apply(tr_te[, live], 1, sum),
         NEW_INC_BY_ORG = dplyr::recode(tr_te$ORGANIZATION_TYPE, !!!inc_by_org),
         NEW_EXT_SOURCES_MEAN = apply(tr_te[, c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")], 1, mean),
         NEW_SCORES_STD = apply(tr_te[, c("EXT_SOURCE_1", "EXT_SOURCE_2", "EXT_SOURCE_3")], 1, sd))%>%
  mutate_all(funs(ifelse(is.nan(.), NA, .))) %>% 
  mutate_all(funs(ifelse(is.infinite(.), NA, .))) %>% 
  data.matrix()

#---------------------------
cat("Save & load dataset...\n")
save(tr_te, file = paste0(data_dir, "//Calculation//input_bigmatrix_short.RData"), version = NULL)
save(tri, file = paste0(data_dir, "//Calculation//input_tri.RData"), version = NULL)
save(y, file = paste0(data_dir, "//Calculation//input_y.RData"), version = NULL)

#load(file = paste0(data_dir, "//Calculation//input_bigmatrix_short.RData"), .GlobalEnv)
#load(file = paste0(data_dir, "//Calculation//input_tri.RData"), .GlobalEnv)
#load(file = paste0(data_dir, "//Calculation//input_y.RData"), .GlobalEnv)
gc()

#---------------------------
cat("Save & load long dataset...\n")
saveRDS(tr_te, file = paste0(data_dir, "//Calculation//input_bigmatrix_long.rds"))

#---------------------------
lgbm_feat = data.table(Feature = character(), Gain = numeric(), Cover = numeric(), Frequency = numeric())
lgbm_pred_list = list()
lgbm_score = vector()
cat("Preparing data...\n")
for (i in 1:5) {
  tr_te = readRDS(paste0(data_dir, "//Calculation//input_bigmatrix_long.rds"))
  load(file = paste0(data_dir, "//Calculation//input_tri.RData"), .GlobalEnv)
  load(file = paste0(data_dir, "//Calculation//input_y.RData"), .GlobalEnv)
  
  #dtest <- lgb.Dataset(data = tr_te[-tri, ]) #it seems that this approach do not work for LightGBM. Raise questions for this.
  dtest <- tr_te[-tri, ]
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
                  learning_rate=0.02, # in source - 0.02
                  num_leaves=127,
                  #colsample_bytree=0.9497036,
                  #subsample=0.8715623,
                  #max_depth=8,
                  #reg_alpha=0.04,
                  #reg_lambda=0.073,
                  #min_split_gain=0.0222415,
                  #min_child_weight=40,
                  feature_fraction = 0.5, #originaly 0.5
                  bagging_freq = 1,
                  bagging_fraction = 0.8,
                  use_missing = TRUE,
                  is_unbalance = TRUE)
  
  m_gbm_cv = lgb.train(params = lgb.grid,
                       data = dtrain,
                       num_threads = 10,
                       nrounds = 5,
                       eval_freq = 20,
                       #boosting = 'dart', # todo: check the difference
                       #num_leaves = 255, # typical: 255, usually {15, 31, 63, 127, 255, 511, 1023, 2047, 4095}.
                       #eval = "binary_error", #can place own validation function here #unknown parameter
                       #categorical_feature = categoricals.vec,
                       num_iterations = 3000, #2000, equivalent of n_estimators
                       early_stopping_round = 200,
                       valids = list(train = dval),
                       #nfold = 5, #unknown parameter
                       #stratified = TRUE, #unknown parameter
                       verbose = 2)
  lgbm_pred_list[[i]] = predict(m_gbm_cv, dtest)
  lgbm_feat = rbindlist(list(lgbm_feat, lgb.importance(m_gbm_cv, percentage = TRUE)))
  lgbm_score[i] = m_gbm_cv$best_score
}

avg_lgbm = Reduce(`+`, lgbm_pred_list)
avg_lgbm = avg_lgbm/i

lgbm_feat_avg = lgbm_feat %>% group_by(Feature) %>%
  summarize(gain_avg = mean(Gain),
            cover_avg = mean(Cover),
            frequency_avg = mean(Frequency))
lgbm_score_avg = mean(lgbm_score)

#---------------------------
read_csv(file.path(data_dir, "//Models//sample_submission.csv")) %>%  
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
         TARGET = avg_lgbm) %>%
  write_csv(file.path(data_dir, paste0("//Models//new_mod_", round(lgbm_score_avg, 5), ".csv")))

# write file with characteristic parameters
write_csv(lgbm_feat_avg, file.path(data_dir, paste0("//Results//new_mod_", round(lgbm_score_avg, 5), "_importance.csv")))