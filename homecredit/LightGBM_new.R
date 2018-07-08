library(magrittr)
library(lightgbm)
library(moments)
library(data.table)
library(recommenderlab)
library(tidyverse)

set.seed(0)

#---------------------------
cat("Loading data...\n")

data_dir = "C:\\Users\\Eovil\\Desktop\\Kaggle_Homecredit competition"

tr <- read_csv(file.path(data_dir, "application_train.csv"))
te <- read_csv(file.path(data_dir, "application_test.csv"))

#---------------------------
cat("Preprocessing...\n")

fn <- funs(mean, sd, min, max, sum, n_distinct, .args = list(na.rm = TRUE))
time_coef = log(0.5)/(-24) #apply weight coefficient to historical data, coef = 0.5 weight for 24 month ago

#---------------------------
cat("Preprocessing bureau_balance.csv...\n")
bbalance <- read_csv(file.path(data_dir, "bureau_balance.csv"))

# IMPORTANT! This part has low gain at the moment - check showed that old algorythm gave sum gain of 0.03234, new - 0.0457
sum_bbalance <- bbalance %>%
  #to improve: treat warnings
  #to do: delete redundant variables
  #to do: to make sure that new approach works validate against the same number of features!
  filter(!STATUS %in% 'X') %>% #filter out STATUS == 'X' as this mean absense of data
  mutate(STATUS = if_else(STATUS == 'C', -1, as.numeric(STATUS)), #treat 'C' = closed as -1 #this returns warning, but the result is OK and validated
         STATUS_WEIGHTED = exp(time_coef*(MONTHS_BALANCE))*STATUS) %>%
  group_by(SK_ID_BUREAU) %>%
  mutate(START_STATUS = first(STATUS, MONTHS_BALANCE),
         END_STATUS = last(STATUS, MONTHS_BALANCE)) %>%
  summarise_all(fn)

rm(bbalance); gc()

# old approach
# sum_bbalance <- bbalance %>%
#   mutate_if(is.character, funs(factor(.) %>% as.integer)) %>%
#   group_by(SK_ID_BUREAU) %>%
#   summarise_all(fn)
# rm(bbalance); gc()

#---------------------------
cat("Preprocessing bureau.csv...\n")
bureau <- read_csv(file.path(data_dir, "bureau.csv"))

bureau <- bureau %>% #to do: validate if this approach gives gain
  mutate(CREDIT_ACTIVE_BOOL = if_else(CREDIT_ACTIVE == 'Active', 1, 0),
         CREDIT_CLOSED_BOOL = if_else(CREDIT_ACTIVE == 'Closed', 1, 0),
         CREDIT_SOLD_BOOL = if_else(CREDIT_ACTIVE %in% c('Sold','Bad debt'), 1, 0),
         CREDIT_UNTYPICAL_CURRENCY = if_else(CREDIT_CURRENCY != 'currency 1', 1, 0)) %>% #old approach could be better - check
  select(-c(CREDIT_ACTIVE, CREDIT_CURRENCY))

# table(sum_bureau_test$CREDIT_CURRENCY) #currently continue working on bureau data

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

agr_prev_cc_balance <- cc_balance %>% 
  select(-SK_ID_CURR) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_PREV) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))

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

agr_prev_payments <- payments %>% 
  select(-SK_ID_CURR) %>% 
  mutate(PAYMENT_PERC = AMT_PAYMENT / AMT_INSTALMENT,
         PAYMENT_DIFF = AMT_INSTALMENT - AMT_PAYMENT,
         DPD = DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT,
         DBD = DAYS_INSTALMENT - DAYS_ENTRY_PAYMENT,
         DPD = ifelse(DPD > 0, DPD, 0),
         DBD = ifelse(DBD > 0, DBD, 0)) %>% 
  group_by(SK_ID_PREV) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) 

rm(payments); gc()

#---------------------------
cat("Preprocessing POS_CASH_balance.csv...\n")
pc_balance <- read_csv(file.path(data_dir, "POS_CASH_balance.csv"))

sum_pc_balance <- pc_balance %>% 
  select(-SK_ID_PREV) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_CURR) %>% 
  summarise_all(fn)

agr_prev_pc_balance <- pc_balance %>% 
  select(-SK_ID_CURR) %>% 
  mutate_if(is.character, funs(factor(.) %>% as.integer)) %>% 
  group_by(SK_ID_PREV) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))

rm(pc_balance); gc()

#---------------------------
cat("Preprocessing previous_application.csv...\n")
prev <- read_csv(file.path(data_dir, "previous_application.csv"))

sum_prev <- prev %>%
  #left_join(agr_prev_cc_balance, by = "SK_ID_PREV") %>% #to do: check if gives gain
  #left_join(agr_prev_payments, by = "SK_ID_PREV") %>% #to do: check if gives gain
  #left_join(agr_prev_pc_balance, by = "SK_ID_PREV") %>% #to do: check if gives gain
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
rm(prev, agr_prev_cc_balance, agr_prev_payments, agr_prev_pc_balance); gc()

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
         PHONE_TO_EMPLOY_RATIO = DAYS_LAST_PHONE_CHANGE / DAYS_EMPLOYED,
         # add features from corr check loop
         AMT_DRAWINGS_OTHER_CURRENT_avg + DAYS_LAST_DUE_avg, # new corr = 0.086, diff = 0.059
         DAYS_LAST_DUE_avg + AMT_DRAWINGS_OTHER_CURRENT_sd, # new corr = 0.0856, diff = 0.0586
         CNT_DRAWINGS_OTHER_CURRENT_avg + CNT_INSTALMENT_MATURE_CUM_avg, # new corr = -0.084, diff = 0.0555
         AMT_PAYMENT_CURRENT_avg + DAYS_LAST_DUE_avg, # new corr = 0.08, diff = 0.05385
         
         AMT_CREDIT_SUM_max + RATE_INTEREST_PRIMARY_sd, # new corr = 0.31, check this carefully
         AMT_ANNUITY_min.y + RATE_INTEREST_PRIVILEGED_sd, # new corr = 0.13, check this carefully
         
         RATE_INTEREST_PRIMARY_NA = if_else(is.na(RATE_INTEREST_PRIMARY_avg) | is.nan(RATE_INTEREST_PRIMARY_avg), 0, 1), #added by intuition
         RATE_INTEREST_PRIVILEGED_NA = if_else(is.na(RATE_INTEREST_PRIVILEGED_avg) | is.nan(RATE_INTEREST_PRIVILEGED_avg), 0, 1) #added by intuition
         ) %>%
  select(-one_of(drop_cols))

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
cat("Create additional variables...\n")
tr_te = as.data.table(tr_te); gc()
# 
# # create vars for NaN observations
# # CONCLUSION: NAs already treated by na feature which is enough predicative
# #col_num = ncol(tr_te)
# #for (i in 3:col_num) {
# #  colname = names(tr_te)[i]
# #  tr_te[is.na(eval(as.name(colname)))|is.nan(eval(as.name(colname)))|is.null(eval(as.name(colname)))|is.infinite(eval(as.name(colname))), 
# #        paste0(colname, '_nulls') := 1]
# #  #tr_te[is.na(eval(as.name(paste0(colname, '_nulls')))), paste0(colname, '_nulls') := 0]
# #}
# 
# # outliers marking
# outliers_remove = function(dt,col_from,col_to) {
#   for (i in col_from:col_to) {
#     colname = names(dt)[i]
#     qnt <- quantile(dt[,eval(as.name(colname))], probs=c(.25, .75), na.rm = T)
#     H <- 1.5 * (qnt[2]-qnt[1])
#     dt[eval(as.name(colname)) < (qnt[1] - H), paste0(colname, '_outliers') := -1]
#     dt[eval(as.name(colname)) > (qnt[2] + H), paste0(colname, '_outliers') := 1]
#     #dt[is.na(eval(as.name(paste0(colname, '_outliers')))), paste0(colname, '_outliers') := 0]
#   }
#   return(as.data.table(dt))
# }
# 
# tr_te = outliers_remove(tr_te, col_from = 3, col_to = col_num)
# gc()

# apply random models
# IMPORTANT! It seems that this approach really works. Check file 2rand_cols...csv
vect_fla = c('y ~ CNT_PAYMENT_max + NAME_CONTRACT_STATUS_sum.y', 
             'y ~ REGION_RATING_CLIENT_W_CITY + AMT_APPLICATION_mean',
             'y ~ DPD_n_distinct + LIVE_REGION_NOT_WORK_REGION + NAME_EDUCATION_TYPE',
             'y ~ DAYS_INSTALMENT_min + NAME_INCOME_TYPE + CODE_REJECT_REASON_min',
             'y ~ FLAG_DOCUMENT_7 + DAYS_ENTRY_PAYMENT_sd + FLAG_DOCUMENT_3',
             'y ~ CREDIT_ACTIVE_BOOL_sum + DAYS_CREDIT_mean'
            )
list_params = list(c('CNT_PAYMENT_max', 'NAME_CONTRACT_STATUS_sum.y'), 
                   c('REGION_RATING_CLIENT_W_CITY', 'AMT_APPLICATION_mean'),
                   c('DPD_n_distinct', 'LIVE_REGION_NOT_WORK_REGION', 'NAME_EDUCATION_TYPE'),
                   c('newcol_DAYS_INSTALMENT_min', 'NAME_INCOME_TYPE', 'CODE_REJECT_REASON_min'),
                   c('newcol_FLAG_DOCUMENT_7', 'DAYS_ENTRY_PAYMENT_sd', 'FLAG_DOCUMENT_3'),
                   c('newcol_CREDIT_ACTIVE_BOOL_sum', 'DAYS_CREDIT_mean')
                   )
for (i in 1:length(vect_fla)) {
  fla = vect_fla[i]
  params = list_params[[i]]
  # apply model
  dt_mod = as.data.table(cbind(y, tr_te[1:length(y), params, with = FALSE]))
  mod = lm(data=dt_mod, formula=as.formula(fla)) #to do: add random model here
  tr_te[, paste0('newcol','_', sub('y ~ ', '', fla)) := predict(mod, tr_te[, params, with = FALSE])]
}
rm(fla, params, vect_fla, list_params, dt_mod, mod); gc()

# create matrix from dt without RAM issues
# original article with the method could be found here:
# https://medium.com/data-design/loading-super-large-sparse-data-when-you-cant-load-as-sparse-in-r-2a9f0ad927b2
temp_names = colnames(tr_te)
write_csv(as.data.frame(temp_names), path = paste0(data_dir, "//Calculation//input_colnames.csv"), col_names = TRUE)
write_csv(tr_te, path = paste0(data_dir, "//Calculation//input_bigmatrix.csv"), col_names = TRUE)

temp_names = read.csv(file = paste0(data_dir, "//Calculation//input_colnames.csv"))
rm(tr_te); gc()

n = 10 #set number of parts to split
for (i in 1:n) {
  cat("Loading ", i, "th part.\n", sep = "")
  train_data_temp <- fread(input = paste0(data_dir, "//Calculation//input_bigmatrix.csv"),
                           select = (1+round((i-1)*nrow(temp_names)/n, 0)):round(i*nrow(temp_names)/n, 0),
                           header = TRUE,
                           sep = ",",
                           stringsAsFactors = FALSE,
                           colClasses = rep("numeric", nrow(temp_names)),
                           data.table = TRUE)

  gc(verbose = FALSE)
  if (i > 1) {
    cat("Coercing to matrix.\n", sep = "")
    tr_te_temp <- as.matrix(train_data_temp)
    rm(train_data_temp)
    gc(verbose = FALSE)
    cat("Coercing into dgCMatrix with NA as blank.\n", sep = "")
    tr_te_temp <- dropNA(tr_te_temp)
    gc(verbose = FALSE)
    cat("Column binding the full matrix with the newly created matrix.\n", sep = "")
    tr_te <- cbind(tr_te, tr_te_temp)
    rm(tr_te_temp)
    gc(verbose = FALSE)
  } else {
    cat("Coercing to matrix.\n", sep = "")
    tr_te_temp <- as.matrix(train_data_temp)
    rm(train_data_temp)
    gc(verbose = FALSE)
    cat("Coercing into dgCMatrix with NA as blank.\n", sep = "")
    tr_te <- dropNA(tr_te_temp)
    gc(verbose = FALSE)
  }
}
gc()

#---------------------------
cat("Save & load long dataset...\n")
saveRDS(tr_te, file = paste0(data_dir, "//Calculation//input_bigmatrix_long.rds"))
#readRDS(paste0(data_dir, "//Calculation//input_bigmatrix_long.rds"))

#---------------------------
cat("Preparing data...\n")
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

# parameters taken from https://www.kaggle.com/dromosys/fork-of-fork-lightgbm-with-simple-features-cee847/code
#lgb.grid = list(objective = "binary",
#                metric = "auc",
#                #n_estimators=10000,
#                learning_rate=0.02, # in source - 0.02
#                num_leaves=32,
#                colsample_bytree=0.9497036,
#                subsample=0.8715623,
#                max_depth=8,
#                reg_alpha=0.04,
#                reg_lambda=0.073,
#                min_split_gain=0.0222415,
#                min_child_weight=40,
#                is_unbalance = TRUE)

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
                feature_fraction = 0.8, #originaly 0.5
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

#---------------------------
read_csv(file.path(data_dir, "//Models//sample_submission.csv")) %>%  
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
         TARGET = predict(m_gbm_cv, dtest)) %>%
  write_csv(file.path(data_dir, paste0("//Models//max_cols_", round(m_gbm_cv$best_score, 5), ".csv")))

# write file with characteristic parameters
write_csv(lgb.importance(m_gbm_cv, percentage = TRUE), file.path(data_dir, paste0("//Results//max_cols_", round(m_gbm_cv$best_score, 5), "_importance.csv")))
