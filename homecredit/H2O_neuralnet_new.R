library(smbinning)
library(magrittr)
library(moments)
library(data.table)
library(recommenderlab)
library(tidyverse)
library(h2o)

install.packages('tidyverse')

set.seed(0)

#---------------------------
cat("Loading data...\n")

data_dir = "C:\\Users\\Eovil\\Desktop\\Kaggle_Homecredit competition"

tr <- read_csv(file.path(data_dir, "application_train.csv"))
te <- read_csv(file.path(data_dir, "application_test.csv"))

#---------------------------
cat("Preprocessing...\n")
fn <- funs(mean, sd, min, max, sum, n_distinct, .args = list(na.rm = TRUE))

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
  
  h2o.init(nthreads = -1)
  #dtest <- lgb.Dataset(data = tr_te[-tri, ]) #it seems that this approach do not work for LightGBM. Raise questions for this.
  dtest <- as.h2o(tr_te[-tri, ])
  tr_te <- tr_te[tri, ]
  
  #-----------------------------
  cat("Create validation sample...\n")
  df_test = data.table(tr_te[,c('EXT_SOURCE_1', 'EXT_SOURCE_2', 'EXT_SOURCE_3')], y, id = 1:nrow(tr_te))
  
  df_bin = smbinning(df_test, y = 'y', x = 'EXT_SOURCE_1', p = .1)
  df_test = smbinning.gen(df_test, df_bin, chrname = 'EXT_SOURCE_1_BIN')
  
  df_bin = smbinning(df_test, y = 'y', x = 'EXT_SOURCE_2', p = .1)
  df_test = smbinning.gen(df_test, df_bin, chrname = 'EXT_SOURCE_2_BIN')
  
  df_bin = smbinning(df_test, y = 'y', x = 'EXT_SOURCE_3', p = .1)
  df_test = smbinning.gen(df_test, df_bin, chrname = 'EXT_SOURCE_3_BIN')
  
  df_test[is.na(EXT_SOURCE_2), 
          `:=` (EXT_SOURCE_1_BIN = 'EXT_SOURCE_2_NA', 
                EXT_SOURCE_2_BIN = 'EXT_SOURCE_2_NA', 
                EXT_SOURCE_3_BIN = 'EXT_SOURCE_2_NA')]
  df_test = df_test[,.(EXT_SOURCE_1_BIN, EXT_SOURCE_2_BIN, EXT_SOURCE_3_BIN, y, id)]
  
  df_fin <- df_test %>%
    group_by(EXT_SOURCE_1_BIN, EXT_SOURCE_2_BIN, EXT_SOURCE_3_BIN, y) %>%
    sample_frac(., 0.9)
  
  tri = df_fin$id
  #tri <- caret::createDataPartition(as.factor(y), p = 0.9, list = F) %>% c()

  tr_te = cbind(tr_te, y) #h20 needs facor predictor to be a part of a dataset
  
  dtrain = as.h2o(tr_te[tri, ])
  dval = as.h2o(tr_te[-tri, ])
  cols <- colnames(tr_te)
  
  rm(tr_te, tri, df_bin, df_test); gc()
  
  #---------------------------
  cat("Training model...\n")

  # This step takes a few seconds bc we have to download the data from the internet...
  # train_file <- "https://h2o-public-test-data.s3.amazonaws.com/bigdata/laptop/mnist/train.csv.gz"
  # test_file <- "https://h2o-public-test-data.s3.amazonaws.com/bigdata/laptop/mnist/test.csv.gz"
  # train <- h2o.importFile(train_file)
  # test <- h2o.importFile(test_file)
  
  y <- "y"  #response column: digits 0-9
  x <- cols  #vector of predictor column names
  
  # Since the response is encoded as integers, we need to tell H2O that
  # the response is in fact a categorical/factor column.  Otherwise, it 
  # will train a regression model instead of multiclass classification.
  dtrain[,y] <- as.factor(dtrain[,y])
  dval[,y] <- as.factor(dval[,y])
  nrow(dtrain)
  # initial model. This one works
   dl_fit2 <- h2o.deeplearning(x = x,
                               y = y,
                               training_frame = dtrain,
                               validation_frame = dval, #this is new row
                               model_id = "dl_fit2",
                               epochs = 10, #1000,
                               activation = "TanhWithDropout", #c("Tanh", "TanhWithDropout", "Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout"), 
                               hidden = c(100, 100),
                               hidden_dropout_ratios = c(0.05, 0.05), 
                               score_training_samples = 0, #0 for all #this is new row
                               score_validation_samples = 0, #this is new row
                               stopping_rounds = 20, # 0 = disable early stopping #metric for stoping rounds evaluated after scoring iterations, which connected to iterations of training
                               stopping_metric = "AUC", #c("AUTO", "deviance", "logloss", "MSE", "RMSE", "MAE", "RMSLE", "AUC", "lift_top_group", "misclassification", "mean_per_class_error"), 
                               #seed = 1,
                               verbose = TRUE) #this is new row
  
   h2o.auc(dl_fit2)
   summary(dl_fit2)
   View(h2o.varimp(dl_fit2))
   View(h2o.gainsLift(dl_fit2, valid= T))
   
  nn_model <- h2o.deeplearning( x = x, 
                                y = y, 
                                training_frame = dtrain,
                                validation_frame = dval, 
                                #distribution = "multinomial", 
                                #activation = "RectifierWithDropout", 
                                #hidden = c(32,32,32),
                                #input_dropout_ratio = 0.2, 
                                #sparse = TRUE, 
                                #l1 = 1e-5, 
                                nfolds = 5,
                                keep_cross_validation_predictions = FALSE,
                                keep_cross_validation_fold_assignment = FALSE, 
                                fold_assignment = "Stratified", #c("AUTO", "Random", "Modulo", "Stratified")
                                #fold_column = 'y', #set fold column as target variable
                                score_each_iteration = TRUE,
                                #weights_column = NULL,
                                #offset_column = NULL,
                                #balance_classes = FALSE,
                                #class_sampling_factors = NULL,
                                #max_after_balance_size = 5,
                                #max_hit_ratio_k = 0,
                                #checkpoint = NULL,
                                #pretrained_autoencoder = NULL,
                                #overwrite_with_best_model = TRUE,
                                #use_all_factor_levels = TRUE,
                                #standardize = TRUE,
                                activation = "TanhWithDropout", #c("Tanh", "TanhWithDropout", "Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout"), 
                                hidden = c(20, 20, 20), 
                                epochs = 2000, 
                                #train_samples_per_iteration = -2,
                                #target_ratio_comm_to_comp = 0.05,
                                #adaptive_rate = TRUE,
                                #rho = 0.99,
                                #epsilon = 1e-08, 
                                rate = 0.005,
                                rate_annealing = 1e-06,
                                rate_decay = 1,
                                nesterov_accelerated_gradient = TRUE,
                                #input_dropout_ratio = 0.05,
                                hidden_dropout_ratios = c(0.05, 0.05, 0.05), 
                                l1 = 0, 
                                l2 = 0,
                                max_w2 = 3.4028235e+38, 
                                initial_weight_distribution = "UniformAdaptive", #c("UniformAdaptive", "Uniform", "Normal"), 
                                initial_weight_scale = 1, 
                                loss = "Automatic", #c("Automatic", "CrossEntropy", "Quadratic", "Huber", "Absolute", "Quantile"),
                                distribution = "AUTO", #c("AUTO", "bernoulli", "multinomial", "gaussian", "poisson", "gamma", "tweedie", "laplace", "quantile", "huber"), 
                                quantile_alpha = 0.5,
                                tweedie_power = 1.5,
                                huber_alpha = 0.9, 
                                score_interval = 5, 
                                score_training_samples = 10000, #0 for all
                                score_validation_samples = 0,
                                score_duty_cycle = 0.1,
                                classification_stop = 0, 
                                regression_stop = 1e-06, 
                                stopping_rounds = 200,
                                stopping_metric = "AUC", #c("AUTO", "deviance", "logloss", "MSE", "RMSE", "MAE", "RMSLE", "AUC", "lift_top_group", "misclassification", "mean_per_class_error"), 
                                stopping_tolerance = 0, 
                                #max_runtime_secs = 0,
                                #score_validation_sampling = c("Uniform", "Stratified"),
                                diagnostics = TRUE, 
                                fast_mode = TRUE, 
                                force_load_balance = TRUE,
                                variable_importances = TRUE, 
                                replicate_training_data = TRUE,
                                single_node_mode = FALSE, 
                                shuffle_training_data = FALSE,
                                #missing_values_handling = "MeanImputation", #c("MeanImputation", "Skip"), !!! All rows have missings !!!
                                quiet_mode = FALSE,
                                autoencoder = FALSE, 
                                sparse = FALSE, 
                                col_major = FALSE,
                                average_activation = 0, 
                                sparsity_beta = 0,
                                #max_categorical_features = 2147483647, 
                                reproducible = FALSE,
                                export_weights_and_biases = FALSE, 
                                mini_batch_size = 1,
                                categorical_encoding = "AUTO", #c("AUTO", "Enum", "OneHotInternal", "OneHotExplicit", "Binary", "Eigen", "LabelEncoder", "SortByResponse", "EnumLimited"),
                                elastic_averaging = FALSE, 
                                elastic_averaging_moving_rate = 0.9,
                                elastic_averaging_regularization = 0.001, 
                                verbose = TRUE)

  # ? h2o.deeplearning
 
  lgbm_pred_list[[i]] = rank(as.vector(predict(dl_fit2, dtest)), ties.method = 'first') # predict.H2OModel ?
  lgbm_feat = rbindlist(list(lgbm_feat, lgb.importance(m_gbm_cv, percentage = TRUE)))
  lgbm_score[i] = m_gbm_cv$best_score
}

h2o.performance(dl_fit2) # training metrics
h2o.auc(dl_fit2)
h2o.auc(dl_fit2, train = TRUE, valid = TRUE)
#h2o.performance(dl_fit2, valid = TRUE) # validation metrics 9 10 # Get MSE only
#h2o.mse(dl_fit2, valid = TRUE) # Cross-validated MSE
#h2o.mse(dl_fit2, xval = TRUE)

# print the auc for your model
h2o.auc(nn_model)
h2o.auc(nn_model, train = TRUE, valid = TRUE)
? h2o.auc

avg_lgbm = Reduce(`+`, lgbm_pred_list)
avg_lgbm = avg_lgbm/i
avg_lgbm = avg_lgbm/max(avg_lgbm)

lgbm_feat_avg = lgbm_feat %>% group_by(Feature) %>%
  summarize(gain_avg = mean(Gain),
            cover_avg = mean(Cover),
            frequency_avg = mean(Frequency))
lgbm_score_avg = mean(lgbm_score)

#---------------------------
read_csv(file.path(data_dir, "//Models//sample_submission.csv")) %>%  
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR),
         TARGET = avg_lgbm) %>%
  write_csv(file.path(data_dir, paste0("//Models//new_mod_H2O_", round(lgbm_score_avg, 5), ".csv")))

# write file with characteristic parameters
write_csv(lgbm_feat_avg, file.path(data_dir, paste0("//Results//new_mod_H2O_", round(lgbm_score_avg, 5), "_importance.csv")))
