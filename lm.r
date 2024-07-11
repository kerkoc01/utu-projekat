library(nycflights13)
data("flights")
library(mice)
library(caret)
set.seed(123)

flights$delayed = ifelse(flights$arr_delay > 15, 1, 0)
dataset = na.omit(flights[, c("dep_delay", "air_time", "distance", "delayed")])

#Izbacivanje
num_missing = round(0.20 * nrow(dataset))
missing_indices = sample(1:nrow(dataset), num_missing)
missing_dataset = dataset
missing_dataset$dep_delay[missing_indices] = NA

train_index_srs = sample(1:nrow(missing_dataset), 0.7 * nrow(missing_dataset))
train_data_srs = missing_dataset[train_index_srs, ]
test_data_srs = missing_dataset[-train_index_srs, ]

train_index_ss = createDataPartition(missing_dataset$delayed, p = 0.7, list = FALSE)
train_data_ss = missing_dataset[train_index_ss, ]
test_data_ss = missing_dataset[-train_index_ss, ]

#Imputacija
imputed_train_srs = mice(train_data_srs, method = 'pmm', maxit = 50)
complete_train_srs = complete(imputed_train_srs)

imputed_test_srs = mice::mice(test_data_srs, method = 'pmm', maxit = 50)
complete_test_srs = complete(imputed_test_srs)

imputed_train_ss = mice(train_data_ss, method = 'pmm', maxit = 50)
complete_train_ss = complete(imputed_train_ss)

imputed_test_ss = mice::mice(test_data_ss, method = 'pmm', maxit = 50)
complete_test_ss = complete(imputed_test_ss)

model_imputed_data_srs = lm(delayed ~ air_time + distance + dep_delay, data = complete_train_srs)
predictions_imputed_data_srs = ifelse(predict(model_imputed_data_srs, newdata = complete_test_srs) >= 0.5, 1, 0)
cm_imputed_data_srs = confusionMatrix(table(predictions_imputed_data_srs, complete_test_srs$delayed))

model_imputed_data_ss = lm(delayed ~ air_time + distance + dep_delay, data = complete_train_ss)
predictions_imputed_data_ss = ifelse(predict(model_imputed_data_ss, newdata = complete_test_ss) >= 0.5, 1, 0)
cm_imputed_data_ss = confusionMatrix(table(predictions_imputed_data_ss, complete_test_ss$delayed))

#Model nad podacima nakon izbacivanja redova sa nepostojecim podacima
data_clean = na.omit(missing_dataset)

train_index_srs = sample(1:nrow(data_clean), 0.7 * nrow(data_clean))
train_missing_srs = data_clean[train_index_srs, ]
test_missing_srs = data_clean[-train_index_srs, ]

train_index_ss = createDataPartition(data_clean$delayed, p = 0.7, list = FALSE)
train_missing_ss = data_clean[train_index_ss, ]
test_missing_ss = data_clean[-train_index_ss, ]

model_missing_data_srs = lm(delayed ~ air_time + distance + dep_delay, data = train_missing_srs)
predictions_missing_data_srs = ifelse(predict(model_missing_data_srs, newdata = test_missing_srs) >= 0.5, 1, 0)
cm_missing_data_srs = confusionMatrix(table(predictions_missing_data_srs, test_missing_srs$delayed))

model_missing_data_ss = lm(delayed ~ air_time + distance + dep_delay, data = train_missing_ss)
predictions_missing_data_ss = ifelse(predict(model_missing_data_ss, newdata = test_missing_ss) >= 0.5, 1, 0)
cm_missing_data_ss = confusionMatrix(table(predictions_missing_data_ss, test_missing_ss$delayed))

