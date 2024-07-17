library(nycflights13)
data("flights")
library(mice)
library(rpart)
library(rpart.plot)
library(caret)
set.seed(123)

flights$delayed = ifelse(flights$arr_delay > 15, "Yes", "No")
dataset = na.omit(flights[, c("dep_delay", "air_time", "distance", "delayed")])

#Selektovanje 200
selected_indices = sample(1:nrow(dataset), 200)
dataset = dataset[selected_indices, ]

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

model_imputed_data_srs = rpart(delayed ~ ., data = complete_train_srs, method = "class")
predictions_imputed_data_srs = predict(model_imputed_data_srs, newdata = complete_test_srs, type = "class")
cm_imputed_data_srs = confusionMatrix(table(predictions_imputed_data_srs, complete_test_srs$delayed))

model_imputed_data_ss = rpart(delayed ~ ., data = complete_train_ss, method = "class")
predictions_imputed_data_ss = predict(model_imputed_data_ss, newdata = complete_test_ss, type = "class")
cm_imputed_data_ss = confusionMatrix(table(predictions_imputed_data_ss, complete_test_ss$delayed))

#Model nad podacima nakon izbacivanja redova sa nepostojecim podacima
data_clean = na.omit(missing_dataset)

train_index_srs = sample(1:nrow(data_clean), 0.7 * nrow(data_clean))
train_missing_srs = data_clean[train_index_srs, ]
test_missing_srs = data_clean[-train_index_srs, ]

train_index_ss = createDataPartition(data_clean$delayed, p = 0.7, list = FALSE)
train_missing_ss = data_clean[train_index_ss, ]
test_missing_ss = data_clean[-train_index_ss, ]

model_missing_data_srs = rpart(delayed ~ ., data = train_missing_srs, method = "class")
predictions_missing_data_srs = predict(model_missing_data_srs, newdata = test_missing_srs, type = "class")
cm_missing_data_srs = confusionMatrix(table(predictions_missing_data_srs, test_missing_srs$delayed))

model_missing_data_ss = rpart(delayed ~ ., data = train_missing_ss, method = "class")
predictions_missing_data_ss = predict(model_missing_data_ss, newdata = test_missing_ss, type = "class")
cm_missing_data_ss = confusionMatrix(table(predictions_missing_data_ss, test_missing_ss$delayed))

