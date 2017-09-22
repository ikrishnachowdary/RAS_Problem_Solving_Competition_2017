#################################################
# Linear Regression                             #
#################################################

pretraining_data = read.csv(file="..//data//pretraining_data_81083.csv")

pretraining_data$eqp_axle_nbr=as.factor(pretraining_data$eqp_axle_nbr)
pretraining_data$EQP_GRP=as.factor(pretraining_data$EQP_GRP)

training_data = pretraining_data

y_columns = c("WHL_PEAK_KIPS")
x_columns = setdiff(names(training_data), c("X", "WHL_AVG_KIPS", "WHL_PEAK_KIPS", "WHL_DYN_KIPS", "WHL_DYN_RATIO", "EQP_GRS_TONS","RPR_WHY_CD", "EQP_GRP"))

formula = reformulate(response = y_columns[1], termlabels = x_columns)

model = lm(formula = formula, data = training_data)
write.csv(file="Statistically_Significant_Variables_Linear_Regression.csv", sort(summary(model)$coefficients[abs(summary(model)$coefficients[, 3]) > 2.58, 3]))

#################################################
# XGBoost                                       #
#################################################

require(xgboost)

#overall_training_data = read.csv(file="..//data//cleaned_training_data.csv")
#inds = which(overall_training_data$KIPDAYS==-999)
#overall_training_data$KIPDAYS[inds] = 0
#write.csv(file="../data/cleaned_training_data.csv", overall_training_data)

#trn_inds = read.table("../data/pre_training_inds.txt", header=FALSE)$V1
#pretraining_data = overall_training_data[trn_inds, ]
#pretraining_data = subset(pretraining_data, select = -c(X) )
#write.csv(file="../data/cleaned_pretraining_data.csv", pretraining_data)

#################################################
# Tuning Parameters                             #
#################################################

#################################################
## XGBoost                                     ##
## Parameters to be tuned:                     ##
## Booster = { Linear and Tree }               ##
###                                           ###
### Booster = Linear                          ###
### Lambda , Alpha                            ###
###                                           ###
### Booster = Tree                            ###
### Learning Rate , Max Depth,                ###
### Min Child Weight, Gamma, Subsample,       ###
### Colsample by tree, Colsample by level     ###
### Alpha                                     ###
#################################################

tune_parameters_of_xgboost <- function(training_data, summarized_results_location) {
	
	#y_columns = c("WHL_PEAK_KIPS", "WHL_AVG_KIPS", "WHL_DYN_KIPS", "WHL_DYN_RATIO")
	y_columns = c("WHL_PEAK_KIPS")
	x_columns = setdiff(names(training_data), c("X", "WHL_AVG_KIPS", "WHL_PEAK_KIPS", "WHL_DYN_KIPS", "WHL_DYN_RATIO", "EQP_GRS_TONS","RPR_WHY_CD"))
	
	objectives = c("reg:linear", "count:poisson")
	error_metrics = c("mae")
	rounds = c(5, 10, 15)
	bsts = c("gblinear", "gbtree")
	
	lm_lambda = c(0, 0.5, 1)
	lm_alpha = c(0, 0.5, 1)
	lm_lambda_bias = c(0, 0.5, 1)
	
	tm_eta = c(0.5, 0.7, 0.9, 1.0)
	tm_max_depth = c(4, 6, 8)
	tm_subsample = c(0.3, 0.5, 0.7)
  	tm_colsample_bytree = c(0.3, 0.5, 0.7)
  	tm_colsample_bylevel = c(0.3, 0.5, 0.7)
  	
  	num = 15000
  	
  	y_ = character(num)
  	obj_ = character(num)
  	error_metric_ = character(num)
  	rounds_ = numeric(num)
  	bst_ = character(num)
  	
  	lambda_ = numeric(num)
  	alpha_ = numeric(num)
  	lambda_bias_ = numeric(num)
  	
  	eta_ = numeric(num)
  	max_depth_ = numeric(num)
  	subsample_ = numeric(num)
  	colsample_bytree_ = numeric(num)
  	colsample_bylevel_ = numeric(num)
    test_error_ = numeric(num)
  	
	count = 1
	
	for (i in 1:length(y_columns)){
		y_column = y_columns[i]
		for (obj in objectives){
			for (error_metric in error_metrics){
				for (num_round in rounds){
					for (bst in bsts){
						if (bst == "gblinear"){
							for (lambda in lm_lambda){
								for (alpha in lm_alpha){
									for (lambda_bias in lm_lambda_bias){
										model = xgb.cv(data = data.matrix(training_data[x_columns]), label = data.matrix(training_data[y_column]), objective=obj, eval_metric=error_metric, booster=bst, nrounds = num_round, lambda=lambda, alpha=alpha, lambda_bias=lambda_bias, nfold=5, nthread=4)										
										
										y_[count] = y_column
										obj_[count] = obj
										error_metric_[count] = error_metric
										rounds_[count] = rounds
										bst_[count] = bst
										lambda_[count] = lambda
										alpha_[count] = alpha
										lambda_bias_[count] = lambda_bias
										eta_[count] = 0
										max_depth_[count] = 0
										subsample_[count] = 0
										colsample_bytree_[count] = 0
										colsample_bylevel_[count] = 0
										test_error_[count] = max(model$evaluation_log$test_mae_mean)
										
										summarized_results = data.frame(Y=y_, Objective=obj_, Error_Metric=error_metric_, Rounds=rounds, Booster=bst_, LM_Lambda=lambda_, LM_Alpha=alpha_, LM_Lambda_Bias=lambda_bias_, Eta=eta_, Max_Depth=max_depth_, Subsample=subsample_, Colsample_bytree=colsample_bytree_, Colsample_bylevel=colsample_bylevel_, Error=test_error_)
										
										write.csv(summarized_results[c(1:count), ], file = summarized_results_location, row.names = FALSE)
										count = count + 1
										
									}
								}
							}						
						}else{
						  	
							for (eta in tm_eta){
								for (max_depth in tm_max_depth){
									for (subsample in tm_subsample){
										for (colsample_bytree in tm_colsample_bytree){
											for (colsample_bylevel in tm_colsample_bylevel){
												model = xgb.cv(data = data.matrix(training_data[x_columns]), label = data.matrix(training_data[y_column]), objective=obj, eval_metric=error_metric, booster=bst, nrounds = num_round, eta=eta, max_depth=max_depth, subsample=subsample, colsample_bytree=colsample_bytree, colsample_bylevel=colsample_bylevel, nfold=5, nthread=4)										
										
												y_[count] = y_column
												obj_[count] = obj
												error_metric_[count] = error_metric
												rounds_[count] = rounds
												bst_[count] = bst
												lambda_[count] = 0
												alpha_[count] = 0
												lambda_bias_[count] = 0
												eta_[count] = eta
												max_depth_[count] = max_depth
												subsample_[count] = subsample
												colsample_bytree_[count] = colsample_bytree
												colsample_bylevel_[count] = colsample_bylevel
												test_error_[count] = max(model$evaluation_log$test_mae_mean)
										
												summarized_results = data.frame(Y=y_, Objective=obj_, Error_Metric=error_metric_, Rounds=rounds, Booster=bst_, LM_Lambda=lambda_, LM_Alpha=alpha_, LM_Lambda_Bias=lambda_bias_, Eta=eta_, Max_Depth=max_depth_, Subsample=subsample_, Colsample_bytree=colsample_bytree_, Colsample_bylevel=colsample_bylevel_, Error=test_error_)
										
												write.csv(summarized_results[c(1:count), ], file = summarized_results_location, row.names = FALSE)
												count = count + 1
											}
										}										
									}
								}
							}										
						}
					}	
				}	
			}
		}		
	}
}

pretraining_data = read.csv(file="..//data//pretraining_data_811.csv")

pretraining_data$eqp_axle_nbr=as.factor(pretraining_data$eqp_axle_nbr)
pretraining_data$EQP_GRP=as.factor(pretraining_data$EQP_GRP)

summarized_results_location = "..//data//summarized_results_1.csv"
tune_parameters_of_xgboost(pretraining_data, summarized_results_location)

#################################################
# Final Training                                #
#################################################

training_data = read.csv(file="..//data//cleaned_training_data.csv")
training_data$eqp_axle_nbr=as.factor(training_data$eqp_axle_nbr)

y_columns = c("WHL_PEAK_KIPS")
x_columns = setdiff(names(training_data), c("X", "WHL_AVG_KIPS", "WHL_PEAK_KIPS", "WHL_DYN_KIPS", "WHL_DYN_RATIO", "EQP_GRS_TONS","RPR_WHY_CD", "EQP_GRP"))

bst <- xgboost(data = data.matrix(training_data[x_columns]), label = data.matrix(training_data[y_columns[1]]), objective="reg:linear", eval_metric="mae", booster="gbtree", nrounds = 25, eta=1, max_depth=6, subsample=1.0, colsample_bytree=1.0, colsample_bylevel=1.0, nthread=4)

testing_data = read.csv(file="..//data//cleaned_testing_data.csv")
testing_data$eqp_axle_nbr=as.factor(testing_data$eqp_axle_nbr)

predicted_peak_kips = predict(bst, data.matrix(testing_data[x_columns]))
error = abs(testing_data$WHL_PEAK_KIPS - predicted_peak_kips)
info = data.frame( Actual_Value = testing_data$WHL_PEAK_KIPS, Predicted_Value = predicted_peak_kips, Absolute_Error = error)
write.csv(info, file="Predictions.csv")
