using DataFrames, DataFramesMeta
using ScikitLearn.GridSearch: GridSearchCV

data = readtable("Predictions.csv")
delete!(data, [:x])
data[:Objective_1_Binary_Error] = [data[:Absolute_Error][i] <= 2.0?true:false for i in 1:nrow(data)]
data[:Alarm_Actual] = [data[:Actual_Value][i]>=90.0?true:false for i in 1:nrow(data)]
data[:Alarm_Predicted] = [data[:Predicted_Value][i]>=90.0?true:false for i in 1:nrow(data)]
writetable("Predictions.csv", data)
