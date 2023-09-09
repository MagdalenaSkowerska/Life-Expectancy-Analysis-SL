setwd("~/Documents/university/Statistical learning")

library(dplyr)
library(tidyr)
library(outliers)
library(MASS)
library(ggpubr)
library(corrplot)
library(ggplot2)
library(car)
library(rpart.plot)
library(party)
library("partykit")
library(rpart)
library(leaps)
library(randomForest)
require(randomForest)
require(gbm)
library(cluster)
library(knitr)

#read data
df_life_expectancy = read.csv("life_expectancy.csv")
df_diet= read.csv("diet.csv")
df_happiness=read.csv("world-happiness-report.csv")
df_alcohol=read.csv("AlcoholSubstanceAbuse.csv")
df_uhc=read.csv("uhcCoverage.csv")
df_tabacco=read.csv("tobaccoAge15.csv")
df_pollution=read.csv("share-deaths-air-pollution.csv")


#Select year=2015
df_life_expectancy=df_life_expectancy[df_life_expectancy$Year=="2015",]
df_happiness=df_happiness[df_happiness$year=="2015",]
df_uhc=df_uhc[df_uhc$Period=="2015",]

#Select year=2015 and the indicators values both sexes
df_alcohol=df_alcohol[df_alcohol$Period=="2015" & df_alcohol$Dim1=="Both sexes",]
df_tabacco=df_tabacco[df_tabacco$Period=="2015" & df_tabacco$Dim1=="Both sexes",]
df_pollution=df_pollution[df_pollution$Year=="2015",]


#Select the appropriate columns
subset_fields <- c("Country", "Schooling", "Life.expectancy")
df_life_expectancy <- df_life_expectancy[,subset_fields]

subset_fields <- c("Country.name", "Life.Ladder", "Perceptions.of.corruption", "Log.GDP.per.capita")
df_happiness <- df_happiness[, subset_fields]

subset_fields <- c("Location", "First.Tooltip")
df_alcohol <- df_alcohol[,subset_fields]

subset_fields <- c("Location", "First.Tooltip")
df_uhc <- df_uhc[, subset_fields]

subset_fields <- c("Location", "First.Tooltip")
df_tabacco <- df_tabacco[, subset_fields]

subset_fields <- c("Area", "Item", "Value")
df_diet <- df_diet[, subset_fields]

subset_fields <- c("Entity", "Deaths...Cause..All.causes...Risk..Air.pollution...Sex..Both...Age..Age.standardized..Percent.")
df_pollution <- df_pollution[, subset_fields]


#rename the columns
names(df_diet)[names(df_diet) == "Area"] <- "Country"
names(df_diet)[names(df_diet) == "Item"] <- "Diet.Item"
names(df_happiness)[names(df_happiness) == "Country.name"] <- "Country"
names(df_alcohol)[names(df_alcohol) == "Location"] <- "Country"
names(df_alcohol)[names(df_alcohol) == "First.Tooltip"] <- "Alcohol.Indicator"
names(df_uhc)[names(df_uhc) == "Location"] <- "Country"
names(df_uhc)[names(df_uhc) == "First.Tooltip"] <- "UHC.Indicator"
names(df_tabacco)[names(df_tabacco) == "Location"] <- "Country"
names(df_tabacco)[names(df_tabacco) == "First.Tooltip"] <- "Tabacco.Indicator"
names(df_pollution)[names(df_pollution) == "Entity"] <- "Country"
names(df_pollution)[names(df_pollution) == "Deaths...Cause..All.causes...Risk..Air.pollution...Sex..Both...Age..Age.standardized..Percent."] <- "Pollution death rate"
names(df_pollution)[names(df_pollution) == "Pollution death rate"] <- "Pollution.death.rate"


#transform diet dataframe row values of a column into new columns 
df_diet <- pivot_wider(df_diet, id_cols = Country, names_from = "Diet.Item", values_from = Value)

#rename the columns
names(df_diet)[names(df_diet) == "Meat, Other"] <- "Meat.Indicator"
names(df_diet)[names(df_diet) == "Fruits, other"] <- "Fruits.Indicator"
names(df_diet)[names(df_diet) == "Vegetables, other"] <- "Vegetables.Indicator"
names(df_diet)[names(df_diet) == "Marine Fish, Other"] <- "Fish.Indicator"
names(df_diet)[names(df_diet) == "Sugar (Raw Equivalent)"] <- "Sugar.Indicator"

#Reorder the columns so that the Life.expectancy is the first one
df_life_expectancy <- df_life_expectancy[,c("Life.expectancy", "Schooling", "Country")]

#join datasets by country
final_df <- df_life_expectancy %>% 
  inner_join(df_diet, by = "Country") %>%
  inner_join(df_happiness, by = "Country") %>%
  inner_join(df_alcohol, by = "Country") %>%
  inner_join(df_uhc, by = "Country") %>%
  inner_join(df_tabacco, by = "Country") %>%
  inner_join(df_pollution, by = "Country")

############################################
#Data Cleaning

#Count missing values in each column
missing_count <- colSums(is.na(final_df))

#Total count of missing values in the entire data frame
total_missing_count <- sum(missing_count)

print(total_missing_count)
print(missing_count)

#Remove rows with missing values
final_df <- na.omit(final_df)

# Create a dataframe without the country column
final_df_no_country=final_df %>% select_if(is.numeric)

#Normality assumption check
shapiro.test(final_df_no_country$Life.expectancy)

ggdensity(final_df_no_country, x = "Life.expectancy", fill = "lightgray", title = "Life.expectancy") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

#Log transform the response variable
logexpactancy=log(final_df_no_country$Life.expectancy)
shapiro.test(logexpactancy)

#Box-cox transformation of the response variable
response_variable<-final_df_no_country$Life.expectancy
boxcox_output <- boxcox(lm(response_variable ~ 1))
#Extract the optimal lambda value
lambda <- boxcox_output$x[which.max(boxcox_output$y)]
#Apply the Box-Cox transformation using the optimal lambda
transformed_response <- if (abs(lambda) < 1e-6) log(response_variable) else (response_variable^lambda - 1) / lambda
shapiro.test(transformed_response)

# Normality assumption check for the residuals
lm_model <- lm(Life.expectancy ~., data = final_df_no_country)
residuals <- residuals(lm_model)
shapiro_test_result <- shapiro.test(residuals)
print(shapiro_test_result)

# Linearity assumption check
plot(Life.expectancy~Schooling, col="darkgreen", pch=19, cex=1,data=final_df_no_country)
mod<-lm(Life.expectancy~Schooling, data=final_df_no_country)
abline(mod, col="red", lwd=3)

plot(Life.expectancy~Alcohol.Indicator, col="darkgreen", pch=19, cex=1,data=final_df_no_country)
mod<-lm(Life.expectancy~Alcohol.Indicator, data=final_df_no_country)
abline(mod, col="red", lwd=3)

plot(Life.expectancy~Sugar.Indicator, col="darkgreen", pch=19, cex=1,data=final_df_no_country)
mod<-lm(Life.expectancy~Sugar.Indicator, data=final_df_no_country)
abline(mod, col="red", lwd=3)

plot(Life.expectancy~Fish.Indicator, col="darkgreen", pch=19, cex=1,data=final_df_no_country)
mod<-lm(Life.expectancy~Fish.Indicator, data=final_df_no_country)
abline(mod, col="red", lwd=3)

plot(Life.expectancy~Vegetables.Indicator, col="darkgreen", pch=19, cex=1,data=final_df_no_country)
mod<-lm(Life.expectancy~Vegetables.Indicator, data=final_df_no_country)
abline(mod, col="red", lwd=3)

plot(Life.expectancy~Fruits.Indicator, col="darkgreen", pch=19, cex=1,data=final_df_no_country)
mod<-lm(Life.expectancy~Fruits.Indicator, data=final_df_no_country)
abline(mod, col="red", lwd=3)

plot(Life.expectancy~Meat.Indicator, col="darkgreen", pch=19, cex=1,data=final_df_no_country)
mod<-lm(Life.expectancy~Meat.Indicator, data=final_df_no_country)
abline(mod, col="red", lwd=3)

plot(Life.expectancy~Tabacco.Indicator, col="darkgreen", pch=19, cex=1,data=final_df_no_country)
mod<-lm(Life.expectancy~Tabacco.Indicator, data=final_df_no_country)
abline(mod, col="red", lwd=3)

plot(Life.expectancy~UHC.Indicator, col="darkgreen", pch=19, cex=1,data=final_df_no_country)
mod<-lm(Life.expectancy~UHC.Indicator, data=final_df_no_country)
abline(mod, col="red", lwd=3)

plot(Life.expectancy~Pollution.death.rate, col="darkgreen", pch=19, cex=1,data=final_df_no_country)
mod<-lm(Life.expectancy~Pollution.death.rate, data=final_df_no_country)
abline(mod, col="red", lwd=3)

plot(Life.expectancy~Life.Ladder, col="darkgreen", pch=19, cex=1,data=final_df_no_country)
mod<-lm(Life.expectancy~Life.Ladder, data=final_df_no_country)
abline(mod, col="red", lwd=3)

plot(Life.expectancy~Perceptions.of.corruption, col="darkgreen", pch=19, cex=1,data=final_df_no_country)
mod<-lm(Life.expectancy~Perceptions.of.corruption, data=final_df_no_country)
abline(mod, col="red", lwd=3)

plot(Life.expectancy~Log.GDP.per.capita, col="darkgreen", pch=19, cex=1,data=final_df_no_country)
mod<-lm(Life.expectancy~Log.GDP.per.capita, data=final_df_no_country)
abline(mod, col="red", lwd=3)


# Outliers detection
Q1 <- quantile(final_df_no_country$Life.expectancy, 0.25)
Q3 <- quantile(final_df_no_country$Life.expectancy, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
potential_outliers <- which(final_df_no_country$Life.expectancy < lower_bound | final_df_no_country$Life.expectancy > upper_bound)
print(potential_outliers)

boxplot(final_df_no_country$Life.expectancy)

############################################
#Multicollinearity checks

# Create a dataframe with only the predictors 
df_predictors <- final_df_no_country[, !(names(final_df_no_country) %in% c("Life.expectancy"))]

cor_matrix <- cor(df_predictors)
round(cor_matrix,2)
symnum(cor_matrix, abbr.colnames = FALSE)

corrplot(cor_matrix, type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.6)

full.model <- lm(Life.expectancy ~., data = final_df_no_country)
sqrt(vif(full.model)) > 2

#############################################################################
#Evaluation of the different methods to select the most important predictors

######################
# Best subset selection
regfit.full=regsubsets(Life.expectancy~.,data=final_df_no_country, nvmax=13) 
reg.summary=summary(regfit.full)

# Identify the best number of variables
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
which.min(reg.summary$cp)
plot(regfit.full,scale="Cp")

###########################
#forward stepwise selection
regfit.fwd=regsubsets(Life.expectancy~.,data=final_df_no_country,method="forward", nvmax=13)
summary(regfit.fwd)
plot(regfit.fwd,scale="Cp")

#Model Selection Using cross-validation for forward stepwise selection
set.seed(11)
folds=sample(rep(1:5,length=nrow(final_df)))

predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  mat[,names(coefi)]%*%coefi
}

cv.errors=matrix(NA,5,13)

for(k in 1:5){
  best.fit=regsubsets(Life.expectancy~.,data=final_df_no_country[folds!=k,],nvmax=13,method="forward")
  
  for(i in 1:13){
    pred=predict(best.fit,final_df_no_country[folds==k,],id=i)
    cv.errors[k,i]=mean((final_df_no_country$Life.expectancy[folds==k]-pred)^2)
  }
}
mse.cv=apply(cv.errors,2,mean)
plot(mse.cv,pch=19,type="b")

#Create a dataframe with the most important variables
df_selected <- final_df[, c("Life.expectancy","Schooling", "Meat.Indicator", "Alcohol.Indicator", "Log.GDP.per.capita", "Tabacco.Indicator", "UHC.Indicator")]

#check multicollinearity
full.model <- lm(Life.expectancy ~., data = df_selected)
sqrt(vif(full.model)) > 2

#Create a dataframe with the most important variables and without the response variable
df_predictor_sel <- df_selected[, !(names(df_selected) %in% "Life.expectancy")]
cor_matrix1 <- cor(df_predictor_sel)
round(cor_matrix1,2)
symnum(cor_matrix1, abbr.colnames = FALSE)

corrplot(cor_matrix1, type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.6)


# Create a dataframe with the selection of the most important features on the basis of VIF results
df_selected <- final_df[, c("Life.expectancy", "Meat.Indicator", "Alcohol.Indicator", "Log.GDP.per.capita", "Tabacco.Indicator")]

#check multicollinearity
full.model <- lm(Life.expectancy ~., data = df_selected)
sqrt(vif(full.model)) > 2

df_predictor_sel <- df_selected[, !(names(df_selected) %in% "Life.expectancy")]
cor_matrix1 <- cor(df_predictor_sel)
round(cor_matrix1,2)
symnum(cor_matrix1, abbr.colnames = FALSE)

corrplot(cor_matrix1, type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.6)

########################################
# PCA

# Create a dataframe with a scaled variables values - Data Normalization
final_df_scaled <- as.data.frame(scale(final_df_no_country))
final_df_scaled <- final_df_scaled %>% mutate(final_df$Country)

# Set the row names for a dataframe
new_row_names <- final_df$Country
rownames(final_df_scaled) <- new_row_names

PCA_res <- princomp(final_df_scaled[, -c(1, ncol(final_df_scaled))], cor=T)
summary(PCA_res)
screeplot(PCA_res, type = "lines", npcs = min(nrow(final_df_scaled) - 1, ncol(final_df_scaled) - 2), main = "Explained Variance by component")
abline(h = 1, col = "red", lty = 2)  # Line at eigenvalue 1 (Kaiser criterion)
grid(col = "lightgray", lty = "dotted")  # Add gridlines

# Calculate and plot cumulative explained variance
eigenvalues <- PCA_res$sdev^2
cumulative_variance <- cumsum(eigenvalues) / sum(eigenvalues)
print(cumulative_variance)
plot(cumulative_variance, type = "b", xlab = "Number of Principal Components", ylab = "Cumulative Explained Variance", main = "Cumulative Explained Variance Plot")

#Individuate the most influential features on each  component
# Get the loadings matrix
loadings_matrix <- PCA_res$loadings
print(loadings_matrix)

# Specify the principal component index to get the loadings for the specified component
component_index <- 1
loadings <- loadings_matrix[, component_index]
sorted_loadings <- sort(abs(loadings), decreasing = TRUE)
influential_variable_indices <- names(sorted_loadings)
print(influential_variable_indices)

# Specify the principal component index to get the loadings for the specified component
component_index <- 2
loadings <- loadings_matrix[, component_index]
sorted_loadings <- sort(abs(loadings), decreasing = TRUE)
influential_variable_indices <- names(sorted_loadings)
print(influential_variable_indices)


# Specify the principal component index to get the loadings for the specified component
component_index <- 3
loadings <- loadings_matrix[, component_index]
sorted_loadings <- sort(abs(loadings), decreasing = TRUE)
influential_variable_indices <- names(sorted_loadings)
print(influential_variable_indices)

# Specify the principal component index to get the loadings for the specified component
component_index <- 4
loadings <- loadings_matrix[, component_index]
sorted_loadings <- sort(abs(loadings), decreasing = TRUE)
influential_variable_indices <- names(sorted_loadings)
print(influential_variable_indices)


# Calculate the absolute loadings and calculate color intensity based on loadings
absolute_loadings <- abs(PCA_res$loadings)

# Define a color palette
color_palette <- colorRampPalette(c("#7dfabb", "#071aeb"))(100) 
color_intensity <- cut(absolute_loadings, breaks = 100) 

# Create a plot
plot(NA, NA, xlim = c(-1, 1), ylim = c(-1, 1), xlab = "PC1", ylab = "PC2", type = "n")
arrows(0, 0, PCA_res$loadings[, 1], PCA_res$loadings[, 2], length = 0.1, angle = 15, col = color_palette[color_intensity])
text(PCA_res$loadings[, 1], PCA_res$loadings[, 2], labels = influential_variable_indices, pos = 3, col = color_palette[color_intensity])
# Add x and y axes
abline(v = 0, h = 0, col = "gray", lty = 2)

# Create an plot
plot(NA, NA, xlim = c(0, 1), ylim = c(0, 1), xlab = "PC1", ylab = "PC2", type = "n")
arrows(0, 0, PCA_res$loadings[, 1], PCA_res$loadings[, 2], length = 0.05, angle = 15, col = color_palette[color_intensity])
text(PCA_res$loadings[, 1], PCA_res$loadings[, 2], labels = influential_variable_indices, pos = 3, col = color_palette[color_intensity])
abline(v = 0, h = 0, col = "gray", lty = 2)


#Select the most important variables considering PCA results
df_selected <- final_df[, c("Life.expectancy", "Meat.Indicator", "Fish.Indicator","Alcohol.Indicator", "Log.GDP.per.capita", "Tabacco.Indicator", "Fruits.Indicator", "Vegetables.Indicator", "Perceptions.of.corruption")]

#check multicollinearity
full.model <- lm(Life.expectancy ~., data = df_selected)
sqrt(vif(full.model)) > 2

df_predictor_sel <- df_selected[, !(names(df_selected) %in% "Life.expectancy")]
cor_matrix1 <- cor(df_predictor_sel)
round(cor_matrix1,2)
symnum(cor_matrix1, abbr.colnames = FALSE)

corrplot(cor_matrix1, type = "upper", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.6)



#####################################################
# Decision trees

# Decision tree with all the predictors
# Set a seed for reproducibility
set.seed(12)
# Create a random index to split the data (70% for training, 30% for testing) and  create the training and testing datasets
train_index <- sample(1:nrow(final_df_no_country), 0.7 * nrow(final_df_no_country))
train_data <- final_df_no_country[train_index, ]
test_data <- final_df_no_country[-train_index, ]

# Fit a decision tree model using the Life.expectancy as the target variable and the other columns as features
decision_tree_model <- rpart(Life.expectancy ~ ., data = train_data)
rpart.plot(decision_tree_model, main = "Decision Tree for Life Expectancy")
predictions <- predict(decision_tree_model, newdata = test_data, type = "vector")
actual_labels <- test_data$Life.expectancy

# Calculate RMSE by comparing predicted and actual labels
rmse <- sqrt(mean((predictions - actual_labels)^2))
print(paste("Root Mean Squared Error (RMSE):", rmse))

# ctree
ptree<-ctree(Life.expectancy ~ ., data=train_data, control = ctree_control(mincriterion=0.95, minsplit=0, minbucket=0))
plot(ptree)
predictions <- predict(ptree, newdata = test_data)

# Calculate RMSE by comparing predicted and actual labels
rmse <- sqrt(mean((predictions - actual_labels)^2))
print(paste("Root Mean Squared Error (RMSE):", rmse))

# Decision tree with the selected predictors
# Set a seed for reproducibility
set.seed(12)
train_index <- sample(1:nrow(df_selected), 0.7 * nrow(df_selected))
train_data <- df_selected[train_index, ]
test_data <- df_selected[-train_index, ]

# Fit a decision tree model using the Life.expectancy as the target variable and the other columns as features
decision_tree_model <- rpart(Life.expectancy ~ ., data = train_data)
rpart.plot(decision_tree_model, main = "Decision Tree for Life Expectancy")
predictions <- predict(decision_tree_model, newdata = test_data, type = "vector")
actual_labels <- test_data$Life.expectancy

# Calculate RMSE by comparing predicted and actual labels
rmse <- sqrt(mean((predictions - actual_labels)^2))
print(paste("Root Mean Squared Error (RMSE):", rmse))

# ctree
ptree<-ctree(Life.expectancy ~ ., data=train_data, control = ctree_control(mincriterion=0.99, minsplit=0, minbucket=0))
plot(ptree)
predictions <- predict(ptree, newdata = test_data)

# Calculate RMSE by comparing predicted and actual labels
rmse <- sqrt(mean((predictions - actual_labels)^2))
print(paste("Root Mean Squared Error (RMSE):", rmse))

##########################################
# Random Forest

set.seed(30)
# Create a random index to split the data (70% for training, 30% for testing)
train_index <- sample(1:nrow(final_df_no_country), 0.7 * nrow(final_df_no_country))

oob.err=double(13)
test.err=double(13)
for(mtry in 1:13){
  fit=randomForest(Life.expectancy~.,data=final_df_no_country,subset=train_index,mtry=mtry,ntree=500)
  oob.err[mtry]=fit$mse[100]
  pred=predict(fit,final_df_no_country[-train_index,])
  test.err[mtry]=with(final_df_no_country[-train_index,],mean((Life.expectancy-pred)^2))
  cat(mtry," ")
}

matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("Test","OOB"),pch=19,col=c("red","blue"))

#Random forest with the selected mtry
set.seed(30)
train_data <- final_df_no_country[train_index, ]
test_data <- final_df_no_country[-train_index, ]
rf.data = randomForest(Life.expectancy ~ ., data=train_data, ntree=500, mtry=4, importance=TRUE)
rf.data
varImpPlot(rf.data)

predictions <- predict(rf.data,train_data[,-1])
actual_labels <- train_data$Life.expectancy
rmse <- sqrt(mean((predictions - actual_labels)^2))
print(paste("Train Root Mean Squared Error (RMSE):", rmse))

predictions <- predict(rf.data,test_data[,-1])
actual_labels <- test_data$Life.expectancy
rmse <- sqrt(mean((predictions - actual_labels)^2))
print(paste("Test Root Mean Squared Error (RMSE):", rmse))


# Random forest considering only the selected predictors
set.seed(30)
train_index <- sample(1:nrow(df_selected), 0.7 * nrow(df_selected))

oob.err=double(4)
test.err=double(4)
for(mtry in 1:4){
  fit=randomForest(Life.expectancy~.,data=df_selected,subset=train_index,mtry=mtry,ntree=100)
  oob.err[mtry]=fit$mse[100]
  pred=predict(fit,df_selected[-train_index,])
  test.err[mtry]=with(df_selected[-train_index,],mean((Life.expectancy-pred)^2))
  cat(mtry," ")
}

matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("Test","OOB"),pch=19,col=c("red","blue"))

#Random forest with the selected mtry
set.seed(30)
train_data <- df_selected[train_index, ]
test_data <- df_selected[-train_index, ]
rf.data = randomForest(Life.expectancy ~ ., data=train_data, ntree=100, mtry=3, importance=TRUE)
rf.data
varImpPlot(rf.data)

predictions <- predict(rf.data,train_data[,-1])
actual_labels <- train_data$Life.expectancy
rmse <- sqrt(mean((predictions - actual_labels)^2))
print(paste("Train Root Mean Squared Error (RMSE):", rmse))

predictions <- predict(rf.data,test_data[,-1])
actual_labels <- test_data$Life.expectancy
rmse <- sqrt(mean((predictions - actual_labels)^2))
print(paste("Test Root Mean Squared Error (RMSE):", rmse))

##########################################
# Boosting 

#Xboost with all the predictors
set.seed(8)
train_index <- sample(1:nrow(final_df_no_country), 0.7 * nrow(final_df_no_country))
train_data <- final_df_no_country[train_index, ]
test_data <- final_df_no_country[-train_index, ]

dtrain <- xgb.DMatrix(data = as.matrix(train_data[,-1]), label = train_data[,1])

params <- list(
  objective = "reg:squarederror", 
  eta = 0.01,                     
  max_depth = 4,                 
  nrounds = 1000                
)

# Train the XGBoost model
xgb_model <- xgboost(data = dtrain, params = params, nrounds = params$nrounds)

# Generate predictions on the test dataset for various numbers of trees
n.trees <- seq(from = 100, to = 1000, by = 100)
predmat <- matrix(NA, nrow = nrow(test_data), ncol = length(n.trees))

for (i in seq_along(n.trees)) {
  predmat[, i] <- predict(xgb_model, newdata = as.matrix(test_data[, -1]), ntreelimit = n.trees[i])
}

berr <- apply((predmat - test_data$Life.expectancy)^2, 2, mean)
plot(n.trees, berr, pch = 19, ylab = "Mean Squared Error", xlab = "# Trees", main = "Boosting Test Error")

# Find the lowest Root Mean Squared Error and the corresponding number of trees
lowest_berr <- min(berr)
best_num_trees <- n.trees[which.min(berr)]
rmse <- sqrt(lowest_berr)
cat("Lowest Root Mean Squared Error:", rmse, "\n")
cat("Number of Trees for Lowest Error:", best_num_trees, "\n")

feature_importance <- xgb.importance(model = xgb_model)
xgb.plot.importance(importance_matrix = feature_importance)

# XGBoost considering only 4 predictors
set.seed(8)
train_index <- sample(1:nrow(df_selected), 0.7 * nrow(df_selected))
train_data <- df_selected[train_index, ]
test_data <- df_selected[-train_index, ]

dtrain <- xgb.DMatrix(data = as.matrix(train_data[,-1]), label = train_data[,1])

params <- list(
  objective = "reg:squarederror", 
  eta = 0.01,                     
  max_depth = 4,                 
  nrounds = 1000                
)

# Train the XGBoost model
xgb_model <- xgboost(data = dtrain, params = params, nrounds = params$nrounds)

# Generate predictions on the test dataset for various numbers of trees
n.trees <- seq(from = 100, to = 1000, by = 100)
predmat <- matrix(NA, nrow = nrow(test_data), ncol = length(n.trees))

for (i in seq_along(n.trees)) {
  predmat[, i] <- predict(xgb_model, newdata = as.matrix(test_data[, -1]), ntreelimit = n.trees[i])
}

berr <- apply((predmat - test_data$Life.expectancy)^2, 2, mean)
plot(n.trees, berr, pch = 19, ylab = "Mean Squared Error", xlab = "# Trees", main = "Boosting Test Error")

# Find the lowest Root Mean Squared Error and the corresponding number of trees
lowest_berr <- min(berr)
best_num_trees <- n.trees[which.min(berr)]
rmse <- sqrt(lowest_berr)
cat("Lowest Root Mean Squared Error:", rmse, "\n")
cat("Number of Trees for Lowest Error:", best_num_trees, "\n")

feature_importance <- xgb.importance(model = xgb_model)
xgb.plot.importance(importance_matrix = feature_importance)



# UNSUPERVISED LEARNING

#Clusters

#K-Means
#Data Normalization
final_df_scaled <- as.data.frame(scale(final_df_no_country))
final_df_scaled <- final_df_scaled %>% mutate(final_df$Country)

# Calculate the mean and standard deviation of the predictors from the original data frame
original_mean <- sapply(final_df_no_country, mean)  
original_sd <- sapply(final_df_no_country, sd) 

# Set the row names for a dataframe
new_row_names <- final_df$Country
rownames(final_df_scaled) <- new_row_names

# Data Normalization for the dataframe with only the selected most important predictors
df_selected_scaled <- as.data.frame(scale(df_selected))

# Set the row names for a dataframe
new_row_names <- final_df$Country
rownames(df_selected_scaled) <- new_row_names



#K-Means with all the features
wssplot <- function(data, nc=15, seed=230){
  wss <- (nrow(data)-1)*sum(apply(data,2,var)) 
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(final_df_scaled[,-c(1, ncol(final_df_scaled))], nc=6) 

set.seed(230)
k.means.fit <- kmeans(final_df_scaled[,-c(1, ncol(final_df_scaled))], centers=2) 
table(k.means.fit$cluster, final_df$Life.expectancy)

clusplot(final_df_scaled[,-c(1, ncol(final_df_scaled))], k.means.fit$cluster, 
         main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,labels = 2, lines = 0)

cluster_assignments <- k.means.fit$cluster
final_df_scaled$cluster <- cluster_assignments

# List of variables/columns in the dataset (excluding the 'cluster' and 'final_df$Country' columns) to undo the scaling for each column
variable_names <- names(final_df_scaled)[!names(final_df_scaled) %in% c("cluster","final_df$Country")]

for (col_name in variable_names) {
  scaled_col <- final_df_scaled[[col_name]]
  unscaled_col <- scaled_col * original_sd[col_name] + original_mean[col_name]
  final_df_scaled[[col_name]] <- unscaled_col
}

cluster_summaries <- list()

# Loop through each variable and calculate mean, min, and max by cluster
for (variable_name in variable_names) {
  variable_summary <- final_df_scaled %>%
    group_by(cluster) %>%
    summarize(
      mean_variable = mean(.data[[variable_name]]),
      min_variable = min(.data[[variable_name]]),
      max_variable = max(.data[[variable_name]])
    )
  
  # Append the summary to the list
  cluster_summaries[[variable_name]] <- variable_summary
}

# Print or inspect the results
for (variable_name in variable_names) {
  cat("Summary for", variable_name, "\n")
  print(kable(cluster_summaries[[variable_name]], format = "markdown"))
  cat("\n\n")
}


#K-Means with just the selected features
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var)) 
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(df_selected_scaled[, -1], nc=6)

set.seed(230)
k.means.fit <- kmeans(df_selected_scaled[, -1], 2) 
table(k.means.fit$cluster, df_selected_scaled$Life.expectancy)

clusplot(df_selected_scaled[, -1], k.means.fit$cluster, 
         main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

cluster_assignments <- k.means.fit$cluster
df_selected_scaled$cluster <- cluster_assignments

# List of variables/columns in the dataset (excluding the 'cluster' column) to undo the scaling for each column
variable_names <- names(df_selected_scaled)[!names(df_selected_scaled) %in% c("cluster","final_df$Country")]

#original_mean <- sapply(final_df %>% select_if(is.numeric), mean)  # Replace with actual mean values
#original_sd <- sapply(final_df%>% select_if(is.numeric), sd)  # Replace with actual standard deviation values

for (col_name in variable_names) {
  scaled_col <- df_selected_scaled[[col_name]]
  unscaled_col <- scaled_col * original_sd[col_name] + original_mean[col_name]
  df_selected_scaled[[col_name]] <- unscaled_col
}

cluster_summaries <- list()

# Loop through each variable and calculate mean, min, and max by cluster
for (variable_name in variable_names) {
  variable_summary <- df_selected_scaled %>%
    group_by(cluster) %>%
    summarize(
      mean_variable = mean(.data[[variable_name]]),
      min_variable = min(.data[[variable_name]]),
      max_variable = max(.data[[variable_name]])
    )
  
  # Append the summary to the list
  cluster_summaries[[variable_name]] <- variable_summary
}

# Print or inspect the results
for (variable_name in variable_names) {
  cat("Summary for", variable_name, "\n")
  print(kable(cluster_summaries[[variable_name]], format = "markdown"))
  cat("\n\n")
}


########################################
# Hierarchical Clustering 

#Data Normalization
final_df_scaled <- as.data.frame(scale(final_df_no_country))
final_df_scaled <- final_df_scaled %>% mutate(final_df$Country)

# Set the row names for a dataframe
new_row_names <- final_df$Country
rownames(final_df_scaled) <- new_row_names

# Data Normalization for the dataframe with only the selected most important predictors
df_selected_scaled <- as.data.frame(scale(df_selected))

# Set the row names for a dataframe
new_row_names <- final_df$Country
rownames(df_selected_scaled) <- new_row_names

#Hierarchical Clustering with all the features
set.seed(230)
d <- dist(final_df_scaled[, -c(1, ncol(final_df_scaled))], method = "euclidean")
for (k in 2:10) {
  hc <- hclust(d,method="ward.D")
  cluster_assignments <- cutree(hc, k)
  silhouette_scores <- silhouette(cluster_assignments, d, FUN=mean)
  silhouette_avg <- mean(silhouette_scores[,"sil_width"])
  print(paste("Number of clusters:", k, " - Silhouette Score:", silhouette_avg))
}

H.fit <- hclust(d, method="ward.D")
plot(H.fit) 
abline(h=15, col="red")
groups <- cutree(H.fit, k=2) # cut tree into 2 clusters

# Draw dendogram with red borders around the 2 clusters
rect.hclust(H.fit, k=2, border="red")

boxplot(final_df_scaled$Life.expectancy ~ groups)
boxplot(final_df_scaled$Schooling ~ groups)
boxplot(final_df_scaled$Life.Ladder ~ groups)
boxplot(final_df_scaled$Sugar.Indicator ~ groups)
boxplot(final_df_scaled$Perceptions.of.corruption ~ groups)
boxplot(final_df_scaled$Log.GDP.per.capita ~ groups)
boxplot(final_df_scaled$Alcohol.Indicator ~ groups)
boxplot(final_df_scaled$Tabacco.Indicator ~ groups)
boxplot(final_df_scaled$Pollution.death.rate ~ groups)
boxplot(final_df_scaled$UHC.Indicator ~ groups)
boxplot(final_df_scaled$Vegetables.Indicator ~ groups)
boxplot(final_df_scaled$Fruits.Indicator ~ groups)
boxplot(final_df_scaled$Meat.Indicator ~ groups)
boxplot(final_df_scaled$Fish.Indicator ~ groups)

#Hierarchical Clustering with the selected features
set.seed(230)
d <- dist(df_selected_scaled[, -1], method = "euclidean")
for (k in 2:10) {
  hc <- hclust(d, method = "ward.D")
  cluster_assignments <- cutree(hc, k)
  silhouette_scores <- silhouette(cluster_assignments, d, FUN = mean)
  silhouette_avg <- mean(silhouette_scores[, "sil_width"])
  print(paste("Number of clusters:", k, " - Silhouette Score:", silhouette_avg))
}

H.fit <- hclust(d, method = "ward.D")
plot(H.fit) 
abline(h = 15, col = "red")
groups <- cutree(H.fit, k = 10) # cut tree into 10 clusters
rect.hclust(H.fit, k = 10, border = "red")

boxplot(df_selected_scaled$Life.expectancy ~ groups)
boxplot(df_selected_scaled$Meat.Indicator ~ groups)
boxplot(df_selected_scaled$Log.GDP.per.capita ~ groups)
boxplot(df_selected_scaled$Alcohol.Indicator ~ groups)
boxplot(df_selected_scaled$Tabacco.Indicator ~ groups)

