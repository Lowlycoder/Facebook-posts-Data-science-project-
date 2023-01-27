     ################# Soutenance of Data science ###############


#2 Data analysis

#2.1 The datase

#2.2 Preliminary analysis : descriptive statistics

fbk_posts=read.csv(file='Live_20210128.csv',header=TRUE,row.names=1) #Importing the data set
#1)
#nbr of observations and variables
str(fbk_posts) # There are 7050 obs. of  15 variables
names(fbk_posts)
#2)
mises=is.na(fbk_posts) # Vérification des valeurs manquantes
mises
sum(mises) #There are 28200 missing values
colnames(fbk_posts)

#new fbk_post doc without missing values
new_fbk_posts<- within(fbk_posts, rm(Column1,Column2,Column3,Column4)) 
new_fbk_posts
str(new_fbk_posts) # There are 7050 obs. of  11 variables

#3)
summary(new_fbk_posts)
attach(new_fbk_posts)
plot(new_fbk_posts[3:11])
boxplot(num_reactions)
boxplot(num_comments,freq = FALSE)
boxplot(num_shares)
hist(num_loves)


#2.3 Principal Component Analysis (PCA)

#Theoretical question

#1)

#Practical application :
#1)

names(new_fbk_posts)
num_fbk <- new_fbk_posts[,c("num_comments","num_shares","num_likes","num_loves")]
num_fbk
names(num_fbk)
"Mean = apply(num_fbk,2,mean)
Mean"
Var_num = apply(num_fbk,2,var) # Variances of the 4 colomns
Var_num

# I think  it's not necessary to standardize the variables before performing PCA for this dataset. Because, Our data  

#2)

fbk_post_scaled = scale(num_fbk)
str(fbk_post_scaled)
pr.out = prcomp(fbk_post_scaled,scale= TRUE) # Application de la PCA
plot(pr.out)
pr.out
#the option scale=TRUE scales the variables to have standard deviation one.
names(pr.out)
pr.out$center # output the mean of the variables
pr.out$scale # output the standard deviation of the variables
#PC1
pr.out$rotation[,1]
plot(pr.out$rotation[,1],col='blue')
#PC2
pr.out$rotation[,2]
plot(pr.out$rotation[,2],col='red')
plot(pr.out$rotation[,1],pr.out$rotation[,2],col='red',main="Les deux premieres composantes principales")
biplot(pr.out,scale=0)

#3) Percentage of variance explained (PVE)


std_dev = pr.out$sdev
pr_var = std_dev^2 # variance of all components
pr_var

#proportion of variance explained
pve = pr_var/sum(pr_var)
pve

# pve plot

plot(pve, xlab = "Principal Components",ylab = "PVE",type = "b",col="blue",main="PVE par chaque composante")
#Scree plot and cumulative PVE plot
par(mfrow=c(1,1))
plot(cumsum(pve), xlab="Principal Component", ylab="PVE", type='b',col="red",main="PVE  cumulatif par chaque composante")

#4)



# 2.4 Linear Regression

#" theoretical question :"

#-  R² can take [0-1] range of values
#-  R² = r1² + r2²

# Practical application

names(new_fbk_posts)
#The previous data without these variables : "status_type","status_published","num_comments"
ln_fbk_stat = within(new_fbk_posts,rm("status_type","status_published","num_comments")) 
ln_fbk_stat
attach(ln_fbk_stat)
plot(ln_fbk_stat)
cor(ln_fbk_stat)

# Thanks to the results, we can say that  the variable num_shares is more correlated with the num_loves (cor = 0.8200002) variable
num_shares
Y <- num_shares
X <- num_loves
shares.regsimple = lm(Y~X) # The linear regression of num_shares and num_loves
summary(shares.regsimple)

#1)

# The coefficient estimates
shares.regsimple$coefficients
beta_0 = shares.regsimple$coefficients[1] # coefficient ??0 
beta_0
beta_1 = shares.regsimple$coefficients[2] # coefficient ??1
beta_1

# ??0 is different to zero, so we can say that the variables num_shares and num_loves are dependents
plot(num_shares,num_loves)
abline(beta_0,beta_1,col='red')

# P(ê ??? IC) =  1 - a
# the 95% confidence interval for this coefficient 
confint(shares.regsimple,'X',0.90)
#Interpretation.....
summary(shares.regsimple)
# The p-value << 0   => beta_1 != 0
#Plus beta_1 augmente, plus num_shares augmente
# We cannot say that  ??1 is significantly non zero; because ...
# R² = 0.6724 :That mean that 67.24 % of num_shares variations are explained by num_loves variable.
#As this result is not efficient, we can say that there are others variables which can can be correlated by num_shares


# Feature selection for multiple linear regression

attach(new_fbk_posts) 
names(new_fbk_posts)
install.packages("leaps")
library(leaps)
best_subset_model <- regsubsets(num_shares ~ num_comments+num_likes+num_loves+num_wows+num_hahas+num_sads+num_angrys, data = new_fbk_posts)
best_subset_model
summary(best_subset_model)
# Plot the R-squared versus the number of features
plot(best_subset_model, scale = "r2",col="grey")

# Select the best model
best_model <- summary(best_subset_model)$which[,which.max(summary(best_subset_model)$adjr2)]
best_model

#2) Number of features to keep. Which ones ?

num_features <- sum(best_model)
num_features # I willl keep 4 features
features <- names(best_model)[best_model == TRUE]
features
# I will keep num_wows, num_hahas, num_sads and num_angrys.

#3 It is more appropriate to use the adjusted coefficient of determination instead of the coefficient of determination R²; 
# because The adjusted coefficient of determination (R²) takes into account the number of predictors in the model and adjusts the R² value accordingly.

selected_model = lm(num_shares~num_wows+num_hahas+num_sads+num_angrys)
summary(selected_model)
selected_model$coefficients




#4) For the selected model, the values of the coefficient estimates are :









