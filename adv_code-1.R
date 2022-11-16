# clear all variables & clear the Screen
rm(list=ls())
cat("\014")

# read the dataset
df1 = read.csv("/Users/keyuhuang/Desktop/Ex3(TEAM) - Advertising/Ex3_Data_R.csv")

# explore the dataset
str(df1)
summary(df1)

# to answer Q1.a
# i.short-run response_simple linear model
lm.model1 <- lm(Visitors ~ Total.Spent, data = df1)
summary(lm.model1)

# ii.short-run response_concave logarithmic (Normalizing the cost column)
df1$logTotal.Spent <- log(df1$Total.Spent)
lm.semi_log <- lm(Visitors ~ logTotal.Spent, data = df1)
summary(lm.semi_log)

# iii.short-run response_concave quadratic
df1$Total.Spent_D2 <- ((df1$Total.Spent)^2)
lm.experiment <- lm(Visitors ~ Total.Spent + Total.Spent_D2, data = df1)
summary(lm.experiment)

# to answer Q2.a (determining the Long Run effects)
# Create LAG Sales
df1$lagVisitors <- c(NA, head(df1$Visitors, -1))

# i. simple linear Response
lm.model4 <- lm(Visitors ~ Total.Spent + lagVisitors, data = df1)
summary(lm.model4)

# ii. concave log response
lm.model5 <- lm(Visitors ~ logTotal.Spent + lagVisitors, data = df1)
summary(lm.model5)

# iii. concave quadratic response
lm.model6 <- lm(Visitors ~ Total.Spent + Total.Spent_D2 + lagVisitors, data = df1)
summary(lm.model6)

# Advertising Elasticities
july <- df1[62:66,]
df1
df1[62:66,]
MV=mean(july$Visitors)
MA=mean(july$Total.Spent)
MV
MA
E1 = (MA/MV)*0.46076
E2 = (1/MV)*300.32 
E3 = (MA/MV)*(1.293e+00 + (2*-5.621e-04)*MA)
E4 = (MA/MV)*0.04992/(1-0.92129)
E5 = (1/MV)*30.56469/(1-0.91847)
E6 = (MA/MV)*(6.474e-02 + (2*-9.391e-06)*MA)/(1- 9.192e-01)

# Display Results
E1
E2
E3
E4
E5
E6

# Saturation
S1 = -1.293e+00/(2*-5.621e-04)
S2 = -6.474e-02/(2*-9.391e-06)
S1
S2

