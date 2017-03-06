library(dplyr)
library(FactoMineR)

eurob_all = read.csv("/Users/lehtint9/IODS-project/Project/eurob_85_1_all_data.csv",sep=",",header=TRUE)
str(eurob_all)
dim(eurob_all)
#[1] 27969   524

keep_questions <- c("d62t", "d79_1", "d79_2", "d79_3", "qd2a", "qd3a_1", "qd3a_2", "qd3a_3", "qd4_1", "qd4_2", "qd4_3", "qd5_1", "qd5_2", "qd5_3", "d11r1", "d11")
keep_nationalities <- c("q1.1", "q1.2", "q1.3", "q1.4", "q1.5", "q1.6", "q1.7", "q1.8", "q1.9", "q1.10", "q1.11", "q1.12", "q1.13", "q1.14", "q1.15", "q1.16", "q1.17", "q1.18", "q1.19", "q1.20", "q1.21", "q1.22", "q1.23", "q1.24", "q1.25", "q1.26", "q1.27", "q1.28", "q1.29")
#keep_nationalities <- c("q1.14", "q1.15")

# eurob <- select(eurob_all, one_of(keep_columns))
eurob <- select(eurob_all, one_of(c(keep_questions, keep_nationalities)))

eurob[,"country"] <-NA

for(ind1 in c(1:nrow(eurob))) {
  if(eurob$q1.1[ind1]) {eurob$country[ind1] <- "BELGIUM"}
  if(eurob$q1.2[ind1]) {eurob$country[ind1] <- "DENMARK"}
  if(eurob$q1.3[ind1]) {eurob$country[ind1] <- "GERMANY"}
  if(eurob$q1.4[ind1]) {eurob$country[ind1] <- "GREECE"}
  if(eurob$q1.5[ind1]) {eurob$country[ind1] <- "SPAIN"}
  if(eurob$q1.6[ind1]) {eurob$country[ind1] <- "FRANCE"}
  if(eurob$q1.7[ind1]) {eurob$country[ind1] <- "IRELAND"}
  if(eurob$q1.8[ind1]) {eurob$country[ind1] <- "ITALY"}
  if(eurob$q1.9[ind1]) {eurob$country[ind1] <- "LUXEMBOURG"}
  if(eurob$q1.10[ind1]) {eurob$country[ind1] <- "NETHERLANDS"}
  if(eurob$q1.11[ind1]) {eurob$country[ind1] <- "PORTUGAL"}
  if(eurob$q1.12[ind1]) {eurob$country[ind1] <- "UK"}
  if(eurob$q1.13[ind1]) {eurob$country[ind1] <- "AUSTRIA"}
  if(eurob$q1.14[ind1]) {eurob$country[ind1] <- "SWEDEN"}
  if(eurob$q1.15[ind1]) {eurob$country[ind1] <- "FINLAND"}
  if(eurob$q1.16[ind1]) {eurob$country[ind1] <- "CYPRUS"}
  if(eurob$q1.17[ind1]) {eurob$country[ind1] <- "CZECH"}  
  if(eurob$q1.18[ind1]) {eurob$country[ind1] <- "ESTONIA"}  
  if(eurob$q1.19[ind1]) {eurob$country[ind1] <- "HUNGARY"}  
  if(eurob$q1.20[ind1]) {eurob$country[ind1] <- "LATVIA"}  
  if(eurob$q1.21[ind1]) {eurob$country[ind1] <- "LITHUANIA"}  
  if(eurob$q1.22[ind1]) {eurob$country[ind1] <- "MALTA"}  
  if(eurob$q1.23[ind1]) {eurob$country[ind1] <- "POLAND"}  
  if(eurob$q1.24[ind1]) {eurob$country[ind1] <- "SLOVAKIA"}  
  if(eurob$q1.25[ind1]) {eurob$country[ind1] <- "SLOVENIA"}  
  if(eurob$q1.26[ind1]) {eurob$country[ind1] <- "BULGARIA"}  
  if(eurob$q1.27[ind1]) {eurob$country[ind1] <- "ROMANIA"}  
  if(eurob$q1.28[ind1]) {eurob$country[ind1] <- "CROATIA"}  
  if(eurob$q1.29[ind1]) {eurob$country[ind1] <- "OTHER COUNTRIES"}  
}

eurob$country <- factor(eurob$country)

dim(eurob)
# [1] 27969    45

# d62t
# filter out internet non-users: d62t == 1 or 2
# filter out those respondents who do not use internet
eurob_filtered <- filter(eurob, d62t == 1 | d62t == 2)
#ftable(eurob_$d62t)
dim(eurob_filtered)
#[1] 20903    15

# d79
# non-users of platforms d79_1 & d79_2 & d79_3 == 6 or 7
# filter out those respondents who do not use platforms
eurob_filtered <- filter(eurob_filtered, d79_1 <6 & d79_2 <6 & d79_3 <6)

dim(eurob_filtered)
# [1] 11594    15

# The questions included "Don't know" as a response option, and also spontaneous responses
# such as "I do not use the internet" were recorded. For this analysis, I will keep only
# responses that are in the pre-given categories, and that did not include "Don't know".
# This means all data is filtered so that only complete cases that do not include
# "Don't know" or the spontaneous responses are kept.

# qd2a
# poistetaan muut vastaukset paitsi 1, 2, 3
eurob_filtered <- filter(eurob_filtered, qd2a < 4)
#dim(eurob_)
#[1] 11350    15

# qd3
# poistetaan muut vastaukset paitsi 1, 2, 3, 4
eurob_filtered <- filter(eurob_filtered, qd3a_1 <5 & qd3a_2 <5 & qd3a_3 <5)
#dim(eurob_)
#[1] 10498    15

# qd4
# poistetaan muut vastaukset paitsi 1, 2, 3, 4
eurob_filtered <- filter(eurob_filtered, qd4_1 <5 & qd4_2 <5 & qd4_3 <5)
#dim(eurob_)
#[1] 10045    15

# qd5
# poistetaan muut vastaukset paitsi 1, 2, 3, 4
eurob_filtered <- filter(eurob_filtered, qd5_1 <5 & qd5_2 <5 & qd5_3 <5)

# After these filters, we are down to 9500 responses, compared to to 11954 that we started with.
dim(eurob_filtered)
#[1] 9500   15


# FACTORING VARIABLES FOR MCA MODEL

# Next, we will form new factor variables for data analysis.
# this includes performing a number of transformations to variables.

#initialize empty data frame
eurob_mca_data <- data.frame(matrix(NA, nrow = nrow(eurob_filtered), ncol = 0))

# d79
# First, we create a new variable: heavy users of platforms. Those respondents who use search engines 
# and social media every day or almost every day. This is a factor variable with two levels: light_user
# and heavy_user
eurob_mca_data$internetuse <- factor((eurob_filtered$d79_1 == 1) & (eurob_filtered$d79_2 == 1))
levels(eurob_mca_data$internetuse) <- c("light_user", "heavy_user")
#eurob_$d79heavy <- (eurob_$d79_1 == 1) & (eurob_$d79_2 == 1) & (eurob_$d79_3 == 1)
#eurob_$heavy <- (eurob_$d79_1 == 1) & (eurob_$d79_2 == 1)

# qd2a = 1 ja qd2a = 2 --> usually read t&c
# qd2a = 3 --> usually don't read
#
# The first transformation is done reading terms & conditions. We combine three levels of cases of
# responses to the "usually read" and "usually dont'read" categories
eurob_mca_data$termsconditions <- factor(round((eurob_filtered$qd2a+0.1)/2))
levels(eurob_mca_data$termsconditions) <- c("read","don't read")

# Next we rename the age group variable and give it more descriptive level labels. 
#eurob_mca_data <- rename(eurob_mca_data, agegroup = d11r1)
eurob_mca_data$agegroup <- factor(eurob_filtered$d11r1)
levels(eurob_mca_data$agegroup) <- c("15-24", "25-39", "40-54", "55+")

# Next we will combine levels of the variables so 
# that "completely agree" and "somewhat agree" are combined into "agree" and the same combining is done with
# "completely disagree" and "somewhat disagree" into "disagree".

# qd3a
# combine variable values: 1 and 2 represent total agreement, 3 and 4 total disagreement
# qd3a_behavior: qd3a_1 == 1 or 2 --> agree, 3 or 4 --> disagree
# qd3a_interest: qd3a_2 == 1 or 2 --> agree, 3 or 4 --> disagree
# qd3a_commercial: qd3a_3 == 1 or 2 --> agree, 3 or 4 --> disagree

#eurob_$behavior <- round((eurob_$qd3a_1+0.1)/2)
eurob_mca_data$behavior <- factor(round((eurob_filtered$qd3a_1+0.1)/2))
levels(eurob_mca_data$behavior) <- c("agree", "disagree")

# eurob_$interest <- round((eurob_$qd3a_2+0.1)/2)
eurob_mca_data$interest <- factor(round((eurob_filtered$qd3a_2+0.1)/2))
levels(eurob_mca_data$interest) <- c("agree", "disagree")

# eurob_$commercial <- round((eurob_$qd3a_3+0.1)/2)
eurob_mca_data$commercial <- factor(round((eurob_filtered$qd3a_3+0.1)/2))
levels(eurob_mca_data$commercial) <- c("agree", "disagree")

# qd4_awareness: qd4a_1 == 1 or 2 --> agree, 3 or 4 --> disagree
# qd4_concern: qd4_2 == 1 or 2 --> agree, 3 or 4 --> disagree
# qd4_regulation: qd4_3 == 1 or 2 --> agree, 3 or 4 --> disagree

#eurob_$awareness <- round((eurob_$qd4_1+0.1)/2)
eurob_mca_data$awareness <- factor(round((eurob_filtered$qd4_1+0.1)/2))
levels(eurob_mca_data$awareness) <- c("agree", "disagree")

#eurob_$concern <- round((eurob_$qd4_2+0.1)/2)
eurob_mca_data$concern <- factor(round((eurob_filtered$qd4_2+0.1)/2))
levels(eurob_mca_data$concern) <- c("agree", "disagree")

#eurob_$regulation <- round((eurob_$qd4_3+0.1)/2)
eurob_mca_data$regulation <- factor(round((eurob_filtered$qd4_3+0.1)/2))
levels(eurob_mca_data$regulation) <- c("agree", "disagree")

# Finally, we make a summary variable out of the experienced comfort to data collection
# practices of three different kinds of internet services. For this we take the average experienced
# comfort across these three kinds of services.

# tehdään summamuuttuja
# qd5_sum <- (qd5_1+qd5_2+qd5_3)/3
# pyöristetään kokonaislukuun
# --> kategoriat 1&2 = comfortable, 3&4 = uncomfortable

# eurob_$comfort <- round((eurob_$qd5_1 + eurob_$qd5_2 + eurob_$qd5_3 + 0.1)/6)
eurob_mca_data$comfort <- factor(round((eurob_filtered$qd5_1 + eurob_filtered$qd5_2 + eurob_filtered$qd5_3 + 0.1)/6))
levels(eurob_mca_data$comfort) <- c("comfortable", "uncomfortable")

#for(index in c(1:nrow(eurob_model))) {
#  if(eurob_model$q1.14[index]) {country[index] <- "Sweden"}
#  if(eurob_model$q1.14[index]) {country[index] <- "Finland"}
#  }

# country: suomalaiset -> 1, muuten 0
#eurob_model <- filter(eurob_, q1.14 == 1 | q1.15 == 1)
#eurob_model <- mutate(eurob_model, country = q1.14 == 1)
#eurob_model$country <- factor(eurob_model$country)
#levels(eurob_model$country) <- c("Sweden", "Finland")

eurob_mca_data$country <- eurob_filtered$country

#model_columns <- c("agegroup", "internetuse", "termsconditions", "behavior", "interest", "commercial", "awareness", "concern", "regulation", "comfort", "country")
mca_columns <- c("internetuse", "termsconditions", "behavior", "interest", "commercial", "awareness", "concern", "regulation", "comfort", "country")
eurob_mca_data <- select(eurob_mca_data, one_of(c(mca_columns)))

# filter countries to inspect
eurob_mca_data <- filter(eurob_mca_data, country == "FINLAND" | country == "SWEDEN" | country == "ROMANIA")

eurob_mca <- MCA(eurob_mca_data, graph = FALSE)
#summary(eurob_mca)

plot(eurob_mca, invisible=c("ind"), habillage = "quali")

# df1$heavy <- heavy


# Principal component analysis

eurob_pca_data <- data.frame(matrix(NA, nrow = nrow(eurob_filtered), ncol = 0))
eurob_pca_data$search <- eurob_filtered$d79_1
eurob_pca_data$socialmedia <- eurob_filtered$d79_2
eurob_pca_data$termsconditions <- eurob_filtered$qd2a
eurob_pca_data$age <- eurob_filtered$d11
eurob_pca_data$behavior <- eurob_filtered$qd3a_1
eurob_pca_data$interest <- eurob_filtered$qd3a_2
eurob_pca_data$commercial <- eurob_filtered$qd3a_3
eurob_pca_data$awareness <- eurob_filtered$qd4_1
eurob_pca_data$concern <- eurob_filtered$qd4_2
eurob_pca_data$regulation <- eurob_filtered$qd4_3
eurob_pca_data$comfort <- (eurob_filtered$qd5_1 + eurob_filtered$qd5_2 + eurob_filtered$qd5_3)/3


# eurob_pca_data$comfort_socialmedia <- eurob_filtered$qd5_1
# eurob_pca_data$comfort_search <- eurob_filtered$qd5_2
# eurob_pca_data$comfort_marketplace <- eurob_filtered$qd5_3
eurob_pca_data_std <- scale(eurob_pca_data)

eurob_pca <- prcomp(eurob_pca_data_std)
summary(eurob_pca)

biplot(eurob_pca, choices = 1:2, cex=c(0.5, 1), col = c("grey40", "deeppink2"))

# d11r1
# 4 kategoriaa:
# 1=15-24
# 2=25-39
# 3=40-54
# 4=55+

# maa
# "q1.1", "q1.2", "q1.3", "q1.4", "q1.5", "q1.6", "q1.7", "q1.8", "q1.9", "q1.10", "q1.11", "q1.12", "q1.13", "q1.14", "q1.15", "q1.16", "q1.17", "q1.18", "q1.19", "q1.20", "q1.21", "q1.22", "q1.23", "q1.24", "q1.25", "q1.26", "q1.27", "q1.28", "q1.29", "q1.30"

