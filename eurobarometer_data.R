# IODS final project
# Tuukka Lehtiniemi
# 6 March 2017

# I will use function in the dplyr library
library(dplyr)

# I use the dataset of Eurobarometer survey 85.1.
# This data (like other Eurobarometer data) is published in SPSS formats.
# Eurobarometer datasets are available at GESIS, see http://dx.doi.org/10.4232/1.12591
# I opened the data in SPSS and exported it as a CSV file.
# This CSV file is then read into R.
eurob_all = read.csv("/Users/lehtint9/IODS-final/eurob_85_1_all_data.csv",sep=",",header=TRUE)

# The dataset contains observations and variables as follows  
dim(eurob_all)
#[1] 27969   524

# Of these variables, I am interested in ony a handful. Justifiation for chosen variables
# is provided in the final project report.
# Questions of interest are as follows:
keep_questions <- c("d62t", "d79_1", "d79_2", "d79_3", "qd2a", "qd3a_1", "qd3a_2", "qd3a_3", "qd4_1", "qd4_2", "qd4_3", "qd5_1", "qd5_2", "qd5_3")
# Demographic information as follows:
keep_demographics <- c("d11", "d11r1")
# Nationalities of respondents:
keep_nationalities <- c("q1.1", "q1.2", "q1.3", "q1.4", "q1.5", "q1.6", "q1.7", "q1.8", "q1.9", "q1.10", "q1.11", "q1.12", "q1.13", "q1.14", "q1.15", "q1.16", "q1.17", "q1.18", "q1.19", "q1.20", "q1.21", "q1.22", "q1.23", "q1.24", "q1.25", "q1.26", "q1.27", "q1.28", "q1.29")

# Choose only the listed columns
eurob <- select(eurob_all, one_of(c(keep_questions, keep_demographics, keep_nationalities)))

# Each EU nationality (plus one for other ountries) has their own variable in the data
# I transform the nationality into one variable.
# Note that by making this choice we will lose information on respondents with multiple
# nationalities.

eurob[,"nationality"] <-NA
for(ind1 in c(1:nrow(eurob))) {
  if(eurob$q1.1[ind1]) {eurob$nationality[ind1] <- "BELGIUM"}
  if(eurob$q1.2[ind1]) {eurob$nationality[ind1] <- "DENMARK"}
  if(eurob$q1.3[ind1]) {eurob$nationality[ind1] <- "GERMANY"}
  if(eurob$q1.4[ind1]) {eurob$nationality[ind1] <- "GREECE"}
  if(eurob$q1.5[ind1]) {eurob$nationality[ind1] <- "SPAIN"}
  if(eurob$q1.6[ind1]) {eurob$nationality[ind1] <- "FRANCE"}
  if(eurob$q1.7[ind1]) {eurob$nationality[ind1] <- "IRELAND"}
  if(eurob$q1.8[ind1]) {eurob$nationality[ind1] <- "ITALY"}
  if(eurob$q1.9[ind1]) {eurob$nationality[ind1] <- "LUXEMBOURG"}
  if(eurob$q1.10[ind1]) {eurob$nationality[ind1] <- "NETHERLANDS"}
  if(eurob$q1.11[ind1]) {eurob$nationality[ind1] <- "PORTUGAL"}
  if(eurob$q1.12[ind1]) {eurob$nationality[ind1] <- "UK"}
  if(eurob$q1.13[ind1]) {eurob$nationality[ind1] <- "AUSTRIA"}
  if(eurob$q1.14[ind1]) {eurob$nationality[ind1] <- "SWEDEN"}
  if(eurob$q1.15[ind1]) {eurob$nationality[ind1] <- "FINLAND"}
  if(eurob$q1.16[ind1]) {eurob$nationality[ind1] <- "CYPRUS"}
  if(eurob$q1.17[ind1]) {eurob$nationality[ind1] <- "CZECH"}  
  if(eurob$q1.18[ind1]) {eurob$nationality[ind1] <- "ESTONIA"}  
  if(eurob$q1.19[ind1]) {eurob$nationality[ind1] <- "HUNGARY"}  
  if(eurob$q1.20[ind1]) {eurob$nationality[ind1] <- "LATVIA"}  
  if(eurob$q1.21[ind1]) {eurob$nationality[ind1] <- "LITHUANIA"}  
  if(eurob$q1.22[ind1]) {eurob$nationality[ind1] <- "MALTA"}  
  if(eurob$q1.23[ind1]) {eurob$nationality[ind1] <- "POLAND"}  
  if(eurob$q1.24[ind1]) {eurob$nationality[ind1] <- "SLOVAKIA"}  
  if(eurob$q1.25[ind1]) {eurob$nationality[ind1] <- "SLOVENIA"}  
  if(eurob$q1.26[ind1]) {eurob$nationality[ind1] <- "BULGARIA"}  
  if(eurob$q1.27[ind1]) {eurob$nationality[ind1] <- "ROMANIA"}  
  if(eurob$q1.28[ind1]) {eurob$nationality[ind1] <- "CROATIA"}  
  if(eurob$q1.29[ind1]) {eurob$nationality[ind1] <- "OTHER COUNTRIES"}  
}

# Transform nationality variable into a factor with nationalities as levels
eurob$nationality <- factor(eurob$nationality)

# The data now has observations and variables as follows
dim(eurob)
# [1] 27969    46

# Next I apply a series of filters to the dataset.

# FILTER 1
# I am interested in internet users only.
# Therefore I filter out those who do not use the internet. 
# Using internet corresponds to d62t == 1 | 2
eurob_filtered <- filter(eurob, d62t == 1 | d62t == 2)
# The data now has observations and variables as follows
dim(eurob_filtered)
# [1] 20903    46

# FILTER 2
# I am interested in internet users who use platfrom services.
# Therefore I filter out those who do not use platforms 
# Non-users of platforms are d79_1 | d79_2 | d79_3 == 6 | 7
eurob_filtered <- filter(eurob_filtered, d79_1 <6 & d79_2 <6 & d79_3 <6)
# The data now has observations and variables as follows
dim(eurob_filtered)
# [1] 11594    15

# FILTER 3
#
# Some of the questions included "Don't know" as a response option
# also spontaneous responses such as "I do not use the internet" were recorded.
# For this analysis, I will keep only responses that are in the pre-given categories
# and that did not include "Don't know".
# This means all data is filtered so that only complete cases that do not include
# "Don't know" or the spontaneous responses are kept.
#
# qd2a: we include only observations 1, 2, 3
# qd3, qd4, qd5: we include only observations 1, 2, 3, 4
eurob_filtered <- filter(eurob_filtered, qd2a < 4)
eurob_filtered <- filter(eurob_filtered, qd3a_1 <5 & qd3a_2 <5 & qd3a_3 <5)
eurob_filtered <- filter(eurob_filtered, qd4_1 <5 & qd4_2 <5 & qd4_3 <5)
eurob_filtered <- filter(eurob_filtered, qd5_1 <5 & qd5_2 <5 & qd5_3 <5)
# After these filters, we are down to 9500 observations
dim(eurob_filtered)
#[1] 9500   46


# Next, I will form the dataset that will be used in the data analysis
# The dataset will include two kinds of variables:
# - numeric variables that are used in PCA
# - factor variables that are used in MCA

# Initializing a data frame
eurob_data <- data.frame(matrix(NA, nrow = nrow(eurob_filtered), ncol = 0))

# NUMERIC VARIABLES
# First, the numeric variables, which are simpler.
# I just give the variables in eurob_filtered new, more descriptive names.
eurob_data$search <- eurob_filtered$d79_1
eurob_data$socialmedia <- eurob_filtered$d79_2
eurob_data$termsconditions <- eurob_filtered$qd2a
eurob_data$age <- eurob_filtered$d11
eurob_data$behavior <- eurob_filtered$qd3a_1
eurob_data$interest <- eurob_filtered$qd3a_2
eurob_data$commercial <- eurob_filtered$qd3a_3
eurob_data$awareness <- eurob_filtered$qd4_1
eurob_data$concern <- eurob_filtered$qd4_2
eurob_data$regulation <- eurob_filtered$qd4_3
# The "comfort" question includes actually three questions, for which I perform averaging
# to reach one summary "comfort" variable
eurob_data$comfort <- (eurob_filtered$qd5_1 + eurob_filtered$qd5_2 + eurob_filtered$qd5_3)/3

# FACTOR VARIABLES
# Next, the factor versions of variables for MCA. 
# This includes performing a number of transformations to variables
# in the form of combining responses to factor levels

# First, I create a new variable: heavy users of platforms.
# This includes those respondents who use search engines and social media every day or 
# almost every day.
# This is a factor variable with two levels: light_user and heavy_user
eurob_data$F_internetuse <- factor((eurob_filtered$d79_1 == 1) & (eurob_filtered$d79_2 == 1))
levels(eurob_data$F_internetuse) <- c("light_user", "heavy_user")

# I combine three levels of cases of reading terms & conditions to 
# "usually read" and "usually dont'read" categories
eurob_data$F_termsconditions <- factor(round((eurob_filtered$qd2a+0.1)/2))
levels(eurob_data$F_termsconditions) <- c("read","don't read")

# I rename the age group variable and give it more descriptive level labels. 
eurob_data$F_age <- factor(eurob_filtered$d11r1)
levels(eurob_data$F_age) <- c("15-24", "25-39", "40-54", "55+")

# I combine levels of six variables so that
# - "completely agree" and "somewhat agree" are combined into "agree" 
# - "completely disagree" and "somewhat disagree" are combined into "disagree".
# (the addition and rounding are used as a dirty hack to get the desired two categories)

eurob_data$F_behavior <- factor(round((eurob_filtered$qd3a_1+0.1)/2))
levels(eurob_data$F_behavior) <- c("agree", "disagree")
eurob_data$F_interest <- factor(round((eurob_filtered$qd3a_2+0.1)/2))
levels(eurob_data$F_interest) <- c("agree", "disagree")
eurob_data$F_commercial <- factor(round((eurob_filtered$qd3a_3+0.1)/2))
levels(eurob_data$F_commercial) <- c("agree", "disagree")
eurob_data$F_awareness <- factor(round((eurob_filtered$qd4_1+0.1)/2))
levels(eurob_data$F_awareness) <- c("agree", "disagree")
eurob_data$F_concern <- factor(round((eurob_filtered$qd4_2+0.1)/2))
levels(eurob_data$F_concern) <- c("agree", "disagree")
eurob_data$F_regulation <- factor(round((eurob_filtered$qd4_3+0.1)/2))
levels(eurob_data$F_regulation) <- c("agree", "disagree")

# I make a summary variable out of the experienced comfort to data use practices 
# of three different kinds of internet services. 
# For this I take the average experienced comfort across these three kinds of services.
eurob_data$F_comfort <- factor(round((eurob_filtered$qd5_1 + eurob_filtered$qd5_2 + eurob_filtered$qd5_3 + 0.1)/6))
levels(eurob_data$F_comfort) <- c("comfortable", "uncomfortable")

#Finally, I take the factored nationality variable along
eurob_data$F_nationality <- eurob_filtered$nationality

# I store the dataset so that it can be later read for analysis
write.csv(eurob_data, file= "/Users/lehtint9/IODS-final/eurob_data.csv", row.names = FALSE)
