
library(dplyr)
library(caret)
library(ggplot2)
library(nnet)
library(rpart)
library(e1071)
install.packages("e1071")

# set to run paralell
library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)


###
###  Download the data
###

training.values <- 'https://s3.amazonaws.com/drivendata/data/7/public/4910797b-ee55-40a7-8668-10efd5c1b960.csv'
training.labels <- 'https://s3.amazonaws.com/drivendata/data/7/public/0bf8bc6e-30d0-4c50-956a-603fc693d966.csv'
test.values <- 'https://s3.amazonaws.com/drivendata/data/7/public/702ddfc5-68cd-4d1d-a0de-f5f566f76d91.csv'
submission.format <- 'https://s3.amazonaws.com/drivendata/data/7/public/SubmissionFormat.csv'

d <- read.csv(training.values, stringsAsFactors = F)
d.labels <- read.csv(training.labels)

d <- d %>%
  left_join(d.labels)

d$recorded_by <- NULL
d$wpt_name <- NULL
d$subvillage <- NULL
d$ward <- NULL
d$scheme_name <- NULL
d$installer <- NULL
d$num_private <- NULL


d$district_code <- as.factor(d$district_code)
d$date_recorded <- as.Date(d$date_recorded, format = '%Y-%m-%d')



###
### create our own train (r) and test (e) from the provided training data  
###

set.seed('3456')
t.index <- createDataPartition(d$status_group, p= .5, list=FALSE, times=1)
r <- d[t.index,]
e <- d[-t.index,]

###
###  Explore
###


# some guidance can be 

# prepare training scheme
control <- trainControl(method="repeatedcv", number=5, repeats=3)
# train the model

system.time(
(model <- train(status_group~., data=r, method="lvq", preProcess="scale", trControl=control))
)
# the above took 30130.23 seconds
(30130.23/60)/60
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)



system.time(
  model <- train(status_group~., data=r, method="gbm", trControl=control))



# create some maps

ggplot(data=r, aes(x=longitude, y=latitude, color = status_group)) +
  geom_point(alpha=.5) +
  coord_cartesian(xlim=c(29,41)) +
  theme(panel.background = element_blank())

ggplot(data=r, aes(x=longitude, y=latitude, color = as.factor(district_code))) +
  geom_point() +
  coord_cartesian(xlim=c(29,41)) +
  theme(panel.background = element_blank())

# chi square on different geographies
tbl <- table(d$basin, d$status_group)
chisq.test(tbl)



###
### Construction year
###
r$cy <-  ifelse(r$construction_year==0,1990,r$construction_year)

ggplot(data=d, aes(x=ifelse(construction_year==0,1990,construction_year), fill=status_group)) +
  geom_bar(position='fill')  +
  theme(panel.background = element_blank())

# what to do with missing variables

###
### Population
###

ggplot(data=subset(r, population!=0), aes(x=population)) +
  geom_histogram(binwidth = 50) +
  coord_cartesian(xlim=c(0,1500))

ggplot(data=subset(r, population!=0), aes(x=population, fill=status_group)) +
  geom_histogram(binwidth = 100, position="fill") +
  coord_cartesian(xlim=c(0,500))

###
### extraction type
###

ggplot(data=r, aes(x=extraction_type, fill=status_group)) +
  geom_bar()  +
  theme(panel.background = element_blank())


