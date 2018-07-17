install.packages("tidyverse")
install.packages("knitr")
install.packages("lubridate")
install.packages("arules")
install.packages("arulesViz")
install.packages("plyr")


library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)



retail <- read.csv(file.choose())

#retail
head(retail,100)

retail <- retail[complete.cases(retail), ]


retail <- retail %>% mutate(article_batch = as.factor(article_batch))

retail <- retail %>% mutate(store_id = as.factor(store_id))
retail$Date <- as.Date(retail$sale_date)
#retail$Time <- format(retail$InvoiceDate,"%H:%M:%S")

retail$bill_no <- as.numeric(as.character(retail$bill_no))

retail
#########################

#  retail$Time <- as.factor(retail$Time)
#  a <- hms(as.character(retail$Time))
#   retail$Time = hour(a)

#retail %>% 
# ggplot(aes(x=Time)) + 
#geom_histogram(stat="count",fill="indianred") 


###  How many items each customer buy? ###

detach("package:plyr", unload=TRUE) 

retail %>% 
  group_by(bill_no) %>% 
  
  summarize(n_items = mean(quantity)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(fill="indianred", bins = 100000) + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))


###  Top 10 best sellers  ###

tmp <- retail %>% 
  group_by(bill_no, article_batch) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
tmp <- head(tmp, n=10)
#tail(tmp,5)
tmp                      #gives a customer buys a item this much time

tmp %>% 
  ggplot(aes(x=reorder(article_batch,count), y=count))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()

### Association rules for online retailer  ###

########################################################
retail_sorted <- retail[order(retail$customer_code),]

head(retail_sorted,10)

library(plyr)
#install.packages("ddply")
#library(ddply)
itemList <- ddply(retail,c("customer_code","sale_date"), 
                  function(df1)paste(df1$article_batch, 
                                     collapse = ","))

head(itemList,100)
############################################################

itemList$customer_code <- NULL
itemList$sale_date <- NULL
colnames(itemList) <- c("items")


head(itemList,100)

write.csv(itemList,"Delium_1.csv", quote = FALSE, row.names = TRUE)

#d<-read.csv('Delium_1.csv')

print('Description of the transactions')
tr <- read.transactions('Delium_1.csv', format = 'single', sep=',')
tr
summary(tr)

head(tr,20)

##################################################################




rules <- apriori(tr, parameter = list(supp=0.001, conf=0.9))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)

inspect(rules)
