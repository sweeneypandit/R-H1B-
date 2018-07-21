install.packages("maps")
library(maps)
library('sqldf')
library('gglot2')
library(ggmap)
-------------

#importing the file
file<-file.choose()
h1b.file<-read.csv(file)
#omitting NA's
h1b.file<-na.omit(h1b.file)
h1b_file<-na.omit(h1b.file)
View(h1b.file)
tapply(h1b.file$EMPLOYER_NAME,list(name=h1b.file$EMPLOYER_NAME),length)
#getting a count of the number of H1B's sponsored by the companies
df1<-sqldf('select count(h1b_file.EMPLOYER_NAME), h1b_file.EMPLOYER_NAME
from h1b_file group by h1b_file.EMPLOYER_NAME')
#selecting the top 5 companies
df1<-sqldf('select count(h1b_file.EMPLOYER_NAME), h1b_file.EMPLOYER_NAME
from h1b_file group by h1b_file.EMPLOYER_NAME having count(h1b_file.EMPLOYER_NAME)>200')

#single dimension (1)
#creating a pie chart
#top 5 companies that file H1B visas with LCA
slices<-df1$`count(h1b_file.EMPLOYER_NAME)`
lbl<-c('AMAZON CORPORATE LLC','CAPGEMINI AMERICA INC','DELOITTE CONSULTING LLP',
       'ERNST & YOUNG U.S. LLP','SYNTEL CONSULTING INC.')
pie(slices,labels = lbl)

#multi dimension (1)
#creating a grouped bar chart
#Number of H1B visas filed with LCA from 2011-2016 with their case status type
#using the tapply function to find the count of case statuses filed by case type and year
case.status.by.year<-tapply(h1b.file$CASE_STATUS,list(h1b.file$CASE_STATUS, h1b.file$YEAR),length)
barplot(case.status.by.year,names.arg = colnames(case.status.by.year) ,beside= TRUE, ylab="number",
        xlab="year",col=c("dodgerblue","rosybrown","red","blue"),
        main="case status by year")
legend("topleft", legend = rownames(case.status.by.year), col=c("dodgerblue","rosybrown","red","blue")
       ,pch = c("*","*"), cex=0.6 )

#making sure my data file has these statuses
h1b.file<-h1b.file[h1b.file$CASE_STATUS=='CERTIFIED'| h1b.file$CASE_STATUS=='DENIED'
                   | h1b.file$CASE_STATUS=='CERTIFIED-WITHDRAWN'|
                     h1b.file$CASE_STATUS=='WITHDRAWN',]
#Distribution of the salaries of the employees whose case status is Certified(Multidimension 2)
#setting my data file to certified status
h1b.file1<-h1b.file[h1b.file$CASE_STATUS=='CERTIFIED',]

#omiting the NA's
h1b.file1<-na.omit(h1b.file1)

#creating a boxplot to see the distribution of salaries of certified by year
boxplot(h1b.file1$PREVAILING_WAGE~h1b.file1$YEAR,ylim=range(1,300000))

#popular job title associated while filing for h1b visas(single dimension 2)
#writing a statement to get count of top job titles
df1<-sqldf('select count(h1b_file.JOB_TITLE), h1b_file.JOB_TITLE
from h1b_file group by h1b_file.JOB_TITLE having count(h1b_file.SOC_NAME)>20000')
#renaming column to count
colnames(df1)[colnames(df1)=="count(h1b_file.JOB_TITLE)"] <- "Count"

#changing the datatype
df1$JOB_TITLE<-as.numeric(df1$JOB_TITLE)
df1$Count<-as.numeric(as.integer(df1$Count))

#plotting the two parameters on ggplot with point and bar graph
p<-ggplot(data=df1, aes(x=JOB_TITLE, y=Count)) 
p<-p + geom_bar(stat="identity")
p<- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#creating city and states column
h1b.file$WORKSITE<-as.character(as.factor(h1b.file$WORKSITE))
h1b.file$city<- sapply(strsplit(as.character(h1b.file$WORKSITE),','), "[", 1)
h1b.file$state<- sapply(strsplit(as.character(h1b.file$WORKSITE),','), "[", 2)
#removing white spaces after splitting the term
h1b.file$state<-gsub("^\\s+|\\s+$","",h1b.file$state)
# state, city offering the most h1b visas
agg.data<-aggregate(h1b.file$CASE_STATUS,
                    by=list(h1b.file$state)
                    ,length)
#sorting the data
sort(agg.data$Group.1)

rownames(agg.data)<-NULL

#plotting US map using ggmap and ggplot
us <- map_data("state") 
#mapping the analysis of city and state count of h1b cases
map1<- ggplot(agg.data, aes(map_id = Group.1))
#combining the mappings of analysis over the US map
map1<-map1 + geom_map(map = us, aes(fill = agg.data$x), color= 'white')
#formatting the map
map1<- map1 + expand_limits(x=us$long,y=us$lat)
map1<-map1 + coord_map()
#standardizing the numbers
options(scipen = 9)

#cities offering h1b visas (SD)
#creating a pie chart
df1<-sqldf('select count(h1b_file.city), h1b_file.city
from h1b_file group by h1b_file.city having count(h1b_file.city)>30000')
slices<- df1$`count(h1b_file.city)`
pie(slices,labels =df1$city)

#distribution of employment type (single dimension)
barplot(table(h1b.file$FULL_TIME_POSITION),col=c("dodgerblue","rosybrown"))
