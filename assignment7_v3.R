
library(ggplot2)

# Load data - note that your file path may be different
titanic <- read.csv("./data/titanic.csv")
str(titanic)

# Optional - You may make the changes for some variables prior to plotting.
# titanic$Survived <- factor(titanic$Survived, levels=c(0,1), labels=c("No","Yes"))
# titanic$Pclass <-  factor(titanic$Pclass, levels=c(1,2,3), labels=c("1st","2nd","3rd"))


# 1. Generate a series of bar charts to describe the: (a) gender, (b) ticket class, and (c) survival of the passengers onboard.

# 1a - Bar Chart for Gender
ggplot(titanic, aes(x=Sex, fill=Sex)) + 
  geom_bar() + 
  labs(title="Gender of the passengers onboard")
  #ggtitle('Gender of the passengers')  #alternative way to set title

# 1b - Bar Chart for Ticket Class (simple version)
ggplot(titanic, aes(x=Pclass, fill=factor(Pclass))) + 
  geom_bar() + 
  ggtitle("Ticket class of the passengers onboard") + 
  xlab("ticket class") +
  theme(legend.position="none")

# 1b - Bar Chart for Ticket Class
library(dplyr)
titanic %>%
  mutate(Pclass=factor(x=Pclass, levels=c(1,2,3), labels=c("1st", "2nd", "3rd"))) %>%
  ggplot(aes(x=Pclass, fill=Pclass)) + 
  geom_bar() + 
  ggtitle("Ticket class of the passengers onboard") + 
  xlab("ticket class") +
  theme(legend.position="none")

# 1c - Bar Chart for Survival
titanic %>%
  mutate(Survived = factor(x=Survived, levels=c(0,1), labels=c("No","Yes"))) %>%
  ggplot(aes(x=Survived, fill=Survived)) + 
  geom_bar() + 
  ggtitle("Survival of the passengers onboard") + 
  theme(legend.position="none")


# 2. Generate a histogram for the passengers' age. Furthermore, describe the passengers' age using the following two boxplots: (i) age per ticket class, and (ii) age based on survival.

# 2a. Histogram (show frequency)
ggplot(titanic, aes(x=Age)) + 
  geom_histogram(fill="steelblue", bins=10, na.rm=TRUE) + 
  ggtitle("Distribution of age of the passengers onboard") 

# 2a. Histogram (show density instead of frequency)
ggplot(titanic, aes(x=Age)) + 
  geom_density(na.rm=TRUE) +
  ggtitle("Distribution of age of the passengers onboard") 

# 2a. Histogram (show histogram and density)
ggplot(titanic, aes(x=Age, y=..density..)) + 
  geom_histogram(fill="steelblue", bins=10, na.rm=TRUE) + 
  geom_density(na.rm=TRUE) +
  ggtitle("Distribution of age of the passengers onboard") 

# 2b. Boxplot (group by ticket class)
titanic %>%
  mutate(Pclass=factor(x=Pclass, levels=c(1,2,3), labels=c("1st","2nd","3rd"))) %>%
  ggplot(aes(x=Pclass, y=Age, fill=Pclass)) + 
  geom_boxplot(na.rm=TRUE) + 
  ggtitle("Age of the passengers onboard") + 
  xlab("ticket class") + 
  theme(legend.position="none")

# 2c. Boxplot (group by survival)
titanic %>%
  mutate(Survived=factor(x=Survived, levels=c(0,1), labels=c("No","Yes"))) %>%
  mutate(Pclass=factor(x=Pclass, levels=c(1,2,3), labels=c("1st","2nd","3rd"))) %>%
  ggplot(aes(x=Survived, y=Age, fill=Survived)) + 
  geom_boxplot(na.rm=TRUE) + 
  ggtitle("Age of the passengers onboard") + 
  theme(legend.position="none")


# 3. Generate a histogram for the travel fare and a table showing the number of people who did not pay - you may want to check on Google why a handful of people was on board for free! 

# Why some could travel for free?
# Several people got free tickets, e.g., employees of the company

# 3a. Histogram for Ticket fare
ggplot(titanic, aes(x=Fare, y=..density..)) + 
  geom_histogram(fill="steelblue", bins=30, na.rm=TRUE) + 
  ggtitle("Ticket fare of the passengers onboard") 

# 3b. Table for Paid vs Unpaid
paid_table <- table(titanic$Fare != 0)
names(paid_table) <- c("did not pay","paid")
paid_table


# 4. A chart of your choice to describe the family size per ticket class

titanic$family_size <- titanic$SibSp + titanic$Parch + 1 #compute family size
table(titanic$family_size)

# View 1 -- facet_grid(Pclass~.)
titanic %>%
  mutate(Pclass=factor(x=Pclass, levels=c(1,2,3), labels=c("1st","2nd","3rd"))) %>%
  ggplot(aes(x=family_size, y=..density.., fill = Pclass)) +
  geom_histogram(bins=11) +
  facet_grid(Pclass ~ ., scales="free")  + 
  ggtitle("Family size per ticket class")

# View 2 -- facet_grid(~Pclass)
titanic %>%
  mutate(Pclass=factor(x=Pclass, levels=c(1,2,3), labels=c("1st","2nd","3rd"))) %>%
  ggplot(aes(x=family_size, y=..density.., fill = Pclass)) +
  geom_histogram(bins=11) +
  facet_grid(~ Pclass)  + 
  ggtitle("Family size per ticket class")


# 5. A series of stacked bar charts to show the how survival differs for different gender and ticket class

# 5a. Bar Chart - Survival by Gender
titanic %>%
  mutate(Survived=factor(x=Survived, levels=c(0,1), labels=c("No","Yes"))) %>%
  ggplot(aes(fill=Survived, x=Sex)) + 
  geom_bar(position ="stack") +
  ggtitle("Survival by gender") + 
  xlab("gender") +
  guides(fill=guide_legend("survival"))


# 5a. Bar Chart - Survival by Ticket Class

#position='stack'
x <- titanic %>%
  mutate(Survived=factor(x=Survived, levels=c(0,1), labels=c("No","Yes"))) %>%
  mutate(Pclass=factor(x=Pclass, levels=c(1,2,3), labels=c("1st","2nd","3rd"))) %>%
  ggplot(aes(fill=Survived, x=Pclass)) + 
  geom_bar(position ="stack") +
  ggtitle("Survival by ticket class") + 
  xlab("ticket class") +
  guides(fill=guide_legend("survival"))
x

#position='stack' with data labels (1)
x + geom_text(aes(label=..count..),
              stat="count",
              size=3,
              position="stack",
              vjust=1.5)


#position='stack' with data labels (2)
totals<- titanic %>% group_by(Pclass) %>% summarise(total=n())
x + geom_text(data=totals,
              aes(x=Pclass, y=total, label=total, fill=NULL),
              size=3,
              nudge_y=30)
  
#position='dodge'
titanic %>%
  mutate(Survived=factor(x=Survived, levels=c(0,1), labels=c("No","Yes"))) %>%
  mutate(Pclass=factor(x=Pclass, levels=c(1,2,3), labels=c("1st","2nd","3rd"))) %>%
  ggplot(aes(fill=Survived, x=Pclass)) + 
  geom_bar(position='dodge') +
  ggtitle("Survival by ticket class") + 
  xlab("ticket class") +
  geom_text(
    aes(label=..count..),
    stat="count",
    size=3,
    vjust=1.5, 
    position=position_dodge(width=0.9))
  

# 6. A violin chart describing how survival related to age and gender

titanic %>%
  mutate(Survived=factor(x=Survived, levels=c(0,1), labels=c("No","Yes"))) %>%
  ggplot(aes(x=Sex, y=Age, fill=Survived)) + 
  geom_violin(na.rm=TRUE, adjust=0.5)  #adjust bandwidth for density fit


# 7. A violin chart describing the survival rate related to age and ticket class

titanic %>%
  mutate(Survived=factor(x=Survived, levels=c(0,1), labels=c("No","Yes"))) %>%
  mutate(Pclass=factor(x=Pclass, levels=c(1,2,3), labels=c("1st","2nd", "3rd"))) %>%
  ggplot(aes(x=Pclass, y=Age, fill=Survived)) + 
  geom_violin(na.rm=TRUE, adjust=0.5) + 
  xlab("ticket class")


