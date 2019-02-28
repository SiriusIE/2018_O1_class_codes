# 1. load the built-in dataset USArrests, 
# print het first 10 rows and check its structure (class of object and variables types)

data(USArrests)
head(USArrests, 10)
str(USArrests)

# 2. Get a summary of the Assault variable
summary(USArrests$Assault)

# 3. Create a new variable in USArrests equal to the ratio of Murder to Assault,
# and name it ratio_murder_assault
USArrests$ratio_murder_assault<-USArrests$Murder/USArrests$Assault

# 4. which state has the minimun ratio of Murder/Assault
rownames(USArrests)[which.min(USArrests$ratio_murder_assault)]

# 5. order the data.frame by decreasing level of UrbanPop 
USArrests<-USArrests[order(-USArrests$UrbanPop), ]

# 6. Eliminate the variable Murder from the data.frame
USArrests$Murder<-NULL

# 7. Create a variable in USArrests named state_name from the dataframe's rownames
USArrests$state_name<-rownames(USArrests)

# 8. read the csv file df_sates.csv with the option stringsAsFactors=FALSE, 
# and assign it to the data.frame df_states
df_states<-read.csv('df_states.csv', sep=';', stringsAsFactors = F)
str(df_states)

# 9. Merge both data.frames (df_states and USArrest) into a new dataframe called df_states_2
df_states_2<-merge(df_states, USArrests, by.x='name', by.y='state_name')
str(df_states_2)

# 10. Create a new data.frame (meassures_by_division) containing the information for the average 
# LifeExp. and Assault by levels of division, but only for those states with no missing values 
# for any variable. 

meassures_by_division<-aggregate(x=df_states_2[complete.cases(df_states_2), c('Life.Exp', 'Assault')],
                                 FUN = mean, 
                                 by=list(division=df_states_2[complete.cases(df_states_2),'division']))


print(meassures_by_division)