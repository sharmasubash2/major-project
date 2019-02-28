filePath = "/Users/subashsharma/Documents/major-project";

# Append all the in-patient data

setwd(filePath)
files=list.files(pattern = ".csv$");
Indat <- NULL;
for (file in files){
  #print (paste(filePath,file,sep="/"));
  if (grepl("IPDC",file)){
  Indat = rbind(Indat,read.csv(paste(filePath,file,sep="/")));
  }
}

# Write Data in file
write.csv(Indat,"ipdc.csv")


# Append all the outpatients data

Outdat <- NULL;
for (file in files){
  #print (paste(filePath,file,sep="/"));
  if (grepl("OP",file)){
    Outdat = rbind(Outdat,read.csv(paste(filePath,file,sep="/")));
  }
}

# Write Data in file
write.csv(Outdat,"op.csv")

# Exploratory Data Analysis
library(DataExplorer)


# Column details such as categorical or continuous
plot_str(Indat)
# missing data plot of each column
plot_missing(Indat)
# Frequency of Continuous variables
plot_histogram(Indat)
#plot density
plot_density(Indat)
#rm(list=ls())
#multivariate Analysis

plot_correlation(Indat, type = 'continuous','Archive.Date')

#plot bar
plot_bar(Indat)
# Create Report

#create_report(Indat)


In_Out=rbind(Outdat,Indat);
