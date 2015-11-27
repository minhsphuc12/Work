#Load packages
library(rio)
library(data.table)

#RESET FUNCTION
reset = function () {
  data_pathData=file.path(getwd(),'VNDIRECT/Database/201501.xlsx')
  data =import(data_pathData, col_types = 
                 c('text','text','text','numeric','numeric','numeric',
                   'numeric','numeric','numeric','text'))
  data [,1] = NULL
  for (i in 2:length(colnames(data))) {
    colnames(data) [i] = paste0 (colnames(data)[i],"201501")
  }
  write.table(data,file = 'Database.csv',row.names = FALSE, sep = ",", qmethod = "double")
# 
  print ('..................... reset done')
}

#RESHAPE COLUMN FUCTION
reshaping = function (number_of_column, number_of_group) {
    #     number_of_column = 1 + 8*3
    #     number_of_group  = 8
    period_updated = round (number_of_column/number_of_group - 1)
    
    g2 = c(); g3 = c(); g4 = c();g5 = c();g6 = c();g7 = c();g8 = c();g9 = c()
    
    for (i in 1:period_updated) {
        
        g2 = c(g2,number_of_group + period_updated*0 + i +1)
        g3 = c(g3,number_of_group + period_updated*1 + i +1)
        g4 = c(g4,number_of_group + period_updated*2 + i +1)
        g5 = c(g5,number_of_group + period_updated*3 + i +1)
        g6 = c(g6,number_of_group + period_updated*4 + i +1)
        g7 = c(g7,number_of_group + period_updated*5 + i +1)
        g8 = c(g8,number_of_group + period_updated*6 + i +1)
        g9 = c(g9,number_of_group + period_updated*7 + i +1)  
    }
    
    g2 = c(2, g2)
    g3 = c(3, g3)
    g4 = c(4, g4)
    g5 = c(5, g5)
    g6 = c(6, g6)
    g7 = c(7, g7)
    g8 = c(8, g8)
    g9 = c(9, g9)
    
    reshape = c(1, g2,g3,g4,g5,g6,g7,g8,g9)
    
    return (reshape)
} 


#UPDATE FUNCTION
update_database = function (period) {
  
  #read database
  data_pathData=file.path(getwd(),'Database.csv')
  data=read.table(data_pathData, header= TRUE, sep = ',')
  a = length(colnames(data)) 
  
  print ('..................... read database done')
  
  init_classes =c ('character')
#   add_classes = c('character','numeric','numeric','numeric','numeric','numeric','numeric','Date')
#   for (i in 1:round(a/8)) { init_classes = append (init_classes,add_classes) }
  data=read.table(data_pathData, header= TRUE, sep = ',',
                   colClasses = 'character',
                  )
  print ('..................... reformat database done')
  
  #RENAME COLUMN TO PERIOD
  data_pathUpdate=file.path(getwd(),paste0('VNDIRECT/Database/',period,'.xlsx'))
  Update = import (data_pathUpdate)
  print ('..................... read update done')
  
  #PROCESS UPDATE FILE, ADD PERIOD TO COLUMN NAMES
  Update[,1] = NULL
  for (i in 1:length(colnames(Update))) {
    colnames(Update)[i] = paste0 (colnames(Update)[i],period)
  }
  print ('..................... rename column done')
  
  #MERGE
  Database = merge(Update,data, by.x = colnames(Update)[1], by.y = colnames(data)[1] , all = TRUE)
  print ('..................... merge done')
  
  #RESHAPE
  Database = Database [reshaping(ncol(Database),8)]
  print ('..................... reshape done')
  
  #WRITE TO FILE
  write.table(Database,file = 'Database.csv',row.names = FALSE, sep = ",", qmethod = "double")
  print ('..................... update database done')
  
}

reset()

update_database('201502')
update_database('201503')
update_database('201504')
update_database('201505')
update_database('201506')
update_database('201507')
update_database('201508')
update_database('201509')
update_database('201510')
