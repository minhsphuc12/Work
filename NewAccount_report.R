# Install Packages
install.packages('rio')
install.packages('data.table')

#Load packages
library(zoo)
library(data.table)

#NEW ACCOUNT DATA
#read data
getwd()
data_path=file.path(getwd(),'VNDIRECT/Trade_Report/NewAccount_Bo@.xlsx')
newacc=import(data_path)

#Convert Data Dates
newacc$FIRST_DAY_ORDER=as.IDate(newacc$FIRST_DAY_ORDER,"1899-12-30")
newacc$TXDATE_FIRST_TRANS_CI=as.IDate(newacc$TXDATE_FIRST_TRANS_CI,"1899-12-30")
newacc$TXDATE_FIRST_TRANS_SE=as.IDate(newacc$TXDATE_FIRST_TRANS_SE,"1899-12-30")
newacc$TXDATE_FIRST_LOAN=as.IDate(newacc$TXDATE_FIRST_LOAN,"1899-12-30")

#Add revised period
newacc$PERIOD_REVISED = paste0(sprintf("%04.0f",year(newacc$OPNDATE+1296000)),
                               "/",
                               sprintf("%02.0f",month(newacc$OPNDATE+1296000)))
#Add Careby

newacc$CAREBY = ifelse(substring(newacc$ACTYPE,1,1)==1,"MASS",
                       ifelse(substring(newacc$ACTYPE,1,1)==2,"MG",
                              ifelse(substring(newacc$ACTYPE,1,1)==3,"VIP","OTHER")))
#Add Time To NAV, Time to Order, Time to Deal
newacc$TIME_TO_CI = as.numeric (difftime(newacc$TXDATE_FIRST_TRANS_CI,newacc$OPNDATE), units = "days")
newacc$TIME_TO_SE = as.numeric (difftime(newacc$TXDATE_FIRST_TRANS_SE,newacc$OPNDATE), units = "days")
newacc$TIME_TO_NAV = with (newacc,pmin (TIME_TO_CI,TIME_TO_SE,na.rm=TRUE))
newacc$TIME_TO_ORDER = as.numeric (difftime(newacc$FIRST_DAY_ORDER,newacc$OPNDATE), units = "days")
newacc$TIME_TO_DEAL = as.numeric (difftime(newacc$TXDATE_FIRST_LOAN,newacc$OPNDATE), units = "days")

#reorder for viewaibility
newacc = newacc [c(1,2,13,3,4,12,5,6,7,8,9,10,11,14,15,16,17,18)]

# Count Account All/Careby
T0=subset(newacc,PERIOD_REVISED == "2015/09")
T1=subset(newacc,PERIOD_REVISED == "2015/08")
T2=subset(newacc,PERIOD_REVISED == "2015/07")
T12=subset(newacc,PERIOD_REVISED == "2014/09")


AccountQuant_T0_All=nrow(T0)
AccountQuant_T1_All=nrow(T1)
AccountQuant_T2_All=nrow(T2)
AccountQuant_T12_All=nrow(T12)

AccountQuant_T0_MASS=nrow(subset(T0,CAREBY == "MASS"))
AccountQuant_T1_MASS=nrow(subset(T1,CAREBY == "MASS"))
AccountQuant_T2_MASS=nrow(subset(T2,CAREBY == "MASS"))
AccountQuant_T12_MASS=nrow(subset(T12,CAREBY == "MASS"))

AccountQuant_T0_MG=nrow(subset(T0,CAREBY == "MG"))
AccountQuant_T1_MG=nrow(subset(T1,CAREBY == "MG"))
AccountQuant_T2_MG=nrow(subset(T2,CAREBY == "MG"))
AccountQuant_T12_MG=nrow(subset(T12,CAREBY == "MG"))

AccountQuant_T0_VIP=nrow(subset(T0,CAREBY == "VIP"))
AccountQuant_T1_VIP=nrow(subset(T1,CAREBY == "VIP"))
AccountQuant_T2_VIP=nrow(subset(T2,CAREBY == "VIP"))
AccountQuant_T12_VIP=nrow(subset(T12,CAREBY == "VIP"))

# Count Account with NAV/Trade/Debt in 15 days All/Careby
# NAV
Account_NAV15_T0_All = sum(T0$TIME_TO_NAV[!is.na(T0$TIME_TO_NAV)] <= 15)
Account_NAV15_T1_All = sum(T1$TIME_TO_NAV[!is.na(T1$TIME_TO_NAV)] <= 15)
Account_NAV15_T2_All = sum(T2$TIME_TO_NAV[!is.na(T2$TIME_TO_NAV)] <= 15)
Account_NAV15_T12_All = sum(T12$TIME_TO_NAV[!is.na(T12$TIME_TO_NAV)] <= 15)

Account_NAV15_T0_MASS = sum(subset(T0,CAREBY == "MASS")$TIME_TO_NAV[!is.na(subset(T0,CAREBY == "MASS")$TIME_TO_NAV)] <=15)
Account_NAV15_T1_MASS = sum(subset(T1,CAREBY == "MASS")$TIME_TO_NAV[!is.na(subset(T1,CAREBY == "MASS")$TIME_TO_NAV)] <=15)
Account_NAV15_T2_MASS = sum(subset(T2,CAREBY == "MASS")$TIME_TO_NAV[!is.na(subset(T2,CAREBY == "MASS")$TIME_TO_NAV)] <=15)
Account_NAV15_T12_MASS = sum(subset(T12,CAREBY == "MASS")$TIME_TO_NAV[!is.na(subset(T12,CAREBY == "MASS")$TIME_TO_NAV)] <=15)

Account_NAV15_T0_MG = sum(subset(T0,CAREBY == "MG")$TIME_TO_NAV[!is.na(subset(T0,CAREBY == "MG")$TIME_TO_NAV)] <=15)
Account_NAV15_T1_MG = sum(subset(T1,CAREBY == "MG")$TIME_TO_NAV[!is.na(subset(T1,CAREBY == "MG")$TIME_TO_NAV)] <=15)
Account_NAV15_T2_MG = sum(subset(T2,CAREBY == "MG")$TIME_TO_NAV[!is.na(subset(T2,CAREBY == "MG")$TIME_TO_NAV)] <=15)
Account_NAV15_T12_MG = sum(subset(T12,CAREBY == "MG")$TIME_TO_NAV[!is.na(subset(T12,CAREBY == "MG")$TIME_TO_NAV)] <=15)

Account_NAV15_T0_VIP = sum(subset(T0,CAREBY == "VIP")$TIME_TO_NAV[!is.na(subset(T0,CAREBY == "VIP")$TIME_TO_NAV)] <=15)
Account_NAV15_T1_VIP = sum(subset(T1,CAREBY == "VIP")$TIME_TO_NAV[!is.na(subset(T1,CAREBY == "VIP")$TIME_TO_NAV)] <=15)
Account_NAV15_T2_VIP = sum(subset(T2,CAREBY == "VIP")$TIME_TO_NAV[!is.na(subset(T2,CAREBY == "VIP")$TIME_TO_NAV)] <=15)
Account_NAV15_T12_VIP = sum(subset(T12,CAREBY == "VIP")$TIME_TO_NAV[!is.na(subset(T12,CAREBY == "VIP")$TIME_TO_NAV)] <=15)

# TRADE
Account_TRADE15_T0_All = sum(T0$TIME_TO_ORDER[!is.na(T0$TIME_TO_ORDER)] <= 15)
Account_TRADE15_T1_All = sum(T1$TIME_TO_ORDER[!is.na(T1$TIME_TO_ORDER)] <= 15)
Account_TRADE15_T2_All = sum(T2$TIME_TO_ORDER[!is.na(T2$TIME_TO_ORDER)] <= 15)
Account_TRADE15_T12_All = sum(T12$TIME_TO_ORDER[!is.na(T12$TIME_TO_ORDER)] <= 15)

Account_TRADE15_T0_MASS = sum(subset(T0,CAREBY == "MASS")$TIME_TO_ORDER[!is.na(subset(T0,CAREBY == "MASS")$TIME_TO_ORDER)] <=15)
Account_TRADE15_T1_MASS = sum(subset(T1,CAREBY == "MASS")$TIME_TO_ORDER[!is.na(subset(T1,CAREBY == "MASS")$TIME_TO_ORDER)] <=15)
Account_TRADE15_T2_MASS = sum(subset(T2,CAREBY == "MASS")$TIME_TO_ORDER[!is.na(subset(T2,CAREBY == "MASS")$TIME_TO_ORDER)] <=15)
Account_TRADE15_T12_MASS = sum(subset(T12,CAREBY == "MASS")$TIME_TO_ORDER[!is.na(subset(T12,CAREBY == "MASS")$TIME_TO_ORDER)] <=15)

Account_TRADE15_T0_MG = sum(subset(T0,CAREBY == "MG")$TIME_TO_ORDER[!is.na(subset(T0,CAREBY == "MG")$TIME_TO_ORDER)] <=15)
Account_TRADE15_T1_MG = sum(subset(T1,CAREBY == "MG")$TIME_TO_ORDER[!is.na(subset(T1,CAREBY == "MG")$TIME_TO_ORDER)] <=15)
Account_TRADE15_T2_MG = sum(subset(T2,CAREBY == "MG")$TIME_TO_ORDER[!is.na(subset(T2,CAREBY == "MG")$TIME_TO_ORDER)] <=15)
Account_TRADE15_T12_MG = sum(subset(T12,CAREBY == "MG")$TIME_TO_ORDER[!is.na(subset(T12,CAREBY == "MG")$TIME_TO_ORDER)] <=15)

Account_TRADE15_T0_VIP = sum(subset(T0,CAREBY == "VIP")$TIME_TO_ORDER[!is.na(subset(T0,CAREBY == "VIP")$TIME_TO_ORDER)] <=15)
Account_TRADE15_T1_VIP = sum(subset(T1,CAREBY == "VIP")$TIME_TO_ORDER[!is.na(subset(T1,CAREBY == "VIP")$TIME_TO_ORDER)] <=15)
Account_TRADE15_T2_VIP = sum(subset(T2,CAREBY == "VIP")$TIME_TO_ORDER[!is.na(subset(T2,CAREBY == "VIP")$TIME_TO_ORDER)] <=15)
Account_TRADE15_T12_VIP = sum(subset(T12,CAREBY == "VIP")$TIME_TO_ORDER[!is.na(subset(T12,CAREBY == "VIP")$TIME_TO_ORDER)] <=15)

# DEAL
Account_DEAL15_T0_All = sum(T0$TIME_TO_DEAL[!is.na(T0$TIME_TO_DEAL)] <= 15)
Account_DEAL15_T1_All = sum(T1$TIME_TO_DEAL[!is.na(T1$TIME_TO_DEAL)] <= 15)
Account_DEAL15_T2_All = sum(T2$TIME_TO_DEAL[!is.na(T2$TIME_TO_DEAL)] <= 15)
Account_DEAL15_T12_All = sum(T12$TIME_TO_DEAL[!is.na(T12$TIME_TO_DEAL)] <= 15)

Account_DEAL15_T0_MASS = sum(subset(T0,CAREBY == "MASS")$TIME_TO_DEAL[!is.na(subset(T0,CAREBY == "MASS")$TIME_TO_DEAL)] <=15)
Account_DEAL15_T1_MASS = sum(subset(T1,CAREBY == "MASS")$TIME_TO_DEAL[!is.na(subset(T1,CAREBY == "MASS")$TIME_TO_DEAL)] <=15)
Account_DEAL15_T2_MASS = sum(subset(T2,CAREBY == "MASS")$TIME_TO_DEAL[!is.na(subset(T2,CAREBY == "MASS")$TIME_TO_DEAL)] <=15)
Account_DEAL15_T12_MASS = sum(subset(T12,CAREBY == "MASS")$TIME_TO_DEAL[!is.na(subset(T12,CAREBY == "MASS")$TIME_TO_DEAL)] <=15)

Account_DEAL15_T0_MG = sum(subset(T0,CAREBY == "MG")$TIME_TO_DEAL[!is.na(subset(T0,CAREBY == "MG")$TIME_TO_DEAL)] <=15)
Account_DEAL15_T1_MG = sum(subset(T1,CAREBY == "MG")$TIME_TO_DEAL[!is.na(subset(T1,CAREBY == "MG")$TIME_TO_DEAL)] <=15)
Account_DEAL15_T2_MG = sum(subset(T2,CAREBY == "MG")$TIME_TO_DEAL[!is.na(subset(T2,CAREBY == "MG")$TIME_TO_DEAL)] <=15)
Account_DEAL15_T12_MG = sum(subset(T12,CAREBY == "MG")$TIME_TO_DEAL[!is.na(subset(T12,CAREBY == "MG")$TIME_TO_DEAL)] <=15)

Account_DEAL15_T0_VIP = sum(subset(T0,CAREBY == "VIP")$TIME_TO_DEAL[!is.na(subset(T0,CAREBY == "VIP")$TIME_TO_DEAL)] <=15)
Account_DEAL15_T1_VIP = sum(subset(T1,CAREBY == "VIP")$TIME_TO_DEAL[!is.na(subset(T1,CAREBY == "VIP")$TIME_TO_DEAL)] <=15)
Account_DEAL15_T2_VIP = sum(subset(T2,CAREBY == "VIP")$TIME_TO_DEAL[!is.na(subset(T2,CAREBY == "VIP")$TIME_TO_DEAL)] <=15)
Account_DEAL15_T12_VIP = sum(subset(T12,CAREBY == "VIP")$TIME_TO_DEAL[!is.na(subset(T12,CAREBY == "VIP")$TIME_TO_DEAL)] <=15)

# Calculate proportion Account with NAV/Trade/Debt in 15 days All/Careby
PERCENTAGE_NAV15_T0_MASS = Account_NAV15_T0_MASS / AccountQuant_T0_MASS
PERCENTAGE_NAV15_T1_MASS = Account_NAV15_T1_MASS / AccountQuant_T1_MASS
PERCENTAGE_NAV15_T2_MASS = Account_NAV15_T2_MASS / AccountQuant_T2_MASS
PERCENTAGE_NAV15_T12_MASS = Account_NAV15_T12_MASS / AccountQuant_T12_MASS

PERCENTAGE_NAV15_T0_MG = Account_NAV15_T0_MG / AccountQuant_T0_MG
PERCENTAGE_NAV15_T1_MG = Account_NAV15_T1_MG / AccountQuant_T1_MG
PERCENTAGE_NAV15_T2_MG = Account_NAV15_T2_MG / AccountQuant_T2_MG
PERCENTAGE_NAV15_T12_MG = Account_NAV15_T12_MG / AccountQuant_T12_MG

PERCENTAGE_NAV15_T0_VIP = Account_NAV15_T0_VIP / AccountQuant_T0_VIP
PERCENTAGE_NAV15_T1_VIP = Account_NAV15_T1_VIP / AccountQuant_T1_VIP
PERCENTAGE_NAV15_T2_VIP = Account_NAV15_T2_VIP / AccountQuant_T2_VIP
PERCENTAGE_NAV15_T12_VIP = Account_NAV15_T12_VIP / AccountQuant_T12_VIP

PERCENTAGE_TRADE15_T0_MASS = Account_TRADE15_T0_MASS / AccountQuant_T0_MASS
PERCENTAGE_TRADE15_T1_MASS = Account_TRADE15_T1_MASS / AccountQuant_T1_MASS
PERCENTAGE_TRADE15_T2_MASS = Account_TRADE15_T2_MASS / AccountQuant_T2_MASS
PERCENTAGE_TRADE15_T12_MASS = Account_TRADE15_T12_MASS / AccountQuant_T12_MASS

PERCENTAGE_TRADE15_T0_MG = Account_TRADE15_T0_MG / AccountQuant_T0_MG
PERCENTAGE_TRADE15_T1_MG = Account_TRADE15_T1_MG / AccountQuant_T1_MG
PERCENTAGE_TRADE15_T2_MG = Account_TRADE15_T2_MG / AccountQuant_T2_MG
PERCENTAGE_TRADE15_T12_MG = Account_TRADE15_T12_MG / AccountQuant_T12_MG

PERCENTAGE_TRADE15_T0_VIP = Account_TRADE15_T0_VIP / AccountQuant_T0_VIP
PERCENTAGE_TRADE15_T1_VIP = Account_TRADE15_T1_VIP / AccountQuant_T1_VIP
PERCENTAGE_TRADE15_T2_VIP = Account_TRADE15_T2_VIP / AccountQuant_T2_VIP
PERCENTAGE_TRADE15_T12_VIP = Account_TRADE15_T12_VIP / AccountQuant_T12_VIP

PERCENTAGE_DEAL15_T0_MASS = Account_DEAL15_T0_MASS / AccountQuant_T0_MASS
PERCENTAGE_DEAL15_T1_MASS = Account_DEAL15_T1_MASS / AccountQuant_T1_MASS
PERCENTAGE_DEAL15_T2_MASS = Account_DEAL15_T2_MASS / AccountQuant_T2_MASS
PERCENTAGE_DEAL15_T12_MASS = Account_DEAL15_T12_MASS / AccountQuant_T12_MASS

PERCENTAGE_DEAL15_T0_MG = Account_DEAL15_T0_MG / AccountQuant_T0_MG
PERCENTAGE_DEAL15_T1_MG = Account_DEAL15_T1_MG / AccountQuant_T1_MG
PERCENTAGE_DEAL15_T2_MG = Account_DEAL15_T2_MG / AccountQuant_T2_MG
PERCENTAGE_DEAL15_T12_MG = Account_DEAL15_T12_MG / AccountQuant_T12_MG

PERCENTAGE_DEAL15_T0_VIP = Account_DEAL15_T0_VIP / AccountQuant_T0_VIP
PERCENTAGE_DEAL15_T1_VIP = Account_DEAL15_T1_VIP / AccountQuant_T1_VIP
PERCENTAGE_DEAL15_T2_VIP = Account_DEAL15_T2_VIP / AccountQuant_T2_VIP
PERCENTAGE_DEAL15_T12_VIP = Account_DEAL15_T12_VIP / AccountQuant_T12_VIP

# CREATE REPORT
output = data.frame()
output [1,1] = ""
output [1,2] = ""
output [1,3] = ""
output [1,4] = ""
colnames (output) = c('T_0','T_1','T_2','T_12')
rbind (output,c()

