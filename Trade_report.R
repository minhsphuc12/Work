#Load packages
library(rio)
library(data.table)


# TRADE REPORT
# Read Data
data_pathT0=file.path(getwd(),'VNDIRECT/Trade_Report/Trade201510.xlsx')
data_pathT1=file.path(getwd(),'VNDIRECT/Trade_Report/Trade201509.xlsx')
data_pathT2=file.path(getwd(),'VNDIRECT/Trade_Report/Trade201508.xlsx')
data_pathT3=file.path(getwd(),'VNDIRECT/Trade_Report/Trade201507.xlsx')
data_pathT4=file.path(getwd(),'VNDIRECT/Trade_Report/Trade201506.xlsx')
data_pathT12=file.path(getwd(),'VNDIRECT/Trade_Report/Trade201410.xlsx')
TradeT0=import(data_pathT0)
TradeT1=import(data_pathT1)
TradeT2=import(data_pathT2)
TradeT3=import(data_pathT3)
TradeT4=import(data_pathT4)
TradeT12=import(data_pathT12)

DATE_CHECK = "2015-10-30 UTC"
# Convert Data Dates: NO NEED

# Add ActiveStatus 
TradeT0$STATUS = ifelse (((TradeT0$GTGD==0)& (TradeT0$NAV_END==0))& (TradeT0$NAV_INIT==0), "CLOSE", "ACTIVE")
TradeT1$STATUS = ifelse (((TradeT1$GTGD==0)& (TradeT1$NAV_END==0))& (TradeT1$NAV_INIT==0), "CLOSE", "ACTIVE")
TradeT2$STATUS = ifelse (((TradeT2$GTGD==0)& (TradeT2$NAV_END==0))& (TradeT2$NAV_INIT==0), "CLOSE", "ACTIVE")
TradeT3$STATUS = ifelse (((TradeT3$GTGD==0)& (TradeT3$NAV_END==0))& (TradeT3$NAV_INIT==0), "CLOSE", "ACTIVE")
TradeT4$STATUS = ifelse (((TradeT4$GTGD==0)& (TradeT4$NAV_END==0))& (TradeT4$NAV_INIT==0), "CLOSE", "ACTIVE")
TradeT12$STATUS = ifelse (((TradeT12$GTGD==0)& (TradeT12$NAV_END==0))& (TradeT12$NAV_INIT==0), "CLOSE", "ACTIVE")

# Add Careby
TradeT0$CAREBY = ifelse(substring(TradeT0$ACTYPE,1,1)==1,"MASS",
                       ifelse(substring(TradeT0$ACTYPE,1,1)==2,"MG",
                              ifelse(substring(TradeT0$ACTYPE,1,1)==3,"VIP","OTHER")))
TradeT1$CAREBY = ifelse(substring(TradeT1$ACTYPE,1,1)==1,"MASS",
                        ifelse(substring(TradeT1$ACTYPE,1,1)==2,"MG",
                               ifelse(substring(TradeT1$ACTYPE,1,1)==3,"VIP","OTHER")))
TradeT2$CAREBY = ifelse(substring(TradeT2$ACTYPE,1,1)==1,"MASS",
                        ifelse(substring(TradeT2$ACTYPE,1,1)==2,"MG",
                               ifelse(substring(TradeT2$ACTYPE,1,1)==3,"VIP","OTHER")))
TradeT3$CAREBY = ifelse(substring(TradeT3$ACTYPE,1,1)==1,"MASS",
                        ifelse(substring(TradeT3$ACTYPE,1,1)==2,"MG",
                               ifelse(substring(TradeT3$ACTYPE,1,1)==3,"VIP","OTHER")))
TradeT4$CAREBY = ifelse(substring(TradeT4$ACTYPE,1,1)==1,"MASS",
                        ifelse(substring(TradeT4$ACTYPE,1,1)==2,"MG",
                               ifelse(substring(TradeT4$ACTYPE,1,1)==3,"VIP","OTHER")))
TradeT12$CAREBY = ifelse(substring(TradeT12$ACTYPE,1,1)==1,"MASS",
                        ifelse(substring(TradeT12$ACTYPE,1,1)==2,"MG",
                               ifelse(substring(TradeT12$ACTYPE,1,1)==3,"VIP","OTHER")))



# Add First Debt Check

TradeT0$FIRST_DEBT = ifelse(  ((as.numeric (difftime (as.Date(TradeT0$FIRST_DAY_OF_DEAL,"%d/%m/%Y"), as.Date(DATE_CHECK)), units = "days")) >-30) & 
                              ((as.numeric (difftime (as.Date(TradeT0$FIRST_DAY_OF_DEAL,"%d/%m/%Y"), as.Date(DATE_CHECK)), units = "days")) <= 0)  ,
                            "YES","NO")

TradeT1$FIRST_DEBT = ifelse(((as.numeric (difftime (as.Date(TradeT1$FIRST_DAY_OF_DEAL,"%d/%m/%Y"), as.Date(DATE_CHECK)), units = "days")) >-30) & 
                              ((as.numeric (difftime (as.Date(TradeT1$FIRST_DAY_OF_DEAL,"%d/%m/%Y"), as.Date(DATE_CHECK)), units = "days")) <= 0),
                            "YES","NO")
TradeT2$FIRST_DEBT = ifelse(((as.numeric (difftime (as.Date(TradeT2$FIRST_DAY_OF_DEAL,"%d/%m/%Y"), as.Date(DATE_CHECK)), units = "days")) >-30) & 
                              ((as.numeric (difftime (as.Date(TradeT2$FIRST_DAY_OF_DEAL,"%d/%m/%Y"), as.Date(DATE_CHECK)), units = "days")) <= 0),
                            "YES","NO")
TradeT3$FIRST_DEBT = ifelse(((as.numeric (difftime (as.Date(TradeT3$FIRST_DAY_OF_DEAL,"%d/%m/%Y"), as.Date(DATE_CHECK)), units = "days")) >-30) & 
                              ((as.numeric (difftime (as.Date(TradeT3$FIRST_DAY_OF_DEAL,"%d/%m/%Y"), as.Date(DATE_CHECK)), units = "days")) <= 0),
                            "YES","NO")
TradeT4$FIRST_DEBT = ifelse(((as.numeric (difftime (as.Date(TradeT4$FIRST_DAY_OF_DEAL,"%d/%m/%Y"), as.Date(DATE_CHECK)), units = "days")) >-30) & 
                              ((as.numeric (difftime (as.Date(TradeT4$FIRST_DAY_OF_DEAL,"%d/%m/%Y"), as.Date(DATE_CHECK)), units = "days")) <= 0),
                            "YES","NO")
TradeT12$FIRST_DEBT = ifelse(((as.numeric (difftime (as.Date(TradeT12$FIRST_DAY_OF_DEAL,"%d/%m/%Y"), as.Date(DATE_CHECK)), units = "days")) >-30) & 
                              ((as.numeric (difftime (as.Date(TradeT12$FIRST_DAY_OF_DEAL,"%d/%m/%Y"), as.Date(DATE_CHECK)), units = "days")) <= 0),
                            "YES","NO")



# Sum NAV/Trade Amount/Trade Fee/Interest (Careby)
Total_NAV_MASS = c()
Total_NAV_MASS[1] = sum (subset(TradeT0,CAREBY== "MASS")$NAV_END)
Total_NAV_MASS[2] = sum (subset(TradeT1,CAREBY== "MASS")$NAV_END)
Total_NAV_MASS[3] = sum (subset(TradeT2,CAREBY== "MASS")$NAV_END)
Total_NAV_MASS[4] = sum (subset(TradeT12,CAREBY== "MASS")$NAV_END)

Total_NAV_MG = c()
Total_NAV_MG[1] = sum (subset(TradeT0,CAREBY== "MG")$NAV_END)
Total_NAV_MG[2] = sum (subset(TradeT1,CAREBY== "MG")$NAV_END)
Total_NAV_MG[3] = sum (subset(TradeT2,CAREBY== "MG")$NAV_END)
Total_NAV_MG[4] = sum (subset(TradeT12,CAREBY== "MG")$NAV_END)

Total_NAV_VIP = c()
Total_NAV_VIP[1]  = sum (subset(TradeT0,CAREBY== "VIP")$NAV_END)
Total_NAV_VIP[2] = sum (subset(TradeT1,CAREBY== "VIP")$NAV_END)
Total_NAV_VIP[3] = sum (subset(TradeT2,CAREBY== "VIP")$NAV_END)
Total_NAV_VIP[4] = sum (subset(TradeT12,CAREBY== "VIP")$NAV_END)

Total_TRADE_MASS = c()
Total_TRADE_MASS[1] = sum (subset(TradeT0,CAREBY== "MASS")$GTGD)
Total_TRADE_MASS[2] = sum (subset(TradeT1,CAREBY== "MASS")$GTGD)
Total_TRADE_MASS[3] = sum (subset(TradeT2,CAREBY== "MASS")$GTGD)
Total_TRADE_MASS[4] = sum (subset(TradeT12,CAREBY== "MASS")$GTGD)

Total_TRADE_MG = c()
Total_TRADE_MG[1] = sum (subset(TradeT0,CAREBY== "MG")$GTGD)
Total_TRADE_MG[2] = sum (subset(TradeT1,CAREBY== "MG")$GTGD)
Total_TRADE_MG[3] = sum (subset(TradeT2,CAREBY== "MG")$GTGD)
Total_TRADE_MG[4] = sum (subset(TradeT12,CAREBY== "MG")$GTGD)

Total_TRADE_VIP = c()
Total_TRADE_VIP[1] = sum (subset(TradeT0,CAREBY== "VIP")$GTGD)
Total_TRADE_VIP[2] = sum (subset(TradeT1,CAREBY== "VIP")$GTGD)
Total_TRADE_VIP[3] = sum (subset(TradeT2,CAREBY== "VIP")$GTGD)
Total_TRADE_VIP[4] = sum (subset(TradeT12,CAREBY== "VIP")$GTGD)

Total_TRADE_FEE_MASS = c()
Total_TRADE_FEE_MASS[1] = sum (subset(TradeT0,CAREBY== "MASS")$FEE)
Total_TRADE_FEE_MASS[2] = sum (subset(TradeT1,CAREBY== "MASS")$FEE)
Total_TRADE_FEE_MASS[3] = sum (subset(TradeT2,CAREBY== "MASS")$FEE)
Total_TRADE_FEE_MASS[4] = sum (subset(TradeT12,CAREBY== "MASS")$FEE)

Total_TRADE_FEE_MG = c()
Total_TRADE_FEE_MG[1] = sum (subset(TradeT0,CAREBY== "MG")$FEE)
Total_TRADE_FEE_MG[2] = sum (subset(TradeT1,CAREBY== "MG")$FEE)
Total_TRADE_FEE_MG[3] = sum (subset(TradeT2,CAREBY== "MG")$FEE)
Total_TRADE_FEE_MG[4] = sum (subset(TradeT12,CAREBY== "MG")$FEE)

Total_TRADE_FEE_VIP = c()
Total_TRADE_FEE_VIP[1] = sum (subset(TradeT0,CAREBY== "VIP")$FEE)
Total_TRADE_FEE_VIP[2] = sum (subset(TradeT1,CAREBY== "VIP")$FEE)
Total_TRADE_FEE_VIP[3] = sum (subset(TradeT2,CAREBY== "VIP")$FEE)
Total_TRADE_FEE_VIP[4] = sum (subset(TradeT12,CAREBY== "VIP")$FEE)

Total_INTEREST_MASS = c()
Total_INTEREST_MASS[1] = sum (subset(TradeT0,CAREBY== "MASS")$INTEREST)
Total_INTEREST_MASS[2] = sum (subset(TradeT1,CAREBY== "MASS")$INTEREST)
Total_INTEREST_MASS[3] = sum (subset(TradeT2,CAREBY== "MASS")$INTEREST)
Total_INTEREST_MASS[4] = sum (subset(TradeT12,CAREBY== "MASS")$INTEREST)

Total_INTEREST_MG = c()
Total_INTEREST_MG[1] = sum (subset(TradeT0,CAREBY== "MG")$INTEREST)
Total_INTEREST_MG[2] = sum (subset(TradeT1,CAREBY== "MG")$INTEREST)
Total_INTEREST_MG[3] = sum (subset(TradeT2,CAREBY== "MG")$INTEREST)
Total_INTEREST_MG[4] = sum (subset(TradeT12,CAREBY== "MG")$INTEREST)

Total_INTEREST_VIP = c()
Total_INTEREST_VIP[1] = sum (subset(TradeT0,CAREBY== "VIP")$INTEREST)
Total_INTEREST_VIP[2] = sum (subset(TradeT1,CAREBY== "VIP")$INTEREST)
Total_INTEREST_VIP[3] = sum (subset(TradeT2,CAREBY== "VIP")$INTEREST)
Total_INTEREST_VIP[4] = sum (subset(TradeT12,CAREBY== "VIP")$INTEREST)

# Calculate median of trade fee (Careby)
Median_FEE_MASS = c()
Median_FEE_MASS [1] = median (subset(subset(TradeT0,CAREBY== "MASS"), FEE>0) $FEE)
Median_FEE_MASS [2] = median (subset(subset(TradeT1,CAREBY== "MASS"), FEE>0) $FEE)
Median_FEE_MASS [3] = median (subset(subset(TradeT2,CAREBY== "MASS"), FEE>0) $FEE)
Median_FEE_MASS [4] = median (subset(subset(TradeT12,CAREBY== "MASS"), FEE>0) $FEE)

Median_FEE_MG = c()
Median_FEE_MG [1] = median (subset(subset(TradeT0,CAREBY== "MG"), FEE>0) $FEE)
Median_FEE_MG [2] = median (subset(subset(TradeT1,CAREBY== "MG"), FEE>0) $FEE)
Median_FEE_MG [3] = median (subset(subset(TradeT2,CAREBY== "MG"), FEE>0) $FEE)
Median_FEE_MG [4] = median (subset(subset(TradeT12,CAREBY== "MG"), FEE>0) $FEE)

Median_FEE_VIP = c()
Median_FEE_VIP [1] = median (subset(subset(TradeT0,CAREBY== "VIP"), FEE>0) $FEE)
Median_FEE_VIP [2] = median (subset(subset(TradeT1,CAREBY== "VIP"), FEE>0) $FEE)
Median_FEE_VIP [3] = median (subset(subset(TradeT2,CAREBY== "VIP"), FEE>0) $FEE)
Median_FEE_VIP [4] = median (subset(subset(TradeT12,CAREBY== "VIP"), FEE>0) $FEE)

# Count Account/ with Trade/ with Debt / with first Debt (Careby)
Account_ALL_MASS = c()
Account_ALL_MASS [1] = sum (!is.na(subset(TradeT0,CAREBY== "MASS")$STATUS [subset(TradeT0,CAREBY== "MASS")$STATUS != "CLOSE"]))
Account_ALL_MASS [2] = sum (!is.na(subset(TradeT1,CAREBY== "MASS")$STATUS [subset(TradeT1,CAREBY== "MASS")$STATUS != "CLOSE"]))
Account_ALL_MASS [3] = sum (!is.na(subset(TradeT2,CAREBY== "MASS")$STATUS [subset(TradeT2,CAREBY== "MASS")$STATUS != "CLOSE"]))
Account_ALL_MASS [4] = sum (!is.na(subset(TradeT12,CAREBY== "MASS")$STATUS [subset(TradeT12,CAREBY== "MASS")$STATUS != "CLOSE"]))

Account_ALL_MG = c()
Account_ALL_MG [1] = sum (!is.na(subset(TradeT0,CAREBY== "MG")$STATUS [subset(TradeT0,CAREBY== "MG")$STATUS != "CLOSE"]))
Account_ALL_MG [2] = sum (!is.na(subset(TradeT1,CAREBY== "MG")$STATUS [subset(TradeT1,CAREBY== "MG")$STATUS != "CLOSE"]))
Account_ALL_MG [3] = sum (!is.na(subset(TradeT2,CAREBY== "MG")$STATUS [subset(TradeT2,CAREBY== "MG")$STATUS != "CLOSE"]))
Account_ALL_MG [4] = sum (!is.na(subset(TradeT12,CAREBY== "MG")$STATUS [subset(TradeT12,CAREBY== "MG")$STATUS != "CLOSE"]))

Account_ALL_VIP = c()
Account_ALL_VIP [1] = sum (!is.na(subset(TradeT0,CAREBY== "VIP")$STATUS [subset(TradeT0,CAREBY== "VIP")$STATUS != "CLOSE"]))
Account_ALL_VIP [2] = sum (!is.na(subset(TradeT1,CAREBY== "VIP")$STATUS [subset(TradeT1,CAREBY== "VIP")$STATUS != "CLOSE"]))
Account_ALL_VIP [3] = sum (!is.na(subset(TradeT2,CAREBY== "VIP")$STATUS [subset(TradeT2,CAREBY== "VIP")$STATUS != "CLOSE"]))
Account_ALL_VIP [4] = sum (!is.na(subset(TradeT12,CAREBY== "VIP")$STATUS [subset(TradeT12,CAREBY== "VIP")$STATUS != "CLOSE"]))

Account_TRADE_MASS = c()
Account_TRADE_MASS [1] = sum (!is.na(subset(TradeT0,CAREBY== "MASS")$GTGD [subset(TradeT0,CAREBY== "MASS")$GTGD >0]))
Account_TRADE_MASS [2] = sum (!is.na(subset(TradeT1,CAREBY== "MASS")$GTGD [subset(TradeT1,CAREBY== "MASS")$GTGD >0]))
Account_TRADE_MASS [3] = sum (!is.na(subset(TradeT2,CAREBY== "MASS")$GTGD [subset(TradeT2,CAREBY== "MASS")$GTGD >0]))
Account_TRADE_MASS [4] = sum (!is.na(subset(TradeT12,CAREBY== "MASS")$GTGD [subset(TradeT12,CAREBY== "MASS")$GTGD >0]))

Account_TRADE_MG = c()
Account_TRADE_MG [1] = sum (!is.na(subset(TradeT0,CAREBY== "MG")$GTGD [subset(TradeT0,CAREBY== "MG")$GTGD >0]))
Account_TRADE_MG [2] = sum (!is.na(subset(TradeT1,CAREBY== "MG")$GTGD [subset(TradeT1,CAREBY== "MG")$GTGD >0]))
Account_TRADE_MG [3] = sum (!is.na(subset(TradeT2,CAREBY== "MG")$GTGD [subset(TradeT2,CAREBY== "MG")$GTGD >0]))
Account_TRADE_MG [4] = sum (!is.na(subset(TradeT12,CAREBY== "MG")$GTGD [subset(TradeT12,CAREBY== "MG")$GTGD >0]))

Account_TRADE_VIP = c()
Account_TRADE_VIP [1] = sum (!is.na(subset(TradeT0,CAREBY== "VIP")$GTGD [subset(TradeT0,CAREBY== "VIP")$GTGD >0]))
Account_TRADE_VIP [2] = sum (!is.na(subset(TradeT1,CAREBY== "VIP")$GTGD [subset(TradeT1,CAREBY== "VIP")$GTGD >0]))
Account_TRADE_VIP [3] = sum (!is.na(subset(TradeT2,CAREBY== "VIP")$GTGD [subset(TradeT2,CAREBY== "VIP")$GTGD >0]))
Account_TRADE_VIP [4] = sum (!is.na(subset(TradeT12,CAREBY== "VIP")$GTGD [subset(TradeT12,CAREBY== "VIP")$GTGD >0]))

Account_INTEREST_MASS = c()
Account_INTEREST_MASS [1] = sum (!is.na(subset(TradeT0,CAREBY== "MASS")$INTEREST [subset(TradeT0,CAREBY== "MASS")$INTEREST >0]))
Account_INTEREST_MASS [2] = sum (!is.na(subset(TradeT1,CAREBY== "MASS")$INTEREST [subset(TradeT1,CAREBY== "MASS")$INTEREST >0]))
Account_INTEREST_MASS [3] = sum (!is.na(subset(TradeT2,CAREBY== "MASS")$INTEREST [subset(TradeT2,CAREBY== "MASS")$INTEREST >0]))
Account_INTEREST_MASS [4] = sum (!is.na(subset(TradeT12,CAREBY== "MASS")$INTEREST [subset(TradeT12,CAREBY== "MASS")$INTEREST >0]))

Account_INTEREST_MG = c()
Account_INTEREST_MG [1] = sum (!is.na(subset(TradeT0,CAREBY== "MG")$INTEREST [subset(TradeT0,CAREBY== "MG")$INTEREST >0]))
Account_INTEREST_MG [2] = sum (!is.na(subset(TradeT1,CAREBY== "MG")$INTEREST [subset(TradeT1,CAREBY== "MG")$INTEREST >0]))
Account_INTEREST_MG [3] = sum (!is.na(subset(TradeT2,CAREBY== "MG")$INTEREST [subset(TradeT2,CAREBY== "MG")$INTEREST >0]))
Account_INTEREST_MG [4] = sum (!is.na(subset(TradeT12,CAREBY== "MG")$INTEREST [subset(TradeT12,CAREBY== "MG")$INTEREST >0]))

Account_INTEREST_VIP = c()
Account_INTEREST_VIP [1] = sum (!is.na(subset(TradeT0,CAREBY== "VIP")$INTEREST [subset(TradeT0,CAREBY== "VIP")$INTEREST >0]))
Account_INTEREST_VIP [2] = sum (!is.na(subset(TradeT1,CAREBY== "VIP")$INTEREST [subset(TradeT1,CAREBY== "VIP")$INTEREST >0]))
Account_INTEREST_VIP [3] = sum (!is.na(subset(TradeT2,CAREBY== "VIP")$INTEREST [subset(TradeT2,CAREBY== "VIP")$INTEREST >0]))
Account_INTEREST_VIP [4] = sum (!is.na(subset(TradeT12,CAREBY== "VIP")$INTEREST [subset(TradeT12,CAREBY== "VIP")$INTEREST >0]))

Account_FIRSTDEBT_MASS = c()
Account_FIRSTDEBT_MASS[1] = sum (!is.na(subset(TradeT0,CAREBY== "MASS")$FIRST_DEBT [subset(TradeT0,CAREBY== "MASS")$FIRST_DEBT == "YES"]))
Account_FIRSTDEBT_MASS[2] = sum (!is.na(subset(TradeT1,CAREBY== "MASS")$FIRST_DEBT [subset(TradeT1,CAREBY== "MASS")$FIRST_DEBT == "YES"]))
Account_FIRSTDEBT_MASS[3] = sum (!is.na(subset(TradeT2,CAREBY== "MASS")$FIRST_DEBT [subset(TradeT2,CAREBY== "MASS")$FIRST_DEBT == "YES"]))
Account_FIRSTDEBT_MASS[4] = sum (!is.na(subset(TradeT12,CAREBY== "MASS")$FIRST_DEBT [subset(TradeT12,CAREBY== "MASS")$FIRST_DEBT == "YES"]))

Account_FIRSTDEBT_MG = c()
Account_FIRSTDEBT_MG[1] = sum (!is.na(subset(TradeT0,CAREBY== "MG")$FIRST_DEBT [subset(TradeT0,CAREBY== "MG")$FIRST_DEBT == "YES"]))
Account_FIRSTDEBT_MG[2] = sum (!is.na(subset(TradeT1,CAREBY== "MG")$FIRST_DEBT [subset(TradeT1,CAREBY== "MG")$FIRST_DEBT == "YES"]))
Account_FIRSTDEBT_MG[3] = sum (!is.na(subset(TradeT2,CAREBY== "MG")$FIRST_DEBT [subset(TradeT2,CAREBY== "MG")$FIRST_DEBT == "YES"]))
Account_FIRSTDEBT_MG[4] = sum (!is.na(subset(TradeT12,CAREBY== "MG")$FIRST_DEBT [subset(TradeT12,CAREBY== "MG")$FIRST_DEBT == "YES"]))

Account_FIRSTDEBT_VIP = c()
Account_FIRSTDEBT_VIP[1] = sum (!is.na(subset(TradeT0,CAREBY== "VIP")$FIRST_DEBT [subset(TradeT0,CAREBY== "VIP")$FIRST_DEBT == "YES"]))
Account_FIRSTDEBT_VIP[2] = sum (!is.na(subset(TradeT1,CAREBY== "VIP")$FIRST_DEBT [subset(TradeT1,CAREBY== "VIP")$FIRST_DEBT == "YES"]))
Account_FIRSTDEBT_VIP[3] = sum (!is.na(subset(TradeT2,CAREBY== "VIP")$FIRST_DEBT [subset(TradeT2,CAREBY== "VIP")$FIRST_DEBT == "YES"]))
Account_FIRSTDEBT_VIP[4] = sum (!is.na(subset(TradeT12,CAREBY== "VIP")$FIRST_DEBT [subset(TradeT12,CAREBY== "VIP")$FIRST_DEBT == "YES"]))

# Calculate proportion Account with Trade/ with Debt / with first Debt (Careby)
PERCENTAGE_TRADE_MASS = Account_TRADE_MASS / Account_ALL_MASS
PERCENTAGE_TRADE_MG = Account_TRADE_MG / Account_ALL_MG
PERCENTAGE_TRADE_VIP = Account_TRADE_VIP / Account_ALL_VIP

PERCENTAGE_INTEREST_MASS = Account_INTEREST_MASS / Account_ALL_MASS
PERCENTAGE_INTEREST_MG = Account_INTEREST_MG / Account_ALL_MG
PERCENTAGE_INTEREST_VIP = Account_INTEREST_VIP / Account_ALL_VIP


PERCENTAGE_FIRSTDEBT_MASS = Account_FIRSTDEBT_MASS / Account_ALL_MASS
PERCENTAGE_FIRSTDEBT_MG = Account_FIRSTDEBT_MG / Account_ALL_MG
PERCENTAGE_FIRSTDEBT_VIP = Account_FIRSTDEBT_VIP / Account_ALL_VIP


# Calculate proportion Account full withdraw (Careby)
mergeT0 = merge (TradeT1, TradeT2, by = "AFACCTNO")
mergeT0 = merge (TradeT0, mergeT0, by = "AFACCTNO")
mergeT0 $ CHURN = ifelse((mergeT0$NAV_END.y > 20*mergeT0$NAV_END) &
                           ((mergeT0$NAV_END.y > 20*mergeT0$NAV_END.x) & (mergeT0$NAV_END.y >=500000))
                         ,"CHURN","RETAIN")
ACCOUNT_CHURN_T0_MASS = length (subset(subset (mergeT0, CAREBY == "MASS"), CHURN == "CHURN")$CHURN)
ACCOUNT_CHURN_T0_MG = length (subset(subset (mergeT0, CAREBY == "MG"), CHURN == "CHURN")$CHURN)
ACCOUNT_CHURN_T0_VIP = length (subset(subset (mergeT0, CAREBY == "VIP"), CHURN == "CHURN")$CHURN)

PERCENTAGE_CHURN_T0_MASS = ACCOUNT_CHURN_T0_MASS / Account_ALL_MASS [3]
PERCENTAGE_CHURN_T0_MG = ACCOUNT_CHURN_T0_MASS / Account_ALL_MG [3]
PERCENTAGE_CHURN_T0_VIP = ACCOUNT_CHURN_T0_MASS / Account_ALL_VIP [3]

# Calculate 90% Percenticle NAV
TOP_90PERCENTILE_MASS = c()
TOP_90PERCENTILE_MASS [1] = quantile (subset(TradeT0, CAREBY == "MASS")$NAV_END, 0.9, na.rm = "TRUE" )
TOP_90PERCENTILE_MASS [2] = quantile (subset(TradeT1, CAREBY == "MASS")$NAV_END, 0.9, na.rm = "TRUE" )
TOP_90PERCENTILE_MASS [3] = quantile (subset(TradeT2, CAREBY == "MASS")$NAV_END, 0.9, na.rm = "TRUE" )
TOP_90PERCENTILE_MASS [4] = quantile (subset(TradeT12, CAREBY == "MASS")$NAV_END, 0.9, na.rm = "TRUE")

TOP_90PERCENTILE_MG = c()
TOP_90PERCENTILE_MG [1] = quantile (subset(TradeT0, CAREBY == "MG")$NAV_END, 0.9, na.rm = "TRUE" )
TOP_90PERCENTILE_MG [2] = quantile (subset(TradeT1, CAREBY == "MG")$NAV_END, 0.9, na.rm = "TRUE" )
TOP_90PERCENTILE_MG [3] = quantile (subset(TradeT2, CAREBY == "MG")$NAV_END, 0.9, na.rm = "TRUE" )
TOP_90PERCENTILE_MG [4] = quantile (subset(TradeT12, CAREBY == "MG")$NAV_END, 0.9, na.rm = "TRUE")

TOP_90PERCENTILE_VIP = c()
TOP_90PERCENTILE_VIP [1] = quantile (subset(TradeT0, CAREBY == "VIP")$NAV_END, 0.9, na.rm = "TRUE" )
TOP_90PERCENTILE_VIP [2] = quantile (subset(TradeT1, CAREBY == "VIP")$NAV_END, 0.9, na.rm = "TRUE" )
TOP_90PERCENTILE_VIP [3] = quantile (subset(TradeT2, CAREBY == "VIP")$NAV_END, 0.9, na.rm = "TRUE" )
TOP_90PERCENTILE_VIP [4] = quantile (subset(TradeT12, CAREBY == "VIP")$NAV_END, 0.9, na.rm = "TRUE")

# Calculate NAV/Trade Fee/Interest top 10% (Careby)
TOP_NAV_MASS = c()
TOP_NAV_MASS[1] = sum (subset (subset(TradeT0, CAREBY == "MASS"), NAV_END >= TOP_90PERCENTILE_MASS[1]) $NAV_END)
TOP_NAV_MASS[2] = sum (subset (subset(TradeT1, CAREBY == "MASS"), NAV_END >= TOP_90PERCENTILE_MASS[2]) $NAV_END)
TOP_NAV_MASS[3] = sum (subset (subset(TradeT2, CAREBY == "MASS"), NAV_END >= TOP_90PERCENTILE_MASS[3]) $NAV_END)
TOP_NAV_MASS[4] = sum (subset (subset(TradeT12, CAREBY == "MASS"), NAV_END >= TOP_90PERCENTILE_MASS[4]) $NAV_END)

TOP_FEE_MASS = c()
TOP_FEE_MASS[1] = sum (subset (subset(TradeT0, CAREBY == "MASS"), NAV_END >= TOP_90PERCENTILE_MASS[1]) $FEE)
TOP_FEE_MASS[2] = sum (subset (subset(TradeT1, CAREBY == "MASS"), NAV_END >= TOP_90PERCENTILE_MASS[2]) $FEE)
TOP_FEE_MASS[3] = sum (subset (subset(TradeT2, CAREBY == "MASS"), NAV_END >= TOP_90PERCENTILE_MASS[3]) $FEE)
TOP_FEE_MASS[4] = sum (subset (subset(TradeT12, CAREBY == "MASS"), NAV_END >= TOP_90PERCENTILE_MASS[4]) $FEE)

TOP_INTEREST_MASS = c()
TOP_INTEREST_MASS[1] = sum (subset (subset(TradeT0, CAREBY == "MASS"), NAV_END >= TOP_90PERCENTILE_MASS[1]) $INTEREST)
TOP_INTEREST_MASS[2] = sum (subset (subset(TradeT1, CAREBY == "MASS"), NAV_END >= TOP_90PERCENTILE_MASS[2]) $INTEREST)
TOP_INTEREST_MASS[3] = sum (subset (subset(TradeT2, CAREBY == "MASS"), NAV_END >= TOP_90PERCENTILE_MASS[3]) $INTEREST)
TOP_INTEREST_MASS[4] = sum (subset (subset(TradeT12, CAREBY == "MASS"), NAV_END >= TOP_90PERCENTILE_MASS[4]) $INTEREST)

TOP_NAV_MG = c()
TOP_NAV_MG[1] = sum (subset (subset(TradeT0, CAREBY == "MG"), NAV_END >= TOP_90PERCENTILE_MG[1]) $NAV_END)
TOP_NAV_MG[2] = sum (subset (subset(TradeT1, CAREBY == "MG"), NAV_END >= TOP_90PERCENTILE_MG[2]) $NAV_END)
TOP_NAV_MG[3] = sum (subset (subset(TradeT2, CAREBY == "MG"), NAV_END >= TOP_90PERCENTILE_MG[3]) $NAV_END)
TOP_NAV_MG[4] = sum (subset (subset(TradeT12, CAREBY == "MG"), NAV_END >= TOP_90PERCENTILE_MG[4]) $NAV_END)

TOP_FEE_MG = c()
TOP_FEE_MG[1] = sum (subset (subset(TradeT0, CAREBY == "MG"), NAV_END >= TOP_90PERCENTILE_MG[1]) $FEE)
TOP_FEE_MG[2] = sum (subset (subset(TradeT1, CAREBY == "MG"), NAV_END >= TOP_90PERCENTILE_MG[2]) $FEE)
TOP_FEE_MG[3] = sum (subset (subset(TradeT2, CAREBY == "MG"), NAV_END >= TOP_90PERCENTILE_MG[3]) $FEE)
TOP_FEE_MG[4] = sum (subset (subset(TradeT12, CAREBY == "MG"), NAV_END >= TOP_90PERCENTILE_MG[4]) $FEE)

TOP_INTEREST_MG = c()
TOP_INTEREST_MG[1] = sum (subset (subset(TradeT0, CAREBY == "MG"), NAV_END >= TOP_90PERCENTILE_MG[1]) $INTEREST)
TOP_INTEREST_MG[2] = sum (subset (subset(TradeT1, CAREBY == "MG"), NAV_END >= TOP_90PERCENTILE_MG[2]) $INTEREST)
TOP_INTEREST_MG[3] = sum (subset (subset(TradeT2, CAREBY == "MG"), NAV_END >= TOP_90PERCENTILE_MG[3]) $INTEREST)
TOP_INTEREST_MG[4] = sum (subset (subset(TradeT12, CAREBY == "MG"), NAV_END >= TOP_90PERCENTILE_MG[4]) $INTEREST)

TOP_NAV_VIP = c()
TOP_NAV_VIP[1] = sum (subset (subset(TradeT0, CAREBY == "VIP"), NAV_END >= TOP_90PERCENTILE_VIP[1]) $NAV_END)
TOP_NAV_VIP[2] = sum (subset (subset(TradeT1, CAREBY == "VIP"), NAV_END >= TOP_90PERCENTILE_VIP[2]) $NAV_END)
TOP_NAV_VIP[3] = sum (subset (subset(TradeT2, CAREBY == "VIP"), NAV_END >= TOP_90PERCENTILE_VIP[3]) $NAV_END)
TOP_NAV_VIP[4] = sum (subset (subset(TradeT12, CAREBY == "VIP"), NAV_END >= TOP_90PERCENTILE_VIP[4]) $NAV_END)

TOP_FEE_VIP = c()
TOP_FEE_VIP[1] = sum (subset (subset(TradeT0, CAREBY == "VIP"), NAV_END >= TOP_90PERCENTILE_VIP[1]) $FEE)
TOP_FEE_VIP[2] = sum (subset (subset(TradeT1, CAREBY == "VIP"), NAV_END >= TOP_90PERCENTILE_VIP[2]) $FEE)
TOP_FEE_VIP[3] = sum (subset (subset(TradeT2, CAREBY == "VIP"), NAV_END >= TOP_90PERCENTILE_VIP[3]) $FEE)
TOP_FEE_VIP[4] = sum (subset (subset(TradeT12, CAREBY == "VIP"), NAV_END >= TOP_90PERCENTILE_VIP[4]) $FEE)

TOP_INTEREST_VIP = c()
TOP_INTEREST_VIP[1] = sum (subset (subset(TradeT0, CAREBY == "VIP"), NAV_END >= TOP_90PERCENTILE_VIP[1]) $INTEREST)
TOP_INTEREST_VIP[2] = sum (subset (subset(TradeT1, CAREBY == "VIP"), NAV_END >= TOP_90PERCENTILE_VIP[2]) $INTEREST)
TOP_INTEREST_VIP[3] = sum (subset (subset(TradeT2, CAREBY == "VIP"), NAV_END >= TOP_90PERCENTILE_VIP[3]) $INTEREST)
TOP_INTEREST_VIP[4] = sum (subset (subset(TradeT12, CAREBY == "VIP"), NAV_END >= TOP_90PERCENTILE_VIP[4]) $INTEREST)

# Calculate proportion Trade Fee/ Interest top 10% (Careby)
PERCENTAGE_TOP_NAV_MASS = TOP_NAV_MASS / Total_NAV_MASS
PERCENTAGE_TOP_FEE_MASS = TOP_FEE_MASS / Total_TRADE_FEE_MASS
PERCENTAGE_TOP_INTEREST_MASS = TOP_INTEREST_MASS / Total_INTEREST_MASS

PERCENTAGE_TOP_NAV_MG = TOP_NAV_MG / Total_NAV_MG
PERCENTAGE_TOP_FEE_MG = TOP_FEE_MG / Total_TRADE_FEE_MG
PERCENTAGE_TOP_INTEREST_MG = TOP_INTEREST_MG / Total_INTEREST_MG

PERCENTAGE_TOP_NAV_VIP = TOP_NAV_VIP / Total_NAV_VIP
PERCENTAGE_TOP_FEE_VIP = TOP_FEE_VIP / Total_TRADE_FEE_VIP
PERCENTAGE_TOP_INTEREST_VIP = TOP_INTEREST_VIP / Total_INTEREST_VIP

# Calculate proportion Full Withdraw top 10% (Careby)
TOP_CHURN_T0_MASS  = length (subset ( subset (subset(mergeT0, CAREBY == "MASS"), CHURN == "CHURN"), NAV_END.y >= TOP_90PERCENTILE_MASS[3]) $CHURN)
TOP_CHURN_T0_MG  = length (subset ( subset (subset(mergeT0, CAREBY == "MG"), CHURN == "CHURN"), NAV_END.y >= TOP_90PERCENTILE_MG[3]) $CHURN)
TOP_CHURN_T0_VIP  = length (subset ( subset (subset(mergeT0, CAREBY == "VIP"), CHURN == "CHURN"), NAV_END.y >= TOP_90PERCENTILE_VIP[3]) $CHURN)

PERCENTAGE_TOP_CHURN_T0_MASS = TOP_CHURN_T0_MASS / Account_ALL_MASS [3]
PERCENTAGE_TOP_CHURN_T0_MG = TOP_CHURN_T0_MG / Account_ALL_MG[3]
PERCENTAGE_TOP_CHURN_T0_VIP = TOP_CHURN_T0_VIP / Account_ALL_VIP[3]

output = data.frame()
output [1,1] = ""
output [1,2] = ""
output [1,3] = ""
output [1,4] = ""
output [1,5] = ""
output [1,6] = ""
colnames (output) = c('Indicators','CAREBY','T_0','T_1','T_2','T_12')

output = rbind (output,c('Tong NAV','MASS',Total_NAV_MASS))
output = rbind (output,c('','MG',Total_NAV_MG))
output = rbind (output,c('','VIP',Total_NAV_VIP))

output = rbind (output,c('Tong so TK','MASS',Account_ALL_MASS))
output = rbind (output,c('','MG',Account_ALL_MG))
output = rbind (output,c('','VIP',Account_ALL_VIP))

output = rbind (output,c('Tong so TK co GD trong thang','MASS',Account_TRADE_MASS))
output = rbind (output,c('','MG',Account_TRADE_MG))
output = rbind (output,c('','VIP',Account_TRADE_VIP))

output = rbind (output,c('Ti le TK co GD trong thang','MASS',PERCENTAGE_TRADE_MASS))
output = rbind (output,c('','MG',PERCENTAGE_TRADE_MG))
output = rbind (output,c('','VIP',PERCENTAGE_TRADE_VIP))

output = rbind (output,c('Tong gia tri GD trong thang','MASS',Total_TRADE_MASS))
output = rbind (output,c('','MG',Total_TRADE_MG))
output = rbind (output,c('','VIP',Total_TRADE_VIP))

output = rbind (output,c('Tong phi GD trong thang','MASS',Total_TRADE_FEE_MASS))
output = rbind (output,c('','MG',Total_TRADE_FEE_MG))
output = rbind (output,c('','VIP',Total_TRADE_FEE_VIP))

output = rbind (output,c('Phi GD cua KH dai dien','MASS',Median_FEE_MASS))
output = rbind (output,c('','MG',Median_FEE_MG))
output = rbind (output,c('','VIP',Median_FEE_VIP))

output = rbind (output,c('Ti le su dung STPC','MASS',PERCENTAGE_INTEREST_MASS))
output = rbind (output,c('','MG',PERCENTAGE_INTEREST_MG))
output = rbind (output,c('','VIP',PERCENTAGE_INTEREST_VIP))

output = rbind (output,c('Ti le su dung SPTC lan dau','MASS',PERCENTAGE_FIRSTDEBT_MASS))
output = rbind (output,c('','MG',PERCENTAGE_FIRSTDEBT_MG))
output = rbind (output,c('','VIP',PERCENTAGE_FIRSTDEBT_VIP))

output = rbind (output,c('Doanh thu tu SPTC','MASS',Total_INTEREST_MASS))
output = rbind (output,c('','MG',Total_INTEREST_MG))
output = rbind (output,c('','VIP',Total_INTEREST_VIP))

output = rbind (output,c('Ti le KH rut trang NAV','MASS',PERCENTAGE_CHURN_T0_MASS,c("","","")))
output = rbind (output,c('','MG',PERCENTAGE_CHURN_T0_MG,c("","","")))
output = rbind (output,c('','VIP',PERCENTAGE_CHURN_T0_VIP,c("","","")))

output = rbind (output,c('Muc thap nhat top 10% NAV','MASS',TOP_90PERCENTILE_MASS))
output = rbind (output,c('','MG',TOP_90PERCENTILE_MG))
output = rbind (output,c('','VIP',TOP_90PERCENTILE_VIP))

output = rbind (output,c('Ti le NAV top 10% / Tong NAV','MASS',PERCENTAGE_TOP_NAV_MASS))
output = rbind (output,c('','MG',PERCENTAGE_TOP_NAV_MG))
output = rbind (output,c('','VIP',PERCENTAGE_TOP_NAV_VIP))

output = rbind (output,c('Tong phi GD top','MASS',TOP_FEE_MASS))
output = rbind (output,c('','MG',TOP_FEE_MG))
output = rbind (output,c('','VIP',TOP_FEE_VIP))

output = rbind (output,c('Ti le phi GD top','MASS',PERCENTAGE_TOP_FEE_MASS))
output = rbind (output,c('','MG',PERCENTAGE_TOP_FEE_MG))
output = rbind (output,c('','VIP',PERCENTAGE_TOP_FEE_VIP))

output = rbind (output,c('Doanh thu SPTC top','MASS',TOP_INTEREST_MASS))
output = rbind (output,c('','MG',TOP_INTEREST_MG))
output = rbind (output,c('','VIP',TOP_INTEREST_VIP))

output = rbind (output,c('Ti le doanh thu SPTC top','MASS',PERCENTAGE_TOP_INTEREST_MASS))
output = rbind (output,c('','MG',PERCENTAGE_TOP_INTEREST_MG))
output = rbind (output,c('','VIP',PERCENTAGE_TOP_INTEREST_VIP))

output = rbind (output,c('Ti le rut trang NAV top','MASS',PERCENTAGE_TOP_CHURN_T0_MASS,c("","","")))
output = rbind (output,c('','MG',PERCENTAGE_TOP_CHURN_T0_MG,c("","","")))
output = rbind (output,c('','VIP',PERCENTAGE_TOP_CHURN_T0_VIP,c("","","")))

#output = rbind (output,c('','',))

#WriteToFile
write.table(output,file = 'sampletrade.csv',row.names = FALSE, sep = "")

Threshold_Asset =   10^5
Threshold_Trade =   10^5
Threshold_MVC =  5*10^9

mergeT0 $ CUSTOMER = ifelse (mergeT0$STATUS == "ACTIVE", 'Y', 'N')
mergeT0 $ ASSET = ifelse (mergeT0$NAV_END > Threshold_Asset, 'Y', 'N')
mergeT0 $ TRADE = ifelse (mergeT0$GTGD > Threshold_Trade, 'Y', 'N')
mergeT0 $ MVC = ifelse (mergeT0$GTGD > Threshold_MVC, 'Y', 'N')
mergeT0 $ ENTER_CUSTOMER = ifelse (mergeT0$STATUS.x == "CLOSE" & mergeT0$STATUS == "ACTIVE" ,'Y','N')
mergeT0 $ ENTER_ASSET = ifelse    (mergeT0$NAV_END.x <= Threshold_Asset & mergeT0$NAV_END > Threshold_Asset ,'Y','N')
mergeT0 $ ENTER_TRADE = ifelse    (mergeT0$GTGD.x    <= Threshold_Trade & mergeT0$GTGD    > Threshold_Trade ,'Y','N')
mergeT0 $ ENTER_MVC =   ifelse    (mergeT0$GTGD.x    <= Threshold_MVC   & mergeT0$GTGD    > Threshold_MVC   ,'Y','N')

mergeT0 $ CHURN_CUSTOMER = ifelse (mergeT0$STATUS.x == "ACTIVE" & mergeT0$STATUS == "CLOSE" ,'Y','N')
mergeT0 $ CHURN_ASSET = ifelse    (mergeT0$NAV_END.x > Threshold_Asset & mergeT0$NAV_END <= Threshold_Asset ,'Y','N')
mergeT0 $ CHURN_TRADE = ifelse    (mergeT0$GTGD.x    > Threshold_Trade & mergeT0$GTGD    <= Threshold_Trade ,'Y','N')
mergeT0 $ CHURN_MVC =   ifelse    (mergeT0$GTGD.x    > Threshold_MVC   & mergeT0$GTGD    <= Threshold_MVC   ,'Y','N')

table (mergeT0$CAREBY, mergeT0$CUSTOMER)
table (mergeT0$CAREBY, mergeT0$ASSET)
table (mergeT0$CAREBY, mergeT0$TRADE)
table (mergeT0$CAREBY, mergeT0$MVC)

table (mergeT0$CAREBY, mergeT0$CHURN_CUSTOMER)
table (mergeT0$CAREBY, mergeT0$CHURN_ASSET)
table (mergeT0$CAREBY, mergeT0$CHURN_TRADE)
table (mergeT0$CAREBY, mergeT0$CHURN_MVC)

table (mergeT0$CAREBY, mergeT0$ENTER_CUSTOMER)
table (mergeT0$CAREBY, mergeT0$ENTER_ASSET)
table (mergeT0$CAREBY, mergeT0$ENTER_TRADE)
table (mergeT0$CAREBY, mergeT0$ENTER_MVC)



length(mergeT0$CAREBY)
length(mergeT0$ASSET)

nrow(subset(mergeT0,STATUS=="ACTIVE" ))

CHURN_CUSTOMER = nrow(subset(subset(mergeT0,STATUS.x == "ACTIVE"), STATUS == "CLOSE"))
CHURN_ASSET = nrow(subset(subset(mergeT0,NAV_END.x > Threshold_Asset), NAV_END <= Threshold_Asset))
CHURN_TRADE = nrow(subset(subset(mergeT0,GTGD.x > Threshold_Trade), GTGD <= Threshold_Trade))
CHURN_MVC = nrow(subset(subset(mergeT0,GTGD.x > Threshold_MVC), GTGD <= Threshold_MVC))

ENTER_CUSTOMER = nrow(subset(subset(mergeT0,STATUS.x == "CLOSE"), STATUS == "ACTIVE"))
ENTER_ASSET = nrow(subset(subset(mergeT0,NAV_END.x <= Threshold_Asset), NAV_END > Threshold_Asset))
ENTER_TRADE = nrow(subset(subset(mergeT0,GTGD.x <= Threshold_Trade), GTGD > Threshold_Trade))
ENTER_MVC = nrow(subset(subset(mergeT0,GTGD.x <= Threshold_MVC), GTGD > Threshold_MVC))

GROUP_CUSTOMER = nrow(subset(mergeT0,STATUS == "ACTIVE"))
GROUP_ASSET = nrow(subset(mergeT0,NAV_END >Threshold_Asset))
GROUP_TRADE = nrow(subset(mergeT0,GTGD >Threshold_Trade))
GROUP_MVC = nrow (subset(mergeT0,GTGD >Threshold_MVC))



