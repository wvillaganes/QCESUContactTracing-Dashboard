CCT <- read_excel(path = "C:/Users/QCESU - WILSON/Documents/CESU MDB FILES/New priority cases 4.0.xlsx", sheet = "CCT")

CCT$Timestamp <- as.Date(CCT$Timestamp, "%m/%d/%Y %H:%M:%S")

names(CCT)
attach(CCT)
table(Timestamp, `REPORTING OF`)
