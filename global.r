suppressMessages(library(shinydashboard))
#library(data.table)
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(ggmap))
suppressMessages(library(data.table))
suppressMessages(library(plotly))
suppressMessages(library(leaflet))
suppressMessages(library(htmltools))
suppressMessages(library(RColorBrewer))
suppressMessages(library(threejs))
suppressMessages(library(DT))
suppressMessages(library(shiny))
suppressMessages(library(lubridate))
suppressMessages(library(tidyverse))




if (dir.exists("D:/STUDIES/01_DataAnalytics/CBAnalytics/Data"))
{
  setwd("D:/STUDIES/01_DataAnalytics/CBAnalytics/Data")
}

# if (dir.exists("/media/chilambuselvan/Studies & OS files/Official/KEICRMreporting/CRMReporting/Data"))
# {
#   setwd("/media/chilambuselvan/Studies & OS fgetiles/Official/KEICRMreporting/CRMReporting/Data")
# }
# 
# if (dir.exists("D:/Reporting/KEICRMreporting/CRMReporting/Data"))
# {
#   setwd("D:/Reporting/KEICRMreporting/CRMReporting/Data")
# }

#setwd("F:/Official/KEICRMreporting/CRMReporting/Data")

if (!exists("AccDB"))
{
  # OppClosed = read.xlsx("closed.xlsx",sheet = 1,startRow = 1, colNames = TRUE,detectDates = TRUE,rowNames = FALSE)
  # OppOpen = read.xlsx("open.xlsx",sheet = 1,startRow = 1, colNames = TRUE,detectDates = TRUE,rowNames = FALSE)
  files <- list.files(path ="D:/STUDIES/01_DataAnalytics/CBAnalytics/Data",pattern = ".CSV")
  temp <- lapply(files, fread,sep=",",na.strings="")
  AccDB <- rbindlist(temp)
  AccDB= unique(AccDB)
  

  
  #Change Dates
  as.Date(AccDB$`Transaction date`, "%d.%m.%Y")
  as.Date(AccDB$`Value date`, "%d.%m.%Y")
  tempDate=dmy(AccDB$`Transaction date`)
  AccDB$TransactionYear= year(tempDate)
  AccDB$TransactionMonth= month(tempDate,label = TRUE)
  rm(tempDate)
  #
  #Estimate Inputs
  ValTransactionYear=unique(AccDB$TransactionYear[!is.na(AccDB$TransactionYear)])
  ValTransactionMonth=c("Jan","Feb","Mar","Apr","May","June","Jul","Aug","Sep","Oct","Nov","Dec")
  #
  
 
  # To remove mentioned columns
  AccDB[, c("Account of initiator","IBAN of account of initiator","Bank code of account of initiator"):=NULL]
  #
  AccDB$Company= AccDB$`Booking text`
  AccDB$Company=ifelse(grepl("*Interest income*",AccDB$Category,ignore.case = TRUE),'Commerz Bank',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Bank Fees & Service Fees*",AccDB$Category,ignore.case = TRUE),'Commerz Bank',AccDB$Company)
  AccDB$Company=ifelse(grepl("*DM FIL*",AccDB$`Booking text`,ignore.case = TRUE),'DM',AccDB$Company)
  AccDB$Company=ifelse(grepl("*EDEKA*",AccDB$`Booking text`,ignore.case = TRUE),'EDEKA',AccDB$Company)
  AccDB$Company=ifelse(grepl("*T.S.*",AccDB$`Booking text`,ignore.case = TRUE),'T.S.Foods',AccDB$Company)
  AccDB$Company=ifelse(grepl("*KARSTADT*",AccDB$`Booking text`,ignore.case = TRUE),'KARSTADT',AccDB$Company)
  AccDB$Company=ifelse(grepl("*BVG*",AccDB$`Booking text`,ignore.case = TRUE),'BVG',AccDB$Company)
  AccDB$Company=ifelse(grepl("*S Bahn*",AccDB$`Booking text`,ignore.case = TRUE),'DB',AccDB$Company)
  AccDB$Company=ifelse(grepl("*VATTENFALL *",AccDB$`Booking text`,ignore.case = TRUE),'VATTENFALL',AccDB$Company)
  AccDB$Company=ifelse(grepl("*LIDL*",AccDB$`Booking text`,ignore.case = TRUE),'LIDL',AccDB$Company)
  AccDB$Company=ifelse(grepl("*ALDI*",AccDB$`Booking text`,ignore.case = TRUE),'ALDI',AccDB$Company)
  AccDB$Company=ifelse(grepl("*MCDONALD*",AccDB$`Booking text`,ignore.case = TRUE),'MCDONALD',AccDB$Company)
  AccDB$Company=ifelse(grepl("*GIDA*",AccDB$`Booking text`,ignore.case = TRUE),'EURO GIDA',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Telefonica Germany GmbH*",AccDB$`Booking text`,ignore.case = TRUE),'O2 Blau',AccDB$Company)
  AccDB$Company=ifelse(grepl("*AMAZON*",AccDB$`Booking text`,ignore.case = TRUE),'AMAZON',AccDB$Company)
  AccDB$Company=ifelse(grepl("*9908409001*",AccDB$`Booking text`,ignore.case = TRUE),'SONY TV EMI',AccDB$Company)
  AccDB$Company=ifelse(grepl("*ENVIVAS*",AccDB$`Booking text`,ignore.case = TRUE),'TK (ENVIVAS)',AccDB$Company)
  AccDB$Company=ifelse(grepl("*REWE*",AccDB$`Booking text`,ignore.case = TRUE),'REWE',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Maria Rosenkranz*",AccDB$`Booking text`,ignore.case = TRUE),'KITA',AccDB$Company)
  AccDB$Company=ifelse(grepl("*DEKRA*",AccDB$`Booking text`,ignore.case = TRUE),'DEKRA',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Schindler*",AccDB$`Booking text`,ignore.case = TRUE),'Schindler',AccDB$Company)
  AccDB$Company=ifelse(grepl("*DEICHMANN*",AccDB$`Booking text`,ignore.case = TRUE),'DEICHMANN',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Get Grocery*",AccDB$`Booking text`,ignore.case = TRUE),'Get Grocery',AccDB$Company)
  AccDB$Company=ifelse(grepl("*PRIMARK*",AccDB$`Booking text`,ignore.case = TRUE),'PRIMARK',AccDB$Company)
  AccDB$Company=ifelse(grepl("*SATURN*",AccDB$`Booking text`,ignore.case = TRUE),'SATURN',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Bundesagentur*",AccDB$`Booking text`,ignore.case = TRUE),'BURGERAMT',AccDB$Company)
  AccDB$Company=ifelse(grepl("*TEDi*",AccDB$`Booking text`,ignore.case = TRUE),'TEDi',AccDB$Company)
  AccDB$Company=ifelse(grepl("*KIEFHOLZ*",AccDB$`Booking text`,ignore.case = TRUE),'Badminton Club',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Decathlon*",AccDB$`Booking text`,ignore.case = TRUE),'Decathlon',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Transferwise*",AccDB$`Booking text`,ignore.case = TRUE),'India Citibank',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Fundus*",AccDB$`Booking text`,ignore.case = TRUE),'G+B Housing',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Lush*",AccDB$`Booking text`,ignore.case = TRUE),'LUSH',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Deutsche Bank*",AccDB$`Booking text`,ignore.case = TRUE),'Deutsche Bank',AccDB$Company)
  AccDB$Company=ifelse(grepl("*BLZ10040000*",AccDB$`Booking text`,ignore.case = TRUE),'Commerz Bank',AccDB$Company)
  AccDB$Company=ifelse(grepl("*5621562403*",AccDB$`Booking text`,ignore.case = TRUE),'HOME Internet',AccDB$Company)
  AccDB$Company=ifelse(grepl("*LEISTENSCHNEIDER*",AccDB$`Booking text`,ignore.case = TRUE),'PHOTO STUDIO',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Alphacomm*",AccDB$`Booking text`,ignore.case = TRUE),'Lyca Recharge',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Prasanna*",AccDB$`Booking text`,ignore.case = TRUE),'Friends',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Sudhasine*",AccDB$`Booking text`,ignore.case = TRUE),'Friends',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Arun*",AccDB$`Booking text`,ignore.case = TRUE),'Friends',AccDB$Company)
  AccDB$Company=ifelse(grepl("*BUVANESWARAN*",AccDB$`Booking text`,ignore.case = TRUE),'Friends',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Subhas Dandapani*",AccDB$`Booking text`,ignore.case = TRUE),'Friends',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Reena Devi Sonniya Kuppusamy*",AccDB$`Booking text`,ignore.case = TRUE),'Friends',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Ashok*",AccDB$`Booking text`,ignore.case = TRUE),'Ashok',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Woolworth*",AccDB$`Booking text`,ignore.case = TRUE),'Woolworth',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Provinzial*",AccDB$`Booking text`,ignore.case = TRUE),'Provinzial',AccDB$Company)
  AccDB$Company=ifelse(grepl("*NIRWANA *",AccDB$`Booking text`,ignore.case = TRUE),'NIRWANA',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Confiserie Reichert*",AccDB$`Booking text`,ignore.case = TRUE),'Confiserie CAKE SHOP',AccDB$Company)
  AccDB$Company=ifelse(grepl("*NIKE00550*",AccDB$`Booking text`,ignore.case = TRUE),'NIKE',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Toys R US*",AccDB$`Booking text`,ignore.case = TRUE),'Toys R US',AccDB$Company)
  AccDB$Company=ifelse(grepl("*MYRA*",AccDB$`Booking text`,ignore.case = TRUE),'MYRA',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Abschnitt A II *",AccDB$`Booking text`,ignore.case = TRUE),'',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Kartenzahlung CA*",AccDB$`Booking text`,ignore.case = TRUE),'C&A',AccDB$Company)
  AccDB$Company=ifelse(grepl("*HOTEL ALFONSO*",AccDB$`Booking text`,ignore.case = TRUE),'HOTEL ALFONSO',AccDB$Company)
  AccDB$Company=ifelse(grepl("*mister.lady*",AccDB$`Booking text`,ignore.case = TRUE),'Mister Lady',AccDB$Company)
  AccDB$Company=ifelse(grepl("*EINZAHLAUTOMAT *",AccDB$`Booking text`,ignore.case = TRUE),'COIN DEPOSIT',AccDB$Company)
  AccDB$Company=ifelse(grepl("*PENNY*",AccDB$`Booking text`,ignore.case = TRUE),'PENNY',AccDB$Company)
  AccDB$Company=ifelse(grepl("*YELLOW POINT*",AccDB$`Booking text`,ignore.case = TRUE),'YELLOW POINT',AccDB$Company)
  AccDB$Company=ifelse(grepl("*FedEx*",AccDB$`Booking text`,ignore.case = TRUE),'FedEx',AccDB$Company)
  AccDB$Company=ifelse(grepl("*ULLRICH*",AccDB$`Booking text`,ignore.case = TRUE),'ULLRICH',AccDB$Company)
  AccDB$Company=ifelse(grepl("*RF43X677966775*",AccDB$`Booking text`,ignore.case = TRUE),'ARD',AccDB$Company)
  AccDB$Company=ifelse(grepl("*CYBERPORT*",AccDB$`Booking text`,ignore.case = TRUE),'CYBERPORT',AccDB$Company)
  AccDB$Company=ifelse(grepl("*BIO COMPANY*",AccDB$`Booking text`,ignore.case = TRUE),'BIO COMPANY',AccDB$Company)
  AccDB$Company=ifelse(grepl("*CINESTAR*",AccDB$`Booking text`,ignore.case = TRUE),'CINESTAR',AccDB$Company)
  AccDB$Company=ifelse(grepl("*KLARNA AB Wish*",AccDB$`Booking text`,ignore.case = TRUE),'WISH',AccDB$Company)
  AccDB$Company=ifelse(grepl("*KLARNA*",AccDB$`Booking text`,ignore.case = TRUE),'Klarna',AccDB$Company)
  AccDB$Company=ifelse(grepl("*ROSSMANN*",AccDB$`Booking text`,ignore.case = TRUE),'ROSSMANN',AccDB$Company)
  AccDB$Company=ifelse(grepl("*BEZIRKSAMT*",AccDB$`Booking text`,ignore.case = TRUE),'BURGERAMT',AccDB$Company)
  AccDB$Company=ifelse(grepl("*E-CENTER*",AccDB$`Booking text`,ignore.case = TRUE),'EUROPA CENTER',AccDB$Company)
  AccDB$Company=ifelse(grepl("*KENTUCKY FRIED CHICKEN*",AccDB$`Booking text`,ignore.case = TRUE),'KFC',AccDB$Company)
  AccDB$Company=ifelse(grepl("*NETTO MARKEN-DISCOU*",AccDB$`Booking text`,ignore.case = TRUE),'NETTO',AccDB$Company)
  AccDB$Company=ifelse(grepl("*LABO 017092203961*",AccDB$`Booking text`,ignore.case = TRUE),'BURGERAMT',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Martin Kunschert*",AccDB$`Booking text`,ignore.case = TRUE),'UNKNOWN',AccDB$Company)
  AccDB$Company=ifelse(grepl("*DP-Tech *",AccDB$`Booking text`,ignore.case = TRUE),'UNKNOWN',AccDB$Company)
  AccDB$Company=ifelse(grepl("*ERSATZKARTE*",AccDB$`Booking text`,ignore.case = TRUE),'Commerz Bank',AccDB$Company)
  AccDB$Company=ifelse(grepl("*10014015201*",AccDB$`Booking text`,ignore.case = TRUE),'UNKNOWN WITHDRAWL',AccDB$Company)
  AccDB$Company=ifelse(grepl("*MAC GEIZ*",AccDB$`Booking text`,ignore.case = TRUE),'MAC GEIZ',AccDB$Company)
  AccDB$Company=ifelse(grepl("*NATURKUNDE*",AccDB$`Booking text`,ignore.case = TRUE),'NATURKUNDE MUSEUM',AccDB$Company)
  AccDB$Company=ifelse(grepl("*ALNATURA*",AccDB$`Booking text`,ignore.case = TRUE),'ALNATURA',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Heinemann*",AccDB$`Booking text`,ignore.case = TRUE),'Heinemann',AccDB$Company)
  AccDB$Company=ifelse(grepl("*NORMA*",AccDB$`Booking text`,ignore.case = TRUE),'NORMA',AccDB$Company)
  AccDB$Company=ifelse(grepl("*SPORTHAUS OLYMPIA*",AccDB$`Booking text`,ignore.case = TRUE),'OLYMPIA',AccDB$Company)
  AccDB$Company=ifelse(grepl("*GA 2078*",AccDB$`Booking text`,ignore.case = TRUE),'OTHER BANK',AccDB$Company)
  AccDB$Company=ifelse(grepl("*IQOPTION*",AccDB$`Booking text`,ignore.case = TRUE),'IQ OPTION',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Media Markt *",AccDB$`Booking text`,ignore.case = TRUE),'Media Markt',AccDB$Company)
  AccDB$Company=ifelse(grepl("*NAGARAJAN RAMANATHAN Zurueck*",AccDB$`Booking text`,ignore.case = TRUE),'NAGARAJ Housing',AccDB$Company)
  AccDB$Company=ifelse(grepl("*NAGARAJAN RAMANATHAN Kaution zurueck*",AccDB$`Booking text`,ignore.case = TRUE),'NAGARAJ Housing',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Deutsche Post*",AccDB$`Booking text`,ignore.case = TRUE),'Deutsche Post',AccDB$Company)
  AccDB$Company=ifelse(grepl("*POCO*",AccDB$`Booking text`,ignore.case = TRUE),'POCO',AccDB$Company)
  AccDB$Company=ifelse(grepl("*OBI BAU*",AccDB$`Booking text`,ignore.case = TRUE),'OBI BAU',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Nagarajan Ramanathan BYLADEM1001*",AccDB$`Booking text`,ignore.case = TRUE),'NAGARAJ Housing',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Tankstelle *",AccDB$`Booking text`,ignore.case = TRUE),'Petrol Bunk',AccDB$Company)
  AccDB$Company=ifelse(grepl("*GS 40000974*",AccDB$`Booking text`,ignore.case = TRUE),'O2 Blau',AccDB$Company)
  AccDB$Company=ifelse(grepl("*LITTLE JOHN BIKES *",AccDB$`Booking text`,ignore.case = TRUE),'LITTLE JOHN BIKES ',AccDB$Company)
  AccDB$Company=ifelse(grepl("*G7751L2X107045373*",AccDB$`Booking text`,ignore.case = TRUE),'Lufthansa',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Immobilien Scout GmbH*",AccDB$`Booking text`,ignore.case = TRUE),'Immobilien Scout',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Postbank*",AccDB$`Booking text`,ignore.case = TRUE),'Postbank',AccDB$Company)
  AccDB$Company=ifelse(grepl("*109234500*",AccDB$`Booking text`,ignore.case = TRUE),'Commerz Bank',AccDB$Company)
  AccDB$Company=ifelse(grepl("*10010060089*",AccDB$`Booking text`,ignore.case = TRUE),'G+B Housing',AccDB$Company)
  AccDB$Company=ifelse(grepl("*PFENNIGLAND*",AccDB$`Booking text`,ignore.case = TRUE),'PFENNIGLAND',AccDB$Company)
  AccDB$Company=ifelse(grepl("*IKEA*",AccDB$`Booking text`,ignore.case = TRUE),'IKEA',AccDB$Company)
  AccDB$Company=ifelse(grepl("*DE12100400000109234501*",AccDB$`Booking text`,ignore.case = TRUE),'G+B Housing',AccDB$Company)
  
  rm(temp)
  #ConsolidatedOpp = rbind(OppClosed,OppOpen)
  #Regionpal=c("forestgreen", "darkorange", "deepskyblue", "dimgray")
  ################################################# Preparing Geo Code fetching####################################
  
  ###### Binding Branch & disctrict based on Branchcode OrigOffice
  # BranchCity=unique(ConsolidatedOpp$Branch.Office)
  # BranchCity=BranchCity[!is.na(BranchCity)]
  # Bcity=data.frame(BranchCity,stringsAsFactors = FALSE)
  # Bcity=sub(" MP", "", Bcity$BranchCity, fixed = TRUE)
  # Bcity=unique(Bcity)
  # Bcity=data.frame(Bcity)
  # 
  # geocodes <- geocode(as.character(Bcity$Bcity),output = "more")
  # geocodes = geocodes %>%
  #   select(lat,lon,administrative_area_level_1)
  # Bcity <- data.frame(Bcity[,1],geocodes,stringsAsFactors = FALSE)
  # write.csv(Bcity,file="BranchGeoCode.csv",row.names = FALSE)
  # 
}
