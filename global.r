suppressMessages(library(shinydashboard))
#library(data.table)
suppressMessages(library(openxlsx))
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
suppressMessages(library(stringr))
suppressMessages(library(fuzzyjoin))
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
  f <- list(family = "Courier New, monospace",size = 18,color = "#7f7f7f")
   KeywordMapping = read.xlsx("KeywordMapping.xlsx",sheet = 1,startRow = 1, colNames = TRUE,detectDates = TRUE,rowNames = FALSE)
  # OppOpen = read.xlsx("open.xlsx",sheet = 1,startRow = 1, colNames = TRUE,detectDates = TRUE,rowNames = FALSE)
  files <- list.files(path ="D:/STUDIES/01_DataAnalytics/CBAnalytics/Data",pattern = ".CSV")
  temp <- lapply(files, fread,sep=",",na.strings="")
  AccDB <- rbindlist(temp)
  #Column based duplicate remove because special CHARS in booking text makes difference
  
  # Remove rows with 0
  AccDB<-AccDB[!(AccDB$Amount==0),]
  #Change column names
  OldNames=c("Transaction date","Value date","Transaction type","Booking text")
  NewNames=c("TransactionDate","ValueDate","TransactionType","BookingText")
  colnames(AccDB)[which(colnames(AccDB) %in% OldNames )] <- NewNames
  AccDB = AccDB %>%
    distinct(TransactionDate,ValueDate,Amount,Company, .keep_all = TRUE)
 
  #remove temp names
  rm(OldNames,NewNames)
  #
  #Change Dates
  as.Date(AccDB$TransactionDate, "%d.%m.%Y")
  as.Date(AccDB$ValueDate, "%d.%m.%Y")
  tempDate=dmy(AccDB$TransactionDate)
  AccDB$TransactionYear= year(tempDate)
  AccDB$TransactionMonth= month(tempDate,label = TRUE)
  rm(tempDate)
  #
 
  
  # InsuranceExpenses=
  #
  AccDB$Category=ifelse(AccDB$Amount < 0,'Expense','Income')
  AccDB$Amount=abs(AccDB$Amount)
  # To remove mentioned columns
  AccDB[, c("Account of initiator","IBAN of account of initiator","Bank code of account of initiator"):=NULL]

  
  AccDB=AccDB %>% fuzzy_inner_join(KeywordMapping, by = c("BookingText" = "SearchKeyword"), match_fun = str_detect)
  
  # AccDB$Company=ifelse(grepl("*Interest income*",AccDB$Category,ignore.case = TRUE),'Commerz Bank',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Bank Fees & Service Fees*",AccDB$Category,ignore.case = TRUE),'Commerz Bank',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*DM FIL*",AccDB$BookingText,ignore.case = TRUE),'DM',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*EDEKA*",AccDB$BookingText,ignore.case = TRUE),'EDEKA',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*T.S.*",AccDB$BookingText,ignore.case = TRUE),'T.S.Foods',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*KARSTADT*",AccDB$BookingText,ignore.case = TRUE),'KARSTADT',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*BVG*",AccDB$BookingText,ignore.case = TRUE),'BVG',AccDB$Company)
  # #BVG Fine
  # AccDB$Company=ifelse(grepl("*Infoscore Forderungsmana*",AccDB$BookingText,ignore.case = TRUE),'BVG',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*S Bahn*",AccDB$BookingText,ignore.case = TRUE),'DB',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*DB AUTOMAT*",AccDB$BookingText,ignore.case = TRUE),'DB',AccDB$Company)
  # 
  # AccDB$Company=ifelse(grepl("*VATTENFALL *",AccDB$BookingText,ignore.case = TRUE),'VATTENFALL',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*LIDL*",AccDB$BookingText,ignore.case = TRUE),'LIDL',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*ALDI*",AccDB$BookingText,ignore.case = TRUE),'ALDI',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*MCDONALD*",AccDB$BookingText,ignore.case = TRUE),'MCDONALD',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Kaufland*",AccDB$BookingText,ignore.case = TRUE),'KAUFLAND',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Apotheke*",AccDB$BookingText,ignore.case = TRUE),'APOTHEKE',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*GIDA*",AccDB$BookingText,ignore.case = TRUE),'EURO GIDA',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Telefonica Germany GmbH*",AccDB$BookingText,ignore.case = TRUE),'O2 Blau',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*AMAZON*",AccDB$BookingText,ignore.case = TRUE),'AMAZON',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*9908409001*",AccDB$BookingText,ignore.case = TRUE),'SONY TV EMI',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*ENVIVAS*",AccDB$BookingText,ignore.case = TRUE),'TK (ENVIVAS)',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*REWE*",AccDB$BookingText,ignore.case = TRUE),'REWE',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Kath. Kirchengemeinde*",AccDB$BookingText,ignore.case = TRUE),'KITA',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*DEKRA*",AccDB$BookingText,ignore.case = TRUE),'DEKRA',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Schindler*",AccDB$BookingText,ignore.case = TRUE),'Schindler',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*DEICHMANN*",AccDB$BookingText,ignore.case = TRUE),'DEICHMANN',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Get Grocery*",AccDB$BookingText,ignore.case = TRUE),'Get Grocery',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*PRIMARK*",AccDB$BookingText,ignore.case = TRUE),'PRIMARK',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*SATURN*",AccDB$BookingText,ignore.case = TRUE),'SATURN',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Bundesagentur*",AccDB$BookingText,ignore.case = TRUE),'BURGERAMT',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*TEDi*",AccDB$BookingText,ignore.case = TRUE),'TEDi',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*KIEFHOLZ*",AccDB$BookingText,ignore.case = TRUE),'Badminton Club',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Decathlon*",AccDB$BookingText,ignore.case = TRUE),'Decathlon',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Transferwise*",AccDB$BookingText,ignore.case = TRUE),'India Citibank',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Fundus*",AccDB$BookingText,ignore.case = TRUE),'G+B Housing',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Lush*",AccDB$BookingText,ignore.case = TRUE),'LUSH',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Deutsche Bank*",AccDB$BookingText,ignore.case = TRUE),'Deutsche Bank',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*BLZ10040000*",AccDB$BookingText,ignore.case = TRUE),'Commerz Bank',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*5621562403*",AccDB$BookingText,ignore.case = TRUE),'HOME Internet',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*LEISTENSCHNEIDER*",AccDB$BookingText,ignore.case = TRUE),'PHOTO STUDIO',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Alphacomm*",AccDB$BookingText,ignore.case = TRUE),'Lyca Recharge',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Prasanna*",AccDB$BookingText,ignore.case = TRUE),'Friends',AccDB$Company)
  # 
  # AccDB$Company=ifelse(grepl("*Sudhasine*",AccDB$BookingText,ignore.case = TRUE),'Friends',AccDB$Company)
  # 
  # AccDB$Company=ifelse(grepl("*Arun*",AccDB$BookingText,ignore.case = TRUE),'Friends',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*BUVANESWARAN*",AccDB$BookingText,ignore.case = TRUE),'Friends',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Subhas Dandapani*",AccDB$BookingText,ignore.case = TRUE),'Friends',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Reena Devi Sonniya Kuppusamy*",AccDB$BookingText,ignore.case = TRUE),'Friends',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Ashok*",AccDB$BookingText,ignore.case = TRUE),'Friends',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Woolworth*",AccDB$BookingText,ignore.case = TRUE),'Woolworth',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Provinzial*",AccDB$BookingText,ignore.case = TRUE),'Provinzial',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*NIRWANA *",AccDB$BookingText,ignore.case = TRUE),'NIRWANA',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Confiserie Reichert*",AccDB$BookingText,ignore.case = TRUE),'Confiserie CAKE SHOP',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*NIKE00550*",AccDB$BookingText,ignore.case = TRUE),'NIKE',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Toys R US*",AccDB$BookingText,ignore.case = TRUE),'Toys R US',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*MYRA*",AccDB$BookingText,ignore.case = TRUE),'MYRA',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Abschnitt A II *",AccDB$BookingText,ignore.case = TRUE),'',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Kartenzahlung CA*",AccDB$BookingText,ignore.case = TRUE),'C&A',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*HOTEL ALFONSO*",AccDB$BookingText,ignore.case = TRUE),'HOTEL ALFONSO',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*mister.lady*",AccDB$BookingText,ignore.case = TRUE),'Mister Lady',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*EINZAHLAUTOMAT *",AccDB$BookingText,ignore.case = TRUE),'COIN DEPOSIT',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*PENNY*",AccDB$BookingText,ignore.case = TRUE),'PENNY',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*YELLOW POINT*",AccDB$BookingText,ignore.case = TRUE),'YELLOW POINT',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*FedEx*",AccDB$BookingText,ignore.case = TRUE),'FedEx',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*ULLRICH*",AccDB$BookingText,ignore.case = TRUE),'ULLRICH',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*RF19X658505321*",AccDB$BookingText,ignore.case = TRUE),'ARD',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*RF43X677966775*",AccDB$BookingText,ignore.case = TRUE),'ARD',AccDB$Company)
  # 
  # AccDB$Company=ifelse(grepl("*ATHI DEVI ASIA*",AccDB$BookingText,ignore.case = TRUE),'Leinstr. IND SHOP',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Global Collect BV*",AccDB$BookingText,ignore.case = TRUE),'Air France',AccDB$Company)
  # 
  # AccDB$Company=ifelse(grepl("*CYBERPORT*",AccDB$BookingText,ignore.case = TRUE),'CYBERPORT',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*BIO COMPANY*",AccDB$BookingText,ignore.case = TRUE),'BIO COMPANY',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*CINESTAR*",AccDB$BookingText,ignore.case = TRUE),'CINESTAR',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*KLARNA AB Wish*",AccDB$BookingText,ignore.case = TRUE),'WISH',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*KLARNA*",AccDB$BookingText,ignore.case = TRUE),'Klarna',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*ROSSMANN*",AccDB$BookingText,ignore.case = TRUE),'ROSSMANN',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*BEZIRKSAMT*",AccDB$BookingText,ignore.case = TRUE),'BURGERAMT',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*E-CENTER*",AccDB$BookingText,ignore.case = TRUE),'EUROPA CENTER',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*KENTUCKY FRIED CHICKEN*",AccDB$BookingText,ignore.case = TRUE),'KFC',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*NETTO MARKEN-DISCOU*",AccDB$BookingText,ignore.case = TRUE),'NETTO',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*LABO 017092203961*",AccDB$BookingText,ignore.case = TRUE),'BURGERAMT',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Martin Kunschert*",AccDB$BookingText,ignore.case = TRUE),'UNKNOWN',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*DP-Tech *",AccDB$BookingText,ignore.case = TRUE),'UNKNOWN',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*ERSATZKARTE*",AccDB$BookingText,ignore.case = TRUE),'Commerz Bank',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*10014015201*",AccDB$BookingText,ignore.case = TRUE),'UNKNOWN WITHDRAWL',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*MAC GEIZ*",AccDB$BookingText,ignore.case = TRUE),'MAC GEIZ',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*NATURKUNDE*",AccDB$BookingText,ignore.case = TRUE),'NATURKUNDE MUSEUM',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*ALNATURA*",AccDB$BookingText,ignore.case = TRUE),'ALNATURA',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Heinemann*",AccDB$BookingText,ignore.case = TRUE),'Heinemann',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*NORMA*",AccDB$BookingText,ignore.case = TRUE),'NORMA',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*SPORTHAUS OLYMPIA*",AccDB$BookingText,ignore.case = TRUE),'OLYMPIA',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*GA 2078*",AccDB$BookingText,ignore.case = TRUE),'OTHER BANK',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*IQOPTION*",AccDB$BookingText,ignore.case = TRUE),'IQ OPTION',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Media Markt *",AccDB$BookingText,ignore.case = TRUE),'Media Markt',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*NAGARAJAN RAMANATHAN Zurueck*",AccDB$BookingText,ignore.case = TRUE),'NAGARAJ Housing',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*NAGARAJAN RAMANATHAN Kaution zurueck*",AccDB$BookingText,ignore.case = TRUE),'NAGARAJ Housing',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Deutsche Post*",AccDB$BookingText,ignore.case = TRUE),'Deutsche Post',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*POCO*",AccDB$BookingText,ignore.case = TRUE),'POCO',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*OBI BAU*",AccDB$BookingText,ignore.case = TRUE),'OBI BAU',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Nagarajan Ramanathan BYLADEM1001*",AccDB$BookingText,ignore.case = TRUE),'NAGARAJ Housing',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Tankstelle *",AccDB$BookingText,ignore.case = TRUE),'Petrol Bunk',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*GS 40000974*",AccDB$BookingText,ignore.case = TRUE),'O2 Blau',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*LITTLE JOHN BIKES *",AccDB$BookingText,ignore.case = TRUE),'LITTLE JOHN BIKES ',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*G7751L2X107045373*",AccDB$BookingText,ignore.case = TRUE),'Lufthansa',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Immobilien Scout GmbH*",AccDB$BookingText,ignore.case = TRUE),'Immobilien Scout',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*Postbank*",AccDB$BookingText,ignore.case = TRUE),'Postbank',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*109234500*",AccDB$BookingText,ignore.case = TRUE),'Commerz Bank',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*10010060089*",AccDB$BookingText,ignore.case = TRUE),'G+B Housing',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*PFENNIGLAND*",AccDB$BookingText,ignore.case = TRUE),'PFENNIGLAND',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*IKEA*",AccDB$BookingText,ignore.case = TRUE),'IKEA',AccDB$Company)
  # AccDB$Company=ifelse(grepl("*DE12100400000109234501*",AccDB$BookingText,ignore.case = TRUE),'G+B Housing',AccDB$Company)
  # 
  #Exceptions
  # Reporting ticket amount
  AccDB$Company=ifelse(AccDB$Amount==1490.55,'Lufthansa',AccDB$Company)
  
  #Estimate Inputs
  ValTransactionYear=unique(AccDB$TransactionYear[!is.na(AccDB$TransactionYear)])
  ValTransactionMonth=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  StandardExpenses=c('G+B Housing','India Citibank','Commerz Bank','HOME Internet','VATTENFALL','Deutsche Bank','Post Bank','KITA','BVG')
  tmp = AccDB %>% filter(!Company %in% StandardExpenses) %>% select(Company) %>% unique() %>% arrange(Company)
  IrregularExpenses=tmp$Company
  #
  rm(tmp)
  rm(temp)  
  #
  #
  
}
