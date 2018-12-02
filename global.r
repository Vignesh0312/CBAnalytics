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
  temp <- lapply(files, fread, sep=",")
  AccDB <- rbindlist(temp)
  AccDB= unique(AccDB)
 
  # AccDB[ , !(names(AccDB) %in% drops)]
  AccDB[, c("Account of initiator","IBAN of account of initiator","Bank code of account of initiator"):=NULL]
  AccDB$Company= AccDB$`Booking text`
  AccDB$Company=ifelse(grepl("*Interest income*",AccDB$Category,ignore.case = TRUE),'Commerz Bank',AccDB$Company)
  AccDB$Company=ifelse(grepl("*DM FIL*",AccDB$Company,ignore.case = TRUE),'DM',AccDB$Company)
  AccDB$Company=ifelse(grepl("*EDEKA*",AccDB$Company,ignore.case = TRUE),'EDEKA',AccDB$Company)
  AccDB$Company=ifelse(grepl("*T.S.*",AccDB$Company,ignore.case = TRUE),'T.S.Foods',AccDB$Company)
  AccDB$Company=ifelse(grepl("*KARSTADT*",AccDB$Company,ignore.case = TRUE),'KARSTADT',AccDB$Company)
  AccDB$Company=ifelse(grepl("*BVG*",AccDB$Company,ignore.case = TRUE),'BVG',AccDB$Company)
  AccDB$Company=ifelse(grepl("*S Bahn*",AccDB$Company,ignore.case = TRUE),'DB',AccDB$Company)
  AccDB$Company=ifelse(grepl("*VATTENFALL *",AccDB$Company,ignore.case = TRUE),'VATTENFALL',AccDB$Company)
  AccDB$Company=ifelse(grepl("*LIDL*",AccDB$Company,ignore.case = TRUE),'LIDL',AccDB$Company)
  AccDB$Company=ifelse(grepl("*ALDI*",AccDB$Company,ignore.case = TRUE),'ALDI',AccDB$Company)
  AccDB$Company=ifelse(grepl("*GIDA*",AccDB$Company,ignore.case = TRUE),'EURO GIDA',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Telefonica Germany GmbH*",AccDB$Company,ignore.case = TRUE),'O2 Blau',AccDB$Company)
  AccDB$Company=ifelse(grepl("*AMAZON*",AccDB$Company,ignore.case = TRUE),'AMAZON',AccDB$Company)
  AccDB$Company=ifelse(grepl("*9908409001*",AccDB$Company,ignore.case = TRUE),'SONY TV EMI',AccDB$Company)
  AccDB$Company=ifelse(grepl("*ENVIVAS*",AccDB$Company,ignore.case = TRUE),'TK (ENVIVAS)',AccDB$Company)
  AccDB$Company=ifelse(grepl("*REWE*",AccDB$Company,ignore.case = TRUE),'REWE',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Maria Rosenkranz*",AccDB$Company,ignore.case = TRUE),'KITA',AccDB$Company)
  AccDB$Company=ifelse(grepl("*DEKRA*",AccDB$Company,ignore.case = TRUE),'DEKRA',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Schindler*",AccDB$Company,ignore.case = TRUE),'Schindler',AccDB$Company)
  AccDB$Company=ifelse(grepl("*DEICHMANN*",AccDB$Company,ignore.case = TRUE),'DEICHMANN',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Get Grocery*",AccDB$Company,ignore.case = TRUE),'Get Grocery',AccDB$Company)
  AccDB$Company=ifelse(grepl("*PRIMARK*",AccDB$Company,ignore.case = TRUE),'PRIMARK',AccDB$Company)
  AccDB$Company=ifelse(grepl("*SATURN*",AccDB$Company,ignore.case = TRUE),'SATURN',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Bundesagentur*",AccDB$Company,ignore.case = TRUE),'BURGERAMT',AccDB$Company)
  AccDB$Company=ifelse(grepl("*TEDi*",AccDB$Company,ignore.case = TRUE),'TEDi',AccDB$Company)
  AccDB$Company=ifelse(grepl("*KIEFHOLZ*",AccDB$Company,ignore.case = TRUE),'Badminton Club',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Decathlon*",AccDB$Company,ignore.case = TRUE),'Decathlon',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Transferwise*",AccDB$Company,ignore.case = TRUE),'India Citibank',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Fundus*",AccDB$Company,ignore.case = TRUE),'G+B Housing',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Lush*",AccDB$Company,ignore.case = TRUE),'LUSH',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Deutsche Bank*",AccDB$Company,ignore.case = TRUE),'Deutsche Bank',AccDB$Company)
  AccDB$Company=ifelse(grepl("*BLZ10040000*",AccDB$Company,ignore.case = TRUE),'Commerz Bank',AccDB$Company)
  AccDB$Company=ifelse(grepl("*5621562403*",AccDB$Company,ignore.case = TRUE),'HOME Internet',AccDB$Company)
  AccDB$Company=ifelse(grepl("*LEISTENSCHNEIDER*",AccDB$Company,ignore.case = TRUE),'PHOTO STUDIO',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Alphacomm*",AccDB$Company,ignore.case = TRUE),'Lyca Recharge',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Prasanna*",AccDB$Company,ignore.case = TRUE),'Friends',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Sudhasine*",AccDB$Company,ignore.case = TRUE),'Friends',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Arun*",AccDB$Company,ignore.case = TRUE),'Friends',AccDB$Company)
  AccDB$Company=ifelse(grepl("*BUVANESWARAN*",AccDB$Company,ignore.case = TRUE),'Friends',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Subhas Dandapani*",AccDB$Company,ignore.case = TRUE),'Friends',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Reena Devi Sonniya Kuppusamy*",AccDB$Company,ignore.case = TRUE),'Friends',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Ashok*",AccDB$Company,ignore.case = TRUE),'Ashok',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Woolworth*",AccDB$Company,ignore.case = TRUE),'Woolworth',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Provinzial*",AccDB$Company,ignore.case = TRUE),'Provinzial',AccDB$Company)
  AccDB$Company=ifelse(grepl("*NIRWANA *",AccDB$Company,ignore.case = TRUE),'NIRWANA',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Confiserie Reichert*",AccDB$Company,ignore.case = TRUE),'Confiserie CAKE SHOP',AccDB$Company)
  AccDB$Company=ifelse(grepl("*NIKE00550*",AccDB$Company,ignore.case = TRUE),'NIKE',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Toys R US*",AccDB$Company,ignore.case = TRUE),'Toys R US',AccDB$Company)
  AccDB$Company=ifelse(grepl("*MYRA*",AccDB$Company,ignore.case = TRUE),'MYRA',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Abschnitt A II *",AccDB$Company,ignore.case = TRUE),'',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Kartenzahlung CA*",AccDB$Company,ignore.case = TRUE),'C&A',AccDB$Company)
  AccDB$Company=ifelse(grepl("*HOTEL ALFONSO*",AccDB$Company,ignore.case = TRUE),'HOTEL ALFONSO',AccDB$Company)
  AccDB$Company=ifelse(grepl("*mister.lady*",AccDB$Company,ignore.case = TRUE),'Mister Lady',AccDB$Company)
  AccDB$Company=ifelse(grepl("*EINZAHLAUTOMAT *",AccDB$Company,ignore.case = TRUE),'COIN DEPOSIT',AccDB$Company)
  AccDB$Company=ifelse(grepl("*PENNY*",AccDB$Company,ignore.case = TRUE),'PENNY',AccDB$Company)
  AccDB$Company=ifelse(grepl("*YELLOW POINT*",AccDB$Company,ignore.case = TRUE),'YELLOW POINT',AccDB$Company)
  AccDB$Company=ifelse(grepl("*FedEx*",AccDB$Company,ignore.case = TRUE),'FedEx',AccDB$Company)
  AccDB$Company=ifelse(grepl("*ULLRICH*",AccDB$Company,ignore.case = TRUE),'ULLRICH',AccDB$Company)
  AccDB$Company=ifelse(grepl("*RF43X677966775*",AccDB$Company,ignore.case = TRUE),'ARD',AccDB$Company)
  AccDB$Company=ifelse(grepl("*CYBERPORT*",AccDB$Company,ignore.case = TRUE),'CYBERPORT',AccDB$Company)
  AccDB$Company=ifelse(grepl("*BIO COMPANY*",AccDB$Company,ignore.case = TRUE),'BIO COMPANY',AccDB$Company)
  AccDB$Company=ifelse(grepl("*CINESTAR*",AccDB$Company,ignore.case = TRUE),'CINESTAR',AccDB$Company)
  AccDB$Company=ifelse(grepl("*KLARNA AB Wish*",AccDB$Company,ignore.case = TRUE),'WISH',AccDB$Company)
  AccDB$Company=ifelse(grepl("*KLARNA*",AccDB$Company,ignore.case = TRUE),'Klarna',AccDB$Company)
  AccDB$Company=ifelse(grepl("*ROSSMANN*",AccDB$Company,ignore.case = TRUE),'ROSSMANN',AccDB$Company)
  AccDB$Company=ifelse(grepl("*BEZIRKSAMT*",AccDB$Company,ignore.case = TRUE),'BURGERAMT',AccDB$Company)
  AccDB$Company=ifelse(grepl("*E-CENTER*",AccDB$Company,ignore.case = TRUE),'EUROPA CENTER',AccDB$Company)
  AccDB$Company=ifelse(grepl("*KENTUCKY FRIED CHICKEN*",AccDB$Company,ignore.case = TRUE),'KFC',AccDB$Company)
  AccDB$Company=ifelse(grepl("*NETTO MARKEN-DISCOU*",AccDB$Company,ignore.case = TRUE),'NETTO',AccDB$Company)
  AccDB$Company=ifelse(grepl("*LABO 017092203961*",AccDB$Company,ignore.case = TRUE),'BURGERAMT',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Martin Kunschert*",AccDB$Company,ignore.case = TRUE),'UNKNOWN',AccDB$Company)
  AccDB$Company=ifelse(grepl("*DP-Tech *",AccDB$Company,ignore.case = TRUE),'UNKNOWN',AccDB$Company)
  AccDB$Company=ifelse(grepl("*ERSATZKARTE*",AccDB$Company,ignore.case = TRUE),'Commerz Bank',AccDB$Company)
  AccDB$Company=ifelse(grepl("*10014015201*",AccDB$Company,ignore.case = TRUE),'UNKNOWN WITHDRAWL',AccDB$Company)
  AccDB$Company=ifelse(grepl("*MAC GEIZ*",AccDB$Company,ignore.case = TRUE),'MAC GEIZ',AccDB$Company)
  AccDB$Company=ifelse(grepl("*NATURKUNDE*",AccDB$Company,ignore.case = TRUE),'NATURKUNDE MUSEUM',AccDB$Company)
  AccDB$Company=ifelse(grepl("*ALNATURA*",AccDB$Company,ignore.case = TRUE),'ALNATURA',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Heinemann*",AccDB$Company,ignore.case = TRUE),'Heinemann',AccDB$Company)
  AccDB$Company=ifelse(grepl("*NORMA*",AccDB$Company,ignore.case = TRUE),'NORMA',AccDB$Company)
  AccDB$Company=ifelse(grepl("*SPORTHAUS OLYMPIA*",AccDB$Company,ignore.case = TRUE),'OLYMPIA',AccDB$Company)
  AccDB$Company=ifelse(grepl("*GA 2078*",AccDB$Company,ignore.case = TRUE),'OTHER BANK',AccDB$Company)
  AccDB$Company=ifelse(grepl("*IQOPTION*",AccDB$Company,ignore.case = TRUE),'IQ OPTION',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Media Markt *",AccDB$Company,ignore.case = TRUE),'Media Markt',AccDB$Company)
  AccDB$Company=ifelse(grepl("*NAGARAJAN RAMANATHAN Zurueck*",AccDB$Company,ignore.case = TRUE),'NAGARAJ Housing',AccDB$Company)
  AccDB$Company=ifelse(grepl("*NAGARAJAN RAMANATHAN Kaution zurueck*",AccDB$Company,ignore.case = TRUE),'NAGARAJ Housing',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Deutsche Post*",AccDB$Company,ignore.case = TRUE),'Deutsche Post',AccDB$Company)
  AccDB$Company=ifelse(grepl("*POCO*",AccDB$Company,ignore.case = TRUE),'POCO',AccDB$Company)
  AccDB$Company=ifelse(grepl("*OBI BAU*",AccDB$Company,ignore.case = TRUE),'OBI BAU',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Nagarajan Ramanathan BYLADEM1001*",AccDB$Company,ignore.case = TRUE),'NAGARAJ Housing',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Tankstelle *",AccDB$Company,ignore.case = TRUE),'Petrol Bunk',AccDB$Company)
  AccDB$Company=ifelse(grepl("*GS 40000974*",AccDB$Company,ignore.case = TRUE),'O2 Blau',AccDB$Company)
  AccDB$Company=ifelse(grepl("*LITTLE JOHN BIKES *",AccDB$Company,ignore.case = TRUE),'LITTLE JOHN BIKES ',AccDB$Company)
  AccDB$Company=ifelse(grepl("*G7751L2X107045373*",AccDB$Company,ignore.case = TRUE),'Lufthansa',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Immobilien Scout GmbH*",AccDB$Company,ignore.case = TRUE),'Immobilien Scout',AccDB$Company)
  AccDB$Company=ifelse(grepl("*Postbank*",AccDB$Company,ignore.case = TRUE),'Postbank',AccDB$Company)
  AccDB$Company=ifelse(grepl("*109234500*",AccDB$Company,ignore.case = TRUE),'Commerz bank',AccDB$Company)
  AccDB$Company=ifelse(grepl("*10010060089*",AccDB$Company,ignore.case = TRUE),'G+B Housing',AccDB$Company)
  AccDB$Company=ifelse(grepl("*PFENNIGLAND*",AccDB$Company,ignore.case = TRUE),'PFENNIGLAND',AccDB$Company)
  AccDB$Company=ifelse(grepl("*IKEA*",AccDB$Company,ignore.case = TRUE),'IKEA',AccDB$Company)
  AccDB$Company=ifelse(grepl("*DE12100400000109234501*",AccDB$Company,ignore.case = TRUE),'G+B Housing',AccDB$Company)
  
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
