#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  #DataFrame for Closed + Open Opp  & selected Market
  reactdataAccountDB=reactive({
    AccDB%>%
      filter(Amount!=0)
    # ConsolidatedOpp%>%
    #   filter(Market.segment==input$MarSegChoose)%>%
    #   group_by(Region,Load,Speed)%>%
    #   summarise(cnt=sum(as.numeric(Quantity),na.rm = TRUE)) %>%
    #   arrange(desc(Region))
  })
  
  reactdataAccDB_ByYearMon=reactive({
    AccDB%>%
      filter(TransactionYear==input$SelYear)%>%
      filter(TransactionMonth==input$SelMonth)
  })
    #Month Based on Year selected
  observe({
    dt=AccDB %>%
      filter(TransactionYear==input$SelYear)
    # if (exists("ValTransactionMonth"))
    # {
    # rm(ValTransactionMonth)
    # }
    ValTransactionMonth=unique(dt$TransactionMonth[!is.na(dt$TransactionMonth)])
  })
   reactdataCon=reactive({
     ValTransactionMonth=AccDB%>%
       filter(TransactionYear==input$ValTransactionYear) %>%
       unique(TransactionMonth[!is.na(TransactionMonth)])
  })
  ######################################## Market Analysis ############################################
  output$HighIncome <- renderValueBox({
    dt=reactdataAccDB_ByYearMon()
    valueBox(
      paste0(max(dt$Amount), " EUR"), paste0("Highest Income ",dt$Company[match(max(dt$Amount),dt$Amount)]), icon = icon("list"),
      # paste0("2254", "EUR"), "Highest Income", icon = icon("list"),
      color = "purple"
    )
  })

  output$HighExp <- renderValueBox({
    dt=reactdataAccDB_ByYearMon()
    valueBox(
      paste0(min(dt$Amount,na.rm = TRUE), " EUR"), paste0("Highest Expense ",dt$Company[match(min(dt$Amount),dt$Amount)]), icon = icon("list"),
      # paste0("2254", "EUR"), "Highest Income", icon = icon("list"),
      color = "purple"
    )
  })
  
  output$MonthlyTrack<-renderPlotly({
  
    #Case 1,)
      dt=reactdataAccDB_ByYearMon()%>%
        filter(Category=="Expense") %>%
        filter(!grepl("*Cash*",TransactionType,ignore.case = TRUE)) %>%
        filter(Company!='G+B Housing' & Company!='India Citibank' ) %>%
        select(TransactionDate,Amount,Company) %>%
        group_by(TransactionDate)
      #     plot_ly(dt, x = dt$Company, y = dt$Amount, name = 'MonthlyTrack', type = 'scatter')
   
   plot_ly() %>%
      add_trace(dt, labels=dt$Company, type='pie', values=dt$Amount) %>%
      layout(p, title=paste0("Expense for ",input$SelMonth, " ", input$SelYear))
    
  })
  ######################################## Overall Companies #####################################################
  output$tabOverAllMarket = renderDataTable({
    Dt=reactdataAccountDB()
    Dt%>%
      select(Company,Amount)%>%
      group_by(Company)%>%
      summarise(TotalAmount=sum(Amount,na.rm = TRUE))%>%
      # arrange(asce(TotalAmount))%>%
      
     #Dt$`Winning.Competitor's.Bid` = paste(Dt$`Winning.Competitor's.Bid`, Dt$`Winning.Competitor's.Bid.Currency`, sep="")
    #columns=c("Region","Load","Speed","cnt")
    datatable(Dt,filter="bottom",class = 'cell-border stripe',rownames = FALSE)
  })
  ##################################### Data View #####################################################
  output$tabOverAllCompanies = renderDataTable({
    Dt=reactdataAccountDB()
    
    #Dt$`Winning.Competitor's.Bid` = paste(Dt$`Winning.Competitor's.Bid`, Dt$`Winning.Competitor's.Bid.Currency`, sep="")
    #columns=c("Region","Load","Speed","cnt")
    datatable(Dt,filter="bottom",class = 'cell-border stripe',rownames = FALSE)
  })
  ##################################### Missing items summary View #####################################################
  output$tabmissingSummary = renderDataTable({
    Dt=data.frame(AccDB)
    missing.summary <- sapply(Dt, function(x) sum(is.na(x))) 
    indexs.missing <- sapply(Dt, function(x) sum(is.na(x))) > 0 
    num.variable.missing <- length(missing.summary[indexs.missing])
    
    freq.table.miss <- data.frame(Variable = names(missing.summary[indexs.missing]), Number.of.Missing = as.integer(missing.summary[indexs.missing]), 
                                   Percentage.of.Missing = paste0(round(as.numeric(prop.table(missing.summary[indexs.missing]))*100,2)," %") )
    
    freq.table.miss <- freq.table.miss %>% 
      select(Variable:Percentage.of.Missing) %>%
      arrange(desc(Number.of.Missing))
    
    datatable(freq.table.miss)
  })
  
})
