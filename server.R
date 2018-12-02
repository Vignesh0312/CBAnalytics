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
    AccDB
    # ConsolidatedOpp%>%
    #   filter(Market.segment==input$MarSegChoose)%>%
    #   group_by(Region,Load,Speed)%>%
    #   summarise(cnt=sum(as.numeric(Quantity),na.rm = TRUE)) %>%
    #   arrange(desc(Region))
  })
  
  output$tabOverAllMarket = renderDataTable({
    Dt=reactdataAccountDB()
    Dt%>%
      group_by(Category,Company)%>%
      select(Category,Company)%>%
      unique()%>%
    #Dt$`Winning.Competitor's.Bid` = paste(Dt$`Winning.Competitor's.Bid`, Dt$`Winning.Competitor's.Bid.Currency`, sep="")
    #columns=c("Region","Load","Speed","cnt")
    datatable(Dt,filter="bottom",class = 'cell-border stripe',rownames = FALSE
              )
  })

  output$tabOverAllCompanies = renderDataTable({
    Dt=reactdataAccountDB()
    
    #Dt$`Winning.Competitor's.Bid` = paste(Dt$`Winning.Competitor's.Bid`, Dt$`Winning.Competitor's.Bid.Currency`, sep="")
    #columns=c("Region","Load","Speed","cnt")
    datatable(Dt,filter="bottom",class = 'cell-border stripe',rownames = FALSE
    )
  })
  
  
})
