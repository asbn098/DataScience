#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(data.table)
library(ggplot2)
library(grid)
library(DT)
library(dplyr)
library(rpart)
library(cluster)
library(arules)
library(arulesViz)
library(lubridate)
library(tidyr)

loan <-read.csv(file = "C:/Users/ParkJiyeon/kivaloans.csv", header=TRUE)
MPI <-read.csv("C:/Users/ParkJiyeon/kiva_mpi_region_locations.csv", header=TRUE)

m<-subset(loan, select=c(term_in_months,repayment_interval))


MA <- merge(loan, MPI)
md<-subset(MA, select = c("funded_amount", "country", "world_region", "sector"))


 
MA1 <- select(MA, MPI, world_region) %>% filter(!is.na(MPI))
MA2 <- select(MA, funded_amount, world_region) %>% filter(!is.na(funded_amount))
mpi_nomiss <- MA %>% filter(!is.na(MPI)) %>% select(country, MPI, world_region, MPI)
funded_nomiss <- MA %>% filter(!is.na(funded_amount)) %>% select(country, funded_amount, world_region, funded_amount)
MPI_mean <- tapply(MA1$MPI, MA1$world_region, mean)
funded_amount_mean <- tapply(MA2$funded_amount, MA2$world_region, mean)
ui <- tagList(
  navbarPage(
  "shiny",
  tabPanel("Show data",
           
           
           sidebarPanel(
             conditionalPanel(
               'input.dataset === "loan"',
               checkboxGroupInput("show_vars", "Columns in loan to show:",
                                  names(loan), selected = names(loan))
             )
           ),
           mainPanel(
             tabsetPanel(
               id = 'dataset',
               tabPanel("loan", DT::dataTableOutput("mytable1"))
             )
           )
  ),
  tabPanel("Show data2",
           
           
           sidebarPanel(
             conditionalPanel(
               'input.dataset2 === "MPI"',
               checkboxGroupInput("show_vars2", "Columns in MPI to show:",
                                  names(MPI), selected = names(MPI))
             )
           ),
           mainPanel(
             tabsetPanel(
               id = 'dataset2',
               tabPanel("MPI", DT::dataTableOutput("mytable2"))
             )
           )
  ),


 #panel 2
  tabPanel("Term in month - Repayment interval EDA",
           
             
        
             
             # Sidebar with controls to select a dataset and specify the number
             # of observations to view
             
             sidebarPanel(
               selectInput("repaymentType", "Choose a repayment type:", 
                           choices = c("bullet", "irregular", "monthly","weekly"))
               
               
             ),
             
             # Show a summary of the dataset and an HTML table with the requested
             # number of observations
             mainPanel(
               plotOutput('plot2'),
               tabsetPanel(
                 tabPanel("Term in months and Repayment Summary", verbatimTextOutput("summary2")),
                 
                 tabPanel("Term in months and Repayment Plot",  plotOutput("Plot2"))
                 
                 
               )
             )
           ),
 #panel 3
  tabPanel("Funded amound EDA",
    tabPanel("Country-Sector Rank",
                    sidebarPanel(
                      
                      numericInput("obs2", "Number of observations to view:", 10)
                    ),
                    mainPanel(
                      tabsetPanel(
                        tabPanel("Continent-Country-Funded amount", tableOutput("summary41")),
                        
                        tabPanel("County-Sector-Funded amount",  tableOutput("summary42")),
                        tabPanel("Countinent-Country-Sector-Funded amount",  tableOutput("summary43")),
                        tabPanel("Funded amount per Country",  plotOutput("plot41"))
                        
                        
                      )
                    )
  )),
 #panel 4
 tabPanel("Continent EDA",
          mainPanel(
            tabsetPanel(
              tabPanel("South Asia",  plotOutput("continentMPI1"),plotOutput("continentFunded1")),
              tabPanel("Sub-Saharan Africa",  plotOutput("continentMPI2"),plotOutput("continentFunded2")),
              tabPanel("Latin America and Caribbean",  plotOutput("continentMPI3"),plotOutput("continentFunded3")),
              tabPanel("East Asia and the Pacific",  plotOutput("continentMPI4"),plotOutput("continentFunded4")),
              tabPanel("Arab States",  plotOutput("continentMPI5"),plotOutput("continentFunded5")),
              tabPanel("MPI mean per continent",  plotOutput("MPIcontinent")), 
              tabPanel("Fund Amount mean per continent",  plotOutput("FundAmountContinent"))
              
              
            )
          )
          
          
 ),
  
 #panel 5
 tabPanel("Regression",
          mainPanel(
            tabsetPanel(
              tabPanel("display the results", verbatimTextOutput("result5")),
              
              
              tabPanel("cross-validation results ",  plotOutput("plot51")),
              tabPanel("detailed summary of splits", verbatimTextOutput("summary5")),
              tabPanel("cross-validation results2",  plotOutput("plot52")),
              tabPanel("plot tree ",  plotOutput("plot53"))
  
)
)),
#panel 6
tabPanel("Clustering",
          mainPanel(
             tabsetPanel(
               tabPanel("Clustering Plot1", plotOutput("plot61")),
               tabPanel("Siloutte Plot", plotOutput("plot62")),
               tabPanel("Clustering Plot2",  plotOutput("plot63"))
             
              
               
               
             )
           )),
#panel 7
tabPanel("Association Rule",
         sidebarPanel(
           
           
           numericInput("obs3", "Number of observations to view:", 10)
         ),
         mainPanel(
           tabsetPanel(
             tabPanel("Inspect", verbatimTextOutput("inspect")),
             
             tabPanel("plot7",  plotOutput("plot7"))
             
             
             
           )
         ))

)
)
server <- function(input, output) {
  
  # 1st panel output
  diamonds2 = loan[sample(nrow(loan), 1000), ]
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(diamonds2[, input$show_vars, drop = FALSE])
  })
  # MPI
  diamonds3 = MPI[sample(nrow(MPI), 1000), ]
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(diamonds3[, input$show_vars2, drop = FALSE])
  })
  
  


  #panel 2 output
  output$Plot2 <- renderPlot({
    data <- switch(input$repaymentType,
                   "bullet" = m[m$repayment_interval == "bullet",],
                   "irregular" = m[m$repayment_interval == "irregular",],
                   "monthly" = m[m$repayment_interval == "monthly",],
                   "weekly" = m[m$repayment_interval == "weekly",]
                   
                   
    )
    
    
    
    
    ggplot(data, aes(x=term_in_months)) + 
      geom_density(fill = NA) + 
      geom_line(stat = "density") + 
      expand_limits(y = 0) + 
      ggtitle("Kernel Density Curve by repayment interval_overlap")
    
    
    
    
    
    
    
    
  })
  output$plot2<- renderPlot({
    ggplot(m, aes(x=term_in_months, colour = repayment_interval)) + 
      geom_density(fill = NA) + 
      geom_line(stat = "density") + 
      expand_limits(y = 0) + 
      ggtitle("Kernel Density Curve by repayment interval_overlap")
    
  })
  output$summary2 <- renderPrint({
    aggregate(term_in_months~repayment_interval,m,mean)
  })
  #panel 3 output
  output$summary41 <- renderTable({
    a <- aggregate(funded_amount ~ world_region+country, MA, mean)
    data.frame(a)
    a %>% group_by(world_region)
    a %>% arrange(-funded_amount) %>% mutate(Rank=1:nrow(a)) %>% head(input$obs2)
  })
  
  output$summary42 <- renderTable({
    a <- aggregate(funded_amount ~ sector+country, MA, mean)
    data.frame(a)
    a %>% group_by(sector)
    a %>% arrange(-funded_amount) %>% mutate(Rank=1:nrow(a)) %>% head(input$obs2)
  })
  output$summary43 <- renderTable({
    a <- aggregate(funded_amount ~ world_region+country+sector, MA, mean)
    data.frame(a)
    a %>% group_by(world_region)
    a %>% arrange(-funded_amount) %>% mutate(Rank=1:nrow(a)) %>% head(input$obs2)
  })
  
  output$plot41 <- renderPlot({
    
    loans_dt  <- as.data.table(loan)
    nloans <- loans_dt[, .N, by=country]
    nloans$N_z <- round((nloans$N - mean(nloans$N))/sd(nloans$N), 2)
    nloans$type <- ifelse(nloans$N_z < 0, "below", "above")
    nloans <- nloans[order(N_z),]
    nloans$country <- factor(nloans$country, levels = nloans$country)
    nloans <- tail(nloans, 38)
    
    ggplot(nloans, aes(x=country, y=N_z, label=N_z)) +
      geom_bar(stat='identity', aes(fill=type), width=.5)  +
      scale_fill_manual(name="Number of loans",
                        labels = c("Above Average", "Below Average"),
                        values = c("above"="#42b9ff", "below"="#42f4ee")) +
      labs(title= "Funded amount per country") + 
      coord_flip()
    
    
  })
  #panel 4 output
  output$continentMPI1 <- renderPlot({
    Asia <- filter(mpi_nomiss, world_region == "South Asia") %>% select(country, MPI)
    aggregate(MPI~country, Asia,mean) %>% 
      ggplot(aes(x = MPI, y = country, color = MPI)) + geom_point()
    
  })
  
  output$continentFunded1 <- renderPlot({
    
    Asia2 <- filter(funded_nomiss, world_region == "South Asia") %>% select(country, funded_amount)
    aggregate(funded_amount~country, Asia2,mean) %>% 
      ggplot(aes(x = funded_amount, y = country, color = funded_amount)) + geom_point()
  })
  
  
  output$continentMPI2 <- renderPlot({
    Sub_Saharan_Africa <- filter(mpi_nomiss, world_region == "Sub-Saharan Africa")
    aggregate(MPI~country, Sub_Saharan_Africa,mean) %>% 
      ggplot(aes(x = MPI, y = country, color = MPI)) + geom_point()
    
    
  })
  output$continentFunded2 <- renderPlot({
    
    Sub_Saharan_Africa2 <- filter(funded_nomiss, world_region == "Sub-Saharan Africa")
    aggregate(funded_amount~country, Sub_Saharan_Africa2,mean) %>% 
      ggplot(aes(x = funded_amount, y = country, color = funded_amount)) + geom_point()
    
  })
  
  output$continentMPI3 <- renderPlot({
    Latin_America_and_Caribbean <- filter(mpi_nomiss, world_region == "Latin America and Caribbean")
    aggregate(MPI~country, Latin_America_and_Caribbean, mean) %>% 
      ggplot(aes(x = MPI, y = country, color = MPI)) + geom_point()
    
  })
  output$continentFunded3 <- renderPlot({
    
    Latin_America_and_Caribbean2 <- filter(funded_nomiss, world_region == "Latin America and Caribbean")
    aggregate(funded_amount~country, Latin_America_and_Caribbean2, mean) %>% 
      ggplot(aes(x = funded_amount, y = country, color = funded_amount)) + geom_point()
  })
  
  
  
  output$continentMPI4 <- renderPlot({
    East_Asia_and_the_Pacific <- filter(mpi_nomiss, world_region == "East Asia and the Pacific")
    aggregate(MPI~country, East_Asia_and_the_Pacific, mean) %>% 
      ggplot(aes(x = MPI, y = country, color = MPI)) + geom_point()
    
  })
  output$continentFunded4 <- renderPlot({
    
    East_Asia_and_the_Pacific2 <- filter(funded_nomiss, world_region == "East Asia and the Pacific")
    aggregate(funded_amount~country, East_Asia_and_the_Pacific2, mean) %>% 
      ggplot(aes(x = funded_amount, y = country, color = funded_amount)) + geom_point()
  })
  
  
  output$continentMPI5 <- renderPlot({
    Arab_States <- filter(mpi_nomiss, world_region == "Arab States")
    aggregate(MPI~country, Arab_States, mean) %>% 
      ggplot( aes(x = MPI, y = country, color = MPI)) + geom_point()
    
  })
  output$continentFunded5 <- renderPlot({
    
    Arab_States2 <- filter(funded_nomiss, world_region == "Arab States")
    aggregate(funded_amount~country, Arab_States2, mean) %>% 
      ggplot( aes(x = funded_amount, y = country, color = funded_amount)) + geom_point()
  })
  
  # Generate a summary of the dataset
  
  
  output$MPIcontinent <- renderPlot({
    barplot(MPI_mean,
            main = "MPI mean per continent",
            col = "darkred"
    )
  })
  
  # Show the first "n" observations
  output$table <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  
  
  output$FundAmountContinent <- renderPlot({
    barplot(funded_amount_mean,
            main = "Fund Amount mean per continent",
            col = "blue"
    )
    
  })
 #panel 5 output
  activity_num <- loan %>% group_by(activity) %>% summarise(n=n())
  activity_more_than_1000 <- activity_num[activity_num$n > 1000,]
  main_innerjoin <- merge(x = loan, y = activity_more_than_1000 , by = "activity")
  funded_amount_mean_activity <- aggregate(funded_amount ~ activity, main_innerjoin, mean)
  main_innerjoin$term_in_months <- as.numeric(main_innerjoin$term_in_months)
  term_in_months_sum <- aggregate(term_in_months ~ activity, main_innerjoin, sum)
  regression_main <- Reduce(function(x, y) merge(x, y, all=TRUE), list(funded_amount_mean_activity, term_in_months_sum))
 
  m2 <-rpart(term_in_months~funded_amount+sector ,method = "anova",data = loan)
  
  output$result5 <- renderPrint({
    
    
    printcp(m2)
    
  })
  
  output$plot51 <- renderPlot({
    plotcp(m2)
  })
  output$summary5 <- renderPrint({
    summary(m2)
  })
  
  output$plot52 <- renderPlot({
    
    par(mfrow=c(1,2))
    rsq.rpart(m2)
  })
  output$plot53 <- renderPlot({
    
    plot(m2,uniform=TRUE,main="Regression Tree for Loan amount")
    text(m2,use.n=TRUE,all=TRUE,cex=.9)
    
  })
  #panel 6 output
  MD<-merge(loan,MPI)
  loan <- na.omit(loan) 
  loan2 <- select(loan, funded_amount, lender_count, country)
  aaaa <- aggregate(funded_amount~country, loan2, sum)
  bbbb <- aggregate(MPI~country, MD, mean)
  data.frame(aaaa)
  data.frame(bbbb)
  c <- merge(aaaa,bbbb) 
  pc <- pam(c, 3)
  output$plot61 <- renderPlot({
    
    clusplot(pc, main = 'clusterplot', color=TRUE, shade=TRUE,
         label=2, lines=0, xlab='funded-count', ylab='MPI')
  })
  output$plot62 <- renderPlot({
    plot(pc, main = 'clusterplot', color=TRUE, shade=TRUE,
         label=2, lines=0, xlab='funded-count', ylab='MPI')
  })
  
  output$plot63 <- renderPlot({
    plot(c, col=pc$clustering)
  })
  #panel 7 output
  country_sector <- select(loan, sector, country)
  country_sector1 <- sample_n(country_sector, 10000)
  country_sector_list <- split(country_sector1$sector, country_sector1$country)
  country_sector_list_trans <- as(country_sector_list, "transactions")
  country_sector_list_rules <- apriori(country_sector_list_trans)
  
  rule_interest <- subset(country_sector_list_rules, items %in% c("Health"))
  
  output$inspect <- renderPrint({
    inspect(rule_interest[1:input$obs3])
  })
  
  output$plot7 <- renderPlot({
    plot(country_sector_list_rules[1:input$obs3], method = "graph")
  })
  
  
}

shinyApp(ui, server)
