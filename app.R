library(shiny) 
library(shinythemes)
library(igraph)
library(magrittr)
library(visNetwork)
library(data.table)
library(shinyWidgets)
library(DT)
library(plyr)
library(dplyr)

# UI
ui<- fluidPage(theme = shinytheme("yeti"),
  ##CSS
  tags$head(
    tags$style(
      HTML("
            h1 {
           font-family: 'Calibri';
           font-weight: 500;
           line-height: 1.1;
           color: #4dc3ff;
            }
          h2,h3{
            font-family: 'Calibri';
           font-weight: 250;
           color: #4dc3ff;
           text-align: center;
          }

          h4{
          font-family: 'Calibri';
           font-weight: 250;
           color: #990099;
           }

        #departments, #connections, #deptConnections {
           overflow-y: scroll;
           height:500px;
        }

        #emailSentCount, #emailReceivedCount {
           overflow: scroll;
           height:300px;
        }
        .wellPanel {
          background-color: black;
        }
        #CentHop1, #CentHop2{
          width: 100px;
        }
           ")
    )
  ),
  
  ##Displaying the title
  headerPanel(title = h1('Aditi Sharma - Social Network Analysis')),
  #sidebar
  sidebarPanel(
    wellPanel(style = "background-color: #ffffff;",
    fileInput(inputId = "dept", label = h4("Upload Departament Information")),
    checkboxInput('header1', 'Header', FALSE)),
    br(),
    wellPanel(style = "background-color: #ffffff;",
    fileInput(inputId = "email", label = h4("Upload User-Email Information")),
    checkboxInput('header2', 'Header', FALSE))
  ),
  #main panel 
  mainPanel(
  tabsetPanel(
    #Displaying a summary of the dataset
    tabPanel("HOME", br(),br(), wellPanel(h2("WELCOME TO THE SOCIAL NETWORK ANALYSIS APP")),br()),
   #Loading the data 
    tabPanel("OVERVIEW", 
             br(),
             fluidRow(h3("A GLANCE AT THE DATA")),
             br(),
             fluidRow(column(4,h4("Department Data")), column(4, offset = 4,h4("Emails Data"))),
             #fluidRow(column(4,textOutput("summaryDept")), column(4, offset = 2,textOutput("summaryEmail"))),
          fluidRow(column(4 ,tableOutput("departments")),column(4, offset = 4,tableOutput("connections")))
            ),
    #Displaying connections in the network. Number of connections is selected by the user
    tabPanel("CONNECTIONS",
             br(),
             fluidRow(column(4,numericInput(inputId = "num", label = h4("Number of Connections"), value = 30, min=0))),
             fluidRow(visNetworkOutput("network") )),
      #Sent and Received frequency
    navbarMenu("EMAILS",
        tabPanel("FREQUENCY",
             br(),
             fluidRow(h3("Summary of Top 10 Senders and Receivers")),
             br(),
             fluidRow(column(4, tableOutput("summaryFreqSender")), column(4,offset = 4, tableOutput("summaryFreqReceiver"))),
             br(),
             fluidRow(column(4, h4("Number of emails sent by each user:")), column(4,offset = 4, h4("Number of Emails Received by each user:"))),
             fluidRow(
               column(4,tableOutput('emailSentCount')),column(4, offset=4,tableOutput("emailReceivedCount"))
               )),
    #2 hop neighbors
    tabPanel("TOP SENDERS",
             br(),
            fluidRow(column(4, selectizeInput(inputId = "topSenderHop" , label = h4("Select from top 10 Senders") ,choices = NULL)),
                     column(4,numericInput(inputId = "numSender1", label = h4("Number of 1 Hop Connections"), value = 20, min=0)),
                     column(4,numericInput(inputId = "numSender2", label = h4("Number of 2 Hop Connections"), value = 70, min=0))),
            fluidRow(column(4,h4("Select Node"))),
            fluidRow( column(12,visNetworkOutput("senderHop") )),
            fluidRow("*Nodes have been filtered on the basis of frequency of emails sent")),
  tabPanel("TOP RECEIVERS",
           br(),
           fluidRow(column(4, selectizeInput(inputId = "topReceiverHop" , label = h4("Select from top 10 Receivers") ,choices = NULL)),
                    column(4,numericInput(inputId = "numReceiver1", label = h4("Number of 1 Hop Connections"), value = 20, min=0)),
                    column(4,numericInput(inputId = "numReceiver2", label = h4("Number of 2 Hop Connections"), value = 70, min=0))),
           fluidRow(column(4,h4("Select Node"))),
           fluidRow( column(12,visNetworkOutput("receiverHop"), height="400" )),
           fluidRow("*Nodes have been filtered on the basis of frequency of emails received"))
        ),
  #Centrality
  navbarMenu("CENTRALITY", 
             tabPanel( "VIEW NETWORK",
           fluidRow(column(3, selectizeInput(inputId = "centralityMeasure" , label = h4("Centrality Measure") ,
                                             choices = c("Degree", "Betweenness","In-Degree", "Eigen"))),
           column(3, selectizeInput(inputId = "topCentrality" , label = h4("Top 10 Nodes of Centrality") ,choices = NULL)),
             column(3, numericInput(inputId = "CentHop1" , label = h4("Number of one Hops") , value = 20, min=0)),
             column(3, numericInput(inputId = "CentHop2" , label = h4("Number of two hops") , value = 70, min=0))
           ),
           fluidRow( column(12,visNetworkOutput("hopCentrality") )),
           fluidRow("*Nodes for displaying hops above have been filtered on the basis of eigen centrality.",br(), 
                    "To display more nodes change the value of input one hop or two hop nodes.",br(),
                    "Hover over the nodes to view the value of the centrality measure selected")
           ),
           tabPanel("TABLE",
                    fluidRow(column(4, selectizeInput(inputId = "centralityMeasureTbl" , label = h4("Centrality Measure") ,
                                                      choices = c("Degree", "Betweenness","In-Degree", "Eigen")))),
             fluidRow(column(12,tableOutput('centralityTable')))
           ),
           tabPanel("OBSERVATIONS",
                    fluidRow(br(),"Summary:",br(),
                             "> User/Node 160 has the highest degree centrality, betweenness and in-degree centrality. This means that node 160 is the central or critical node in our network. It is highly influential.", br(),
                             "> Similarly Nodes 121, 107, 62, 129 also have high values for all three centralities and are important to the network.",br(), 
                             "> This can also be seen through the calculated eigen centralities which indicate the importance of these nodes.", br(), 
                             "> These users have the most information and are the most connected in our network.",br(),
                             "> Node 129's degree centrality is based more on the number of emails sent instead of received as it is not a part of the top 10 list of in-degree centrality.", br(),
                             "> This indicates that node 129 is well connected in terms of giving out information",br(),
                             "> Node 377 has a high betweenness which means that a lot of information passes through it.",br(),
                             "> This can be seen through the graph as well where 377 is further connected to nodes 129 and 62 which are also of high betweenness.", br(),
                             "> Thus it acts as a bridge for many nodes and handles the flow of information.", br(),
                             "> Node 128 has a high in-degree centrality only which could mean that this user is important and people are trying to connect to this user via email.", br(),
                             ">Nodes of high importance like 160, 434, 114, 129 are sending mails to node 128.",br(),
                             "> The nodes identified as those with top 10 eigen centrality are the ones that influence the whole network not just the nodes which are directly connected to them.",br()
                             ,"> From the graph we can see that Node 160 is well connected to other nodes of high importance for example node 114.",br(),
                             "> Node 377 can also be seen to be connected to nodes 129 and 62 who also have high betweenness."
                    ) )),
  navbarMenu( "DEPARTMENT",
              tabPanel("VIEW NETWORK",
                       fluidRow(column(4,offset = 3, sliderInput(inputId = "numDept", label = h4("Display connections upto rank:"), value = 60, min=0, max =1000 ))),
                       fluidRow(visNetworkOutput("deptNetwork")),
                       fluidRow("*Ranking is based on frequency of emails sent/received between the departments.")
              ),
              tabPanel("SUMMARY", br(),h4("Number of Emails Sent from one department to another"),fluidRow(tableOutput("deptConnections")))
              
            
  )
  
  )
)
)

server <- function(input, output, session){
  
  observe({
    #Read Department Details
    inFileDept = input$dept
    if (is.null(inFileDept)) {
      return(NULL)
    }
    dept = read.table(inFileDept$datapath, header = input$header1)
    #Set column names to UserId and DeptId
    colnames(dept) <- c("UserId" , "DeptId")
    #Read Email Details
    inFileEmail <- input$email
    if(is.null(inFileEmail))
      return(NULL)
    emails<-read.table(inFileEmail$datapath, header = input$header2)
    #Setting column names to from and to
    colnames(emails)<- c("from","to")
    #Display n connections
    subsetEmail <- head(emails, input$num)
    
    #Finding top 10 Senders - Hops
    freqSender<- as.data.frame(table(emails[1]))
    freqSender<- data.frame(UserId=freqSender$Var1, EmailsSent=freqSender$Freq)
    topSender <- head(freqSender[order(-freqSender[2]),],10)
    
    #Finding top 10 Senders - Hops
    freqReceiver<- as.data.frame(table(emails[2]))
    freqReceiver <- data.frame(UserId=freqReceiver$Var1, EmailsReceived=freqReceiver$Freq)
    topReceiver <- head(freqReceiver[order(-freqReceiver[2]),],10)
    
    #Centrality Measures
    centrality <- function(emailData, measure, count=NULL){
      # To find the top "count" nodes with the given centrality measure
      
      if(measure == "Degree")
      {
        graph <- graph.data.frame(emailData, directed=T)
        V(graph)$degree <- centr_degree(graph, mode = "all")$res
        V(graph)$title <- paste("Degree:",V(graph)$degree)
        nodes <- get.data.frame(graph, what="vertices")
        nodes <- data.frame(id = nodes$name, degree = nodes$degree, title = nodes$title)
        nodes <- nodes[order(nodes$degree, decreasing = T),]
      }
      else if(measure == "Betweenness")
      {
        graph <- graph.data.frame(emailData, directed=T)
        V(graph)$betweeness <- betweenness(graph)
        V(graph)$title <- paste("Betweenness:",V(graph)$betweeness)
        nodes <- get.data.frame(graph, what="vertices")
        nodes <- data.frame(id = nodes$name, betweeness = nodes$betweeness,title = nodes$title)
        nodes <- nodes[order(nodes$betweeness, decreasing = T),]
      }
      else if(measure == "In-Degree")
      {
        
        graph <- graph.data.frame(emailData, directed=T)
        V(graph)$indegree <- centr_degree(graph, mode = "in")$res
        V(graph)$title <- paste("In Degree:",V(graph)$indegree)
        nodes <- get.data.frame(graph, what="vertices")
        nodes <- data.frame(id = nodes$name, indegree = nodes$indegree, title = nodes$title)
        nodes <- nodes[order(nodes$indegree, decreasing = T),]
      }
      else if(measure == "Eigen")
      {
        graph <- graph.data.frame(emailData, directed=T)
        V(graph)$eigen <- centr_eigen(graph)$vector
        V(graph)$title <- paste("Eigen:",V(graph)$eigen)
        nodes <- get.data.frame(graph, what="vertices")
        nodes <- data.frame(id = nodes$name, eigen = nodes$eigen, title = nodes$title)
        nodes <- nodes[order(nodes$eigen, decreasing = T),]
        
      }
      if(is.null(count))
      {
        deg <- nodes
      }
      else
      {
        deg <- head(nodes,count)
      }
      deg
    }
    
    #Function to display the 2 hop neighbors by filtering neighbors on the basis of top degree centrality
    twohops <- function( nodeId, emailData)
    {
      #Calculation 2 hop neighbors on the basis of top degree centralities
      #Degree centrality for all the nodes
      nodeDeg <- centrality(emailData, "Eigen")
      
      if(input$centralityMeasure == "In-Degree")
      {
        #Filter email data and fetch only those node where email is sent from nodeId
        oneHop <- emailData[ which(emailData$to  %in% nodeId), ]
        #oneHop <- subset(emailData, emailData$from == nodeId$id)
        #Selects top 10 columns on the basis of degree
        oneHop <- merge(oneHop, nodeDeg, by.x = "from" , by.y = "id" , all.x = TRUE)
        oneHop <- oneHop[order(oneHop$eigen, decreasing = T),]
        data1 <- head(oneHop,input$CentHop1)
        data1$eigen <- NULL
        data1$title <- NULL
        #Finding the second hop neighbors on the basis of degree centrality
        twoHop <- emails[ which(emailData$to  %in% data1$from), ]
        data2 <- head(twoHop,input$CentHop2)
        unique(rbind(data1,data2))
      }
      else
      {
        #Filter email data and fetch only those node where email is sent from nodeId
        oneHop <- emailData[ which(emailData$from  %in% nodeId), ]
        #oneHop <- subset(emailData, emailData$from == nodeId$id)
        #Selects top 10 columns on the basis of degree
        oneHop <- merge(oneHop, nodeDeg, by.x = "to" , by.y = "id" , all.x = TRUE)
        oneHop <- oneHop[order(oneHop$eigen, decreasing = T),]
        data1 <- head(oneHop,input$CentHop1)
        data1$eigen <- NULL
        data1$title <- NULL
        #Finding the second hop neighbors on the basis of degree centrality
        twoHop <- emails[ which(emailData$from  %in% data1$to), ]
        data2 <- head(twoHop,input$CentHop2)
        unique(rbind(data1,data2))
      }
    }
    
    #Aggregation by departments
    
    deptSender <- merge(emails, dept, by.x ="from", by.y = "UserId")
    names(deptSender)[names(deptSender) == "DeptId"] <- "SenderDept"
    
    deptRec <- merge(deptSender,dept, by.x="to", by.y = "UserId")
    names(deptRec)[names(deptRec) == "DeptId"] <- "ReceiverDept"
    
    deptRec$from <- NULL
    deptRec$to <- NULL

    deptAgg <- plyr::count(deptRec, c('SenderDept','ReceiverDept'))
    colnames(deptAgg) <- c("from","to","frequency")
    deptAgg <- deptAgg[order(deptAgg$frequency, decreasing = T),]
    deptAgg <- deptAgg %>% mutate(rank = dense_rank(desc(frequency)))
    
    
    ##############################
    ##############################
    #OUTPUT FUNCTIONS
    #Displays departments summary
    output$departments <- renderTable({
      head(dept,300)
    },
    bordered = TRUE)
     
  
    #Displays emails summary
  output$connections <- renderTable({
    head(emails,300)
  },
  bordered = TRUE)
  
  
  #Displays num of emails sent
  output$emailSentCount <- renderTable(
    {
      freqSender
    },
    bordered = TRUE
  )
  #Displays num of emails received
  output$emailReceivedCount <- renderTable(
    {
      freqReceiver
    }, 
    bordered = TRUE
    
  )
  
  output$summaryFreqSender <- renderTable(
    {
      topSender
    },
    bordered = TRUE,
    align = "c"
  )
  
  output$summaryFreqReceiver <- renderTable(
    {
      topReceiver
    },
    bordered = TRUE,
    align = "c"
    
  )

  
  output$network <- renderVisNetwork(
    {
      if(input$num == 0)
        return(NULL)
      net <- graph_from_data_frame(subsetEmail, directed=TRUE)
      data <- toVisNetworkData(net)
      visNetwork(nodes = data$nodes, edges = data$edges)%>%
        visNodes(color = "#33ccff" )%>%
        visEdges(arrows ="to, from" ) %>%
        visOptions(highlightNearest = list(enabled = TRUE), nodesIdSelection = TRUE)%>%
        visPhysics(stabilization = FALSE)%>% visInteraction(navigationButtons = TRUE)
    }
  )
  

  
 #2 hop neighbors
  updateSelectizeInput(session, "topSenderHop", choices = topSender$UserId, server = TRUE, options = NULL, selected = head(topSender$UserId , 1))
  updateSelectizeInput(session, "topReceiverHop", choices = topReceiver$UserId, server = TRUE, options = NULL, selected = head(topReceiver$UserId , 1))
  
  output$senderHop <- renderVisNetwork(
    {
      if(is.null(input$numSender1) || is.na(input$numSender1)
         || is.null(input$numSender2) || is.na(input$numSender2) 
         || input$numSender1 == 0 )
        return(NULL)
      if(is.null(input$topSenderHop))
        return(NULL)
      newdata <- emails[ which(emails$from  %in% input$topSenderHop), ]
      #Sorting the one hop neighbors by frquency
      newdata <-merge(newdata,freqSender, by.x="to", by.y = "UserId")
      newdata <- newdata[order(-newdata$EmailsSent),]
      newdata <- head(newdata, input$numSender1)
      newdata$EmailsSent <- NULL
      #Finding the two hop neighbors
      newdata1 <- emails[ which(emails$from  %in% newdata$to), ]
      newdata1 <- head(newdata1, input$numSender2)
      hops <- rbind(newdata, newdata1)
    colnames(hops) <- c("from","to")
    net <- graph_from_data_frame(hops, directed=TRUE)
    data <- toVisNetworkData(net)
    visNetwork(nodes = data$nodes, edges = data$edges)%>%
      visNodes(color = "#4dc3ff" )%>%
      visEdges(arrows ="from" ) %>%
    visOptions(highlightNearest = list(enabled = TRUE), nodesIdSelection = 
list(enabled = TRUE, selected= input$topSenderHop, style = 'background: #f2f2f2; color: black;'))%>%
    visPhysics(stabilization = FALSE)%>% visInteraction(navigationButtons = TRUE)
    }
  )
  
  output$receiverHop <- renderVisNetwork(
    {
      if(is.na(input$numReceiver1) || is.null(input$numReceiver1) || is.na(input$numReceiver2) || is.null(input$numReceiver2)
         || input$numReceiver1 == 0)
        return(NULL)
      if(is.null(input$topReceiverHop))
        return(NULL)
      
      freqReceiver<- as.data.frame(table(emails[2]))
      freqReceiver <- data.frame(UserId=freqReceiver$Var1, EmailsReceived=freqReceiver$Freq)
      topReceiver <- head(freqReceiver[order(-freqReceiver[2]),],10)
      
      newdata <- emails[ which(emails$to  %in% input$topReceiverHop), ]
      #Sorting the one hop neighbors by frquency
      newdata <-merge(newdata,freqReceiver, by.x="from", by.y = "UserId")
      newdata <- newdata[order(-newdata$EmailsReceived),]
      newdata <- head(newdata, input$numReceiver1)
      newdata$EmailsReceived <- NULL
      #Finding the two hop neighbors
      newdata1 <- emails[ which(emails$to  %in% newdata$from), ]
      newdata1 <- head(newdata1, input$numReceiver2)
      hops <- rbind(newdata, newdata1)
      
      colnames(hops) <- c("from","to")
      net <- graph_from_data_frame(hops, directed=TRUE)
      net <- simplify(net, remove.multiple = F, remove.loops = T)
      data <- toVisNetworkData(net)
      visNetwork(nodes = data$nodes, edges = data$edges)%>%
        visNodes(color = "#4dc3ff")%>%
        visEdges(arrows ="to" ) %>%
        visOptions(highlightNearest = list(enabled = TRUE), nodesIdSelection = 
                     list(enabled = TRUE, selected=input$topReceiverHop, style = 'background: #f2f2f2; color: black;'))%>%
        visPhysics(stabilization = FALSE)%>% visInteraction(navigationButtons = TRUE)
    }
  )
  updateSelectizeInput(session, "topCentrality", choices = (centrality(emails,input$centralityMeasure,10))$id, 
                       server = TRUE, options = NULL, selected = head((centrality(emails,input$centralityMeasure,10))$id , 1))
  
  output$hopCentrality <- renderVisNetwork(
    {
      if(is.null(input$topCentrality) || input$CentHop1 == 0)
        return(NULL)
      nodeCntr <- centrality(emails,input$centralityMeasure)
      
      if(input$centralityMeasure == "In-Degree")
      {
        hops <- twohops(input$topCentrality, emails)
        graph <- graph.data.frame(hops, directed=T)
        nodes <- get.data.frame(graph, what="vertices")
        nodes <- data.frame(id = nodes$name)
        department <- dept
        colnames(department) <- c("id","group")
        nodes <- merge(department, nodes, by = "id")
       nodes <- merge(nodes,nodeCntr, by="id")
        edges <- get.data.frame(graph, what="edges")[1:2]
        visNetwork(nodes, edges) %>%
          visEdges(arrows ="to" ) %>%
          visOptions( highlightNearest = TRUE, nodesIdSelection = TRUE, selectedBy = "group")%>%
          visPhysics(stabilization = FALSE) %>% visInteraction(navigationButtons = TRUE)
      }
      else
        {
          hops <- twohops(input$topCentrality, emails)
          graph <- graph.data.frame(hops, directed=F)
          nodes <- get.data.frame(graph, what="vertices")
          nodes <- data.frame(id = nodes$name)
          department <- dept
          colnames(department) <- c("id","group")
          nodes <- merge(department, nodes, by = "id")
          nodes <- merge(nodes,nodeCntr, by="id")
          edges <- get.data.frame(graph, what="edges")[1:2]
          visNetwork(nodes, edges) %>%
            visEdges(arrows ="to, from" ) %>%
            visOptions( highlightNearest = TRUE, nodesIdSelection = TRUE, selectedBy = "group")%>%
            visPhysics(stabilization = FALSE) %>% visInteraction(navigationButtons = TRUE)
      }
      

    }
  )
  
  output$centralityTable <- renderTable(
    {
      tbl <- centrality(emails,input$centralityMeasureTbl,10)
      tbl$title <- NULL
      tbl
    }
  )

  
  updateSliderInput(session, "numDept", max = nrow(deptAgg))
  
  output$deptConnections <- renderTable(
    {
      deptAgg
    },
    bordered = TRUE
  )
  
  output$deptNetwork <- renderVisNetwork(
    {
      if(is.null(input$numDept) || input$numDept == 0)
        return(NULL)
      deptAggTop <- subset(deptAgg, deptAgg$rank < input$numDept)
      #deptAggTop <- head(deptAgg,input$numDept)
      graph <- graph.data.frame(deptAggTop, directed=T)
      graph <- simplify(graph, remove.loops = TRUE, remove.multiple = TRUE)
      V(graph)$title <- paste("Dense Rank:", deptAgg$rank)
      nodes <- get.data.frame(graph, what="vertices")
      nodes <- data.frame(id = nodes$name, title = nodes$title )
      edges <- get.data.frame(graph, what="edges")[1:2]
      visNetwork(nodes, edges) %>%
        visEdges(arrows ="to, from" ) %>%
        visOptions( highlightNearest = TRUE, nodesIdSelection = TRUE)%>%
        visPhysics(stabilization = FALSE) %>% visInteraction(navigationButtons = TRUE)
    }
  )
  
  })
}

shinyApp(ui = ui, server = server)