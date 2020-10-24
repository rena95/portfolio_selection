call_body <- dashboardBody(
  tags$head(tags$style(HTML('   /* body */
                                .content-wrapper, .right-side {
                                background-color: white;
                                }

                                '))),
  
  plotlyOutput("graph"),
  fluidRow(column(plotlyOutput("graph_2"),width=6),  
           column(plotlyOutput("graph_3"),width=6)
  ),
           fluidRow(plotOutput("graph_4"))
)

