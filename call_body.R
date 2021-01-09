call_body <- dashboardBody(
  tags$head(tags$style(HTML('   /* body */
                                .content-wrapper, .right-side {
                                background-color: white;
                                }

                                '))),
  h6(textOutput("n_obs")),
  plotlyOutput("graph"),
  fluidRow(column(plotlyOutput("graph_2"),width=6),  
           column(plotlyOutput("graph_3"),width=6)),
  fluidRow(column(plotOutput("graph_4")  , width=6),
           column(DT::dataTableOutput(outputId ="summary"), width = 6)),
  fluidRow(plotlyOutput("graph_5"))
  
)

