#install.packages("shiny") #Instalador da biblioteca Shiny

library(shiny)

ui <- bootstrapPage(
  
  div(style="display:inline-block" , img(src="MBA_USP.png", style="position: absolute; width: 10%; margin-left:45%; margin-top: 1%"), img(src="R.png", style="position: absolute ; width: 5%; margin-left: 90%; margin-top: 1%")),
  h3(tags$u(tags$b("Linear Programming Model")), style = "margin-top: 7% ; text-align: center ; color: #005266"),
  br(),

  sidebarLayout(
    sidebarPanel(style = "background-color: #e7f2fe ; border: 1px solid #005266 ; margin-left:2%",
                 h4("Problem construction:", style = "color: #1a1a1a"),
                 div(style="display:inline-block" , selectizeInput("n_var", "Number of Variables:" , width = "140px" , choices = list("2","3","4","5","6","7","8","9","10"))),
                 div(style="display:inline-block ; margin-left:10px" , selectizeInput("n_restrics", "Number of Constraints:" , width = "160px" , choices = list("1","2","3","4","5","6","7","8","9","10"))),
                 div(
                   div(style="display:inline-block "  , radioButtons("max_min", "Optmization:" , choices = list("Max" = 1, "Min" = 2),selected = 1)),
                   div(style="display:inline-block ; margin-left:30px"  , radioButtons("lin_int", "Model:"       , choices = list("Linear" = 1, "Integer" = 2),selected = 1)),
                 ),
                 downloadButton("exemplo_input", "Download Table", style = "background-color: #005266 ; border: 1px solid #005266 ; color: white"),
                 
                 br(),
                 h4("Problem Solving:", style = "color: #1a1a1a"),
                 fileInput("file_opt", "Insert a .xlsx file" , accept = ".xlsx"),
                 actionButton(inputId = "buttom_opt", "Resolve", style = "background-color: #00b33c ; border: 1px solid #009933 ; color: white")
    ),
    
    mainPanel(
      tabsetPanel(
        
        tabPanel("Table in Process",
                 br(),
                 br(),
                 div(style = "display:inline-block ; background-color: #e6f7ff ; border: 1px solid #005266 ; margin-left:5% ; border-radius: 5px ; padding: 10px",
                     tableOutput("table_process"))
        )
      )
    )
  ),
  br(),
  br(),
  h3(tags$u(tags$b("Table of Results")), style = "text-align: center ; color: #005266"),
  div(style="width:30%; margin-left:38% ; text-align:center", tableOutput("table_result")),
    
)


  
  