####################################################
# INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS #
####################################################

#Pacotes utilizados
pacotes <- c("shiny","lpSolve","lpSolveAPI","writexl","readxl")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}

library(shiny)
library(lpSolve)
library(lpSolveAPI)
library(writexl)
library(readxl)

#Código de execução na estrutura de um app Shiny

server <- function(session, input, output) {
  
  ### Script de construção de planilha de entrada entrada ########################
  planilha_entrada <- reactive({
    
    n_var = input$n_var
    n_restric = input$n_restrics
    z_max_min = input$max_min
    z_lin_int = input$lin_int
    
    #Primeira coluna
    col_1 <- c("Objective Function")
    if(z_max_min == 1){
      col_1 <- append(col_1, "Zmax")
    }else{
      col_1 <- append(col_1, "Zmin")
    }
    
    if(z_lin_int == 1){
      col_1 <- append(col_1, c("Linear", "Constraints"))
    }else{
      col_1 <- append(col_1, c("Integer", "Constraints"))
    }
    
    
    for (i in 1:n_restric) {
      var <- paste("R", i , sep = "")
      col_1 <- append(col_1,  var)
    }
    data_entrada <- data.frame(col_1)
    
    #Colunas das variáveis
    for (i in 1:n_var) {
      col_var <- c(paste("X", i, sep = "") , 0, "", paste("X", i, sep = ""), rep(0, n_restric))
      data_entrada <- data.frame(data_entrada, col_var)
    }
    
    #Coluna igualdades
    if(z_max_min == 1){
      col_igualdades <- c("","","","", rep("<=", n_restric))
    }else{
      col_igualdades <- c("","","","",rep(">=", n_restric))
    }
    data_entrada <- data.frame(data_entrada, col_igualdades)
    
    #Coluna final
    col_final <- c("","","","", rep(0 , n_restric))
    data_entrada <- data.frame(data_entrada, col_final)
  })
  
  
  # Script para Download da planilha criada
  output$exemplo_input <- downloadHandler(
    filename = function() {
      paste("Input_model_", Sys.Date(), ".xlsx" , sep="")
    },
    content = function(file) {
      write_xlsx(planilha_entrada(), file , col_names = FALSE)
    }
  )
  
  
  # Script de Exposição das tabelas
  output$table_process <- renderTable( colnames = FALSE, align = "c" , {
    
    inFile <- input$file_opt
    
    if(is.null(inFile)){
      tabela_expo <- planilha_entrada()
    }else{
      file.rename(inFile$datapath, paste(inFile$datapath, ".xlsx", sep=""))
      dados_var <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), col_names = FALSE)
      tabela_expo <- dados_var
    }
    tabela_expo
  })
  
  #Observação de importação de arquivo slsx
  observeEvent(input$buttom_opt, {
    
    #Aviso de não importação
    file1 <- input$file_opt
    if(is.null(file1)){return(
      showModal(modalDialog(
        title = h4(tags$b("Attention!")),
        h4(tags$b("You must insert a .xlsx file!")),
        plotOutput("plot"),
        footer = NULL,
        easyClose = TRUE
      ))
    )}
  })
  
  #Script de cálculo do modelo de Programação Linear
  optimal_result <- eventReactive( input$buttom_opt,{
    
    inFile <- input$file_opt
    file.rename(inFile$datapath, paste(inFile$datapath, ".xlsx", sep=""))
    
    dados <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), col_names = FALSE)
    
    
    #variáveis
    variables_set <- na.omit(as.vector(unlist(dados[1,])))
    variables_set <- variables_set[-1]
    
    #Valores da FO
    values_FO <- na.omit(as.vector(unlist(dados[2,])))
    z_max_min <- values_FO[1]
    func_obj <- as.numeric(values_FO[-1])
    
    #Linear or Integer
    z_lin_int <- as.character(dados[3,1])
    
    #Restrições
    restrics_set <- as.vector(unlist(dados[1]))
    restrics_set <- restrics_set[-c(1:4)]
    
    #Matriz de restrições
    matriz_restrics <- dados[2:(length(variables_set)+1)]
    matriz_restrics <- matriz_restrics[-c(1:4),]
    matriz_restrics <- as.numeric(as.vector(unlist(matriz_restrics)))
    matriz_restrics <- matrix(matriz_restrics, nrow = length(restrics_set))
    
    #Igualdades das restrições
    restrics_max_min <- as.vector(unlist(dados[length(dados)-1]))
    restrics_max_min <- restrics_max_min[-c(1:4)]
    
    #Valores dos limites
    restrics_values <- as.vector(unlist(dados[length(dados)]))
    restrics_values <- as.numeric(restrics_values[-c(1:4)])
    
    # Maximizar ou minimizar ?
    if(z_max_min == "Zmax"){
      lp_max_min = "max"
    }else{
      lp_max_min = "min"
    }
    
    if(z_lin_int == "Integer"){
      result <- lp(lp_max_min, func_obj, matriz_restrics, restrics_max_min, restrics_values, all.int = TRUE)
    }else{
      result <- lp(lp_max_min, func_obj, matriz_restrics, restrics_max_min, restrics_values)
    }
    
    #Resultado da quantidade de cada variável
    result_disc <- as.vector(result$solution)
    weighted_values <- func_obj*result_disc
    
    result_disc <- round(result_disc, 3)
    weighted_values <- round(weighted_values, 3)
    
    variables_set <- append(variables_set, c("", ""))
    func_obj <- append(func_obj, c("", "Z"))
    result_disc <- append(result_disc, c("", sum(weighted_values)))
    
    tabela_resultado <- data.frame(variables_set, func_obj, result_disc)
    
    tabela_resultado
  })
  

  #Exposição da tabela de resultado
  output$table_result <- renderTable( colnames = TRUE, align = "c" , {
    optimal_result()
  })
  
}