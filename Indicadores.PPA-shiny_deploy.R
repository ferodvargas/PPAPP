## app.R ##

options(repos = c(CRAN = "http://cran.rstudio.com"), scipen=999)



library(shiny)
library(shinydashboard)
library(stringr)
library(dplyr)
library(purrr)
library(rlang)
library(ggpubr)
library(grid)
library(gridExtra)
library(readxl)
library(DT)



setwd("C:/Users/fernanda-vargas/Documents/Power BI")

#base.dados <- read.table('Indicadores_PPA.csv', header=T, sep=";", dec=",")
base.dados <- read_excel("Indicadores_PPA.xls")
#View(base.dados)

names(base.dados)
names(base.dados) <- gsub(" ",".", names(base.dados))

#head(base.dados)

sum(duplicated(base.dados$Indicador.de.Resultado.Código)) # nenhum duplicado

class(base.dados$Programa.Eixo.Principal)
base.dados$Eixo <- sapply(1:length(base.dados$Programa.Eixo.Principal), function(x)
  paste0(str_extract_all(string = base.dados$Programa.Eixo.Principal, pattern = "[A-Z]")[[x]], collapse = ""))


id.eixo <- data.frame(matrix(cbind(unique(base.dados$Programa.Eixo.Principal), unique(base.dados$Eixo)), ncol=2, 
                             dimnames=list(c(), c("Nome.Eixo","Eixo"))), stringsAsFactors=FALSE)

eixo_list <- id.eixo %>%
  collect() %>%
  split(.$Nome.Eixo) %>%
  map(~ .$Eixo)



programa_list <- as.list(1:length(unique(base.dados$Programa.Denominação))) %>%
  set_names(unique(base.dados$Programa.Denominação))


tab.programa <- data.frame(cbind(unique(base.dados$Programa.Denominação), 1:length(unique(base.dados$Programa.Denominação))),
                           stringsAsFactors=FALSE)  

base.dados$Id.Programa <- NA

eval(parse(text=paste0("base.dados$Id.Programa[which(base.dados$Programa.Denominação==tab.programa[", 
                       1:length(unique(base.dados$Programa.Denominação)), 
       ",1])] <- tab.programa[", 1:length(unique(base.dados$Programa.Denominação)), ",2]")))



base.dados$Unidade.Medida <- base.dados$Indicador.de.Resultado.Unidade.de.medida
names(table(base.dados$Indicador.de.Resultado.Unidade.de.medida))

base.dados$Unidade.Medida[which(base.dados$Unidade.Medida=="dias")] <- "Dias"
base.dados$Unidade.Medida[which(base.dados$Unidade.Medida=="unidade")] <- "Unidade"

names(table(base.dados$Unidade.Medida))



logo <- base64enc::dataURI(file="logo_Novas_facanhas.png", mime="image/png")


dheader <- dashboardHeader(title = "Indicadores PPA", titleWidth = 250,
                            #tags$li(a(href = 'http://shinyapps.company.com',
                            #          icon("power-off"),
                            #          title = "Back to Apps Home"),
                            #        class = "dropdown"),
                            tags$li(a(img(src = logo, #src = 'http://www.clipartbest.com/cliparts/nTX/8nj/nTX8njyEc.jpeg',
                                          title = "Secretaria de Planejamento, Orçamento e Gestão", height = "30px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"))




siderbar <- dashboardSidebar(
  tags$style(HTML(".main-sidebar{
                                 width: 250px;
                                }")),
  sidebarMenu(# Add buttons to choose the way you want to select your data
    radioButtons(inputId = "eixo", 
                  "EIXO ESTRATÉGICO:",
                  choices = c("Sociedade com Qualidade de Vida", 
                              "Desenvolvimento Empreendedor", 
                              "Estado Sustentável",
                              "Governança e Gestão")),
  sidebarMenu(
    selectInput(
      inputId = "programa",
      label = "PROGRAMA TEMÁTICO:",
      choices = programa_list,
      #selected = 99,
      size = 1,
      selectize = FALSE
    )#, actionLink("remove", "Remove detail tabs")
  )
)
)



frow1 <- fluidRow(column(10,
  #column(12,
  valueBoxOutput("value1"),
  valueBoxOutput("value2"),
  valueBoxOutput("value3")
 )
)



frow2 <- fluidRow(column(12,
  box(solidHeader=TRUE, height = "80px", width = 8,
  selectInput("select the input", label = "Ação Programática:", inputId = "acao")
  ))
)



frow3 <- fluidRow(column(12,
             #column(width = 8,           
                   box(solidHeader=TRUE, height = "80px", width = 8,
                       selectInput("select the input", label = "Indicador de Resultado:", inputId = "indicador")
                     #)
                   )))#,
             #column(width = 4,
            #        img(src="http://images.all-free-download.com/images/graphiclarge/natural_beauty_highdefinition_picture_166133.jpg", 
            #            width=110)
            # ))
                 


ods <- base64enc::dataURI(file="ODS_logo_PPA.jpg", mime="image/jpg")

frow4 <- fluidRow(column(width = 12,
  column(width = 8,
         box(#status = "primary", 
           width = NULL, solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE, 
           plotOutput("plot", height = "370px")
         )
         
  ),
        column(width = 4,
    #img(src="http://images.all-free-download.com/images/graphiclarge/natural_beauty_highdefinition_picture_166133.jpg", 
    #   width=110),
         box(width = NULL, solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE,
           #plotOutput("plot.table", height = "210px")
           dataTableOutput("plot.table")
           ),
    #img(src="http://images.all-free-download.com/images/graphiclarge/natural_beauty_highdefinition_picture_166133.jpg", 
    #  width=110)
         #box(
           img(src=ods, height = "100px")
           #,width = 4)
  )
))



dbody <- dashboardBody(frow1, frow2, frow3, frow4,
      tags$head(
         tags$style(
           HTML(
                             '.skin-black .main-sidebar  {color: #FFFFFF; background-color: #FF9C33;}
        .skin-black .span12 { background-color: #ffffff;}
        .skin-black .main-header .navbar  { background-color: #ffffff;}
        .skin-black .main-header > .logo { background-color: #ffffff;}
        .skin-black .main-header > .logo:hover { background-color: #ffffff;}
        .skin-black .main-header .logo, .skin-black .main-header .navbar { transition: color 0s; }'
           )
         )
       ))


ui <- dashboardPage(skin = "black",
      dheader,
      siderbar,
      dbody
)




server <- function(input, output, session) {
  
  idprograma <- reactive({
    names(table(base.dados %>%
    filter(Programa.Eixo.Principal == input$eixo) %>%
    select("Programa.Denominação")))
})
  
  idacao <- reactive({
    names(table(base.dados %>%
                  filter(Programa.Eixo.Principal == input$eixo) %>%
                  filter(Programa.Denominação == input$programa) %>%
                  select("Ação.Programática.Denominação")))
  })
  
  idindicador <- reactive({
    names(table(base.dados %>%
                  filter(Programa.Eixo.Principal == input$eixo) %>%
                  filter(Programa.Denominação == input$programa) %>%
                  filter(Ação.Programática.Denominação == input$acao) %>%
                  select("Indicador.de.Resultado.Denominação")))
  })
  
  
observe({
  updateSelectInput(session, "programa", 
                    choices = idprograma()
  )})

observe({
  updateSelectInput(session, "acao", 
                    choices = idacao()
  )})

observe({
  updateSelectInput(session, "indicador", 
                    choices = idindicador()
  )})


output$value1 <- renderValueBox({
  valueBox(
    length(names(table(base.dados %>%
                                 filter(Programa.Eixo.Principal == input$eixo) %>%
                                 select("Programa.Denominação"))))
    ,'Programas Temáticos no Eixo'
    ,icon = icon("info")
    ,color = "maroon")  
})

output$value2 <- renderValueBox({
  valueBox(
    formatC(length(names(table(base.dados %>%
                                 filter(Programa.Eixo.Principal == input$eixo) %>%
                                 filter(Programa.Denominação == input$programa) %>%
                                 select("Ação.Programática.Denominação")))), format="d", big.mark=',')
    ,'Ações Programáticas no Programa'
    ,icon = icon("stats",lib='glyphicon')
    ,color = "purple")  
})

output$value3 <- renderValueBox({ 
  valueBox(
    formatC(sum(table(base.dados %>%
                        filter(Programa.Eixo.Principal == input$eixo) %>%
                        filter(Programa.Denominação == input$programa) %>%
                        select("Ação.Programática.Denominação"))), format="d", big.mark=',')
    ,'Indicadores de Resultado'
    ,icon = icon("list") #icon("gbp",lib='glyphicon')
    ,color = "green")  
})


observeEvent(input$indicador,{
  if(is.null(input$indicador)) return(NULL)
  indicadorSelect <- input$indicador
  print ('plot')
  # Assuming only one entry for each mont per candybar
  df <- data.frame(matrix(cbind(as.character(c(2015:2019)), 
                                as.numeric(base.dados %>%
                                             filter(Programa.Eixo.Principal == input$eixo) %>%
                                             filter(Programa.Denominação == input$programa) %>%
                                             filter(Ação.Programática.Denominação == input$acao) %>%
                                             filter(Indicador.de.Resultado.Denominação == input$indicador) %>%
                                             select(names(base.dados)[16:20]))), ncol=2, 
                          dimnames=list(c(),c("ano","valor"))), stringsAsFactors=FALSE)
  
  df$valor <- as.numeric(df$valor)
  
  alvo <-  as.numeric(base.dados %>%
                        filter(Programa.Eixo.Principal == input$eixo) %>%
                        filter(Programa.Denominação == input$programa) %>%
                        filter(Ação.Programática.Denominação == input$acao) %>%
                        filter(Indicador.de.Resultado.Denominação == input$indicador)  %>%
                        select(names(base.dados)[15]))
  
  
  output$plot <- renderPlot({
    
    if(base.dados %>%
             filter(Programa.Eixo.Principal == input$eixo) %>%
             filter(Programa.Denominação == input$programa) %>%
             filter(Ação.Programática.Denominação == input$acao)  %>%
             filter(Indicador.de.Resultado.Denominação == input$indicador)  %>%
             select(names(base.dados)[21])=="continua"){

      ggplot(data=df, aes(x=ano, y=valor, group=1)) +
        geom_line(size=0.8, col="gray") +
        geom_text(data=df, aes(x=ano, y=valor, label=as.character(df$valor)),
                  vjust=-1, col="black", size=3.8, fontface=2) +  #fontface=2  
        geom_hline(yintercept=alvo, col="red", linetype="dashed", size=0.6) + 
        ylim((min(c(df$valor, alvo), na.rm=TRUE)-(min(c(df$valor, alvo), na.rm=TRUE)*0.1)), 
             (max(c(df$valor, alvo), na.rm=TRUE)+(max(c(df$valor, alvo), na.rm=TRUE)*0.1))) + 
        annotate("text", x=1.0, y=alvo, hjust = 2, vjust = -1, label = "Meta", size=3.8, col="red") +
        geom_point(size=2) + 
        xlab("") + ylab("") +
        theme(axis.text.x=element_text(face="bold", color="black", size=10, angle=0))
    } else {
      
      ggplot(data=df, aes(x=ano, y=valor, label=as.character(df$valor))) + 
        geom_bar(position = "dodge", stat = "identity", fill=c("olivedrab3")) + 
        geom_text(data=df, aes(x=ano, y=valor, label=as.character(df$valor)),
                  vjust=-1, col="black", size=3.8, fontface=2) +  #fontface=2
        geom_hline(yintercept=alvo, col="red", linetype="dashed", size=0.6) + 
        annotate("text", x=1.0, y=alvo, hjust = 2, vjust = -1, label = "Meta", size=3.8, col="red") +
        ylim(0,#(min(c(df$valor, alvo), na.rm=TRUE)-(min(c(df$valor, alvo), na.rm=TRUE)*0.1)), 
             (max(c(df$valor, alvo), na.rm=TRUE)+(max(c(df$valor, alvo), na.rm=TRUE)*0.1))) + 
        xlab("") + ylab("") +
        theme(axis.text.x = element_text(face="bold", color="black", size=10, angle=0))
    }
    
  })
}
)

  output$plot.table = renderDataTable({
    
    datatable(cbind(c("Órgão responsável", "Valor desejado", "Unidade de medida", 
                   "Periodicidade", "Base geográfica", "Fonte"),
                 as.character(base.dados %>%
                                filter(Programa.Eixo.Principal == input$eixo) %>%
                                filter(Programa.Denominação == input$programa) %>%
                                filter(Ação.Programática.Denominação == input$acao) %>%
                                filter(Indicador.de.Resultado.Denominação == input$indicador)  %>%
                                select(names(base.dados)[c(12,15,14,13,8,7)]))), colnames = " ",
                  #rownames = FALSE, colnames=NULL,
                     options = list(
                       lengthMenu = c(30),
                       pageLength = 30,
                       searching = FALSE,
                       paging = FALSE,
                       info = FALSE,
                       ordering = FALSE,
                       scrollX = TRUE))
  })
}

shinyApp(ui, server)



