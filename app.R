# seriesHidro
library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(tidyr)
library(readr)
library(downloader)
library(dplyr)
library(RColorBrewer)
library(scales)
library(lattice)
library(rintrojs)
library(imputeTS)
library(fitdistrplus)
library(e1071)
library(zoo)

Inventario <- read_delim("dados/Inventario.csv", delim = ";", locale = locale(encoding = "latin1", dec = ","))
Inventario$Latitude <- Inventario$Latitude + rnorm(nrow(Inventario),0,0.001)
Inventario$Longitude <- Inventario$Longitude + rnorm(nrow(Inventario),0,0.001)

Inventario <- Inventario[Inventario$Codigo != 44500002,]
Inventario <- Inventario[Inventario$Codigo != 44500001,]


if(!dir.exists("~/DadosHidroWeb")){
    dir.create("~/DadosHidroWeb")
}

# Define barra lateral
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Mapa Iterativo", tabName = "mapa", icon = icon("map")),
        menuItem("Tabela Dinâmica", icon = icon("table"), tabName = "data"),
        menuItem("Series Históricas", icon = icon("table"), tabName = "series",
                 menuSubItem("Seleciona visualização", tabName = "seleciona"),
                 menuSubItem("Visualiza gráfico",tabName = "visualiza")
                 ),
        menuItem("Sobre o aplicativo", icon = icon("info"), tabName = "info")
        )
    )

# Define corpo do dashboard
body <- dashboardBody(
    
    tabItems(
        tabItem(tabName = "mapa",
                div(class = "outer",
                    tags$head(
                        includeCSS("styles.css"),
                        includeScript("gomap.js")
                    ),
                    
                    leafletOutput("mymap", width = "100%", height = "100%"),
                    absolutePanel(id = "controls",
                                  class = "panel panel-default",
                                  fixed = TRUE,
                                  draggable = TRUE,
                                  top = 100, left = "auto",
                                  right = 80, bottom = "auto",
                                  width = 430, height = "auto",
                                  
                                  h1("Explora estações"),
                                  actionButton("select", "Listar estações selecionadas"),
                                  br(),
                                  tags$hr(),
                                  tableOutput("Selection"),
                                  tags$hr(),
                                  br(),
                                  tags$hr(),
                                  actionButton("download", "Baixar dados das estações"),
                                  tags$hr(),
                                  strong(textOutput("baixados")),
                                  img(src = "logo.png", height = 50, width = 200)
                    )
                    
                )
                
                
        ),
        
        tabItem(tabName = "data",
                
                box(width = 12, title = "Tabela com Inventário de Estações do HidroWeb",
                    status = "primary", solidHeader = TRUE,
                    
                    fluidRow(
                        box(width = 12, title = "Selecione dados para visualização da tabela",
                            status = "info", solidHeader = TRUE, collapsible = TRUE,
                            checkboxGroupInput("show_vars", "Dados para visualização:",
                                               names(Inventario)[!c(names(Inventario) %in%
                                                                        c("Codigo", "Estacao", "EstadoNome", "SubBacia", "Municipio","ResponsavelSigla", "Action"))],
                                               inline = TRUE)  
                        )
                    ),
                    
                    fluidRow(
                        
                        box(width = 12, title = "Aplicação de filtros",
                            status = "info", solidHeader = TRUE, collapsible = TRUE,
                            
                            column(3,
                                   selectInput("estados", strong("Estados"),
                                               choices = unique(Inventario$EstadoNome),
                                               multiple = TRUE)
                            ),
                            
                            column(3,
                                   selectInput("subbacia", strong("Sub-Bacia"),
                                               choices = unique(Inventario$SubBacia),
                                               multiple = TRUE)
                            ),
                            
                            column(3,
                                   
                                   selectInput("municipio", strong("Municipio"),
                                               choices = unique(Inventario$Municipio),
                                               multiple = TRUE)
                            ),
                            
                            column(3,
                                   
                                   selectInput("responsavel", strong("Responsavel"),
                                               choices = unique(Inventario$ResponsavelSigla),
                                               multiple = TRUE)
                            )
                            
                        )
                    ),
                    
                    fluidRow(
                        box(width = 12, title = "Tabela dinâmica",
                            status = "info", solidHeader = TRUE,
                            DT::dataTableOutput("inventario")
                        )
                    )
                    
                    
                    
                    
                )
        ),
        
        tabItem(tabName = "seleciona",
                
                box(width = 12, title = "Visualização de séries históricas",
                    status = "primary", solidHeader = TRUE,
                    
                    box(width = 4, title = "Seleciona tipo de série",
                        status = "info", solidHeader = TRUE, 
                        selectizeInput(label = "Código da estação",
                                       "station", choices = dir("~/DadosHidroWeb")[suppressWarnings(!is.na(as.numeric(as.character(dir("~/DadosHidroWeb")))))],
                                       options = list(create = TRUE)
                        ),
                        
                        selectInput('Vars', 'Dados disponiveis', ""),
                        numericInput("ano_hidro", "Inicio do ano hidrológico",
                                     value = 9, min = 1, max = 12, step = 1),
                        tags$hr(),
                        selectInput('consist', 'Nivel de consistência',
                                    choices = list("Bruto" = "bruto",
                                                   "Consistido" = "consist",
                                                   "Preferencialmente consistido" = "pref",
                                                   "Manter tudo" = "tudo"),
                                    selected = "consist"),
                        
                        selectInput('serie', 'Série que deseja gerar',
                                    choices = list("Médias diárias" = "med_dia",
                                                   "Médias mensais" = "med_mes",
                                                   "Médias anuais" = "med_ano",
                                                   "Maximas anuais" = "max_ano",
                                                   "Q7" = "q7",
                                                   "Minimas anuais" = "min_ano",
                                                   "Minima Q7 anual" = "q7_min",
                                                   "Sazonal" = "sazonal")),
                        
                        tags$hr(),
                        actionButton("gera", "Gerar Série!")
                    ),
                    
                    box(width = 8, title = "Visualização dos dados",
                        status = "info", solidHeader = TRUE,
                        
                        DT::dataTableOutput("dt_tabela"),
                        tags$hr(),
                        downloadButton("downloadData", 'Baixar série')
                        
                        
                    )
                    
                )
                
                
                
                
        ),
        
        tabItem(tabName = "visualiza",
                
                box(width = 12, title = "Visualização de Resultados",
                    status = "primary", solidHeader = TRUE,
                    box(width = 12, title = "Visualização de Gráficos",
                        status = "info", solidHeader = TRUE,
                        plotlyOutput("grafico")
                    ),
                    box(width = 12, title = "Sumário dos Dados",
                        status = "info", solidHeader = TRUE,
                        DT::dataTableOutput("sumario"),
                        helpText("No caso da Q7 min anual, a estimativa da Q7,10 é feita por meio da distribuição Weibull de 2 parâmetros. Ela não admite valores negativos, então, caso existam cotas negativas, o aplicativo não retornará uma estimativa desse valor.")
                    )
                    
                    
                )
                
        ),
        
        tabItem(tabName = "info",
                
                box(width = 12, title = "Informações sobre o aplicativo",
                    status = "primary", solidHeader = TRUE,
                    
                    includeHTML("info_text.html")
                    
                    
                )
                
        )
        
    )
)

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = span(img(src = "logomarcacprmhorizontal_v2.jpg",
                                                     height = 40, width = 40*5),"\t",
                                                 ". Series Historicas Hidro/SGB"),
                                    titleWidth = 650),
                    sidebar,
                    body
)

server <- function(input, output, session) {
    
    RV <- reactiveValues(Clicks = list())
    
    # Mostrar introducao
    observeEvent("", {
        showModal(modalDialog(
            includeHTML("intro_text.html"),
            easyClose = TRUE,
            footer = modalButton("Sair da introdução")
            
        ))
    })
    
    observeEvent(input$intro,{
        removeModal()
    })
    
    
    tabela <- reactive({
        
        return(Inventario)
        
    })
    
    
    showStationPopup <- function(codigo, lat, lng) {
        tabela <- tabela()
        selectedCode <- tabela[tabela$Codigo == codigo,]
        content <- as.character(tagList(
            tags$h4("Estação:", selectedCode$Estacao), tags$br(),
            sprintf("Codigo da estação: %s", selectedCode$Codigo), tags$br(),
            sprintf("Estado: %s", selectedCode$EstadoNome), tags$br(),
            sprintf("Municipio: %s", selectedCode$Municipio), tags$br(),
            sprintf("Area de drenagem: %s km2", selectedCode$AreaDrenagem), tags$br(),
            sprintf("Responsavel: %s ", selectedCode$ResponsavelSigla), tags$br(),
            sprintf("Operador: %s ", selectedCode$OperadoraSigla)
        ))
        leafletProxy("mymap") %>% 
            addPopups(lng = lng, lat = lat, content, layerId = codigo)
    }
    
    observe({
        leafletProxy("mymap") %>% clearPopups()
        event <- input$mymap_marker_click
        if (is.null(event))
            return()
        
        isolate({
            showStationPopup(event$id, event$lat, event$lng)
        })
    })
    
    observe({
        if (is.null(input$goto))
            return()
        isolate({
            map <- leafletProxy("mymap")
            map %>% clearPopups()
            dist <- 0.2
            Codigo <- input$goto$codigo
            lat <- input$goto$lat
            lng <- input$goto$lng
            paste(Codigo)
            map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
        })
    })
    
    output$inventario <- DT::renderDataTable({
        df <- tabela() %>%
            filter(is.null(input$estados) | EstadoNome %in% input$estados) %>%
            filter(is.null(input$subbacia) | SubBacia %in% input$subbacia) %>%
            filter(is.null(input$municipio) | Municipio %in% input$municipio) %>%
            filter(is.null(input$responsavel) | ResponsavelSigla %in% input$responsavel) %>%
            mutate(Action = paste('<a class="go-map" href="" data-lat="', Latitude, '" data-long="', Longitude, '" data-codigo="', Codigo, '"><i class="fa fa-crosshairs"></i></a>', sep="")) %>%
            dplyr::select(c("Codigo", "Estacao", "EstadoNome", "SubBacia", "Municipio",
                     "ResponsavelSigla", input$show_vars, "Action"))
        
        action <- DT::dataTableAjax(session, df, outputId = "Codigo")
        
        DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
        
        
    })
    
    output$mymap <- renderLeaflet({
        leaflet(data = tabela()) %>%
            addProviderTiles("Esri.WorldTopoMap") %>%
            addCircleMarkers(lng = ~Longitude, lat = ~Latitude,
                             color = rgb(0.5,1,0), radius = 7,
                             clusterOptions = markerClusterOptions(),
                             label = ~as.character(Codigo),
                             layerId = ~Codigo)
    })
    
    observeEvent(input$mymap_marker_click, {
        click <- input$mymap_marker_click$id
        print(click) 
        proxy <- leafletProxy("mymap")
        
    
        if(click %in% RV$Clicks){
            RV$Clicks <- RV$Clicks[!(RV$Clicks == click)]
            selection <- tabela() %>% filter(Codigo == click)
            proxy %>%
                addCircleMarkers(data = selection, color = 'orange', fillOpacity = 1, opacity = 1,
                                 lng = ~Longitude, lat = ~Latitude, layerId = ~Codigo)
    
        } else {
            RV$Clicks <- c(RV$Clicks, click)
            selection <- tabela() %>% filter(Codigo %in% RV$Clicks)
            proxy %>%
                addCircleMarkers(data = selection, color = 'red', fillOpacity = 1, opacity = 1,
                                 lng = ~Longitude, lat = ~Latitude, layerId = ~Codigo)
        }
    
    })
    
    observeEvent(input$limpar, {
        RV <- reactiveValues(Clicks = list())
        selection <- tabela()
        
    })
    
    selected <- eventReactive(input$select, {
        selected <- unlist(RV$Clicks)
        selected <- data.frame("Estacoes" = selected)
        return(selected)
    })
    
    output$Selection <- renderTable({
        return(selected())
    })
    
    saida <- eventReactive(input$download, {
        stations <- as.character(selected()$Estacoes)
        Base <- "http://www.snirh.gov.br/hidroweb/rest/api/documento/convencionais?tipo=2&documentos="
        
        OldDir <- getwd()
        setwd("~/DadosHidroWeb")
        
        for(i in 1:length(stations)){
            
            Url <- paste0(Base, as.character(stations[i]))
            print(paste("Baixando de", Url))
            download(Url, paste0(stations[i], ".zip"), mode="wb") 
            unzip(paste0(stations[i],".zip"), exdir = paste0(stations[i]))
        }
        
        Dir <- getwd()
        
        setwd(OldDir)
        
        return(paste("Dados baixados com sucesso em", Dir))
        
    })
    
    observe({
        # get all character or factor columns
        estsChoices <- dir("~/DadosHidroWeb")[suppressWarnings(!is.na(as.numeric(as.character(dir("~/DadosHidroWeb")))))]
        updateSelectizeInput(session, "station",
                             choices = estsChoices, # update choices
                             selected = NULL) # remove selection
    })
    
    
    output$baixados <- renderText({
        return(saida())
    })
    
    
    dados <- reactive({
        unlist(lapply(strsplit(dir(paste0("~/DadosHidroWeb/",input$station)), "_"), `[[`, 1))
    })
    
    observe({
        DadoDisponivel <- intersect(c("cotas", "vazoes"), dados())
        updateSelectInput(session, "Vars", choices = DadoDisponivel)
    })
    
    serieHistorica <- eventReactive(input$gera, {
        
        # Seleciona o nivel de consistencia
        if(input$consist == "bruto"){
            df <- read_delim(paste0("~/DadosHidroWeb/",
                                    input$station,"/",
                                    input$Vars,"_T_",
                                    input$station,".zip"),
                             skip = 13, delim = ";", col_names = TRUE,
                             locale = locale(decimal_mark = ",")) %>%
                filter(MediaDiaria == "1" & NivelConsistencia == "1")
        } else if(input$consist == "consist"){
            df <- read_delim(paste0("~/DadosHidroWeb/",
                                    input$station,"/",
                                    input$Vars,"_T_",
                                    input$station,".zip"),
                             skip = 13, delim = ";", col_names = TRUE,
                             locale = locale(decimal_mark = ",")) %>%
                filter(MediaDiaria == "1" & NivelConsistencia == "2")
        } else if(input$consist == "pref"){
            df <- read_delim(paste0("~/DadosHidroWeb/",
                                    input$station,"/",
                                    input$Vars,"_T_",
                                    input$station,".zip"),
                             skip = 13, delim = ";", col_names = TRUE,
                             locale = locale(decimal_mark = ",")) %>%
                filter(MediaDiaria == "1") %>%
                arrange(desc(NivelConsistencia)) %>%
                filter(!duplicated(Data)) %>%
                arrange(Data)
        } else {
            df <- read_delim(paste0("~/DadosHidroWeb/",
                                    input$station,"/",
                                    input$Vars,"_T_",
                                    input$station,".zip"),
                             skip = 13, delim = ";", col_names = TRUE,
                             locale = locale(decimal_mark = ",")) %>%
                filter(MediaDiaria == "1")
        }
        
        # Seleciona variável
        if(input$Vars == "cotas"){
            
            # Seleciona o tipo de série que se deseja gerar
            if(input$serie == "med_dia"){
                df <- df %>%
                    dplyr::select(c("Data", "NivelConsistencia", "Cota01":"Cota31")) %>%
                    mutate(Data = as.Date(Data, format = "%d/%m/%Y")) %>%
                    gather(key = Dia, value = Cota, -Data, - NivelConsistencia) %>%
                    filter(format(Data, "%d") == "01") %>%
                    mutate(Dia = as.numeric(substr(Dia,5,6)),
                           Data = as.Date(paste0(format(Data, "%Y-%m-"),Dia))) %>%
                    filter(complete.cases(Data)) %>%
                    dplyr::select(c("Data", "Cota", "NivelConsistencia")) %>%
                    arrange(Data)
            } else if(input$serie == "med_mes"){
                df <- df %>%
                    dplyr::select(c("Data", "NivelConsistencia", "Cota01":"Cota31")) %>%
                    mutate(Data = as.Date(Data, format = "%d/%m/%Y")) %>%
                    gather(key = Dia, value = Cota, -Data, - NivelConsistencia) %>%
                    filter(format(Data, "%d") == "01") %>%
                    mutate(Dia = as.numeric(substr(Dia,5,6)),
                           Data = as.Date(paste0(format(Data, "%Y-%m-"),Dia)),
                           Mes = as.Date(paste0("01-",format(Data, "%m-%Y")),
                                         "%d-%m-%Y")) %>%
                    filter(complete.cases(Data)) %>%
                    group_by(Mes) %>%
                    dplyr::summarize(Cota = round(mean(Cota, na.rm = TRUE)),2) %>%
                    ungroup() %>%
                    rename(Data = Mes) %>%
                    dplyr::select(c("Data", "Cota")) %>%
                    arrange(Data)
                
            } else if(input$serie == "med_ano"){
                
                breaks <- seq(as.Date("1934-11-01"), length=4, by="year")
                
                df <- df %>%
                    dplyr::select(c("Data", "NivelConsistencia", "Cota01":"Cota31")) %>%
                    mutate(Data = as.Date(Data, format = "%d/%m/%Y")) %>%
                    gather(key = Dia, value = Cota, -Data, - NivelConsistencia) %>%
                    filter(format(Data, "%d") == "01") %>%
                    mutate(Dia = as.numeric(substr(Dia,5,6)),
                           Data = as.Date(paste0(format(Data, "%Y-%m-"),Dia)),
                           Mes = as.Date(paste0("01-",format(Data, "%m-%Y")),
                                         "%d-%m-%Y")) %>%
                    filter(complete.cases(Data)) %>%
                    filter(complete.cases(Cota))
                
                T1 <- as.numeric(as.character(format(min(df$Data), "%Y")))-1
                T2 <- as.numeric(as.character(format(max(df$Data), "%Y")))+2
                
                breaks <- seq(as.Date(paste0(T1,"-", input$ano_hidro, "-01"), "%Y-%m-%d"), length = I(T2-T1), by="year")
                
                df$ano_hidro <- cut(df$Data, breaks, labels = T1:I(T2-2))
                df$ano_hidro <- as.Date(paste0(df$ano_hidro,"-", input$ano_hidro,"-01"))
                
                df <- df %>% group_by(ano_hidro) %>%
                    dplyr::summarize(Cota = round(mean(Cota, na.rm = TRUE)),2) %>%
                    ungroup() %>%
                    rename(Data = ano_hidro) %>%
                    dplyr::select(c("Data", "Cota")) %>%
                    arrange(Data)
                
            } else if(input$serie == "max_ano"){
                
                breaks <- seq(as.Date("1934-11-01"), length=4, by="year")
                
                df <- df %>%
                    dplyr::select(c("Data", "NivelConsistencia", "Cota01":"Cota31")) %>%
                    mutate(Data = as.Date(Data, format = "%d/%m/%Y")) %>%
                    gather(key = Dia, value = Cota, -Data, - NivelConsistencia) %>%
                    filter(format(Data, "%d") == "01") %>%
                    mutate(Dia = as.numeric(substr(Dia,5,6)),
                           Data = as.Date(paste0(format(Data, "%Y-%m-"),Dia)),
                           Mes = as.Date(paste0("01-",format(Data, "%m-%Y")),
                                         "%d-%m-%Y")) %>%
                    filter(complete.cases(Data)) %>%
                    filter(complete.cases(Cota))
                
                T1 <- as.numeric(as.character(format(min(df$Data), "%Y")))-1
                T2 <- as.numeric(as.character(format(max(df$Data), "%Y")))+2
                
                breaks <- seq(as.Date(paste0(T1,"-", input$ano_hidro, "-01"), "%Y-%m-%d"), length = I(T2-T1), by="year")
                
                df$ano_hidro <- cut(df$Data, breaks, labels = T1:I(T2-2))
                df$ano_hidro <- as.Date(paste0(df$ano_hidro,"-", input$ano_hidro,"-01"))
                
                df <- df %>% group_by(ano_hidro) %>%
                    dplyr::summarize(Cota = round(max(Cota, na.rm = TRUE)),2) %>%
                    ungroup() %>%
                    rename(Data = ano_hidro) %>%
                    dplyr::select(c("Data", "Cota")) %>%
                    arrange(Data)
                
            } else if(input$serie == "q7"){
                df <- df %>%
                    dplyr::select(c("Data", "NivelConsistencia", "Cota01":"Cota31")) %>%
                    mutate(Data = as.Date(Data, format = "%d/%m/%Y")) %>%
                    gather(key = Dia, value = Cota, -Data, - NivelConsistencia) %>%
                    filter(format(Data, "%d") == "01") %>%
                    mutate(Dia = as.numeric(substr(Dia,5,6)),
                           Data = as.Date(paste0(format(Data, "%Y-%m-"),Dia))) %>%
                    filter(complete.cases(Data)) %>%
                    dplyr::select(c("Data", "Cota", "NivelConsistencia")) %>%
                    arrange(Data)
                
                df$q7 <- na_ma(df$Cota, k = 7, weighting = "exponential", maxgap = Inf)
                df$q7 <- zoo::rollmean(df$q7, k = 7, fill = NA, align = "right")
                df <- df %>%
                    dplyr::select(c("Data", "q7")) %>%
                    rename(Cota = q7)
                
                df
                
            } else if(input$serie == "min_ano"){
                
                df <- df %>%
                    dplyr::select(c("Data", "NivelConsistencia", "Cota01":"Cota31")) %>%
                    mutate(Data = as.Date(Data, format = "%d/%m/%Y")) %>%
                    gather(key = Dia, value = Cota, -Data, - NivelConsistencia) %>%
                    filter(format(Data, "%d") == "01") %>%
                    mutate(Dia = as.numeric(substr(Dia,5,6)),
                           Data = as.Date(paste0(format(Data, "%Y-%m-"),Dia)),
                           Mes = as.Date(paste0("01-",format(Data, "%m-%Y")),
                                         "%d-%m-%Y")) %>%
                    filter(complete.cases(Data)) %>%
                    filter(complete.cases(Cota))
                
                T1 <- as.numeric(as.character(format(min(df$Data), "%Y")))-1
                T2 <- as.numeric(as.character(format(max(df$Data), "%Y")))+2
                
                breaks <- seq(as.Date(paste0(T1,"-", input$ano_hidro, "-01"), "%Y-%m-%d"), length = I(T2-T1), by="year")
                
                df$ano_hidro <- cut(df$Data, breaks, labels = T1:I(T2-2))
                df$ano_hidro <- as.Date(paste0(df$ano_hidro,"-", input$ano_hidro,"-01"))
                
                df <- df %>% group_by(ano_hidro) %>%
                    dplyr::summarize(Cota = round(min(Cota, na.rm = TRUE)),2) %>%
                    ungroup() %>%
                    rename(Data = ano_hidro) %>%
                    dplyr::select(c("Data", "Cota")) %>%
                    arrange(Data)
                
                
            } else if(input$serie == "q7_min"){
                df <- df %>%
                    dplyr::select(c("Data", "NivelConsistencia", "Cota01":"Cota31")) %>%
                    mutate(Data = as.Date(Data, format = "%d/%m/%Y")) %>%
                    gather(key = Dia, value = Cota, -Data, - NivelConsistencia) %>%
                    filter(format(Data, "%d") == "01") %>%
                    mutate(Dia = as.numeric(substr(Dia,5,6)),
                           Data = as.Date(paste0(format(Data, "%Y-%m-"),Dia))) %>%
                    filter(complete.cases(Data)) %>%
                    dplyr::select(c("Data", "Cota", "NivelConsistencia")) %>%
                    arrange(Data) %>%
                    filter(complete.cases(Cota))
                
                df$q7 <- na_ma(df$Cota, k = 7, weighting = "exponential", maxgap = Inf)
                df$q7 <- zoo::rollmean(df$q7, k = 7, fill = NA, align = "right")
                df <- df %>%
                    dplyr::select(c("Data", "q7", "NivelConsistencia")) %>%
                    rename(Cota = q7)
                
                T1 <- as.numeric(as.character(format(min(df$Data), "%Y")))-1
                T2 <- as.numeric(as.character(format(max(df$Data), "%Y")))+2
                
                breaks <- seq(as.Date(paste0(T1,"-", input$ano_hidro, "-01"), "%Y-%m-%d"), length = I(T2-T1), by="year")
                
                df$ano_hidro <- cut(df$Data, breaks, labels = T1:I(T2-2))
                df$ano_hidro <- as.Date(paste0(df$ano_hidro,"-", input$ano_hidro,"-01"))
                
                df <- df %>% group_by(ano_hidro) %>%
                    dplyr::summarize(Cota = round(min(Cota, na.rm = TRUE)),2) %>%
                    ungroup() %>%
                    rename(Data = ano_hidro) %>%
                    dplyr::select(c("Data", "Cota")) %>%
                    arrange(Data)
            } else if(input$serie == "sazonal") {
                df <- df %>%
                    dplyr::select(c("Data", "NivelConsistencia", "Cota01":"Cota31")) %>%
                    mutate(Data = as.Date(Data, format = "%d/%m/%Y")) %>%
                    gather(key = Dia, value = Cota, -Data, - NivelConsistencia) %>%
                    filter(format(Data, "%d") == "01") %>%
                    mutate(Dia = as.numeric(substr(Dia,5,6)),
                           Data = as.Date(paste0(format(Data, "%Y-%m-"),Dia))) %>%
                    filter(complete.cases(Data)) %>%
                    dplyr::select(c("Data", "Cota", "NivelConsistencia")) %>%
                    arrange(Data) %>%
                    filter(complete.cases(Cota))
                
                
                df <- df %>%
                    mutate(Dia_Ano = as.numeric(as.character(format(Data, "%j")))) %>%
                    group_by(Dia_Ano) %>%
                    dplyr::summarize(Mediana = median(Cota, na.rm = TRUE),
                                     Max =  max(Cota, na.rm = TRUE),
                                     Min =  min(Cota, na.rm = TRUE),
                                     Q10 =  quantile(Cota, 0.9, na.rm = TRUE),
                                     Q90 =  quantile(Cota, 0.1, na.rm = TRUE)) %>%
                    ungroup() %>%
                    filter(Dia_Ano != "366")
                
            }
            
        } else if(input$Vars == "vazoes"){
            
            
            # Seleciona o tipo de série que se deseja gerar
            if(input$serie == "med_dia"){
                df <- df %>%
                    dplyr::select(c("Data", "NivelConsistencia", "Vazao01":"Vazao31")) %>%
                    mutate(Data = as.Date(Data, format = "%d/%m/%Y")) %>%
                    gather(key = Dia, value = Vazao, -Data, - NivelConsistencia) %>%
                    filter(format(Data, "%d") == "01") %>%
                    mutate(Dia = as.numeric(substr(Dia,6,7)),
                           Data = as.Date(paste0(format(Data, "%Y-%m-"),Dia))) %>%
                    filter(complete.cases(Data)) %>%
                    dplyr::select(c("Data", "Vazao", "NivelConsistencia")) %>%
                    arrange(Data)
            } else if(input$serie == "med_mes"){
                df <- df %>%
                    dplyr::select(c("Data", "NivelConsistencia", "Vazao01":"Vazao31")) %>%
                    mutate(Data = as.Date(Data, format = "%d/%m/%Y")) %>%
                    gather(key = Dia, value = Vazao, -Data, - NivelConsistencia) %>%
                    filter(format(Data, "%d") == "01") %>%
                    mutate(Dia = as.numeric(substr(Dia,6,7)),
                           Data = as.Date(paste0(format(Data, "%Y-%m-"),Dia)),
                           Mes = as.Date(paste0("01-",format(Data, "%m-%Y")),
                                         "%d-%m-%Y")) %>%
                    filter(complete.cases(Data)) %>%
                    group_by(Mes) %>%
                    dplyr::summarize(Vazao = round(mean(Vazao, na.rm = TRUE)),2) %>%
                    ungroup() %>%
                    rename(Data = Mes) %>%
                    dplyr::select(c("Data", "Vazao")) %>%
                    arrange(Data)
                
            } else if(input$serie == "med_ano"){
                
                breaks <- seq(as.Date("1934-11-01"), length=4, by="year")
                
                df <- df %>%
                    dplyr::select(c("Data", "NivelConsistencia", "Vazao01":"Vazao31")) %>%
                    mutate(Data = as.Date(Data, format = "%d/%m/%Y")) %>%
                    gather(key = Dia, value = Vazao, -Data, - NivelConsistencia) %>%
                    filter(format(Data, "%d") == "01") %>%
                    mutate(Dia = as.numeric(substr(Dia,6,7)),
                           Data = as.Date(paste0(format(Data, "%Y-%m-"),Dia)),
                           Mes = as.Date(paste0("01-",format(Data, "%m-%Y")),
                                         "%d-%m-%Y")) %>%
                    filter(complete.cases(Data)) %>%
                    filter(complete.cases(Vazao))
                
                T1 <- as.numeric(as.character(format(min(df$Data), "%Y")))-1
                T2 <- as.numeric(as.character(format(max(df$Data), "%Y")))+2
                
                breaks <- seq(as.Date(paste0(T1,"-", input$ano_hidro, "-01"), "%Y-%m-%d"), length = I(T2-T1), by="year")
                
                df$ano_hidro <- cut(df$Data, breaks, labels = T1:I(T2-2))
                df$ano_hidro <- as.Date(paste0(df$ano_hidro,"-", input$ano_hidro,"-01"))
                
                df <- df %>% group_by(ano_hidro) %>%
                    dplyr::summarize(Vazao = round(mean(Vazao, na.rm = TRUE)),2) %>%
                    ungroup() %>%
                    rename(Data = ano_hidro) %>%
                    dplyr::select(c("Data", "Vazao")) %>%
                    arrange(Data)
                
            } else if(input$serie == "max_ano"){
                
                breaks <- seq(as.Date("1934-11-01"), length=4, by="year")
                
                df <- df %>%
                    dplyr::select(c("Data", "NivelConsistencia", "Vazao01":"Vazao31")) %>%
                    mutate(Data = as.Date(Data, format = "%d/%m/%Y")) %>%
                    gather(key = Dia, value = Vazao, -Data, - NivelConsistencia) %>%
                    filter(format(Data, "%d") == "01") %>%
                    mutate(Dia = as.numeric(substr(Dia,6,7)),
                           Data = as.Date(paste0(format(Data, "%Y-%m-"),Dia)),
                           Mes = as.Date(paste0("01-",format(Data, "%m-%Y")),
                                         "%d-%m-%Y")) %>%
                    filter(complete.cases(Data)) %>%
                    filter(complete.cases(Vazao))
                
                T1 <- as.numeric(as.character(format(min(df$Data), "%Y")))-1
                T2 <- as.numeric(as.character(format(max(df$Data), "%Y")))+2
                
                breaks <- seq(as.Date(paste0(T1,"-", input$ano_hidro, "-01"), "%Y-%m-%d"), length = I(T2-T1), by="year")
                
                df$ano_hidro <- cut(df$Data, breaks, labels = T1:I(T2-2))
                df$ano_hidro <- as.Date(paste0(df$ano_hidro,"-", input$ano_hidro,"-01"))
                
                df <- df %>% group_by(ano_hidro) %>%
                    dplyr::summarize(Vazao = round(max(Vazao, na.rm = TRUE)),2) %>%
                    ungroup() %>%
                    rename(Data = ano_hidro) %>%
                    dplyr::select(c("Data", "Vazao")) %>%
                    arrange(Data)
                
            } else if(input$serie == "q7"){
                df <- df %>%
                    dplyr::select(c("Data", "NivelConsistencia", "Vazao01":"Vazao31")) %>%
                    mutate(Data = as.Date(Data, format = "%d/%m/%Y")) %>%
                    gather(key = Dia, value = Vazao, -Data, - NivelConsistencia) %>%
                    filter(format(Data, "%d") == "01") %>%
                    mutate(Dia = as.numeric(substr(Dia,6,7)),
                           Data = as.Date(paste0(format(Data, "%Y-%m-"),Dia))) %>%
                    filter(complete.cases(Data)) %>%
                    dplyr::select(c("Data", "Vazao", "NivelConsistencia")) %>%
                    arrange(Data)
                
                df$q7 <- na_ma(df$Vazao, k = 7, weighting = "exponential", maxgap = Inf)
                df$q7 <- zoo::rollmean(df$q7, k = 7, fill = NA, align = "right")
                df <- df %>%
                    dplyr::select(c("Data", "q7")) %>%
                    rename(Vazao = q7)
                
                df
                
            } else if(input$serie == "min_ano"){
                
                df <- df %>%
                    dplyr::select(c("Data", "NivelConsistencia", "Vazao01":"Vazao31")) %>%
                    mutate(Data = as.Date(Data, format = "%d/%m/%Y")) %>%
                    gather(key = Dia, value = Vazao, -Data, - NivelConsistencia) %>%
                    filter(format(Data, "%d") == "01") %>%
                    mutate(Dia = as.numeric(substr(Dia,6,7)),
                           Data = as.Date(paste0(format(Data, "%Y-%m-"),Dia)),
                           Mes = as.Date(paste0("01-",format(Data, "%m-%Y")),
                                         "%d-%m-%Y")) %>%
                    filter(complete.cases(Data)) %>%
                    filter(complete.cases(Vazao))
                
                T1 <- as.numeric(as.character(format(min(df$Data), "%Y")))-1
                T2 <- as.numeric(as.character(format(max(df$Data), "%Y")))+2
                
                breaks <- seq(as.Date(paste0(T1,"-", input$ano_hidro, "-01"), "%Y-%m-%d"), length = I(T2-T1), by="year")
                
                df$ano_hidro <- cut(df$Data, breaks, labels = T1:I(T2-2))
                df$ano_hidro <- as.Date(paste0(df$ano_hidro,"-", input$ano_hidro,"-01"))
                
                df <- df %>% group_by(ano_hidro) %>%
                    dplyr::summarize(Vazao = round(min(Vazao, na.rm = TRUE)),2) %>%
                    ungroup() %>%
                    rename(Data = ano_hidro) %>%
                    dplyr::select(c("Data", "Vazao")) %>%
                    arrange(Data)
                
                
            } else if(input$serie == "q7_min"){
                df <- df %>%
                    dplyr::select(c("Data", "NivelConsistencia", "Vazao01":"Vazao31")) %>%
                    mutate(Data = as.Date(Data, format = "%d/%m/%Y")) %>%
                    gather(key = Dia, value = Vazao, -Data, - NivelConsistencia) %>%
                    filter(format(Data, "%d") == "01") %>%
                    mutate(Dia = as.numeric(substr(Dia,6,7)),
                           Data = as.Date(paste0(format(Data, "%Y-%m-"),Dia))) %>%
                    filter(complete.cases(Data)) %>%
                    dplyr::select(c("Data", "Vazao", "NivelConsistencia")) %>%
                    arrange(Data) %>%
                    filter(complete.cases(Vazao))
                
                df$q7 <- na_ma(df$Vazao, k = 7, weighting = "exponential", maxgap = Inf)
                df$q7 <- zoo::rollmean(df$q7, k = 7, fill = NA, align = "right")
                
                df <- df %>%
                    dplyr::select(c("Data", "q7", "NivelConsistencia")) %>%
                    rename(Vazao = q7)
                
                T1 <- as.numeric(as.character(format(min(df$Data), "%Y")))-1
                T2 <- as.numeric(as.character(format(max(df$Data), "%Y")))+2
                
                breaks <- seq(as.Date(paste0(T1,"-", input$ano_hidro, "-01"), "%Y-%m-%d"), length = I(T2-T1), by="year")
                
                df$ano_hidro <- cut(df$Data, breaks, labels = T1:I(T2-2))
                df$ano_hidro <- as.Date(paste0(df$ano_hidro,"-", input$ano_hidro,"-01"))
                
                df <- df %>% group_by(ano_hidro) %>%
                    dplyr::summarize(Vazao = round(min(Vazao, na.rm = TRUE)),2) %>%
                    ungroup() %>%
                    rename(Data = ano_hidro) %>%
                    dplyr::select(c("Data", "Vazao")) %>%
                    arrange(Data)
            } else if(input$serie == "sazonal") {
                df <- df %>%
                    dplyr::select(c("Data", "NivelConsistencia", "Vazao01":"Vazao31")) %>%
                    mutate(Data = as.Date(Data, format = "%d/%m/%Y")) %>%
                    gather(key = Dia, value = Vazao, -Data, - NivelConsistencia) %>%
                    filter(format(Data, "%d") == "01") %>%
                    mutate(Dia = as.numeric(substr(Dia,6,7)),
                           Data = as.Date(paste0(format(Data, "%Y-%m-"),Dia))) %>%
                    filter(complete.cases(Data)) %>%
                    dplyr::select(c("Data", "Vazao", "NivelConsistencia")) %>%
                    arrange(Data) %>%
                    filter(complete.cases(Vazao))
                
                df <- df %>%
                    mutate(Dia_Ano = as.numeric(as.character(format(Data, "%j")))) %>%
                    group_by(Dia_Ano) %>%
                    dplyr::summarize(Mediana = median(Vazao, na.rm = TRUE),
                                     Max =  max(Vazao, na.rm = TRUE),
                                     Min =  min(Vazao, na.rm = TRUE),
                                     Q10 =  quantile(Vazao, 0.9, na.rm = TRUE),
                                     Q90 =  quantile(Vazao, 0.1, na.rm = TRUE)) %>%
                    ungroup() %>%
                    filter(Dia_Ano != "366")
                
            }
            
            
        }
        
    })
    
    # Sumário estatístico
    output$sumario <- DT::renderDataTable({
        df <- serieHistorica()
        
        if(input$Vars == "cotas"){
            
            if(input$serie == "med_dia"){
                df <- dplyr::summarize(df,
                                       CotaMediaDiaria = round(mean(Cota, na.rm = TRUE),2),
                                       DesvioPadrao = round(sd(Cota, na.rm = TRUE),2),
                                       Assimetria = round(skewness(Cota, na.rm = TRUE),2),
                                       CotaMaximaDiaria = round(max(Cota, na.rm = TRUE),2),
                                       DataMaximaDiaria = Data[which.max(Cota)],
                                       CotaMinDiaria = round(min(Cota, na.rm = TRUE),2),
                                       DataMinimaDiaria = Data[which.min(Cota)])
                df <- DT::datatable(df, options = list(lengthMenu = c("")))
            
                    
            } else if(input$serie == "med_mes"){
                df <- dplyr::summarize(df,
                                       CotaMediaMensal = round(mean(Cota, na.rm = TRUE),2),
                                       DesvioPadrao = round(sd(Cota, na.rm = TRUE),2),
                                       Assimetria = round(skewness(Cota, na.rm = TRUE),2),
                                       CotaMaximaMensal = round(max(Cota, na.rm = TRUE),2),
                                       DataMaximaMensal = Data[which.max(Cota)],
                                       CotaMinMensal = round(min(Cota, na.rm = TRUE),2),
                                       DataMinimaMensal = Data[which.min(Cota)])
                df <- DT::datatable(df, options = list(lengthMenu = c("")))
                
                
            } else if(input$serie == "med_ano"){
                df <- dplyr::summarize(df,
                                       CotaMediaAnual = round(mean(Cota, na.rm = TRUE),2),
                                       DesvioPadrao = round(sd(Cota, na.rm = TRUE),2),
                                       Assimetria = round(skewness(Cota, na.rm = TRUE),2),
                                       CotaMaximaAnual = round(max(Cota, na.rm = TRUE),2),
                                       DataMaximaAnual = Data[which.max(Cota)],
                                       CotaMinAnual = round(min(Cota, na.rm = TRUE),2),
                                       DataMinimaAnual = Data[which.min(Cota)])
                df <- DT::datatable(df, options = list(lengthMenu = c("")))
                
                
            } else if(input$serie == "max_ano"){
                df <- dplyr::summarize(df,
                                       CotaMaximaMediaAnual = round(mean(Cota, na.rm = TRUE),2),
                                       DesvioPadrao = round(sd(Cota, na.rm = TRUE),2),
                                       Assimetria = round(skewness(Cota, na.rm = TRUE),2),
                                       CotaMaxima = round(max(Cota, na.rm = TRUE),2),
                                       DataMaxima = Data[which.max(Cota)],
                                       CotaMaximaMinimaAnual = round(min(Cota, na.rm = TRUE),2),
                                       DataMaximaMinima = Data[which.min(Cota)])
                df <- DT::datatable(df, options = list(lengthMenu = c("")))
                
            } else if(input$serie == "q7"){
                
                df <- dplyr::summarize(df,
                                       CotaMinimaMediaAnual = round(mean(Cota, na.rm = TRUE),2),
                                       DesvioPadrao = round(sd(Cota, na.rm = TRUE),2),
                                       Assimetria = round(skewness(Cota, na.rm = TRUE),2),
                                       CotaMinimaMaxima = round(max(Cota, na.rm = TRUE),2),
                                       DataMinimaMaxima = Data[which.max(Cota)],
                                       CotaMinima = round(min(Cota, na.rm = TRUE),2),
                                       DataMinima = Data[which.min(Cota)])
                df <- DT::datatable(df, options = list(lengthMenu = c("")))
                
            } else if(input$serie == "min_ano"){
                
                df <- dplyr::summarize(df,
                                       CotaMinimaMediaAnual = round(mean(Cota, na.rm = TRUE),2),
                                       DesvioPadrao = round(sd(Cota, na.rm = TRUE),2),
                                       Assimetria = round(skewness(Cota, na.rm = TRUE),2),
                                       CotaMinimaMaxima = round(max(Cota, na.rm = TRUE),2),
                                       DataMinimaMaxima = Data[which.max(Cota)],
                                       CotaMinima = round(min(Cota, na.rm = TRUE),2),
                                       DataMinima = Data[which.min(Cota)])
                df <- DT::datatable(df, options = list(lengthMenu = c("")))
                
            } else if(input$serie == "q7_min"){
                
                if(sum(df$Cota < 0) == 0){
                    fw <- fitdist(df$Cota, "weibull")
                    
                    df <- dplyr::summarize(df,
                                           Cota7MediaAnual = round(mean(Cota, na.rm = TRUE),2),
                                           DesvioPadrao = round(sd(Cota, na.rm = TRUE),2),
                                           Assimetria = round(skewness(Cota, na.rm = TRUE),2),
                                           Cota7Maxima = round(max(Cota, na.rm = TRUE),2),
                                           DataCota7Maxima = Data[which.max(Cota)],
                                           Cota7Minima = round(min(Cota, na.rm = TRUE),2),
                                           DataCota7Minima = Data[which.min(Cota)],
                                           Q710 = as.numeric(fw$estimate["scale"]) * (-log(1-1/10))^(1/as.numeric(fw$estimate["shape"])))
                    df <- DT::datatable(df, options = list(lengthMenu = c("")))
                } else {
                    df <- dplyr::summarize(df,
                                           Cota7MediaAnual = round(mean(Cota, na.rm = TRUE),2),
                                           DesvioPadrao = round(sd(Cota, na.rm = TRUE),2),
                                           Assimetria = round(skewness(Cota, na.rm = TRUE),2),
                                           Cota7Maxima = round(max(Cota, na.rm = TRUE),2),
                                           DataCota7Maxima = Data[which.max(Cota)],
                                           Cota7Minima = round(min(Cota, na.rm = TRUE),2),
                                           DataCota7Minima = Data[which.min(Cota)])
                    df <- DT::datatable(df, options = list(lengthMenu = c("")))
                }
                
                
            } else if(input$serie == "sazonal"){
             Hoje <- as.numeric(as.character(format(Sys.Date(), "%j")))
             df <- df[df$Dia_Ano == Hoje,]
             df <- dplyr::select(df, "Mediana", "Max", "Min", "Q90", "Q10")
             colnames(df) <- paste("Cota",c("Mediana", "Máxima", "Mínima", "Q90", "Q10"), "em", format(Sys.Date(), "%d de %B"))
             df <- DT::datatable(df, options = list(lengthMenu = c("")))
                   
            }
            
        } else if(input$Vars == "vazoes"){
            
            if(input$serie == "med_dia"){
                df <- dplyr::summarize(df,
                                       VazaoMediaDiaria = round(mean(Vazao, na.rm = TRUE),2),
                                       DesvioPadrao = round(sd(Vazao, na.rm = TRUE),2),
                                       Assimetria = round(skewness(Vazao, na.rm = TRUE),2),
                                       VazaoMaximaDiaria = round(max(Vazao, na.rm = TRUE),2),
                                       DataMaximaDiaria = Data[which.max(Vazao)],
                                       VazaoMinDiaria = round(min(Vazao, na.rm = TRUE),2),
                                       DataMinimaDiaria = Data[which.min(Vazao)])
                df <- DT::datatable(df, options = list(lengthMenu = c("")))
                
                
            } else if(input$serie == "med_mes"){
                df <- dplyr::summarize(df,
                                       VazaoMediaMensal = round(mean(Vazao, na.rm = TRUE),2),
                                       DesvioPadrao = round(sd(Vazao, na.rm = TRUE),2),
                                       Assimetria = round(skewness(Vazao, na.rm = TRUE),2),
                                       VazaoMaximaMensal = round(max(Vazao, na.rm = TRUE),2),
                                       DataMaximaMensal = Data[which.max(Vazao)],
                                       VazaoMinMensal = round(min(Vazao, na.rm = TRUE),2),
                                       DataMinimaMensal = Data[which.min(Vazao)])
                df <- DT::datatable(df, options = list(lengthMenu = c("")))
                
                
            } else if(input$serie == "med_ano"){
                df <- dplyr::summarize(df,
                                       VazaoMediaAnual = round(mean(Vazao, na.rm = TRUE),2),
                                       DesvioPadrao = round(sd(Vazao, na.rm = TRUE),2),
                                       Assimetria = round(skewness(Vazao, na.rm = TRUE),2),
                                       VazaoMaximaAnual = round(max(Vazao, na.rm = TRUE),2),
                                       DataMaximaAnual = Data[which.max(Vazao)],
                                       VazaoMinAnual = round(min(Vazao, na.rm = TRUE),2),
                                       DataMinimaAnual = Data[which.min(Vazao)])
                df <- DT::datatable(df, options = list(lengthMenu = c("")))
                
                
            } else if(input$serie == "max_ano"){
                df <- dplyr::summarize(df,
                                       VazaoMaximaMediaAnual = round(mean(Vazao, na.rm = TRUE),2),
                                       DesvioPadrao = round(sd(Vazao, na.rm = TRUE),2),
                                       Assimetria = round(skewness(Vazao, na.rm = TRUE),2),
                                       VazaoMaxima = round(max(Vazao, na.rm = TRUE),2),
                                       DataMaxima = Data[which.max(Vazao)],
                                       VazaoMaximaMinimaAnual = round(min(Vazao, na.rm = TRUE),2),
                                       DataMaximaMinima = Data[which.min(Vazao)])
                df <- DT::datatable(df, options = list(lengthMenu = c("")))
                
            } else if(input$serie == "q7"){
                
                df <- dplyr::summarize(df,
                                       VazaoMinimaMediaAnual = round(mean(Vazao, na.rm = TRUE),2),
                                       DesvioPadrao = round(sd(Vazao, na.rm = TRUE),2),
                                       Assimetria = round(skewness(Vazao, na.rm = TRUE),2),
                                       VazaoMinimaMaxima = round(max(Vazao, na.rm = TRUE),2),
                                       DataMinimaMaxima = Data[which.max(Vazao)],
                                       VazaoMinima = round(min(Vazao, na.rm = TRUE),2),
                                       DataMinima = Data[which.min(Vazao)])
                df <- DT::datatable(df, options = list(lengthMenu = c("")))
                
            } else if(input$serie == "min_ano"){
                
                df <- dplyr::summarize(df,
                                       VazaoMinimaMediaAnual = round(mean(Vazao, na.rm = TRUE),2),
                                       DesvioPadrao = round(sd(Vazao, na.rm = TRUE),2),
                                       Assimetria = round(skewness(Vazao, na.rm = TRUE),2),
                                       VazaoMinimaMaxima = round(max(Vazao, na.rm = TRUE),2),
                                       DataMinimaMaxima = Data[which.max(Vazao)],
                                       VazaoMinima = round(min(Vazao, na.rm = TRUE),2),
                                       DataMinima = Data[which.min(Vazao)])
                df <- DT::datatable(df, options = list(lengthMenu = c("")))
                
            } else if(input$serie == "q7_min"){
                
                if(sum(df$Cota < 0) == 0){
                    fw <- fitdist(df$Vazao, "weibull")
                    
                    df <- dplyr::summarize(df,
                                           Vazao7MediaAnual = round(mean(Vazao, na.rm = TRUE),2),
                                           DesvioPadrao = round(sd(Vazao, na.rm = TRUE),2),
                                           Assimetria = round(skewness(Vazao, na.rm = TRUE),2),
                                           Vazao7Maxima = round(max(Vazao, na.rm = TRUE),2),
                                           DataVazao7Maxima = Data[which.max(Vazao)],
                                           Vazao7Minima = round(min(Vazao, na.rm = TRUE),2),
                                           DataVazao7Minima = Data[which.min(Vazao)],
                                           Q710 = as.numeric(fw$estimate["scale"]) * (-log(1-1/10))^(1/as.numeric(fw$estimate["shape"])))
                    df <- DT::datatable(df, options = list(lengthMenu = c("")))
                } else {
                    df <- dplyr::summarize(df,
                                           Vazao7MediaAnual = round(mean(Vazao, na.rm = TRUE),2),
                                           DesvioPadrao = round(sd(Vazao, na.rm = TRUE),2),
                                           Assimetria = round(skewness(Vazao, na.rm = TRUE),2),
                                           Vazao7Maxima = round(max(Vazao, na.rm = TRUE),2),
                                           DataVazao7Maxima = Data[which.max(Vazao)],
                                           Vazao7Minima = round(min(Vazao, na.rm = TRUE),2),
                                           DataVazao7Minima = Data[which.min(Vazao)])
                    df <- DT::datatable(df, options = list(lengthMenu = c("")))
                }
            } else if(input$serie == "sazonal"){
                
                Hoje <- as.numeric(as.character(format(Sys.Date(), "%j")))
                df <- df[df$Dia_Ano == Hoje,]
                df <- dplyr::select(df, "Mediana", "Max", "Min", "Q90", "Q10")
                colnames(df) <- paste("Vazão",c("Mediana", "Máxima", "Mínima", "Q90", "Q10"), "em", format(Sys.Date(), "%d de %B"))
                df <- DT::datatable(df, options = list(lengthMenu = c("")))
                
            }
            
            
        }
        
        
    })
    
    output$dt_tabela <- DT::renderDataTable({
        serieHistorica()
    })
    
    output$grafico <- renderPlotly({
        
        tryCatch({
            df <- serieHistorica()
            
            if(input$serie == "sazonal"){
                if(input$Vars == "cotas"){
                    tituloEixo <- "Cotas (cm)"
                    df$Dia_Ano <- as.numeric(as.character(df$Dia_Ano))
                    Texto <- "A zona de atenção para máximas compreende as cotas entre aquela com 10% de permanência e a cota máxima histórica para aquele dia, enquanto a zona de mínimas compreende a cota com 90% de permanência e a cota mínima."
                    plot_ly(x = df$Dia_Ano, y = df$Mediana,
                            type = 'scatter', mode = 'lines',
                            hoverinfo = "text", name = "Mediana",
                            text = ~paste("</br> Dia no ano:", df$Dia_Ano,
                                          "</br> Cota: ", df$Mediana, "cm")) %>% layout(
                                              title = paste("Grafico da estacao Codigo", input$station),
                                              xaxis = list(title = "Dia no ano"),
                                              yaxis = list(title = tituloEixo),
                                              annotations = list(x = 0.03 , y = 1.03,showarrow = F, xref='paper', yref='paper',
                                                                 text = Texto)) %>%
                        add_ribbons(x = df$Dia_Ano,
                                    ymin = df$Min, ymax = df$Q90,
                                    line = list(color = rgb(1,0.5,0,0.8)),
                                    fillcolor = rgb(1,0.5,0,0.4),
                                    name = "Zona de atenção para Mínimas") %>%
                        add_ribbons(x = df$Dia_Ano,
                                    ymax = df$Max, ymin = df$Q10,
                                    line = list(color = rgb(0.2,0.2,0.8,0.8)),
                                    fillcolor = rgb(0.2,0.2,0.8,0.4),
                                    name = "Atenção para Máximas") %>%
                        add_lines(x = df$Dia_Ano, y = df$Min, hoverinfo = "text", showlegend = FALSE,
                                  line = list(color = rgb(1,0.5,0,0.4), width = 0.5),
                                  text = ~paste("</br> Dia no ano:", df$Dia_Ano, "</br> Cota: ", df$Min, "cm")) %>%
                        add_lines(x = df$Dia_Ano, y = df$Q90, hoverinfo = "text", showlegend = FALSE,
                                  line = list(color = rgb(1,0.5,0,0.4), width = 0.5),
                                  text = ~paste("</br> Dia no ano:", df$Dia_Ano, "</br> Cota: ", df$Q90, "cm")) %>%
                        add_lines(x = df$Dia_Ano, y = df$Max, hoverinfo = "text", showlegend = FALSE,
                                  line = list(color = rgb(0.2,0.2,0.8,0.8), width = 0.5),
                                  text = ~paste("</br> Dia no ano:", df$Dia_Ano, "</br> Cota: ", df$Max, "cm")) %>%
                        add_lines(x = df$Dia_Ano, y = df$Q10, hoverinfo = "text", showlegend = FALSE, ,
                                  line = list(color = rgb(0.2,0.2,0.8,0.8), width = 0.5),
                                  text = ~paste("</br> Dia no ano:", df$Dia_Ano, "</br> Cota: ", df$Q10, "cm"))
                    
                    
                } else if(input$Vars == "vazoes"){
                    tituloEixo <- "Vazões (m³/s)"
                    df$Dia_Ano <- as.numeric(as.character(df$Dia_Ano))
                    Texto <- "A zona de atenção para máximas compreende as vazões entre aquela com 10% de permanência e a vazão máxima histórica para aquele dia, enquanto a zona de mínimas compreende a vazão com 90% de permanência e a vazão mínima."
                    
                    plot_ly(x = df$Dia_Ano, y = df$Mediana,
                            type = 'scatter', mode = 'lines',
                            hoverinfo = "text", name = "Mediana",
                            text = ~paste("</br> Dia no ano:", df$Dia_Ano,
                                          "</br> Vazão: ", df$Mediana, "m³/s")) %>% layout(
                                              title = paste("Grafico da estacao Codigo", input$station),
                                              xaxis = list(title = "Dia no ano"),
                                              yaxis = list(title = tituloEixo),
                                              annotations = list(x = 0.03 , y = 1.03,showarrow = F, xref='paper', yref='paper',
                                                                 text = Texto)) %>%
                        add_ribbons(x = df$Dia_Ano, ymin = df$Min, ymax = df$Q90,
                                    line = list(color = rgb(1,0.5,0,0.8)), fillcolor = rgb(1,0.5,0,0.4),
                                    name = "Zona de atenção para Mínimas") %>%
                        add_ribbons(x = df$Dia_Ano,ymax = df$Max, ymin = df$Q10,
                                    line = list(color = rgb(0.2,0.2,0.8,0.8)), fillcolor = rgb(0.2,0.2,0.8,0.4),
                                    name = "Atenção para Máximas") %>%
                        add_lines(x = df$Dia_Ano, y = df$Min, hoverinfo = "text", showlegend = FALSE,
                                  line = list(color = rgb(1,0.5,0,0.4), width = 0.5),
                                  text = ~paste("</br> Dia no ano:", df$Dia_Ano, "</br> Vazão: ", df$Min, "m³/s")) %>%
                        add_lines(x = df$Dia_Ano, y = df$Q90, hoverinfo = "text", showlegend = FALSE,
                                  line = list(color = rgb(1,0.5,0,0.4), width = 0.5),
                                  text = ~paste("</br> Dia no ano:", df$Dia_Ano, "</br> Vazão: ", df$Q90, "m³/s")) %>%
                        add_lines(x = df$Dia_Ano, y = df$Max, hoverinfo = "text", showlegend = FALSE,
                                  line = list(color = rgb(0.2,0.2,0.8,0.8), width = 0.5),
                                  text = ~paste("</br> Dia no ano:", df$Dia_Ano, "</br> Vazão: ", df$Max, "m³/s")) %>%
                        add_lines(x = df$Dia_Ano, y = df$Q10, hoverinfo = "text", showlegend = FALSE,
                                  line = list(color = rgb(0.2,0.2,0.8,0.8), width = 0.5),
                                  text = ~paste("</br> Dia no ano:", df$Dia_Ano, "</br> Vazão: ", df$Q10, "m³/s"))
                    
                }
            } else {
                x <- as.Date(df$Data, origin = "1970-01-01")
                
                if(input$Vars == "cotas"){
                    tituloEixo <- "Cotas (cm)"
                    y <- df$Cota
                    y <- as.numeric(as.character(y))
                    
                    plot_ly(x = x, y = y,
                            type = 'scatter', mode = 'lines',
                            hoverinfo = "text",
                            text = ~paste("</br> Data:", x,
                                          "</br> Cota: ", y, "cm")) %>% layout(
                                              title = paste("Grafico da estacao Codigo", input$station),
                                              xaxis = list(
                                                  title = "Data"
                                              ),
                                              yaxis = list(
                                                  title = tituloEixo
                                              )
                                          )
                    
                } else if(input$Vars == "vazoes"){
                    tituloEixo <- "Vazoes (m3/s)"
                    y <- df$Vazao
                    
                    plot_ly(x = x, y = y,
                            type = 'scatter', mode = 'lines',
                            hoverinfo = "text",
                            text = ~paste("</br> Data:", x,
                                          "</br> Vazão:", y, "m³/s")) %>% layout(
                                              title = paste("Grafico da estacao Codigo", input$station),
                                              xaxis = list(
                                                  title = "Data"
                                              ),
                                              yaxis = list(
                                                  title = tituloEixo
                                              )
                                          )
                }
            }
        },
        error = function(e) {
            stop(safeError(e))
        })
        
    })
    
    output$downloadData <- downloadHandler(
        filename = function() paste0(input$station, "_",
                                     input$serie, "_",
                                     input$Vars,".csv"),
        content = function(file) {
            write.csv2(serieHistorica(), file, row.names = FALSE)
        }
    )
    
    
    
}

shinyApp(ui, server)