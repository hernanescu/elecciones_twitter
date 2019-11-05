library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(wordcloud)
library(tidytext)
library(tm)
library(proustr)
library(ggwordcloud)
library(openxlsx)


base <- read_csv('data/elecciones2019_total.csv') %>% 
    select(text, created, isRetweet, favoriteCount)

sdal <- read.xlsx('data/SDAL.xlsx') %>% 
    rename('word'=palabra) %>% 
    mutate(media_agrado=as.numeric(media_agrado)) %>% 
    select(word, media_agrado) %>% 
    mutate(media_agrado=case_when(word=='funcionar'~2.625,
                                  word=='clásico'~2.625,
                                  word=='cocina'~2.625,
                                  word=='concepto'~2.375,
                                  word=='conciencia'~2.375,
                                  word=='haz'~2.375,
                                  word=='profesional'~2.375,
                                  word=='teléfono'~2.125,
                                  word=='tono'~2.125,
                                  TRUE~media_agrado)) 

source('data/scripts_app.R')

token_words <- c('rt', 'q', 'v', 'mg', 'fav')

ui <- fluidPage(theme = shinytheme('superhero'),

    # Application title
    titlePanel("#EleccionesArgentina - ¿Qué se dijo en Twitter?"),
    hr(),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h3('Parámetros'),
            hr(),
            h4('Ventana de publicación de tweets'),
            sliderInput("timeRange", label='',
                        min = as.POSIXct("2019-10-27 16:27:37"),
                        max = as.POSIXct("2019-10-28 02:41:05"),
                        value = c(as.POSIXct("2019-10-27 20:00:00"),
                                  as.POSIXct("2019-10-27 21:00:00")),
                        ticks = FALSE,
                        timeFormat = '%T'),
            #actionButton("update", "Update range"),
            hr(),
            h4('Filtro de retweets'),
            switchInput(label = '¿Dejar retweets?', inputId = 'retweet', value = TRUE,
                        onLabel = 'Sí', offLabel = 'No'),
            hr(),
            h3('Sobre la app'),
            hr(),
            h4('¡Bienvenidx!'),
            p('Soy Hernán Escudero, data scientist y sociólogo.'),
            p('El 27 de octubre se llevaron a cabo las elecciones generales en Argentina. 
              Esta app muestra algunos insights relevantes sobre lo que se estuvo twitteando 
              a lo largo de la jornada: palabras más usadas y hashtags más utilizados.'),
            p('También se ofrece una lectura de positividad y negatividad de las palabras encontradas a partir del 
              diccionario SDAL en su versión en español.'),
            p('La base cuenta con 532.308 tweets, recopilados en un lapso de poco más de 10 horas. 
              Puede seleccionar el período de su interés y seleccionar aquellos tweets que no sean retweets. Tenga presente 
              que si la franja horaria es amplia, el procesamiento de las imagenes demorará unos instantes.'),
            p('Si usa esta app para alguna investigación, por favor, cite la fuente.'),
            p('¡Muchas gracias!'),
            hr(),
            h4(strong(p('Contacto'))),
            p('Puede contactarme tanto por correo como por LinkedIn. ¡Cualquier comentario es bien recibido!'),
            tags$a(href="mailto:hernanescu@gmail.com", 'Gmail'),
            br(),
            tags$a(href="https://www.linkedin.com/in/hernanescudero/", 'LinkedIn')
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput(outputId = 'wordcloud'),
            hr(),
            plotOutput(outputId = 'hashtag'),
            hr(),
            plotOutput(outputId = 'user'),
            hr(),
            plotOutput(outputId = 'positivas'),
            hr(),
            plotOutput(outputId = 'negativas')
            #DT::dataTableOutput(outputId = "tweets_table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #inicio server
    
    subset_base = reactive({
        base %>%
            filter(., between(created, input$timeRange[1], input$timeRange[2])) %>% 
            filter(isRetweet==input$retweet)
    })
    
    subset_token <- reactive({
        tw_tokenizer(subset_base()) %>% 
            filter(!word%in%token_words)
        })
    
    subset_hashtag <- reactive({
        tw_hashtag(subset_base()) %>% 
            filter(word!='#eleccionesargentina') %>% 
            filter(word!='#e')
    })
    
    subset_user <- reactive({
        tw_user(subset_base())
    })
    
    subset_cant <- reactive({nrow(subset_base())})
    subset_hora_ini <- reactive({format(input$timeRange[1]-10800, format='%H:%M:%S')})
    subset_hora_fin <- reactive({format(input$timeRange[2]-10800, format='%H:%M:%S')})
    
    # 
    #Wordcloud
    grafico_wordcloud_title = reactive({
        paste0('Wordcloud a partir de ', subset_cant(), ' tweets')
    })
    
    grafico_wordcloud_subtitle = reactive({
        paste0('Período comprendido entre ', subset_hora_ini(), ' y ', subset_hora_fin())
    })
    
    grafico_wordcloud <- reactive({
        
        ggplot(data=subset_token()[1:200,], aes(label=word, size=freq))+
            geom_text_wordcloud_area(rm_outside = TRUE)+
            scale_size_area(max_size=20)+
            theme_minimal(base_size = 14)+
            labs(title=grafico_wordcloud_title(),
                 subtitle = grafico_wordcloud_subtitle(),
                 caption='Fuente: ShinyApp #EleccionesArgentina de Hernán Escudero')+
            theme(plot.title = element_text(hjust = 0),
                  plot.subtitle = element_text(hjust = 0))
        
    })
    
    output$wordcloud <- renderPlot({
        print(grafico_wordcloud())
    })
    
    #Hashtags
    grafico_lolli_title <- reactive({
        paste0('Hashtags más usados a partir de ', subset_cant(), ' tweets')
    })
    
    grafico_hashtag <- reactive({
        
        subset_hashtag()[1:20,]%>% 
            mutate(word2=fct_reorder(word, freq)) %>% 
            ggplot(., aes(x=word2, y=freq))+
            geom_segment(aes(x=word2, xend=word2, y=0, yend=freq), color="grey")+
            geom_point(size=3, color="#094276")+
            coord_flip()+
            theme_minimal(base_size=14)+
            # theme(
            #     panel.grid.minor.y = element_blank(),
            #     panel.grid.major.y = element_blank(),
            #     legend.position="none") +
            xlab("") +
            ylab("Frecuencia")+
            labs(title=grafico_lolli_title(),
                 subtitle = grafico_wordcloud_subtitle(),
                 caption='Fuente: ShinyApp #EleccionesArgentina de Hernán Escudero')+
            theme(plot.title = element_text(hjust = 1),
                  plot.subtitle = element_text(hjust = 1))
        
    })
    
    output$hashtag <- renderPlot({
        print(grafico_hashtag())
    })
    
    #Usuarios
    grafico_user_title <- reactive({
        paste0('Usuarios más mencionados a partir de ', subset_cant(), ' tweets')
    })
    
    grafico_user <- reactive({
        
        subset_user()[1:20,]%>% 
            mutate(word2=fct_reorder(word, freq)) %>% 
            ggplot(., aes(x=word2, y=freq))+
            geom_segment(aes(x=word2, xend=word2, y=0, yend=freq), color="grey")+
            geom_point(size=3, color="#f48700")+
            coord_flip()+
            theme_minimal(base_size=14)+
            # theme(
            #     panel.grid.minor.y = element_blank(),
            #     panel.grid.major.y = element_blank(),
            #     legend.position="none") +
            xlab("") +
            ylab("Frecuencia")+
            labs(title=grafico_user_title(),
                 subtitle = grafico_wordcloud_subtitle(),
                 caption='Fuente: ShinyApp #EleccionesArgentina de Hernán Escudero')+
            theme(plot.title = element_text(hjust = 1),
                  plot.subtitle = element_text(hjust = 1))
        
    })
    
    output$user <- renderPlot({
        print(grafico_user())
    })

    #SDAL
    base_sdal <- reactive({
        
        left_join(subset_token(), sdal) %>% 
            filter(!is.na(freq))
        
    })
    
    grafico_sdal_pos_title <- reactive({
        paste0('Palabras con mayor positivdad y su conteo a partir de ', subset_cant(), ' tweets')
    })
    
    sdal_positivo <- reactive({
        
        base_sdal() %>% 
            arrange(desc(media_agrado)) %>%
            .[1:20,] %>% 
            mutate(word2=fct_reorder(word, freq)) %>% 
            ggplot(., aes(x=word2, y=freq))+
            geom_segment(aes(x=word2, xend=word, y=0, yend=freq), color="grey")+
            geom_point(size=3, color="#38f20e")+
            coord_flip()+
            theme_minimal(base_size = 14)+ 
            xlab("Positividad") +
            ylab("Frecuencia")+
            labs(title=grafico_sdal_pos_title(),
                 subtitle = grafico_wordcloud_subtitle(),
                 caption='Fuente: ShinyApp #EleccionesArgentina de Hernán Escudero')
        
    })
    
    output$positivas <- renderPlot({
        print(sdal_positivo())
    })
    
    grafico_sdal_neg_title <- reactive({
        paste0('Palabras con mayor negatividad y su conteo a partir de ', subset_cant(), ' tweets')
    })
    
    sdal_negativo <- reactive({
        
        base_sdal() %>% 
            arrange((media_agrado)) %>%
            .[1:20,] %>% 
            mutate(word2=fct_reorder(word, freq)) %>% 
            ggplot(., aes(x=word2, y=freq))+
            geom_segment(aes(x=word2, xend=word, y=0, yend=freq), color="grey")+
            geom_point(size=3, color="#fb1111")+
            coord_flip()+
            theme_minimal(base_size = 14)+ 
            xlab("Negatividad") +
            ylab("Frecuencia")+
            labs(title=grafico_sdal_neg_title(),
                 subtitle = grafico_wordcloud_subtitle(),
                 caption='Fuente: ShinyApp #EleccionesArgentina de Hernán Escudero')
        
    })
    
    output$negativas <- renderPlot({
        print(sdal_negativo())
    })
    #fin server
}

# Run the application 
shinyApp(ui = ui, server = server)
