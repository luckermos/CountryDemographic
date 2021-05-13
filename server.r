library(shiny)
library(shinythemes)
library(tidyverse)
library(readxl)
library(plotly)
library(idbr)
library(ggthemes)

server <- function(input, output, session) {
  output$pop_total <- renderPlotly({
    ggplotly(ggplot(pop %>% filter(`Country Name`==input$pais), aes(x=Ano_date, y=Pop_total/1000000)) + 
               geom_point(size=.8, aes(text=paste0(Ano,": ",format(round(Pop_total/1000000,2), decimal.mark=",", big.mark=".",
                                                                   small.mark=".")))) + 
               geom_line(size=.8)  + xlab("") + ggtitle(input$pais) +
               ylab("População Total (em milhões)"), tooltip = "text") %>% layout(hovermode = "x unified")
  })
  
  output$cresc <- renderPlotly({
    ggplotly(ggplot(pop %>% filter(`Country Name`==input$pais), aes(x=Ano_date, y=Pop_grow)) + 
               geom_point(size=.8, aes(text=paste0(Ano,": ",format(round(Pop_grow,2), decimal.mark=",", big.mark=".",
                                                                   small.mark="."),"%"))) + 
               geom_line(size=.8)  + xlab("") +  ggtitle(input$pais) +
               ylab("Crescimento Anual (%)"), tooltip = "text") %>% layout(hovermode = "x unified")
  })
  
  output$sex <- renderPlotly({
    p <- pop %>% filter(`Country Name`==input$pais) %>% mutate(sex="F") %>% 
      select(`Country Name`, `Country Code`, Ano_date, sex, Pop_fem) %>% 
      bind_rows(pop %>% filter(`Country Name`==input$pais) %>% mutate(sex="M", Pop_fem=100-Pop_fem) %>% 
                  select(`Country Name`, `Country Code`, Ano_date, sex, Pop_fem)) %>% 
      mutate("%"=paste0(lubridate::year(Ano_date),": ",round(Pop_fem,2),"%"))
    
    ggplotly(ggplot(p, aes(x=Ano_date, y=Pop_fem, fill=sex, label=`%`)) + 
               geom_area()  + xlab("") + labs(fill="Sexo") + ggtitle(input$pais) +
               ylab("% da População Total"), tooltip = "%") %>% 
      layout(hovermode = "x unified")
  })
  
  output$mort <- renderPlotly({
    ggplotly(ggplot(mort %>% filter(`Country Name`==input$pais) %>% drop_na(Mort), 
                    aes(x=Ano_date, y=Mort, color=tipo, linetype=sex)) + 
               geom_line(size=.5) + 
               geom_point(size=.1, aes(text=paste0(lubridate::year(Ano_date),"<br>", tipo, " - ", 
                                                   sex, ": ", 
                                                   round(Mort,2)))) +
               xlab("") +
               labs(linetype="", color="Mortalidade, Sexo") + ggtitle(input$pais) +
               ylab("Mortes por 1000 nascimentos/crianças/adultos"), tooltip = "text") %>% 
      layout(hovermode = "x unified")
  })
  
  output$fert <- renderPlotly({
    ggplotly(ggplot(pop %>% filter(`Country Name`==input$pais), aes(x=Ano_date, y=Pop_fert)) + 
               geom_point(size=.8, aes(text=paste0(Ano,": ",format(round(Pop_fert,2), decimal.mark=",", big.mark=".",
                                                                   small.mark=".")))) + 
               geom_line(size=.8)  + xlab("") +  ggtitle(input$pais) +
               ylab("Nascimentos por Mulher"), tooltip = "text") %>% layout(hovermode = "x unified")
  })
  
  output$idade <- renderPlotly({
    p <- pop %>% filter(`Country Name`==input$pais) %>% mutate(idade="0-14 Anos") %>% 
      select(`Country Name`, `Country Code`, Ano_date, idade, Pop_0_14) %>% 
      bind_rows(pop %>% filter(`Country Name`==input$pais) %>% mutate(idade="15-65 Anos", Pop_0_14=Pop_15_65) %>% 
                  select(`Country Name`, `Country Code`, Ano_date, idade, Pop_0_14)) %>% 
      bind_rows(pop %>% filter(`Country Name`==input$pais) %>% mutate(idade="+65 Anos", Pop_0_14=Pop_65) %>% 
                  select(`Country Name`, `Country Code`, Ano_date, idade, Pop_0_14)) %>% 
      mutate(idade=factor(idade, levels = c("+65 Anos", "15-65 Anos", "0-14 Anos"))) %>% 
      mutate("%"=paste0(lubridate::year(Ano_date),": ",round(Pop_0_14,2),"%"))
    
    ggplotly(ggplot(p, aes(x=Ano_date, y=Pop_0_14, fill=idade, label=`%`)) + 
               geom_area() + xlab("") + labs(fill="Faixa de Idade") + ggtitle(input$pais) +
               ylab("% da População Total"), tooltip = "%") %>% 
      layout(hovermode = "x unified")
  })
  
  output$urb <- renderPlotly({
    p <- pop %>% filter(`Country Name`==input$pais) %>% mutate(urb="Urbano") %>% 
      select(`Country Name`, `Country Code`, Ano_date, urb, Pop_urb) %>% 
      bind_rows(pop %>% filter(`Country Name`==input$pais) %>% mutate(urb="Rural", Pop_urb=100-Pop_urb) %>% 
                  select(`Country Name`, `Country Code`, Ano_date, urb, Pop_urb)) %>% 
      mutate("%"=paste0(lubridate::year(Ano_date),": ",round(Pop_urb,2),"%"))
    
    ggplotly(ggplot(p, aes(x=Ano_date, y=Pop_urb, fill=urb, label=`%`)) +  ggtitle(input$pais) +
               geom_area()  + xlab("") + labs(fill="") +
               ylab("% da População Total"), tooltip = "%") %>% 
      layout(hovermode = "x unified")
  })
  
  output$pir <- renderPlotly({
    male <- idb1(input$pais2, input$ano, sex = 'male') %>%
      mutate(POP = POP * -1,
             SEX = 'Homens')
    
    female <- idb1(input$pais2, input$ano, sex = 'female') %>%
      mutate(SEX = 'Mulheres')
    
    china <- rbind(male, female) 
    
    # Build the visualization
    
    ggplotly(ggplot(china, aes(x = AGE, y = (POP/sum(Mod(china$POP))), fill = SEX, width = 1)) +
               coord_flip() + 
               geom_bar(data = subset(china, SEX == "Mulheres"), stat = "identity",
                        aes(text=paste0("Idade: ",AGE," Anos<br>",SEX,": ", round(100*(Mod(POP)/sum(Mod(china$POP))), 4),"%"))) +
               geom_bar(data = subset(china, SEX == "Homens"), stat = "identity",
                        aes(text=paste0("Idade: ",AGE," Anos<br>",SEX,": ", round(100*(Mod(POP)/sum(Mod(china$POP))), 4),"%"))) +
               scale_fill_economist() + 
               ggtitle(paste0('Estrutura Populacional: ', input$pais2, ', ', input$ano)) + 
               ylab('% da População Total') + 
               xlab('Idade') + 
               theme(legend.position = "bottom", 
                     legend.title = element_blank()) + 
               guides(fill = guide_legend(reverse = TRUE))+ 
               scale_y_continuous(labels = function(x) paste0(Mod(x)*100, "%")), tooltip="text") 
  })
  
}