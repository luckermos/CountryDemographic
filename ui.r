library(shiny)
library(shinythemes)
library(tidyverse)
library(readxl)
library(plotly)
library(idbr)
library(ggthemes)

idb_api_key('01e46fad417ee1292a3f94bdbeb65ccd1767be16')

pop_total <<- read_excel("dados/API_SP.POP.TOTL_DS2_en_excel_v2_1307373.xls", skip=3) %>% 
  select(-c(`Indicator Code`,`Indicator Name`)) %>% 
  pivot_longer(cols=-c(`Country Name`, `Country Code`),
               names_to = "Ano",
               values_to = "Pop_total")
pop_fem <<- read_excel("dados/API_SP.POP.TOTL.FE.ZS_DS2_en_excel_v2_1223017.xls", skip=3) %>% 
  select(-c(`Indicator Code`,`Indicator Name`)) %>% 
  pivot_longer(cols=-c(`Country Name`, `Country Code`),
               names_to = "Ano",
               values_to = "Pop_fem")
pop_0_14 <<- read_excel("dados/API_SP.POP.0014.TO.ZS_DS2_en_excel_v2_1218574.xls", skip=3) %>% 
  select(-c(`Indicator Code`,`Indicator Name`)) %>% 
  pivot_longer(cols=-c(`Country Name`, `Country Code`),
               names_to = "Ano",
               values_to = "Pop_0_14")
pop_15_65 <<- read_excel("dados/API_SP.POP.1564.TO.ZS_DS2_en_excel_v2_1218572.xls", skip=3) %>% 
  select(-c(`Indicator Code`,`Indicator Name`)) %>% 
  pivot_longer(cols=-c(`Country Name`, `Country Code`),
               names_to = "Ano",
               values_to = "Pop_15_65")
pop_65 <<- read_excel("dados/API_SP.POP.65UP.TO.ZS_DS2_en_excel_v2_1218584.xls", skip=3) %>% 
  select(-c(`Indicator Code`,`Indicator Name`)) %>% 
  pivot_longer(cols=-c(`Country Name`, `Country Code`),
               names_to = "Ano",
               values_to = "Pop_65")
pop_grow <<- read_excel("dados/API_SP.POP.GROW_DS2_en_excel_v2_1217752.xls", skip=3) %>% 
  select(-c(`Indicator Code`,`Indicator Name`)) %>% 
  pivot_longer(cols=-c(`Country Name`, `Country Code`),
               names_to = "Ano",
               values_to = "Pop_grow")
pop_urb <<- read_excel("dados/API_SP.URB.TOTL.IN.ZS_DS2_en_excel_v2_1217650.xls", skip=3) %>% 
  select(-c(`Indicator Code`,`Indicator Name`)) %>% 
  pivot_longer(cols=-c(`Country Name`, `Country Code`),
               names_to = "Ano",
               values_to = "Pop_urb")
pop_fert <<- read_excel("dados/API_SP.DYN.TFRT.IN_DS2_en_excel_v2_1218006.xls", skip=3) %>% 
  select(-c(`Indicator Code`,`Indicator Name`)) %>% 
  pivot_longer(cols=-c(`Country Name`, `Country Code`),
               names_to = "Ano",
               values_to = "Pop_fert")

mort_5_fem <<- read_excel("dados/API_SH.DYN.MORT.FE_DS2_en_excel_v2_1228518.xls", skip=3) %>% 
  select(-c(`Indicator Code`,`Indicator Name`)) %>% 
  pivot_longer(cols=-c(`Country Name`, `Country Code`),
               names_to = "Ano",
               values_to = "Mort")
mort_5_masc <<- read_excel("dados/API_SH.DYN.MORT.MA_DS2_en_excel_v2_1229125.xls", skip=3) %>% 
  select(-c(`Indicator Code`,`Indicator Name`)) %>% 
  pivot_longer(cols=-c(`Country Name`, `Country Code`),
               names_to = "Ano",
               values_to = "Mort")
mort_inf_fem <<- read_excel("dados/API_SP.DYN.IMRT.FE.IN_DS2_en_excel_v2_1232008.xls", skip=3) %>% 
  select(-c(`Indicator Code`,`Indicator Name`)) %>% 
  pivot_longer(cols=-c(`Country Name`, `Country Code`),
               names_to = "Ano",
               values_to = "Mort")
mort_inf_masc <<- read_excel("dados/API_SP.DYN.IMRT.MA.IN_DS2_en_excel_v2_1219138.xls", skip=3) %>% 
  select(-c(`Indicator Code`,`Indicator Name`)) %>% 
  pivot_longer(cols=-c(`Country Name`, `Country Code`),
               names_to = "Ano",
               values_to = "Mort")
mort_adul_fem <<- read_excel("dados/API_SP.DYN.AMRT.FE_DS2_en_excel_v2_1218457.xls", skip=3) %>% 
  select(-c(`Indicator Code`,`Indicator Name`)) %>% 
  pivot_longer(cols=-c(`Country Name`, `Country Code`),
               names_to = "Ano",
               values_to = "Mort")
mort_adul_masc <<- read_excel("dados/API_SP.DYN.AMRT.MA_DS2_en_excel_v2_1218458.xls", skip=3) %>% 
  select(-c(`Indicator Code`,`Indicator Name`)) %>% 
  pivot_longer(cols=-c(`Country Name`, `Country Code`),
               names_to = "Ano",
               values_to = "Mort")

pop <<- inner_join(x=pop_total, y=pop_0_14, by=c("Country Name", "Country Code", "Ano")) %>% 
  inner_join(y=pop_15_65, by=c("Country Name", "Country Code", "Ano")) %>% 
  inner_join(y=pop_65, by=c("Country Name", "Country Code", "Ano")) %>% 
  inner_join(y=pop_fem, by=c("Country Name", "Country Code", "Ano")) %>% 
  inner_join(y=pop_grow, by=c("Country Name", "Country Code", "Ano")) %>% 
  inner_join(y=pop_urb, by=c("Country Name", "Country Code", "Ano")) %>% 
  inner_join(y=pop_fert, by=c("Country Name", "Country Code", "Ano")) %>% 
  mutate(Ano_date=lubridate::ymd(Ano, truncated = 2L))

mort <<- bind_rows(mort_5_fem %>% mutate(sex="F", tipo="Até 5 Anos"),
                   mort_5_masc %>% mutate(sex="M", tipo="Até 5 Anos"),
                   mort_inf_fem %>% mutate(sex="F", tipo="Infantil"),
                   mort_inf_masc %>% mutate(sex="M", tipo="Infantil"),
                   mort_adul_fem %>% mutate(sex="F", tipo="Adulta"),
                   mort_adul_masc %>% mutate(sex="M", tipo="Adulta")) %>% 
  mutate(Ano_date=lubridate::ymd(Ano, truncated = 2L),
         tipo=factor(tipo, levels = c("Adulta", "Infantil", "Até 5 Anos")))

ui <- fluidPage(theme=shinytheme("flatly"),
                headerPanel("Análise Demográfica do Mundo"),
                sidebarLayout(
                  sidebarPanel(selectizeInput("pais", label="Escolha um país ou região:", 
                                              choices=unique(pop$`Country Name`), 
                                              selected="World"),
                               "Os dados utilizados para a construção das pirâmides etárias foram obtidos da ", tags$a(href="https://www.census.gov/programs-surveys/international-programs/about/idb.html", "International Data Base of U.S. Census Bureau"), " por meio do pacote ", tags$a(href="https://cran.r-project.org/web/packages/idbr/README.html", "idbr"), " do software ", tags$a(href="https://cran.r-project.org", "R"),".",
                               "",
                               "Todos os demais indicadores utilizados foram coletados do", tags$a(href="https://www.worldbank.org", "The World Bank."), "Vários outros indicadores podem ser encontrados ", tags$a(href="https://data.worldbank.org/indicator","aqui"),".",
                               tags$h6(" "),
                               tags$h6("Desenvolvido por Lucas Emanuel Ferreira Ramos"),
                               tags$h6("Contato:"),
                               tags$h6(tags$a(href="mailto:luckermos19@gmail.com", "luckermos19@gmail.com")),
                               tags$h6(tags$a(href="https://linkedin.com/in/luckermos/", "linkedin.com/in/luckermos/")),
                                       tags$h6(tags$a(href="https://github.com/luckermos", "https://github.com/luckermos"))
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel(title="População Total",
                               plotlyOutput("pop_total")),
                      tabPanel(title="Crescimento Populacional",
                               plotlyOutput("cresc")),
                      tabPanel(title="Mortalidade",
                               plotlyOutput("mort")),
                      tabPanel(title="Fecundidade",
                               plotlyOutput("fert")),
                      tabPanel(title="População por Sexo",
                               plotlyOutput("sex")),
                      tabPanel(title="População por Faixa de Idade",
                               plotlyOutput("idade")),
                      tabPanel(title="População por Situação de Domicílio",
                               plotlyOutput("urb")),
                      tabPanel(title="Pirâmide Etária",
                               tags$div(selectizeInput("pais2", label="Escolha um país:", 
                                                       choices=countrycode::countryname_dict$country.name.en %>% unique() %>% sort(), 
                                                       selected="Brazil"),  style="display:inline-block"),
                               tags$div(selectizeInput("ano", label="Escolha um ano:", 
                                                       choices=seq(1970,2019,1),
                                                       selected=2019),  style="display:inline-block"),
                               plotlyOutput("pir"))
                    )
                  )
                )
                
                
)