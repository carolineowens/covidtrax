#COVIDTRAX shiny app
#load packages
library(raster)
library(lubridate)
library(plotly)
library(tidyverse)
library(shiny)
#load data

data <- read_csv("https://data.ca.gov/dataset/590188d5-8545-4c93-a9a0-e230f0db7290/resource/926fd08f-cc91-4828-af38-bd45de97f8c3/download/statewide_cases.csv") %>% 
    filter(county != "Unassigned",
           county != "Out Of Country")

pops <- read_csv("countypops.csv") %>% 
    separate(county, into = "county", sep = " County,", extra = "drop") %>% 
    mutate(county = substr(county, start = 2, stop = 20)) %>% 
    select(-pop_2019)

# Define UI for application: people can select county

ui <- fluidPage(

    # Application title
    titlePanel("CA Covid Tracker"),

    #Dropdown input to select county at top of page
    fluidRow(column(width = 12, offset = 1,
            selectInput(inputId = "county",
                        label = "County:",
                        selected = "-Select-",
                        choices = c("-Select-", sort(unique(data$county)))
                        ))
        ),

        # Show the interactive generated plot
           plotlyOutput("movPlot"),
    ) #end UI


# Define server logic required to create the plot
server <- function(input, output) {

#based on county, what is the scalar for tier cutoffs? (# of 100k people in county)
    tier <- reactive({pops[pops$county==input$county,]})
    
#filter data to the county selected, calculate moving average, and clean/tidy
    subdat_pre <- reactive({
        data %>% 
            filter(county == input$county) %>% 
            mutate(date = as.Date(date),
                   movav = movingFun(newcountconfirmed, 7, fun = mean, type = 'to')) %>% 
            select(date, newcases = newcountconfirmed, movav) %>% 
            na.omit() %>% 
            arrange(date)
        })


#calculate the 'days since' columns
    subdat <- reactive({
        subdat_pre() %>%       
        mutate(reda = case_when(movav <= tier()$red ~ 0,
                                T ~ 1),
               redG = cumsum(c(FALSE, as.logical(diff(reda)))),
               redb = c(0, diff(date)) * reda) %>% 
        group_by(redG) %>% 
        mutate(red = cumsum(reda)) %>% #days above purple
        ungroup() %>% 
        select(-c(reda, redG, redb)) %>% 
        mutate(orna = case_when(movav <= tier()$orange ~ 0,
                                movav > tier()$red ~ 0,
                                T ~ 1),
               ornG = cumsum(c(FALSE, as.logical(diff(orna)))),
               ornb = c(0, diff(date)) * orna) %>% 
        group_by(ornG) %>% 
        mutate(orange = cumsum(orna)) %>% #days below red / above orange
        ungroup() %>% 
        select(-c(orna, ornG, ornb)) %>%
        mutate(yela = case_when(movav <= tier()$yellow ~ 0,
                                movav > tier()$orange ~ 0,
                                T ~ 1),
               yelG = cumsum(c(FALSE, as.logical(diff(yela)))),
               yelb = c(0, diff(date)) * yela) %>% 
        group_by(yelG) %>% 
        mutate(yellow = cumsum(yela)) %>% #days below orange / above yellow
        ungroup() %>% 
        select(-c(yela, yelG, yelb)) %>%
        mutate(yela = case_when(movav > tier()$yellow ~ 0,
                                T ~ 1),
               yelG = cumsum(c(FALSE, as.logical(diff(yela)))),
               yelb = c(0, diff(date)) * yela) %>% 
        group_by(yelG) %>% 
        mutate(lowest = cumsum(yela)) %>% #days below yellow
        ungroup() %>% 
        select(-c(yela, yelG, yelb)) %>%
        pivot_longer(cols = c(newcases, movav)) %>% 
        mutate(name = as.factor(name)) 
})

#get current tier status and cases
    status <- reactive({mean(tail(subdat_pre(), 7)$movav)})
    casenow <- reactive({tail(subdat_pre(), 1)$movav})
    
#assign text, tiercol, and val based on current tier and status
    
   yy <- reactive({
       if (status() <= tier()$yellow){
        tiercol <- reactive({"darkgoldenrod1"})
        if (casenow() <= tier()$yellow){
            txt <- reactive({"Days Within Threshold for Yellow Tier: "})
            val <- reactive({tail(subdat()$lowest, 1)})
            }
        else{
            val <- reactive({tail(subdat()$yellow, 1)})
            txt <- reactive({"Days Above Threshold for Yellow Tier"})
            }
    }else if (status() <= tier()$orange){
        tiercol<- reactive({"darkorange1"})
        if (casenow() <= tier()$yellow){
            txt <- reactive({"Days Below Threshold for Yellow Tier: "})
            val <- reactive({tail(subdat()$lowest, 1)})
            }
        else if (casenow() > tier()$yellow & casenow() <= tier()$orange){
            txt <- reactive({"Days Within Threshold for Orange Tier: "})
            val <- reactive({tail(subdat()$yellow, 1)})
            }
        else{
            txt <- reactive({"Days Above Threshold for Orange Tier: "})
            val <- reactive({tail(subdat()$orange, 1)})
            }
    }else if (status() <= tier()$red){
        tiercol <- reactive({"red3"})
        if (casenow() <= tier()$orange){
            txt <- reactive({'Days Below Threshold for Orange Tier: '})
            val <- reactive({tail(subdat()$yellow, 1)})
            }
        else if (casenow() > tier()$orange & casenow() <= tier()$red){
            txt <- reactive({'Days Within Threshold for Red Tier: '})
            val <- reactive({tail(subdat()$orange, 1)})
            }
        else{
            txt <- reactive({'Days Above Threshold for Red Tier: '})
            val <- reactive({tail(subdat()$red, 1)})
            }
    }else{
        tiercol <- reactive({"#d098fa"})
        if (casenow() <= tier()$red){
            txt <- reactive({'Days Below Threshold for Red Tier: '})
            val <- reactive({tail(subdat()$orange, 1)})
            }
        else{
            txt <- reactive({'Days Above Threshold for Purple Tier: '})
            val <- reactive({tail(subdat()$red, 1)})
            }
    }
       
       return(c(txt(),val(),tiercol()))
   })
   
    
#print text for box
        txtbox <- reactive({
            paste(format(max(subdat()$date), format = "%B %d"), "<br>", yy()[1], yy()[2])
            })
        
        
    #calculate top of y scale
        ymaxx <- reactive({max(subdat()$value)})
        
    #plot figure
    output$movPlot <- renderPlotly({
        if(input$county == "-Select-"){NULL}
        else{
        #create static figure using ggplot
        still <- ggplot(data = subdat(), aes(x = date, y = value))+
                 geom_line(aes(group = name, color = name, size = name))+
                 scale_color_manual(values = c("blue", "black"), guide = FALSE, labels = c("7-Day Moving Avg.", "Daily Reported Cases"))+
                 scale_size_manual(values = c(0.2, 0.1), labels = c("7-Day Moving Avg.", "Daily Reported Cases"))+
                 theme_classic()+
                 ylim(min = 0, max = ymaxx())+
                 scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%d %b")+
                 geom_hline(yintercept = tier()$red, lty = 3, size = 0.1, color = "#490180")+
                 geom_hline(yintercept = tier()$orange, lty = 3, size = 0.1, color = "red3")+
                 geom_hline(yintercept = tier()$yellow, lty = 3, size = 0.1, color = "darkorange1")+
                 labs(y = "New Cases per Day", x = "Date", size = "", title = paste(input$county,"County Covid Cases"),
                     caption = "Data from California Open Data Portal https://data.ca.gov")
        #red3 darkorange1 darkgoldenrod1
        #animate figure using Plotly
        ggplotly(still, dynamicTicks = TRUE
                 #, height = 336, width = 672 #these were test dimensions 
                 ) %>%
            layout(legend = list(orientation = "h", y = 1),
                   annotations = list(text = txtbox(),
                                      font = list(size = 10), bordercolor = "#000000", 
                                      bgcolor = case_when(status() <= tier()$yellow ~ "#fff98a",
                                                          status() <= tier()$orange ~ "fca400",
                                                          status() <= tier()$red ~ "#f5b0b1",
                                                          T ~ "#d098fa"), 
                                      showarrow = FALSE,
                                      xref = "paper", x = .95, yref = "paper", y = 0.9)) %>%
            layout(hovermode = 'compare') %>%
            style(name = "7-Day Moving Average",
                  traces = 1) %>%
            style(name = "Daily Reported Cases",
                  traces = 2) %>%
            style(text = paste0(.$x$data[[1]]$name, ": <br>", round(.$x$data[[1]]$y), " cases/day"),
                  traces = 1) %>%
            style(text = paste0(format(.$x$data[[2]]$x, "%d %B"), ": ", .$x$data[[2]]$y, " new cases"),
                  traces = 2) %>%
            style(text = paste("Threshold for Red Tier:", tier()$red), traces = 3) %>%
            style(text = paste("Threshold for Orange Tier:", tier()$orange), traces = 4) %>%
            style(text = paste("Threshold for Yellow Tier:", tier()$yellow), traces = 5)
     }})

}

# Run the application 
shinyApp(ui = ui, server = server)
