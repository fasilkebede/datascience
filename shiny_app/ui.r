library(ggplot2)
library(gganimate)
library(png)
library(shinydashboard)
library(gifski)
library(shinythemes)
library(plotly)
library(shinythemes)
library(forcats)
library(shiny)
library(shinyjs)
library(ggplot2)
library(plotly)
library(DT)
library(shinythemes)
library(tidyverse)
library(bslib)


# Define ui.R ####

dashHeader=dashboardHeader(title = 'Tropical Poultry Genetic Solution (TPGS)',
                           titleWidth = 450,
                      
                           dropdownMenu(
                             type = "notifications",
                             notificationItem(
                               text = "This is a dashboard for TPGS dataset from ACGG and SAPLING",
                               icon =   icon("warning"),
                               status='warning'
                             ),
                             notificationItem(
                               text = "We used linear mixed model to analyse the data",
                               icon =   icon("dashboard"),
                               status='success'
                             ),
                             notificationItem(text = "This dashboard will be automated",
                                              icon =   icon("dashboard"),
                                              status='info'
                             )
                           ),
                           
                           dropdownMenu(
                             type = "task",
                             taskItem(
                               value = 80,
                               color = 'blue' ,
                               'Based on this result, TPGS results for Kenya and Vietnam will be launched very soon'
                             ),
                             taskItem(
                               value = 90,
                               color = 'green' ,
                               'The app will deployed on shinyserver'
                             ),
                             taskItem(
                               value = 50,
                               color = 'red' ,
                               'Show maps of TPGS performance sites'
                             )
                           )       
                           
                           
)                 
dashSidebar=dashboardSidebar(

  sidebarMenu(
    menuItem(text='On-farm Performance Test',
             tabName = 'onfarm',
             icon=icon('dashboard')),
    menuItem(text='On-station Peformance Test',
             tabName = 'onstation',
             icon=icon('dashboard')),
    menuItem(text='Suvival of chickens',
             tabName='animate',
             icon=icon('dashboard'))
    
))
dashBody=dashboardBody(
  useShinyjs(),
  
 
  list(
    actionButton(inputId = "showh", label = "Main page"),
    #actionButton(inputId = "hideh", label = "Hide text"),
    br(),
    hidden(tags$div(id="txt", style='color:blue;', list(helpText("This app is scalable and repeatable. If you want to print this app result, you need 38 pages of word document."),hr())))),
  
  tabItems(
    tabItem(
      tabName='onfarm',
      fluidRow(
        box( width=3,
             collapsible = TRUE,
             title='Description',
             status='success',solidHeader = TRUE,
      
             selectInput("country", "Country", choices = c('Ethiopia','Nigeria','Tanzania'),selected='Ethiopia'),
             selectInput("trait", "Trait", choices = c('Live body weight','Egg productivity'),selected='Live body weight'),
             uiOutput('AEZ'),
             actionButton("gobutton","Toggle the Table")),
        box( width=9,
            plotlyOutput("plotf"),
            br(),
            dataTableOutput(outputId = "tableDTf")  
       ) 
      )

  
     ), 
    tabItem(
      tabName='onstation',
      fluidRow(
        box( width=3,
             collapsible = TRUE,
             title='Controls',
             status='success',solidHeader = TRUE,
           
             selectInput("country2", "Country", choices = c('Ethiopia','Nigeria','Tanzania'),selected='Ethiopia'),
             uiOutput('stationui'),
             uiOutput('traitui'),
             actionButton("gobutton2","Toggle the Table")),
        
        
        box( width=9,
        plotlyOutput("plots"),
        br(),
        dataTableOutput(outputId = "tableDTs")  
             
        ))),
    tabItem(
      tabName='animate',
      fluidRow(
        box( width=3,
             collapsible = TRUE,
             title='Controls',
             status='success',solidHeader = TRUE,
     
             selectInput("country3", "Country", choices = c('Ethiopia','Nigeria','Tanzania'),selected='Ethiopia'),
             uiOutput('stationuia')),
            # actionButton("gobutton3","Toggle the Table")),
       box( width=9,

                         plotlyOutput('plotanim')))
                
        )))
  

ui<- dashboardPage(
  header= dashHeader,
  sidebar=dashSidebar,
  skin = "yellow",
  body=dashBody,
  title=" TPGS Dashboard"
)


# Define server function  
server <- function(input, output) {
  
  output$txtout <- renderText({
    paste( input$txt1, input$txt2, sep = " " )
  })
} # server

onfarm<- read.table("onfarm.txt",h=T)

#convert all character columns to factor ## https://statisticsglobe.com/convert-character-to-factor-in-r
onfarm <- as.data.frame(unclass(onfarm),
                        stringsAsFactors = TRUE)
#Ethiopia
Ethiopia<- onfarm %>% filter(country=='Ethiopia')
Ethiopia$Breed[Ethiopia$Breed == 'S-RIR'] <-'SassoXRIR'
Ethiopia$Breed<- factor(Ethiopia$Breed)
Ethiopia$AEZ<- factor(Ethiopia$AEZ)
Ethiopia$AEZ<- droplevels(Ethiopia$AEZ)
Ethiopia$Breed<- droplevels(Ethiopia$Breed)
Ethiopia$trait<- factor(Ethiopia$trait)
Ethiopia$country<- factor(Ethiopia$country)
Ethiopia$country<- droplevels(Ethiopia$country)

#Nigeria
Nigeria<- onfarm %>% filter(country=='Nigeria')
Nigeria$Breed<- factor(Nigeria$Breed)
Nigeria$AEZ<- factor(Nigeria$AEZ)
Nigeria$AEZ<- droplevels(Nigeria$AEZ)
Nigeria$breed<- droplevels(Nigeria$Breed)
Nigeria$trait<- factor(Nigeria$trait)
Nigeria$country<- factor(Nigeria$country)

#Tanzania

Tanzania<- onfarm %>% filter(country=='Tanzania')

Tanzania$Breed<- factor(Tanzania$Breed)
Tanzania$AEZ<- factor(Tanzania$AEZ)
Tanzania$country<- factor(Tanzania$country)
Tanzania$trait<- factor(Tanzania$trait)
Tanzania$AEZ<- droplevels(Tanzania$AEZ)
Tanzania$Breed<- droplevels(Tanzania$Breed)

onstation<- read.table("onstation.txt",h=T)
#convert all character columns to factor ##
onstation <- as.data.frame(unclass(onstation),
                        stringsAsFactors = TRUE)

#view(onstation)
#str(onstation)
#Ethiopia
Ethiopias<- onstation %>% filter(country=='Ethiopia')
Ethiopias$country <- factor(Ethiopias$country)
Ethiopias$country<- droplevels(Ethiopias$country)

Ethiopias$AEZ <- factor(Ethiopias$AEZ)
Ethiopias$AEZ<- droplevels(Ethiopias$AEZ)

Ethiopias$trait <- factor(Ethiopias$trait)
Ethiopias$trait<- droplevels(Ethiopias$trait)

Ethiopias$Breed <- factor(Ethiopias$Breed)
Ethiopias$Breed<- droplevels(Ethiopias$Breed)

#Nigeria
Nigerias<-onstation%>% filter(country=='Nigeria')
Nigerias$country <- factor(Nigerias$country)
Nigerias$country<- droplevels(Nigerias$country)

Nigerias$AEZ <- factor(Nigerias$AEZ)
Nigerias$AEZ<- droplevels(Nigerias$AEZ)

Nigerias$trait <- factor(Nigerias$trait)
Nigerias$trait<- droplevels(Nigerias$trait)

Nigerias$Breed <- factor(Nigerias$Breed)
Nigerias$Breed<- droplevels(Nigerias$Breed)

#Tanzania

Tanzanias<- onstation %>% filter(country=='Tanzania')
Tanzanias$country <- factor(Tanzanias$country)
Tanzanias$country <- droplevels(Tanzanias$country)

Tanzanias$AEZ <- factor(Tanzanias$AEZ)
Tanzanias$AEZ <- droplevels(Tanzanias$AEZ)

Tanzanias$Breed <- factor(Tanzanias$Breed)
Tanzanias$Breed <- droplevels(Tanzanias$Breed)

Tanzanias$trait <- factor(Tanzanias$trait)
Tanzanias$trait <- droplevels(Tanzanias$trait)

#Survival of chicken
survdat<- read.table('survdat.txt',h=T)
#convert all character columns to factor 
survdat <- as.data.frame(unclass(survdat),
                        stringsAsFactors = TRUE)
survdat$Breed <- factor(survdat$Breed)
survdat$station <- factor(survdat$station)
survdat$country <- factor(survdat$country)
survdat$Breed_Station <- factor(survdat$Breed_Station)

 levels(survdat$station)[levels(survdat$station)=="Funa" ] <- "Funaab"
 levels(survdat$station)[levels(survdat$station)=="Hope"] <- "Fol-Hope"
 levels(survdat$station)[levels(survdat$station)=="Nail"] <- "Naliendele" 
 levels(survdat$station)[levels(survdat$station)=="Sua"] <-   "Sokoine_University"
 write.table(survdat,'survdat.txt',row.names=F,col.names=T,quote=F)
server <- function(input, output) {
  library(shinyjs)
  observeEvent(input$showh,
               show("txt")) # show() is shiny js function, pass the element/widget ID as the argument
  
  observeEvent(input$hideh,
               hide("txt"))
  
  vara<- reactive({
    
    if (is.null(input$country)) {
      return(NULL)
    }
    if (is.null(input$trait)) {
      return(NULL)
    }
    if (input$country=='Ethiopia'&input$trait=='Live body weight') {
      levels(droplevels((Ethiopia %>% filter(trait=='Bwt'))$AEZ))
      
    }else if (input$country=='Nigeria'&input$trait=='Live body weight'){
      levels(droplevels((Nigeria %>% filter(trait=='Bwt'))$AEZ))
      
    }     else if(input$country=='Tanzania'&input$trait=='Live body weight') {
      levels(droplevels((Tanzania %>% filter(trait=='Bwt'))$AEZ))
      
    }else if (input$country=='Ethiopia'&input$trait=='Egg productivity'){
      levels(droplevels((Ethiopia %>% filter(trait=='Egg'))$AEZ))
      
    }else if (input$country=='Nigeria'&input$trait=='Egg productivity'){
      levels(droplevels((Nigeria %>% filter(trait=='Egg'))$AEZ))
      
    }else {
      levels(droplevels((Tanzania %>% filter(trait=='Egg'))$AEZ))
      
    }
    
    
  })
  
  
  
  output$AEZ<- renderUI({
    selectInput('aez','Agro ecology zone',choices=vara())
  })
  
  
  
  datf<- reactive({
    onfarm%>% filter(country%in%input$country& AEZ%in%input$aez)
  })
  output$plotf <- renderPlotly({
    
    
    
    dataf<-    datf()
    df=dataf
    ylabs<- c('Number of eggs per bird per year ','Live body weight-at-week-20(g)')
    if ( input$trait=='Egg productivity'){
      ylabc2<- ylabs[1]
      df<- datf() %>% filter(trait=='Egg')
      maxval=df$lsmean +df$Standard_error
      minval=df$lsmean - df$Standard_error
      limits=aes(ymax = maxval, ymin=minval)
    }
    if ( input$trait=='Live body weight'){
      ylabc2<- ylabs[2]
      df<- datf() %>% filter(trait=='Bwt')
      maxval=df$lsmean +df$Standard_error
      minval=df$lsmean - df$Standard_error
    }
    plotf= plot_ly(x=droplevels(df$Breed),y=df$lsmean,type='bar',color=df$Breed,showlegend=T, error_y=list(array=df$Standard_error)) %>%
      layout(xaxis=list(title=NA),yaxis=list(title=ylabc2),barmode='group')
    plotf
    
    
  }
  )
  output$tableDTf <- renderDataTable({
    
    if( input$trait=='Egg productivity'){
      df<- datf() %>% filter(trait=='Egg')
    }
    if( input$trait=='Live body weight'){
      df<- datf() %>% filter(trait=='Bwt')
    }
    na.omit(df[,1:4])
    
    
  })
  
  observeEvent(input$gobutton, {
    # every time the button is pressed, alternate between hiding and showing the plot
    toggle("tableDTf")
  })   
  
  ## On station server
  vars<- reactive({
    switch( input$country2,
            'Ethiopia'= factor(levels(Ethiopias$AEZ)),
            'Nigeria'=factor(levels(Nigerias$AEZ)),
            'Tanzania'=factor(levels(Tanzanias$AEZ)))
  })
  
  output$stationui<- renderUI({
    selectInput('Station','Station',choices=vars())
  })
  
  vart<- reactive({
    switch( input$country2,
            'Ethiopia'= c(levels(Ethiopias$trait),'Survival'),
            'Nigeria'=c(levels(Nigerias$trait),'Survival'),
            'Tanzania'=c(levels(Tanzanias$trait),'Survival'))
  })
  
  output$traitui<- renderUI({
    selectInput('Trait','Trait',choices=vart())
  })
  
  dats<- reactive({
    
    
    if ( input$Trait!='Survival'){
      onstation %>% filter(AEZ%in%input$Station&country%in%input$country2)
    }
    
    else {
      
      survdat %>% filter(country%in%input$country2&station%in%input$Station)
      
    }
    
  })
  
  
  output$plots <- renderPlotly({
    
    
    
    
    dataf=dats()
    
    
    ylabs<- c('Number of eggs per bird per year egg','Live body weight-at-week-20(g)','Rate of feed intake/bird/day(g)')
    if ( input$Trait=='Egg'){
      
      df<- dats() %>% filter(trait=='Egg')
      maxval=df$lsmean +df$Standard_error
      minval=df$lsmean - df$Standard_error
      limits=aes(ymax = maxval, ymin=minval)
      ylabc2<- ylabs[1]
    }
    if ( input$Trait=='Bwt'){
      df<- dats() %>% filter(trait=='Bwt')
      maxval=df$lsmean +df$Standard_error
      minval=df$lsmean - df$Standard_error
      limits=aes(ymax = maxval, ymin=minval)
      ylabc2<- ylabs[2]
    }
    if ( input$Trait=='RFI'){
      df<- dats() %>% filter(trait=='RFI')
      maxval=df$lsmean +df$Standard_error
      minval=df$lsmean - df$Standard_error
      limits=aes(ymax = maxval, ymin=minval)
      ylabc2<- ylabs[3]
    }
    
    if ( input$Trait!='Survival'){
      df$Breed<- droplevels(df$Breed)
      df$AEZ<- droplevels(df$AEZ)
      
      plotf= plot_ly(x=df$Breed,y=df$lsmean,type='bar',color=df$Breed,showlegend=T, error_y=list(array=df$Standard_error)) %>%
        layout(xaxis=list(title=NA),yaxis=list(title=ylabc2),barmode='group')
      plotf
    }
    
    else {
      dataf$Breed<- droplevels(dataf$Breed)
      dataf$station<- droplevels(dataf$station)
      p<-ggplot(dataf, aes(x=week, y=Proportion_survived, group=factor(Breed))) +
        geom_line(aes(color=Breed))+
        geom_point(aes(color=Breed))
      
      
    }
    
  })
  
  
  output$tableDTs <- renderDataTable({
    req(input$Trait)
    if ( input$Trait=='Egg'){
      
      df<- dats() %>% filter(trait=='Egg')
      
    }
    if ( input$Trait=='Bwt'){
      df<- dats() %>% filter(trait=='Bwt')
    }
    
    if ( input$Trait=='RFI'){
      df<- dats() %>% filter(trait=='RFI')
    }
    if ( input$Trait=='Survival'){
      df<- dats() 
    }
    na.omit(df[,1:4])
    
    
  })
  
  observeEvent(input$gobutton2, {
    toggle("tableDTs")
  })   
  
  
  varsa<- reactive({
    switch( input$country3,
            'Ethiopia'= factor(levels(Ethiopias$AEZ)),
            'Nigeria'=factor(levels(Nigerias$AEZ)),
            'Tanzania'=factor(levels(Tanzanias$AEZ)))
  })
  
  output$stationuia<- renderUI({
    selectInput('Stationa','Station',choices=varsa())
  })
  
  datsa<- reactive({
    
    survdat %>% filter(country%in%input$country3&station%in%input$Stationa)
    
  })
  
  
  
  require(tidyverse)
  
  output$plotanim<- renderPlotly({
    
    if (input$country3=='Nigeria'&input$Stationa=='Fol-Hope'){
      newdat<- datsa()%>% filter(week>21)
    }else{
      newdat<- datsa()
    }
    newdat$Breed<- droplevels(newdat$Breed)
    newdat2= newdat%>% complete(Breed,week,fill=list(Proportion_survived=0))
    cumulative_launches <- newdat %>%
      split(f = .$week) %>%
      accumulate(., ~bind_rows(.x, .y)) %>%
      bind_rows(.id = "frame")
    # Create the cumulative animation
    cumulative_launches %>%
      plot_ly(x = ~week, y = ~Proportion_survived, color = ~Breed) %>%
      add_lines(frame = ~frame)
    
  })
}


# Create Shiny object
shinyApp(ui = ui, server = server)
