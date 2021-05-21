library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(httr)
library(readr)
library(lubridate)
library(DT)
library(plotly)

CCSOData <- read.csv("C:/Users/jyday/Desktop/CCSOData.csv")

CCSOData$BookingYear <- year(mdy(CCSOData$BOOKING.DATE))
CCSOData$ReleaseYear <- year(mdy(CCSOData$RELEASED.DATE))

# plot 1 data:
CCSO_1 = CCSOData%>%filter(SEX == "Male" | SEX == "Female")%>%
  filter(RACE == "White" | RACE == "Black" | RACE == "Hispanic" | RACE ==
           "Asian/Pacific Islander" | RACE == "Native American")%>%
  filter(EMPLOYMENT.STATUS == "Employed - Full Time" | EMPLOYMENT.STATUS == "Unemployed" | EMPLOYMENT.STATUS == "Employed - Part Time" | EMPLOYMENT.STATUS == "Self Employed")%>%
  filter(Days.in.Jail != "#VALUE!" & Days.in.Jail != "0")
CCSO_1$Days.in.Jail = as.numeric(CCSO_1$Days.in.Jail)
CCSO_1$Employment = ifelse(CCSO_1$EMPLOYMENT.STATUS == "Unemployed", "Unemployed", "Employed")


ui<-fluidPage(

  pageWithSidebar(
    headerPanel("CCSO Scatter Plot"),
    sidebarPanel(
      sliderInput(inputId = "year", label = "Select year range", value = c(2013,2014), min = 2011, max = 2016),
      checkboxGroupInput(inputId = "sex", label = "Select sex", choices = list("female" = "Female", "male" = "Male"), selected = "Female"),
      selectInput(inputId = "race", label = "Select race", choices = list("White" = "White", "Black" = "Black",  "Hispanic" = "Hispanic", "Asian/Pacific Islander"= "Asian/Pacific Islander","Native American" = "Native American"),
                  selected = "Black"),
      checkboxGroupInput(inputId = "employment", label = "Select employment status", choices = list("Employed" = "Employed", "Unemployed" = "Unemployed"),
                         selected = "Unemployed"),
      checkboxInput(inputId = "avgage",
                    label = strong("Show average age at arrest"),
                    value = FALSE),
      
      checkboxInput(inputId = "avgdays",
                    label = strong("Show average days in jail"),
                    value = FALSE)
      #selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
      #numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
    ),
    mainPanel(
      p("This is an app to show the scatter of age at arrest and days in jails"),
      p("You could make options of year, sex, race and employment status in the left side bar."),
      uiOutput(outputId='figure1')
    )
  ),

  pageWithSidebar(
    headerPanel("CCSO Timeline Plot"),
    sidebarPanel(
      sliderInput(inputId = "year2", label = "Select year range", value = c(2011,2014), min = 2011, max = 2016),
      #checkboxGroupInput(inputId = "sex2", label = "Select sex", choices = list("female" = "Female", "male" = "Male"), selected = "Female"),
      #selectInput(inputId = "sex2", label = "Select sex", choices = list("female" = "Female", "male" = "Male"), selected = "Female"),
      #selectInput(inputId = "marriage2", label = "Select maritial status", choices = list("single" = "Single", "married" = "Married", "divorced" = "Divorced", "seperated" = "Seperated"), selected = "Single"),
      #selectInput(inputId = "employment2", label = "Select employment status", choices = list("full time" = "Employed - Full Time", "part time" = "Employed - Part Time", "student" = "Student", "unemployed" = "Unemployed"), selected = "Student"),
      checkboxGroupInput(inputId = "employment2", label = "Select employment status", choices = list("Employed" = "Employed", "Unemployed" = "Unemployed"),
                         selected = "Unemployed"),
      selectInput(inputId = "race2", label = "Select race", choices = list("White" = "White", "Black" = "Black",  "Hispanic" = "Hispanic", "Asian/Pacific Islander"= "Asian/Pacific Islander","Native American" = "Native American"),
                  selected = "Black"),
    ),
    mainPanel(
      p("This is a timeline plot to show the number of days jails of different kinds of people"),
      p("You could make options of year, sex, race, maritial status and employment status in the left side bar."),
      plotOutput(outputId = "figure2")
    )
  )
)

server<-function(input, output) {

  output$figure1 <- renderUI({
    
    CCSO_cur = CCSO_1 %>% 
      filter(RACE == input$race)%>%
      filter(BookingYear >= input$year[1] & BookingYear <= input$year[2])%>%
      filter(SEX %in% input$sex)%>%
      filter(Employment %in% input$employment) %>%
      mutate(text=paste0('<b>City: </b>',CITY,'<br><b>Zip code: </b>',ZIP.CODE,'<br>'))
    
    pp<-ggplot(CCSO_cur, aes(x = Days.in.Jail, y = Age.at.Arrest,text=text)) + 
      geom_point(data = CCSO_cur %>% filter(SEX == "Female" & Employment == "Employed"), color = "dodgerblue", shape = 3, alpha = 0.5, size = 1)+
      geom_point(data = CCSO_cur %>% filter(SEX == "Male" & Employment == "Employed"), color = "orange",  shape = 3, alpha = 0.5, size = 1)+
      geom_point(data = CCSO_cur %>% filter(SEX == "Female" & Employment == "Unemployed"), color = "dodgerblue",  alpha = 0.1, size = 1)+
      geom_point(data = CCSO_cur %>% filter(SEX == "Male" & Employment == "Unemployed"), color = "orange",   alpha = 0.1, size = 1)+
      theme_minimal() + 
      ggtitle(sprintf(
        "Age at Arrest vs. Days in Jail from %s to %s",
        input$year[1], input$year[2])) +
      labs(x="Days in  Jail", y="Age at Arrest")
    
    if(input$avgage) pp<-pp+geom_hline(yintercept = mean(CCSO_cur$Age.at.Arrest))
    if(input$avgdays) pp<-pp+geom_vline(xintercept = mean(CCSO_cur$Days.in.Jail))
    
    
    CCSO_cur.out<-CCSO_cur %>% 
      select(Days.in.Jail,Age.at.Arrest,SEX,Employment) %>%
      group_by(SEX,Employment) %>% 
      summarize(Days.in.Jail.mean=mean(Days.in.Jail,na.rm=T),
                Age.at.Arrest.mean=mean(Age.at.Arrest,na.rm=T))
    
    table.out<-datatable(CCSO_cur.out,options=list(dom='t',paging=F),rownames=F,width=400) %>%
      formatRound(columns=3:4,digits=2) %>%
      formatStyle('SEX',backgroundColor=styleEqual(c('Female','Male'),c("dodgerblue","orange")))
    
    tagList(ggplotly(pp),tags$br(),table.out)
    
  })

  output$figure2 <- renderPlot({
    
    CCSO_cur = CCSO_1 %>%
      filter(Employment %in% input$employment2)%>%
      filter(RACE == input$race2)%>%
      filter(BookingYear >= input$year2[1] & BookingYear <= input$year2[2])
      #filter(SEX %in% input$sex2)%>%
      #filter(MARITIAL.STATUS %in% input$marriage2)%>%
    
    yrs <- split(CCSO_cur,CCSO_cur$BookingYear)
    daysyrs_f <- sapply(yrs,function(a){
      mean(nrow(a%>%filter(SEX == 'Female')),na.rm =TRUE)
    })
    daysyrs_m <- sapply(yrs,function(a){
      mean(nrow(a%>%filter(SEX == 'Male')),na.rm =TRUE)
    })
    
    df_cur = data.frame(year = unique(CCSO_cur$BookingYear),female = daysyrs_f, male = daysyrs_m)
    
    ggplot(data = df_cur) + 
      geom_point(mapping = aes(x = year, y = female,color = "female"))+
      geom_line(mapping = aes(x = year, y = female,color = "female"))+
      geom_point(mapping = aes(x = year, y = male,color = "male"))+
      geom_line(mapping = aes(x = year, y = male,color = "male"))+
      theme_minimal() + 
      ggtitle(sprintf(
        "Num of Individuals Booked %s to %s",
        input$year2[1], input$year2[2])) +
      labs(x="Year", y="Num of Individuals Booked")
    
  })
}

shinyApp(ui = ui, server = server)
