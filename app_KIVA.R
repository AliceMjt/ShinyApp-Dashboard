library(tidyverse)
library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(rsconnect)
library(tinytex)
library(DT)
library(RColorBrewer)
library(tidyverse)
library(tmap)        # Geographical plotting
library(gapminder)   # Country level data
library(maps)        # Geographical datasets
library(sf)          # Shape files (sf) are a common geographical format 
library(plotly)      # Dynamic graphs
library(leaflet) 
library(ggplot2) 
library(gganimate)
library(gifski)
library(png)
library(transformr)
library(directlabels)
library(shinycssloaders)
library(animation)


load("kiva_all.RData")
load("kiva_countries_9.RData")
#options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

###############################################################################################

ui <- dashboardPage(    title = "KIVA-DASHBOARD",
                        skin = "green",
                        
                        
                        ####HEADER AVEC TITRE  
                        dashboardHeader(
                          title = img(src="kiva_logo.jpg", height = 45, align = "left"), #tagList(shiny::icon("bar-chart-o"), Microfinancing
                          titleWidth=250
                          
                          
                          #FIN du dashboardHEADER
                        ),
                        
                        ####BARRE DE GAUCHE     
                        dashboardSidebar(
                          width = 250,
                          sidebarMenu(
                            menuItem("Overview", tabName = "onglet1-content", icon = icon("font-awesome"))),
                          sidebarMenu(
                                      menuItem("World Repartition", tabName = "onglet2", icon = icon("dashboard"), id="tab2_reveal",
                                               sidebarMenuOutput("onglet2flowdown"))),#actionButton('reveal_tab', 'SUBMIT TO REVEAL'))),#menuSubItem("SUBMIT TO REVEAL",tabName="onglet2-content"))),
                          sidebarMenu(
                            menuItem("Sector", tabName = "onglet3", icon = icon("list-ol"),id="fMenu",
                                     sidebarMenuOutput("onglet3flowdown"))),
                          sidebarMenu(
                            menuItem("Stakeholders", tabName="onglet4",icon = icon("database"),badgeLabel = "NEW", badgeColor = "green")
                            
                            
                            #FIN du sidebarMENU
                          )
                          
#FIN du DashboardSIDEBAR
),
                        
####CORPS DU DASHBOARD
dashboardBody( fluidPage(
tabItems(
                     tabItem(
                     "onglet1-content",

                              box(title = strong("MICROFINANCING WITH KIVA : LOANS THAT CHANGE LIVES !"), width = 7, status = "success",
                                  "Kiva is the world's first online lending platform connecting online lenders to entrepreneurs, extending financial services to financially excluded people around the world.
                                  Working as an online crowdfunding platform; it collects money for borrowers who have submitted one project for which they need financing. Donations can be very small and chosen to be allocated or not to one particular project. 100% of every dollar lended on Kiva goes to funding loans.",
                                  p(),strong("This dashboard aims to display all the specificities of the activities of this American NGO and understand its overall impact over the years of 2014 and 2017.")),
                                  
                              box(strong(em("Introduction to Kiva NGO")),p(),width=5,HTML('<iframe width="352" height="176" src="//www.youtube.com/embed/hmjTwp_MViU""
                                                 frameborder="0" allowfullscreen></iframe>'),p(),"- Retrospective and perspectives -"),
                              box(title = strong("86 COUNTRIES"), width = 3,status = "danger", code("Global scope on all continents"),p(),p()),
                              box(title = strong(">$1.38 BILLIONS"), width = 3,status = "danger", code("Loans funded through Kiva"),p(),p()),
                              box(title = strong(">3.4 MILLIONS"), width = 3,status = "danger", code("Borrowers from 1.8M Lenders"),p(),p()),
                              valueBox(value="$25", subtitle = "EACH DONATION", icon = icon("usd"),color = "green",width = 3),
                              box(title = "World Distribution of Kiva Loans", width = 7, leafletOutput("mymap"),height="460px"),
                                  #p(),chooseSliderSkin("Modern"),
                                  #sliderInput("map_years","TIME FRAME", min=2014, max=2017,value = c(2014,2017), sep="", step=1)),
                              box(title = "Global Loan Distribution", width = 5, height="460px",plotOutput("plot_histg")), 
                                  #p(),chooseSliderSkin("Flat"),
                                  #sliderInput("map_years2","TIME FRAME", min=2014, max=2017,value = c(2014,2017), sep="", step=1)),
                              
                              fluidRow( 
                                tabBox( title = tagList(shiny::icon("bar-chart-o"), "General Observations"),
                                        id = "tabset1", height = "300px", width=12,  # status = "danger", solidHeader = TRUE
                                        tabPanel("CONTINENT", DT::dataTableOutput("table1")),
                                        tabPanel("SECTOR", DT::dataTableOutput("table2"))))
                       #Fin du TabItem Menu
                       ),
                            
                            
                            
                            
                            # box(title = "Global Observations", status = "danger", solidHeader = TRUE, width=12, 
                            #     tabPanel("Observations", DT::dataTableOutput("table1")))),
                            # 
                            
                        tabItem(
                        "onglet2-content",
                              
                              tabBox( title = "COUNTRY", side = "right",
                                      id = "tabset2", height = "550px", width=7,
                                      tabPanel("Lowest Loan Median",plotOutput("plot3", height = 550)),
                                      tabPanel("Highest Loan Median", plotOutput("plot4", height = 550))),
                              
                              
                              box(title = "CONTINENT",footer = "en US",
                                  width = 5,height = "610px", #solidHeader = TRUE
                                  tabPanel("Countries Through Time", plotOutput("plot2", height = 510))),
                              
                              box(title = "ACTIVITY",
                                  width = 12, #solidHeader = TRUE
                                  tabPanel("Loans Activity Type", DT::dataTableOutput("table_activity"))) 

                        #Fin du TabItem
                        ),
                            
                            
                        tabItem(
                        "onglet3-content1",
                        #box(width = 3, height="100px",titre="hello","gif"),#,img(src="serie.gif", height = 45)),#imageOutput("testgif")),
                        valueBox(value="1''30", subtitle = "LOADING TIME", icon =  icon("paragraph"),
                                 color = "orange",width = 12),
                        
                        #fluidRow(column(width=6,withSpinner(
                          box(width = 6, height="500px", imageOutput("plot_animate")),#type=2))),
                        #fluidRow(column(width=6,withSpinner(
                          box(width = 6, height="500px", imageOutput("plot_animate2"))#,type=2)))
                          
                            
                        #Fin du TabItem
                        ),
                     
                     tabItem(
                       "onglet3-content2",
                       box(title = strong("Loans Sectorial Distribution"),width=12,height="500px",plotOutput("plot_distr"))
                       # box(title = strong("World Results"),#footer = "en US",
                       #          width = 4,height = "500px", #solidHeader = TRUE,
                       #          tabPanel("Countries through time", plotOutput("plot1", height = 350)))

                       #Fin du TabItem
                     ),  
                        tabItem(
                        "onglet3-content3",
                        valueBox(value="AGR",  icon =  icon("seedling"), subtitle = "AGRICULTURE",
                                 color = "green",width = 3),
                        box(title = strong("Agricultural Sector"), width = 9,
                            tabPanel("Most Popular Use of loans", DT::dataTableOutput("table_agriculture"))),
                        box(title = strong("Health Sector"), width = 9,
                            tabPanel("Most Popular Use of loans", DT::dataTableOutput("table_health"))),
                        valueBox(value="HLT",  icon =  icon("heartbeat"), subtitle = "HEALTH",
                                 color = "orange",width = 3),
                        box(title = strong("Services Sector"), width = 9,
                                tabPanel("Most Popular Use of loans", DT::dataTableOutput("table_services"))),
                        valueBox(value="SVC",  icon =  icon("user-graduate"), subtitle = "SERVICES",
                                 color = "blue",width = 3),
                        box(title = strong("Personal Use"), width = 9,
                            tabPanel("Most Popular Use of loans", DT::dataTableOutput("table_personal"))),
                        valueBox(value="PPL",  icon =  icon("users"), subtitle = "USER",
                                 color = "orange",width = 3)
                                
                                     
                                     #imageOutput("plot_hist_img2")))#plotOutput("plot_AG")),
                        # p(),chooseSliderSkin("Modern"),
                        # sliderInput("AG_years","TIME FRAME", min=2014, max=2017,value = c(2014,2017), sep="", step=1)),
                             
                        #Fin du TabItem
                         ),
                            
                       
                            
                       tabItem(
                       "onglet4",
                              box(title = "STORIES OF KIVAers", width = 12,
                                  "Kiva empowers. Every project is special with a successful story which deserves to be shared. Each donation has multiple impacts such as :",p(),p(),
                                  "- giving donators the opportunity to make a real difference in the world and involving their own community into creating an impact",p(),"- tackling some of today’s most important social issues through the lens of microlending",p(),"- touching the lives of thousands of people in the process"),
                              # box(title = "AGRICULTURE", width = 3, "TO BE BUILT !"),
                              # box(title = "ET TROIS ZEROS", width = 3, "TO BE BUILT !"),
                       box(strong("EMPOWERED TO LAUNCH A NEW BUSINESS"),p(), width=6,
                              HTML('<iframe width="400" height="200" src="//www.youtube.com/embed/pfQXapuig2k"
                              "frameborder="0" allowfullscreen></iframe>'),p(),"Jacqueline fled violence in Burundi with her children. As a refugee in Rwanda she had to rebuild her life and loan from Kiva lenders helped her start a business.",p()),
                       box(strong("AND PURSUE WHAT THEY BELIEVE IN !"),p(), width=6,
                              HTML('<iframe width="400" height="200" src="//www.youtube.com/embed/VRKnUSFnZEA""
                               "frameborder="0" allowfullscreen></iframe>'),p(),"Katrina and Keely first met in the summer of 2013 and bonded instantly over urban farming in the city area of New York city thanks to Kiva microfinancing.",p()),
                       box(strong("AND LEVERAGE LIFE QUALITY"),p(), width=6,
                           HTML('<iframe width="400" height="200" src="//www.youtube.com/embed/EdHwvqEWGm0""
                               "frameborder="0" allowfullscreen></iframe>'),p(),"Mohammad is a forty-two-year-old married man. He is the father of four sons. Six years ago, Mohammad opened a small cafe in the city,
                                where he sells soft drinks, coffee and tea. It's a good business and earns a good income but it wasn't enough to cover family expenses.",p()),
                       box(strong("WHILE INVOLVING YOUR OWN COMMUNITY"),p(), width=6,
                           HTML('<iframe width="400" height="200" src="//www.youtube.com/embed/3EPCSpzs5rE""
                               "frameborder="0" allowfullscreen></iframe>'),p(),"Kiva U envisions a world where all students and educators are empowered with the tools and opportunities to become informed, inspired, and mobilized global citizens. Through experiential learning, digital collaboration, and the power of human connections, Kiva U provides a platform for young people to take action and change lives via microfinance and financial inclusion.",p())
                       
                       
                        #Fin du TabItem
                        )
                     
                     
#Fin des TabItems   
),
###NOTIFICATION AU MILIEU
tags$head(
  tags$style(
    HTML(".shiny-notification {
         position:fixed;
         top: calc(50%);
         left: calc(50%);
         color: grey;
         font-size: 16px;}")))
                          
#FIN du DashboardBODY
))


  
                        
#FIN du DashboardPAGE
)

###########SERVEUR################################################################################
server = (function(input, output,session) {
  
  ###MENU SLIDER

  observeEvent(input$onglet2flowdown, {
    updateTabsetPanel(session, "onglet2",
                      selected = "onglet2-content")
  })
  
  #####DROPDOWN MENU ONGLET WORLD REPARTITION
  output$onglet2flowdown <- renderMenu({
    menu_list <- list(
      menuSubItem("Submit to reveal graphs",tabName="onglet2-content",selected=T),
      chooseSliderSkin("Modern"), #or font : "Flat"
      sliderInput("Menu_years", h5(strong("Select Years")),     #setSliderColor(slider_colors, 1:length(slider_colors)),
                  min = 2014, max = 2017, 
                  value = c(2014, 2017),
                  step = 1, sep = "",
                  width=250), 
      #shinyWidgets::
      pickerInput("Menu_continent","Select Continent", choices = list("Africa" = "Africa",
                                                                      "Asia"   = "Asia", 
                                                                      "Central America" = "Central_America",
                                                                      "Europe" = "Europe",
                                                                      "South America" = "South_America",
                                                                      "Middle America" = "Middle_America",
                                                                      "North America"= "North_America",
                                                                      "Oceania" = "Oceania"),
                  options = list(`actions-box` = TRUE),
                  multiple = T,
                  selected = c("Africa","Asia","Central_America","Europe","South_America","Middle_America","North_America","Oceania")
      ),
      #shinyWidgets::
      pickerInput("Menu_country","Select Country",
                  choices=
                    list(
                      # "Benin","Burkina Faso","Burundi","Cameroon","Congo","Cote D'Ivoire","Egypt", "Ghana", "Kenya", "Lesotho", "Madagascar", "Malawi", "Mali", "Mozambique", "Namibia", "Nigeria", "Rwanda", "Senegal", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Tanzania", "The Democratic Republic of the Congo", "Togo", "Uganda", "Zambia", "Zimbabwe",
                      # "Afghanistan", "Armenia", "Azerbaijan", "Bhutan", "Cambodia", "China", "Georgia", "India", "Indonesia", "Kyrgyzstan", "Lao People's Democratic Republic", "Mongolia", "Myanmar", "Nepal", "Pakistan", "Philippines", "Tajikistan", "Thailand", "Timor-Leste", "Vietnam",
                      # "Belize", "Costa Rica", "Dominican Republic", "Ecuador", "El Salvador", "Haiti", "Honduras", "Nicaragua", "Panama", "Puerto Rico", "Saint Vincent and the Grenadines", "Virgin Islands", "Mexico",
                      # "Albania", "Kosovo", "Moldova", "Ukraine",
                      # "Bolivia", "Brazil", "Guatemala", "Paraguay", "Peru", "Suriname","Chile", "Colombia",
                      # "Iraq", "Israel", "Jordan", "Lebanon", "Liberia", "Turkey", "Yemen","Palestine",
                      # "United States","Canada","Guam",
                      # "Samoa", "Solomon Islands", "Vanuatu"),
                      "Africa" = list("Benin","Burkina Faso","Burundi","Cameroon","Congo","Cote D'Ivoire","Egypt", "Ghana", "Kenya", "Lesotho", "Madagascar", "Malawi", "Mali", "Mozambique", "Namibia", "Nigeria", "Rwanda", "Senegal", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Tanzania", "The Democratic Republic of the Congo", "Togo", "Uganda", "Zambia", "Zimbabwe"),
                      "Asia"= list("Afghanistan", "Armenia", "Azerbaijan", "Bhutan", "Cambodia", "China", "Georgia", "India", "Indonesia", "Kyrgyzstan", "Lao People's Democratic Republic", "Mongolia", "Myanmar", "Nepal", "Pakistan", "Philippines", "Tajikistan", "Thailand", "Timor-Leste", "Vietnam"),
                      "Central_America"= list("Belize", "Costa Rica", "Dominican Republic", "Ecuador", "El Salvador", "Haiti", "Honduras", "Nicaragua", "Panama", "Puerto Rico", "Saint Vincent and the Grenadines", "Virgin Islands", "Mexico"),
                      "Europe" = list("Albania", "Kosovo", "Moldova", "Ukraine"),
                      "South_America" = list("Bolivia", "Brazil", "Guatemala", "Paraguay", "Peru", "Suriname","Chile", "Colombia"),
                      "Middle_East" = list("Iraq", "Israel", "Jordan", "Lebanon", "Liberia", "Turkey", "Yemen","Palestine"),
                      "North_America" = list("United States","Canada","Guam"),
                      "Oceania" = list("Samoa", "Solomon Islands", "Vanuatu")),
                  options = list(`actions-box` = TRUE),multiple = T,
                  selected = c( "Benin","Burkina Faso","Burundi","Cameroon","Congo","Cote D'Ivoire","Egypt", "Ghana", "Kenya", "Lesotho", "Madagascar", "Malawi", "Mali", "Mozambique", "Namibia", "Nigeria", "Rwanda", "Senegal", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Tanzania", "The Democratic Republic of the Congo", "Togo", "Uganda", "Zambia", "Zimbabwe",
      "Afghanistan", "Armenia", "Azerbaijan", "Bhutan", "Cambodia", "China", "Georgia", "India", "Indonesia", "Kyrgyzstan", "Lao People's Democratic Republic", "Mongolia", "Myanmar", "Nepal", "Pakistan", "Philippines", "Tajikistan", "Thailand", "Timor-Leste", "Vietnam",
      "Belize", "Costa Rica", "Dominican Republic", "Ecuador", "El Salvador", "Haiti", "Honduras", "Nicaragua", "Panama", "Puerto Rico", "Saint Vincent and the Grenadines", "Virgin Islands", "Mexico",
      "Albania", "Kosovo", "Moldova", "Ukraine",
      "Bolivia", "Brazil", "Guatemala", "Paraguay", "Peru", "Suriname","Chile", "Colombia",
      "Iraq", "Israel", "Jordan", "Lebanon", "Liberia", "Turkey", "Yemen","Palestine",
      "United States","Canada","Guam",
      "Samoa", "Solomon Islands", "Vanuatu")
      ),     
      shiny::checkboxGroupInput("Menu_genders", h5(strong("Borrower Gender")), 
                                choices = list("Female" = "female", 
                                               "Male" = "male"),
                                selected = c("female", "male"))
      

      
    )
    sidebarMenu(.list = menu_list)
  })
  
  #####DROPDOWN MENU ONGLET SECTOR
  
  output$onglet3flowdown <- renderMenu({
    menu_list <- list(
      menuItem("Overview", tabName = "onglet3-content1", icon = icon("dashboard"),selected=T),
      menuItem("Repartition", tabName = "onglet3-content2", icon = icon("list-ol")),
      menuItem("Usages", tabName = "onglet3-content3", icon = icon("database"))
      #menuItem("Stories", tabName = "onglet3-content4", icon = icon("font-awesome"))
    )
    sidebarMenu(.list = menu_list)
  })
  


  
  ######################## ONGLET 1 HOMEPAGE
  
  
  ###### INTEGRATION VIDEO
  
  # output$video <- renderUI({
  #     HTML(paste0('<iframe width="200" height="100" src="https://youtu.be/hmjTwp_MViU"
  #                 frameborder="0" allowfullscreen></iframe>'))
  #   })
  # 
  # 
  
  
  
  ####### INTEGRATION MAP
  
  datamap <- kiva_countries_9 #eventReactive(input$map_years,#%>% filter(year == 2015)  
  
  # datamap <- reactive({
  #  kiva_countries_9 <- kiva_countries_9 %>% dplyr::filter (year >= input$map_years[1], year <= input$map_years[2]) 
  #  return(kiva_countries_9)})
  
  palet <- colorBin("Greens", domain = datamap %>% pull(Total_Loans_Number),bins = c(10,100,500,1000,4000,7000,10000,13000))  
  
  labels <- sprintf(                                                  # Below we define the labels
    "<strong>%s</strong><br/>TOTAL : %g loans<br/>Average Loan Amount: $%s
    <br/>Median Loan Amount: $%s<br/>Maximum Loan Amount: $%s<br/>Average Repayment Time: %s months",       
    datamap$country,datamap$Total_Loans_Number,datamap$Average_Loan_Amount,datamap$Median_Loan_Amount,
    datamap$Maximum_Loan_Amount,datamap$Average_Repayment_Time_in_months) %>% lapply(htmltools::HTML)   
  
  output$mymap <- renderLeaflet({  
    datamap%>% 
      data.frame() %>%                                # Turn into dataframe (technical)
      sf::st_sf() %>%                                 # Format in sf
      st_transform("+init=epsg:4326") %>%             # Convert in particular coordinate reference  
      leaflet(options = leafletOptions(Zoom=1.5, minZoom = 1.3, maxZoom = 3, dragging=TRUE)) %>% 
      addPolygons(fillColor = ~palet(Total_Loans_Number), weight = 2, opacity = 1, color = "white",dashArray = "1",fillOpacity = 0.7,
                  highlight = highlightOptions(weight = 3,color = "#CBCBCB", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
                  label = labels,    
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 6px"),
                                              textsize = "10px", direction = "auto")) %>%
      addLegend(pal = palet, values = ~Total_Loans_Number, opacity = 0.7, title = "Map Legend", position = "bottomright")
  }) #FIN DU MAP
  
  
  
  
  ####### HISTOGRAMME
  # data_filtered_H <- kiva_all() %>% group_by(year) %>%
  #   summarize(nb_loan=n())
  # 
  # #pal <- colorRampPalette(colors = c("lighgreen", "green"))(8)
  # 
  # output$plotHome <-renderPlot({ data_filtered_H %>% 
  #     ggplot(aes(x=data_filtered_H$year,y=data_filtered_H$nb_loan))+ geom_bar()
  #    #+ scale_fill_gradientn(theme(axis.text.x = element_text(angle=90, hjust = 1)))
  # })
  
  
  
  # output$plot_histg<- renderPlot({ 
  # d <- kiva_all %>% group_by(loan_amount) %>% summarise(nb_loans = n())
  # d %>% ggplot( aes(x =loan_amount))+#geom_histogram(fill="#69b3a2", color="black", alpha=0.9)
  #   geom_histogram(aes(y = ..count..),fill="#CC3333", color="black", alpha=0.75) 
  # 
  # })
  
  
  kiva_filtered_histg <- reactive({
    kiva_all1 <- kiva_all %>%
      dplyr::filter(year >= input$map_years2[1], 
                    year <= input$map_years2[2]) %>% 
      group_by(loan_amount) 
    
    return(kiva_all1)})
  
  
  output$plot_histg<- renderPlot({ 
    d <- kiva_all %>% group_by(loan_amount) %>% summarise(nb_loans = n())
    ggplot(d, aes(x =loan_amount)) + geom_histogram(aes(y =..density..),#col="black", 
                                                    fill="#CC3333", color="black",alpha=0.75) +  geom_density(col=11)})
  
  
  #### TABLEAU 1
  datatable1<- reactive({
    kiva_all <- kiva_all %>%
      group_by(continent) %>%
      summarize(Total_Loans_Number=n(),
                Maximum_Loan_Amount=max(loan_amount),
                Median_Loan_Amount=median(loan_amount),
                Average_Loan_Amount=round(mean(loan_amount),0),
                Average_Repayment_Time_in_months=round(mean(term_in_months),0))%>%
      arrange(desc(Total_Loans_Number))})
  #filter return
  output$table1  <- DT::renderDataTable(datatable1(),options = list(autoWidth = FALSE)) #,input$show_vars, drop = FALSE) 
  
  #### TABLEAU 2
  datatable2<- reactive({
    kiva_all <- kiva_all %>%
      group_by(sector) %>%
      summarize(Total_Loans_Number=n(),
                Maximum_Loan_Amount=max(loan_amount),
                Median_Loan_Amount=median(loan_amount),
                Average_Loan_Amount=round(mean(loan_amount),0),
                Average_Repayment_Time_in_months=round(mean(term_in_months),0))%>%
      arrange(desc(Total_Loans_Number))})
  #filter return
  output$table2  <- DT::renderDataTable(datatable2(),options = list(autoWidth = FALSE)) #,input$show_vars, drop = FALSE)
  
  
  
  ################################  ONGLET 2
  
  #### DATA REACTIVE AVEC MENU
  data_filtered <- reactive({               
    kiva_all <- kiva_all %>% dplyr::filter(year >= input$Menu_years[1], 
                                           year <= input$Menu_years[2],
                                           continent %in% input$Menu_continent,
                                           country %in% input$Menu_country,
                                           borrower_genders %in% input$Menu_genders)
                                           
    return(kiva_all)})
  

  
  ##### GRAPH DOUBLE TAB COUNTRIES

  
  #### GRPAHIQUE 1
  data_filtered2 <- reactive({  
    data_filtered2 <- data_filtered() %>% group_by(country) %>%
      summarize(max_loan=max(loan_amount),median_loan=median(loan_amount),min_loan=min(loan_amount),max_loan=max(loan_amount)) %>% 
      arrange(desc(median_loan)) %>% tail(17)
  })
  
  
  
  output$plot3 <-renderPlot({  plot(ggplot(data_filtered2(), aes(x=reorder(country,median_loan), y=median_loan, fill=median_loan))+
                                      geom_bar(stat="identity")+ coord_flip()+
                                      geom_text( aes(label=paste("Min :",round(min_loan)," | Max :",round(max_loan))),
                                                 hjust=1.01,color="white", size=2.7)+
                                      theme(legend.position="none", axis.line = element_line(colour = "steelblue", size = 0.5, linetype = "solid"))+
                                      ggplot2::scale_color_continuous(low="steelblue", high="maroon")+
                                      ggplot2::scale_fill_continuous(low="steelblue", high="maroon")+
                                      labs(title="Countries with the Smallest median loan amount",
                                           subtitle="And their maximum loan amount associated",
                                           x="",
                                           y="Median Loan Amount"))
    progress <- Progress$new(session, min=1, max=100)
    on.exit(progress$close())
    progress$set(message = "THANK YOU FOR YOUR PATIENCE :)",
                 detail = ' The loading time can take longer than expected')
    
    for (i in 1:100) {
      progress$set(value = i)
      Sys.sleep(0.5)}
    })
  
  #### GRAPHIQUE 2
  
  data_filtered3 <- reactive({  
    data_filtered3 <- data_filtered() %>% group_by(country) %>%
      summarize(max_loan=max(loan_amount),median_loan=median(loan_amount),min_loan=min(loan_amount),max_loan=max(loan_amount)) %>% 
      arrange(desc(median_loan)) %>% head(17)
  })
  
  
  output$plot4 <-renderPlot({ ggplot(data_filtered3(), aes(x=reorder(country,median_loan), y=median_loan, fill=median_loan))+
      geom_bar(stat="identity")+ coord_flip()+
      geom_text( aes(label=paste("Min :",round(min_loan)," | Max :",round(max_loan))),
                 hjust=1.01,color="white", size=2.7)+
      theme(legend.position="none", axis.line = element_line(colour = "steelblue", size = 0.5, linetype = "solid"))+
      ggplot2::scale_color_continuous(low="steelblue", high="maroon")+
      ggplot2::scale_fill_continuous(low="steelblue", high="maroon")+
      labs(title="Countries with the biggest median loan amount",
           subtitle="And their maximum loan amount associated",
           x="",
           y="Median Loan Amount")})  
  
  
  
 ##### GRAPH BLEU CONTINENT
  
  output$plot1 <- renderPlot({  data_filtered() %>% 
      group_by(continent) %>%
      summarise(avg_loan = mean(loan_amount)) %>% 
      ggplot(aes(x=fct_infreq(continent),y=avg_loan, fill=continent))+geom_col()+
      theme(legend.position="none", axis.line = element_line(colour = "steelblue", size = 0.5, linetype = "solid"))+
      labs(title="Countries With The Smallest Median Loan Amount",
           subtitle="And their maximum loan amount associated",
           x="",
           y="Median Loan Amount")})    
  
  
  
  # ggplot2:: scale_color_brewer(palette = "Dark2")+
  # ggplot2::  scale_color_manual(values = brewer.pal(n=11,name = "PRGn"))+
  # ggplot2:: scale_fill_gradientn(colours = terrain.colors(3)))
  
  ####### TABLEAU PROJETS LOANS PURPOSES
  
  datatable_activity<- reactive({
    data_filtered <- data_filtered() %>%
      group_by(sector,activity) %>%
      summarize(Total_Loans_Number=n(),
                Average_Loan_Amount=round(mean(loan_amount),0))%>%
      arrange(desc(Total_Loans_Number)) %>% head(10)})
  #filter return
  output$table_activity  <- DT::renderDataTable(datatable_activity(),options = list(autoWidth = FALSE)) 
  
  
  #### GRPAHIQUE 2 MOCHE
  output$plot2<- renderPlot({ 
       
      ggplot(kiva_all,aes(x=continent,y=loan_amount,color=loan_amount))+ geom_point()+
      scale_fill_gradientn(colours = terrain.colors(4))+theme(axis.text.x = element_text(angle=90, hjust = 1))
  })

  
  
  
  ################### MENU SECTOR
  
  
  # output$gif_loading <- renderImage({
  #   filename <- normalizePath(file.path('./www',paste('serie', '.gif', sep='')))
  #   list(src = "serie.gif", alt = paste("serie.gif"))}, deleteFile = FALSE)
  # 
  # 
  ##### ANIMATED PLOT  
  
  output$plot_animate <- renderImage({
    outfile <- tempfile(fileext='.gif')
    GROUP_A <- kiva_all %>% group_by(year,continent,sector) %>% 
      summarize(mean_loan=mean(loan_amount),nb_loan=n(),mean_length=mean(term_in_months))
  p = ggplot(GROUP_A,aes(x=mean_length,y=mean_loan, size = nb_loan, colour = continent)) +
      geom_point(alpha = 0.7, show.legend = FALSE) +
      scale_size(range = c(2, 12)) + scale_x_log10() + facet_wrap(~sector) +
      labs(title = 'Continent distribution among sectors : {as.integer(frame_time)}', x = 'Average Loan Length', y = 'Average Loan Amount') +
      transition_time(year) + ease_aes('linear')
   
    anim_save("outfile.gif", animate(p),duration = 3) 
    list(src = "outfile.gif", contentType = 'image/gif')}
    , deleteFile = TRUE)
  
  
output$plot_animate2 <- renderImage({
  f_outfile <- tempfile(fileext='.gif')
  GROUP_B<-kiva_all %>%
    group_by(year,country, continent) %>%  #,loan_amount,term_in_months) %>%
    summarize(mean_loan=mean(loan_amount),nb_loan=n(),mean_length=mean(term_in_months))
  f = ggplot(GROUP_B, aes(x =mean_length,y=mean_loan, size = nb_loan, color = continent)) + 
    geom_point(alpha = 0.7, show.legend = FALSE) + facet_grid(continent ~ . )+
    scale_size(range = c(2, 16)) +
    #scale_x_log10() +
    labs(title = 'Loan lenght relationship among continents : {as.integer(frame_time)}', x = 'Average Loan Length', y = 'Average Loan Amount') +
    transition_time(year) +
    ease_aes('linear')
  anim_save("f_outfile.gif", animate(f),duration = 3) 
  list(src = "f_outfile.gif", contentType = 'image/gif')}
  , deleteFile = TRUE)
  

####loading time 
output$plot_distr <- renderPlot({
  
  d <- kiva_all %>% mutate( fill = as.factor(sector))
  
 ggplot(d, aes(x = sector, y= loan_amount, fill = sector)) +
    scale_y_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::trans_format("log10", scales::math_format(10^.x))
    ) +
    geom_boxplot() +
    labs(x= 'Sector Type',y = 'Funded Amount', 
         title = paste("Distribution of", ' Funded Amount ')) +
    theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))})
 
#######AGRICULTURE LOANS TABLE

datatable_agriculture <- reactive({
AgriculturalLoans <- kiva_all %>%  dplyr::filter(sector =="Agriculture") %>%  group_by(use)  %>% summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(use = reorder(use,Count)) %>%
  head(5)  %>% na.omit(use)})

output$table_agriculture  <- DT::renderDataTable(datatable_agriculture(),options = list(autoWidth = FALSE)) #,input$show_vars, drop = FALSE)

datatable_health <- reactive({
  healthLoans <- kiva_all %>%  dplyr::filter(sector == "Health") %>%  group_by(use)  %>% summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    ungroup() %>%
    mutate(use = reorder(use,Count)) %>%
    head(5)  %>% na.omit(use)})

output$table_health  <- DT::renderDataTable(datatable_health(),options = list(autoWidth = FALSE)) #,input$show_vars, drop = FALSE)

datatable_services <- reactive({
  educationLoans <- kiva_all %>%  dplyr::filter(sector =="Services") %>%  group_by(use)  %>% summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    ungroup() %>%
    mutate(use = reorder(use,Count)) %>%
    head(5)  %>% na.omit(use)})

output$table_services  <- DT::renderDataTable(datatable_services(),options = list(autoWidth = FALSE)) #,input$show_vars, drop = FALSE)


datatable_personal <- reactive({
  personalLoans <- kiva_all %>%  dplyr::filter(sector =="Personal Use") %>%  group_by(use)  %>% summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    ungroup() %>%
    mutate(use = reorder(use,Count)) %>%
    head(5)  %>% na.omit(use)})

output$table_personal  <- DT::renderDataTable(datatable_personal(),options = list(autoWidth = FALSE)) #,input$show_vars, drop = FALSE)



 
###### GRAPHIQUE AGRICULTURE

# output$plot_AG <- renderPlot({ 
#   
#   # AgriculturalLoans <- reactive({
#   #   kiva_all <- kiva_all %>%  dplyr::filter(sector =="Agriculture", year >= input$AG_years[1], year <= input$AG_years[2]) %>%
#   #     group_by(use)  %>% summarise(Count = n()) %>%
#   #   arrange(desc(Count)) %>%
#   #   ungroup() %>%
#   #   mutate(use = reorder(use,Count)) %>%
#   #   head(10)  %>% na.omit(kiva_all$use)
#   #   return(kiva_all)})
#   # 
# 
#   AgriculturalLoans <- kiva_all %>%  dplyr::filter(sector =="Agriculture") %>%  group_by(use)  %>% summarise(Count = n()) %>%
#     arrange(desc(Count)) %>%
#     ungroup() %>%
#     mutate(use = reorder(use,Count)) %>%
#     head(10)  %>% na.omit(use)
#   
#     ggplot(AgriculturalLoans,aes(x = use,y = Count)) +
#     geom_bar(colour="white", fill = "grey") +  #stat='identity'
#     geom_text(aes(x = use, y = 1, label = paste0("(",Count,")",sep="")),
#               hjust=0, vjust=.5, size = 4, colour = 'black',
#               fontface = 'bold') +
#     labs(x = 'Use of Loans', 
#          y = 'Number')+ 
#          #title = 'Use of Loans and Count') +
#     coord_flip() +
#     theme_bw() })




output$plot_hist_img2 <- renderImage({
  
  s_outfile <- tempfile(fileext='.gif')
  
  AgriculturalLoans <- kiva_all %>%  dplyr::filter(sector =="Agriculture") %>%  group_by(use)%>%
    summarise(Count = n()) %>% arrange(desc(Count)) %>% ungroup() %>%mutate(use = reorder(use,Count)) %>% head(10)  %>% na.omit(use)

    s = ggplot(AgriculturalLoans,aes(x = use,y = Count)) +
      geom_bar(colour="white", fill = "grey") +  #stat='identity'
      geom_text(aes(x = use, y = 1, label = paste0("(",Count,")",sep="")),
                hjust=0, vjust=.5, size = 4, colour = 'black',
                fontface = 'bold') +
      labs(x = 'Use of Loans', 
           y = 'Number')+ 
      #title = 'Use of Loans and Count') +
      coord_flip() +
      theme_bw() + transition_time(NA) +
      ease_aes('linear')
    
  anim_save("s_outfile.gif", animate(s),duration = 3) 
  list(src = "s_outfile.gif", contentType = 'image/gif')}
  , deleteFile = TRUE)
 
  #lie au menu deroulant
  output$value <- renderPrint({ input$select })
  
  #lie au menu deroulant avance bis
  observe({
    print(input$locInput)
  })
  
  #FIN du SERVER 
})

#RUN DE L'APP
shinyApp(ui = ui, server = server)




