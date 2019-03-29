#libraries that will be used to generate the figures for the RShiny report
install.packages("shiny")
install.packages("tidyverse")
install.packages("RColorBrewer")
install.packages("fiftystater")
library(shiny)
library(tidyverse)
library(fiftystater)
library(RColorBrewer)

#read in the file
url <-  "https://raw.githubusercontent.com/usfviz/R-tists-final/master/U.S._Chronic_Disease_Indicators__CDI_.csv"
USCD <- read.csv(url)

# Define server logic required to a map or a barplot
shinyServer(function(input, output) {

  USCD_Topic_Recent <- reactive({
  #filter to include data only relevant to hospitalization
  USCD_hosp <- USCD[str_detect(USCD$Question, "Hospitalization") | str_detect(USCD$Question, "hospitalization"),]
  
  #filter to include appropriate topic
  USCD_Topic_Filter <- USCD_hosp %>% filter(as.character(Topic) == input$Topic & DatavalueFootnote != "No data available")
  
  #select the most recent year of data per state per topic
  MostRecent <- USCD_Topic_Filter %>% arrange(LocationDesc, desc(YearStart)) %>% group_by(LocationDesc, Topic) %>% summarize(YearStart = max(YearStart))
  
  #isolate the most recent year's data
  USCD_Topic_Recent <- USCD_Topic_Filter %>% inner_join(MostRecent, by = c("LocationDesc","Topic", "YearStart"))

  #selecting the appropriate data we want to look at Age-adjusted rates, crude rates, total number
  USCD_Topic_Recent <- USCD_Topic_Recent %>% filter(as.character(DataValueType) == input$DataValueType) %>% arrange(desc(DataValueAlt))

  #Convert cases per 1,000 to cases per 10,000
  USCD_Topic_Recent$DataValueAlt[which(USCD_Topic_Recent$DataValueUnit == "cases per 1,000")] <- USCD_Topic_Recent$DataValueAlt[which(USCD_Topic_Recent$DataValueUnit == "cases per 1,000")]*10
  
  #Convert to mean or median rates/raw totals
  USCD_Topic_Recent <- USCD_Topic_Recent %>% group_by(LocationDesc, Stratification1, StratificationID1) %>% 
    summarize(Means = mean(DataValueAlt, na.rm = TRUE), Medians = median(DataValueAlt, na.rm = TRUE))
  
  #select demographic of interest
  USCD_Topic_Recent <- USCD_Topic_Recent %>% filter(as.character(Stratification1) == input$Stratification1) %>% mutate(state = tolower(LocationDesc))
  
  #remove the United States aggregated data from data source
  USCD_Topic_Recent <- USCD_Topic_Recent %>% filter(as.character(LocationDesc) != "United States")
  })
  
  #create a chart
  output$ChartUSCD <- renderPlot({
    
    #create a ggplot object with identified data
    K <- ggplot(USCD_Topic_Recent())
    
    
    #if we choose map view
    if(input$view == 1){
      
      #color scale 
      getFill <- colorRampPalette(brewer.pal(11, "RdBu"))
      colourCount <- 101
      #add features to that ggplot object
      p <- K + geom_map(data = fifty_states, aes(map_id = id),
                        fill = "grey",
                        colour = "black",
                        map = fifty_states) + 
        # map points to the fifty_states shape data
        geom_map(aes(map_id = state, fill = Medians), colour = "black", map = fifty_states) + 
        expand_limits(x = fifty_states$long, y = fifty_states$lat) +
        coord_map() +
        scale_x_continuous(breaks = NULL) + 
        scale_y_continuous(breaks = NULL) +
        labs(x = "", y = "") +
        theme(panel.background = element_blank(), 
              legend.text = element_text(angle = 90),
              legend.position =  "bottom",
              legend.title = element_blank()) 
      
      #print the ggmap
      p + scale_fill_gradientn(colors = getFill(colourCount)[colourCount:1])
      
    }
    else{
      
      #make the bar chart by adding features
      r <- K + geom_bar(aes(x = LocationDesc, y = Medians, fill = LocationDesc), stat = "Identity") + 
        xlab("State") + ylab("Measurement") + 
        theme_bw() +  
        theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7, vjust = 0.25),
              legend.text = element_text(size=8),
              legend.title = element_blank())
      
      #print the bar chart 
      r
      
    }
    

      
  })
  
  # Downloadable csv of selected dataset
  output$downloadData <- downloadHandler(
    filename <- function(){
      paste(input$DataValueType, input$Stratification1, input$Topic, Sys.Date(), ".csv", sep = "")
    },
    content = function(file){
      write.table(USCD_Topic_Recent(), file, row.names = FALSE, quote = FALSE, sep = ",")
    }
  )
  
})

