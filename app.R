##############################################################################
# PlotTwist: Shiny app for plotting and comparing time-dependent data
# Created by Joachim Goedhart (@joachimgoedhart), first version 2018
# Takes non-tidy, spreadsheet type data as input assuming first column is "Time"
# Non-tidy data is converted into tidy format
# Raw data is displayed with user-defined visibility (alpha)
# The mean is displayed with user-defined visibility (alpha)
# Inferential statistics (95%CI) can be displayed as ribbon
##############################################################################

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)

#Confidence level
Confidence_Percentage = 95
Confidence_level = Confidence_Percentage/100

#Several qualitative color palettes that are colorblind friendly
#From Paul Tol: https://personal.sron.nl/~pault/
#Code to generate vectors in R to use these palettes

Tol_bright <- c('#EE6677', '#228833', '#4477AA', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')
Tol_muted <- c('#88CCEE', '#44AA99', '#117733', '#999933', '#DDCC77', '#CC6677', '#882255', '#AA4499', '#332288', '#DDDDDD')
Tol_light <- c('#BBCC33', '#AAAA00', '#77AADD', '#EE8866', '#EEDD88', '#FFAABB', '#99DDFF', '#44BB99', '#DDDDDD')


#Read a text file (comma separated values)
df_wide_example <- read.csv("Data_wide_example_time_single.csv", na.strings = "")
df_tidy_example <- read.csv("Data_tidy_example_time_multi.csv")

ui <- fluidPage(
  titlePanel("PlotTwist - Plotting Data from Time series"),
  sidebarLayout(
    sidebarPanel(width=3,
                 conditionalPanel(
                   condition = "input.tabs=='Plot'",
                   h4("Data"),
                   radioButtons("data_form", "Data as:", choices = list("Lines" = "dataasline", "Dots" = "dataasdot"), selected = "dataasline"),
                   checkboxInput("thicken", "The plot thickens", value = FALSE),
                   
                   sliderInput("alphaInput", "Visibility of the data", 0, 1, 0.3),
                   h4("Statistics"),
                   sliderInput("alphaInput_summ", "Visibility of the statistics", 0, 1, 1),              

                   checkboxInput("summaryInput", "Show the mean", value=FALSE),
                   #        sliderInput("Input_CI", "Confidence Level", 90, 100, 95),
                   checkboxInput(inputId = "add_CI", label = HTML("Show the 95% CI"), value = FALSE),

                   h4("Plot Layout"),
                   checkboxInput(inputId = "indicate_stim",
                                 label = "Indicate Baseline/Stimulus",
                                 value = FALSE),
                   
                   conditionalPanel(
                     condition = "input.indicate_stim == true",
                     textInput("stim_range", "Range of grey box (from,to,from,to,...)", value = "46,146")),
                   checkboxInput(inputId = "no_grid",
                                 label = "Remove gridlines",
                                 value = FALSE),

                   checkboxInput(inputId = "adjust_scale",
                                 label = "Adjust scale",
                                 value = FALSE),
                   conditionalPanel(
                     condition = "input.adjust_scale == true",
                     textInput("range_x", "Range x-axis (min,max)", value = "0,10")
                     
                   ),
                   conditionalPanel(
                     condition = "input.adjust_scale == true",
                     textInput("range_y", "Range y-axis (min,max)", value = "0,2")
                     
                   ),
                   
                   checkboxInput("color_data", "Use color for the data", value=FALSE),
                   checkboxInput("color_stats", "Use color for the stats", value=FALSE),
                   #                  selectInput("colour_list", "Colour:", choices = ""),
                   conditionalPanel(condition = "input.color_data == true || input.color_stats == true",
                                    radioButtons("adjustcolors", "Color palette:", choices = 
                                                   list("Standard" = 1,
                                                        "Colorblind safe (bright)" = 2,
                                                        "Colorblind safe (muted)" = 3,
                                                        "Colorblind safe (light)" = 4,
                                                        "User defined"=5),
                                                 selected =  1),
                                    conditionalPanel(condition = "input.adjustcolors == 5",
                                                     textInput("user_color_list", "List of names or hexadecimal codes", value = "turquoise2,#FF2222,lawngreen")), 
                                    
                                    h5("",
                                       a("Click here for more info on color names",
                                         href = "http://www.endmemo.com/program/R/color.php", target="_blank"))
                    ),
 
                   numericInput("plot_height", "Height (# pixels): ", value = 480),
                   numericInput("plot_width", "Width (# pixels):", value = 600),
 
                   h4("Labels"),
                   
                   checkboxInput(inputId = "add_title",
                                 label = "Add title",
                                 value = FALSE),
                   conditionalPanel(
                     condition = "input.add_title == true",
                     textInput("title", "Title:", value = "")
                   ),
                  
                   checkboxInput(inputId = "label_axes",
                                 label = "Change labels",
                                 value = FALSE),
                   conditionalPanel(
                     condition = "input.label_axes == true",
                     textInput("lab_x", "X-axis:", value = ""),
                     textInput("lab_y", "Y-axis:", value = "")
                     
                   ),
                   
                   checkboxInput(inputId = "adj_fnt_sz",
                                 label = "Change font size",
                                 value = FALSE),
                   conditionalPanel(
                     condition = "input.adj_fnt_sz == true",
                     numericInput("fnt_sz_ttl", "Size axis titles:", value = 24),
                     numericInput("fnt_sz_ax", "Size axis labels:", value = 18)
                     
                   ),
                   conditionalPanel(
                     condition = "input.color_data == true || input.color_stats == true",
                     checkboxInput(inputId = "add_legend",
                                   label = "Add legend",
                                   value = FALSE))
            ),

                 
                 conditionalPanel(
                   condition = "input.tabs=='Data upload'",
                   h4("Data upload (First column used for x-axis)"),
                   radioButtons(
                     "data_input", "",
                     choices = 
                       list("Example 1 (wide format)" = 1,
                            "Example 2 (tidy format)" = 2,
                            "Upload (multiple) file(s)" = 3,
                            "Paste data" = 4)
                     ,
                     selected =  1),

                   conditionalPanel(
                     condition = "input.data_input=='3'",
                     h5(""),
                     fileInput("upload", "Each file is a group:", multiple = TRUE),
                     selectInput("file_type", "Type of file:",
                                 list("text (csv)" = "text",
                                      "Excel" = "Excel"
                                 ),
                                 selected = "text")
                     ),
                   
                   conditionalPanel(
                     condition = "input.data_input=='4'",
                     h5("Paste data below:"),
                     tags$textarea(id = "data_paste",
                                   placeholder = "Add data here",
                                   rows = 10,
                                   cols = 20, ""),
#                     actionButton("submit_data_button", "Submit data"),
                     radioButtons(
                       "text_delim", "Delimiter",
                       choices = 
                         list("Tab (from Excel)" = "\t",
                              "Space" = " ",
                              "Comma" = ",",
                              "Semicolon" = ";"),
                       selected = "\t")),
                   # 
                   checkboxInput(inputId = "tidyInput",
                                  label = "These data are Tidy (implemented!)",
                                  value = FALSE),
                    conditionalPanel(condition = "input.tidyInput==true",
                      h5("",
                         a("Click here for more info on tidy data",
                           href = "http://thenode.biologists.com/converting-excellent-spreadsheets-tidy-data/education/")),
                      selectInput("x_var", "Select variable for x-axis", choices = ""),
                      selectInput("y_var", "Select variable for y-axis", choices = ""),
                      selectInput("g_var", "Identifier of single measurement", choices = ""),
                      selectInput("c_var", "Identifier of condition", choices = "")

                    )
                 ),
                 
                 conditionalPanel(
                   condition = "input.tabs=='About'",
                   h4("About")    
                 ),
                 
                 conditionalPanel(
                   condition = "input.tabs=='Data Summary'",
                   h4("Data summary")    
                 )
    ),
    mainPanel(
      
      tabsetPanel(id="tabs",
                  tabPanel("Data upload", h4("Data as provided"), dataTableOutput("data_uploaded")),
                  tabPanel("Plot", downloadButton("downloadPlotPDF", "Download pdf-file"), downloadButton("downloadPlotPNG", "Download png-file"), plotOutput("coolplot")
                  ), 
                  tabPanel("Data Summary", tableOutput('data_summary')),
                  tabPanel("About", includeHTML("about.html")
                  )
                  
      )
    )
  )         
)


server <- function(input, output, session) {

  
  
  #####################################
  ###### READ IN / GET DATA ###########
  #####################################
  
df_upload <- reactive({
    if (input$data_input == 1) {
      data <- df_wide_example
      data$id <- "1"

    }  else if (input$data_input == 2) {
#      
      updateSelectInput(session, "tidyInput", selected = TRUE)
      data <- df_tidy_example

    } else if (input$data_input == 3) {
      if (is.null(input$upload)) {
        return(data.frame(x = "Select your datafile", Time=1,Cell=1, id=1))
      } else {
        isolate({
          
          if (input$file_type == "text") {
          #Read the selected files (with read.csv)
          df_input_list <- lapply(input$upload$datapath, read.csv)
          } else if (input$file_type == "Excel"){
          df_input_list <- lapply(input$upload$datapath, read_excel)
          }
          
          #Take the filenames input$cvcs$name and remove anything after "." to get rid of extension   
          names(df_input_list) <- gsub(input$upload$name, pattern="\\..*", replacement="")
          #Merge all the dataframes and use the filenames (without extension) as id    
          df_input <- bind_rows(df_input_list, .id = "id")
          if(input$tidyInput == FALSE ) {
          #Force the first column from the csv file to be labeled as "Time"
          names(df_input)[2] <- "Time"
          }
          data <- df_input
          
        })
      }
    } else if (input$data_input == 4) {
      if (input$data_paste == "") {
        data <- data.frame(x = "Copy your data into the textbox,
                           select the appropriate delimiter, and
                           press 'Submit data'")
      } else {
        
          isolate({
            data <- read_delim(input$data_paste,
                               delim = input$text_delim,
                               col_names = TRUE)
            data$id <- "1"
          })
        
      }
    }

    return(data)
})


#############################################################
#### DISPLAY UPLOADED DATA (exactly as provided ##################

output$data_uploaded <- renderDataTable({
  
  #    observe({ print(input$tidyInput) })
  df_upload()
})
#############################################################


 #################################################
############## Convert data if needed ############
df_upload_tidy <- reactive({
  
  koos <- df_upload()

  if(input$tidyInput == FALSE ) {
    klaas <- gather(koos, Sample, Value, -Time, -id)

    klaas <- klaas %>% mutate (Time = as.numeric(Time), Value = as.numeric(Value))
  }
  
  else if(input$tidyInput == TRUE ) {

   klaas <- koos
   
#        c_choice <- input$c_var
#        g_choice <- input$g_var
#        klaas <- unite(klaas, unique_id, c(c_choice, g_choice), sep="_", remove = FALSE)
        
  }
  return(klaas)
})
 #################################################

####################################
##### Get the Variables ##############

observe({ 
  var_names  <- names(df_upload_tidy())
  var_list <- c("none", var_names)
  #        updateSelectInput(session, "colour_list", choices = var_list)
  updateSelectInput(session, "y_var", choices = var_list, selected="Value")
  updateSelectInput(session, "x_var", choices = var_list, selected="Time")
  updateSelectInput(session, "c_var", choices = var_list, selected="id")
  updateSelectInput(session, "g_var", choices = var_list, selected="Sample")

})
################################### 


###########################################################  
######## Extract the data for display & summary stats #######  

df_selected <- reactive({
  if(input$tidyInput == TRUE ) {
    df_temp <- df_upload_tidy()
    
    x_choice <- input$x_var
    y_choice <- input$y_var
    c_choice <- input$c_var
    g_choice <- input$g_var
    
    
    #Define how each experimental condition is identified
    if (c_choice == "none" || is.null(c_choice)) {
      c_choice <- "id"
    } 
    
    #Define how each individual measurement is identified
    if (g_choice == "none" || is.null(g_choice)) {
      g_choice <- "Sample"
    } 
    
    koos <- df_temp %>% select(Time = !!x_choice , Value = !!y_choice, id=!!c_choice, Sample=!!g_choice)
    koos <- unite(koos, unique_id, c(id, Sample), sep="_", remove = FALSE)
    
  } else if (input$tidyInput == FALSE ) {
#    koos <- df_upload_tidy() %>% filter(!is.na(Value))
    koos <- df_upload_tidy()
    
    koos <- unite(koos, unique_id, c(id, Sample), sep="_", remove = FALSE)
  }
  
  return(koos)
})
###########################################################  


##################################################
#### Caluclate Summary of the DATA for the MEAN ####

df_summary_mean <- reactive({
  koos <- df_selected() %>%
    group_by(Time, id) %>% 
    summarise(n = n(),
              mean = mean(Value, na.rm = TRUE),
              median = median(Value, na.rm = TRUE),
              sd = sd(Value, na.rm = TRUE)) %>%
    mutate(sem = sd / sqrt(n - 1),
           ci_lo = mean + qt((1-Confidence_level)/2, n - 1) * sem,
           ci_hi = mean - qt((1-Confidence_level)/2, n - 1) * sem)
  return(koos)
})

#################################################



 ###########################################
######### DEFINE DOWNLOAD BUTTONS ###########

##### Set width and height of the plot area
width <- reactive ({ input$plot_width })
height <- reactive ({ input$plot_height })

output$downloadPlotPDF <- downloadHandler(
  filename <- function() {
    paste("TimeseriesPlot_", Sys.time(), ".pdf", sep = "")
  },
  content <- function(file) {
    ggsave(file, width = input$plot_width/72,
           height = input$plot_height/72, dpi="retina")
  },
  contentType = "application/pdf" # MIME type of the image
)

output$downloadPlotPNG <- downloadHandler(
  filename <- function() {
    paste("TimeseriesPlot_", Sys.time(), ".png", sep = "")
  },
  content <- function(file) {
    ggsave(file, width = input$plot_width/72,
           height = input$plot_height/72)
  },
  contentType = "application/png" # MIME type of the image
)

 ###########################################  


 ###############################################
############## GENERATE PLOT LAYERS #############      
        
  output$coolplot <- renderPlot(width = width, height = height, {

    #Define how colors are used
    klaas <- df_selected()
    koos <- df_summary_mean()
    klaas <- klaas %>% mutate(id = as.factor(id), unique_id = as.character(unique_id))
    koos <- koos %>% mutate(id = as.factor(id))
    
    number_of_conditions <- nlevels(as.factor(klaas$id))
    if (number_of_conditions == 1) {
      kleur_data <- "unique_id"
    } else if (number_of_conditions > 1) {
        kleur_data <- "id"
      }
  
    if (input$color_data == FALSE) {
      kleur_data <- NULL
    }
 
    #Define the palette that is used, if different from default
    #     newColors <- Tol_bright
    #      newColors <- Tol_muted
    #      newColors <- Tol_vibrant
    newColors <- NULL
    
    if (input$adjustcolors == 2) {
      newColors <- Tol_bright
    } else if (input$adjustcolors == 3) {
      newColors <- Tol_muted
    } else if (input$adjustcolors == 4) {
      newColors <- Tol_light
    } else if (input$adjustcolors == 5) {
      newColors <- gsub("\\s","", strsplit(input$user_color_list,",")[[1]])
    }
            
    max_colors <- nlevels(as.factor(klaas$unique_id))
    if(length(newColors) < max_colors) {
      newColors<-rep(newColors,times=(round(max_colors/length(newColors)))+1)
    }
           
      ########## Define kleur
      #    observe({ print(class(input$colour_list)) })
      # if (input$color_data == FALSE) {
      #   kleur_data <- NULL
      # } else if (input$color_data == TRUE && input$color_choice==1) {
      #   kleur_data <- "id"
      # } else if (input$color_data == TRUE && input$color_choice==2) {
      #   kleur_data <- "unique_id"
      # }
    
    ########## Define how color is mapped onto the data
    #    observe({ print(class(input$colour_list)) })
    if (input$color_stats == FALSE) {
      kleur_stats <- NULL
    } else if (input$color_stats == TRUE) {
      kleur_stats <- "id"
    } 

    #   ########## Set default to Plotting "Time" and "Value"
    #   if (input$x_var == "none") {
    #     x_choice <- "Time"
    #   } else if (input$x_var != "none") {
    #     x_choice <- as.character(input$x_var)
    #   }
    # 
    #   if (input$y_var == "none") {
    #     y_choice <- "Value"
    #   } else if (input$y_var != "none") {
    #     y_choice <- as.character(input$y_var)
    #   }
    # 
    # #Define how each experimental condition is identified
    # if (input$c_var == "none") {
    #   c_choice <- "id"
    # } else if (input$x_var != "none") {
    #   c_choice <- as.character(input$c_var)
    # }
    # 
    # #Define how each individual measurement is identified
    # if (input$g_var == "none") {
    #   g_choice <- "unique_id"
    # } else if (input$x_var != "none") {
    #   g_choice <- as.character(input$g_var)
    # }

    
    #### Command to prepare the plot ####
#    p <- ggplot(df_upload_tidy(), aes(x=Time))
 
    p <- ggplot(data=klaas, aes_string(x="Time")) 
    
         
      #### plot individual measurements ####
    
    if (input$thicken =="TRUE") {
      multiplier <- 4
    } else if (input$thicken =="FALSE"){
      multiplier <- 1
    }
    
    if (input$data_form == "dataasline") {
      p <- p+ geom_line(data=klaas, aes_string(x="Time", y="Value", group="unique_id", color=kleur_data), size=0.5*multiplier, alpha=input$alphaInput)
    } else if (input$data_form == "dataasdot") {
      p <- p + geom_point(data=klaas, aes_string(x="Time", y="Value", group="unique_id", color=kleur_data), size=1*multiplier, alpha=input$alphaInput)
    } 

    if (input$summaryInput == TRUE  && input$add_CI == FALSE) {
      p <- p + geom_line(data=koos, aes_string(x="Time", y="mean", group="id", color=kleur_stats),size=2,alpha=input$alphaInput_summ)
    } else if (input$summaryInput == TRUE  && input$add_CI == TRUE) {
      p <- p + geom_ribbon(data=koos, aes_string(x="Time", ymin="ci_lo", ymax="ci_hi", group="id", fill=kleur_stats), alpha=input$alphaInput_summ/2)
      p <- p + geom_line(data=koos, aes_string(x="Time", y="mean", group="id", color=kleur_stats),size=2,alpha=input$alphaInput_summ)
    } else if (input$summaryInput == FALSE  && input$add_CI == TRUE) {
      p <- p + geom_ribbon(data=koos, aes_string(x="Time", ymin="ci_lo", ymax="ci_hi", group="id", fill=kleur_stats), alpha=input$alphaInput_summ/2)
    }
    
    ########### Do some formatting of the lay-out
    
    p <- p+ theme_light(base_size = 16)
    
    # if a stimulus is applied
    if (input$indicate_stim == TRUE) {
      rang <- as.numeric(strsplit(input$stim_range,",")[[1]])

      #If only one number is entered, a vertical line is added
      if (length(rang) ==1) {
        p <- p + geom_vline(xintercept=rang[1], black="orange", size=1)
      }
      
      #Repeat annotating the grey box for every 'pair' of numbers
      nsteps = floor(length(rang)/2)
      for (i in 0:nsteps) {
        
        p <- p + annotate("rect", xmin=rang[(i*2)+1], xmax=rang[(i*2)+2], ymin=-Inf, ymax=Inf, alpha=0.1, fill="black")
      
       }
    }
    
    # # if the range of values is specified
    if (input$adjust_scale == TRUE) {
      rng_x <- as.numeric(strsplit(input$range_x,",")[[1]])
      p <- p + xlim(rng_x[1],rng_x[2])
      
      rng_y <- as.numeric(strsplit(input$range_y,",")[[1]])
      p <- p + ylim(rng_y[1],rng_y[2])
    }
    

    # if title specified
    if (input$add_title)
      p <- p + ggtitle(input$title)

    # # if labels specified
    if (input$label_axes)
      p <- p + labs(x = input$lab_x, y = input$lab_y)

    # # if font size is adjusted
    if (input$adj_fnt_sz) {
      p <- p + theme(axis.text = element_text(size=input$fnt_sz_ax))
      p <- p + theme(axis.title = element_text(size=input$fnt_sz_ttl))
    }
    
    #remove legend (if selected)
    if (input$add_legend == FALSE) {  
      p <- p + theme(legend.position="none")
    }
    
    #remove gridlines (if selected)
    if (input$no_grid == TRUE) {  
      p <- p+ theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank())
    }

    if (input$adjustcolors >1) {
     p <- p+ scale_color_manual(values=newColors)
     p <- p+ scale_fill_manual(values=newColors)
    }
    

    p
    
  }) #close output$coolplot

 ###############################################
  

    
 ################################################
#### Render the data summary as a table ###########
  
  output$data_summary <- renderTable({
    
    df_out <- message <- data.frame(Note = "Showing selected data in tidy format")
    df_summary_mean()
  })
################################################
  
  } #close server

shinyApp(ui = ui, server = server)