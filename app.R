# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# PlotTwist: Shiny app for plotting and comparing time-dependent data
# Created by Joachim Goedhart (@joachimgoedhart), first version 2018
# Takes non-tidy, spreadsheet type data as input assuming first column is "Time"
# Non-tidy data is converted into tidy format
# Raw data is displayed with user-defined visibility (alpha)
# The mean is displayed with user-defined visibility (alpha)
# Inferential statistics (95%CI) can be displayed as ribbon
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Copyright (C) 2019  Joachim Goedhart
# electronic mail address: j #dot# goedhart #at# uva #dot# nl
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ToDo
# Differentiate between factors and numbers for selecting display in case of tidy data
# Print variables on the axis from the tidy column names
# Optimize facetting of heatmap
# Improve annotation of small multiples (especially text)
# Implement clustering methods

#options(shiny.maxRequestSize=30*1024^2)

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(magrittr)
library(readxl)
library(DT)

##### Uncomment for interactive graph panel
#library(plotly)

#Confidence level
Confidence_Percentage = 95
Confidence_level = Confidence_Percentage/100

#Several qualitative color palettes that are colorblind friendly
#From Paul Tol: https://personal.sron.nl/~pault/
#Code to generate vectors in R to use these palettes

Tol_bright <- c('#EE6677', '#228833', '#4477AA', '#CCBB44', '#66CCEE', '#AA3377', '#BBBBBB')
Tol_muted <- c('#88CCEE', '#44AA99', '#117733', '#999933', '#DDCC77', '#CC6677', '#882255', '#AA4499', '#332288', '#DDDDDD')
Tol_light <- c('#BBCC33', '#AAAA00', '#77AADD', '#EE8866', '#EEDD88', '#FFAABB', '#99DDFF', '#44BB99', '#DDDDDD')


#Read a text file with demo data (comma separated values)
df_wide_example <- read.csv("Data_wide_example_time_single.csv", na.strings = "")
df_tidy_example <- read.csv("Data_tidy_example_time_multi.csv")

###### UI: User interface #########

ui <- fluidPage(
  titlePanel("PlotTwist - Plotting Data from Time series"),
  sidebarLayout(
    sidebarPanel(width=3,
                 conditionalPanel(
                   condition = "input.tabs=='Plot'",
                   h4("Data"),
                   radioButtons("data_form", "Data as:", choices = list("Lines" = "dataasline", "Dots" = "dataasdot"), selected = "dataasline"),
                   checkboxInput("thicken", "The plot thickens", value = FALSE),
                   checkboxInput("multiples", "Small multiples", value = FALSE),
                   
                   sliderInput("alphaInput", "Visibility of the data", 0, 1, 0.3),
                   h4("Statistics"),
                   sliderInput("alphaInput_summ", "Visibility of the statistics", 0, 1, 1),              

                   checkboxInput("summaryInput", "Show the mean", value=FALSE),
                   #        sliderInput("Input_CI", "Confidence Level", 90, 100, 95),
                   checkboxInput(inputId = "add_CI", label = HTML("Show the 95% CI"), value = FALSE),
                   

                   h4("Plot Layout"),
 
                  checkboxInput(inputId = "no_grid",
                                 label = "Remove gridlines",
                                 value = FALSE),
                   
                   checkboxInput(inputId = "change_scale",
                                 label = "Change scale",
                                 value = FALSE),                   
                   
                   
                  conditionalPanel(
                     condition = "input.change_scale == true",
                     textInput("range_x", "Range x-axis (min,max)", value = "")
                     
                   ),
                   conditionalPanel(
                     condition = "input.change_scale == true",
                     textInput("range_y", "Range y-axis (min,max)", value = "")
                     
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
                  NULL),
                 
 

                 
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
                     actionButton("submit_data_button", "Submit data"),
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
                   conditionalPanel(
                     condition = "input.tidyInput==false", selectInput("data_remove", "Select columns to remove", "", multiple = TRUE)),
                   
                    conditionalPanel(condition = "input.tidyInput==true",
                      h5("",
                         a("Click here for more info on tidy data",
                           href = "http://thenode.biologists.com/converting-excellent-spreadsheets-tidy-data/education/")),
                      selectInput("x_var", "Select variable for x-axis", choices = ""),
                      selectInput("y_var", "Select variable for y-axis", choices = ""),
                      selectInput("g_var", "Identifier of single measurement", choices = ""),
                      selectInput("c_var", "Identifier of condition", choices = "")

                    ),
                   h4("Normalization"),
                   checkboxInput(inputId = "normalization",
                                 label = "Data normalization",
                                 value = FALSE),
                   conditionalPanel(condition = "input.normalization==true",
                        radioButtons("norm_type",
                                     "Method:",
                                     choices = list("Divide by maximal value" = "max",
                                                    "Divide by minimal value" = "min", 
                                                    "Rescale between 0 and 1" = "zero_one", 
                                                    "Divide by area/integrated response (I/sum(I))" = "integral", 
                                                    "Difference from baseline (I-I0)" = "diff",
                                                    "Z-score ((I-I0)/SD(I0))" = "z-score",
                                                    "Difference divided by baseline ((I-I0)/I0)" = "perc",
                                                    "Fold change over baseline (I/I0)" = "fold"),

                                     selected = "fold"),
                        
                        conditionalPanel(
                          condition = "input.norm_type=='fold' || input.norm_type=='perc' || input.norm_type=='diff' || input.norm_type=='z-score' ",     
                        textInput("base_range", "Define baseline by rownumbers (start,end)", value = "1,5"))
                        ),
                   conditionalPanel(
                     condition = "input.normalization==true", (downloadButton("downloadData", "Download normalized tidy data (csv)"))),
                   
                   hr(),
                   
                   NULL
                 ),
                     
                    
  ####################### UI Panel for Heatmap ###############
                conditionalPanel(
                  condition = "input.tabs=='Heatmap'",
                  #                   h4("Heatmap"),
                  
                    
                    
                    h4("Plot Layout"),

                  radioButtons(inputId = "ordered",
                               label= "Order of the lines:",
                               choices = list("Alphabetical" = "none", "By maximum value" = "max_int", "By amplitude" = "amplitude", "By integrated response" = "int_int", "From hierarchical clustering" = "hc"),
                               selected = "none"),
                  
                  numericInput ("binning", "Binning of x-axis (1=no binning):", value=1, min = 1, max = 100, step = 1),
                  
                     checkboxInput(inputId = "show_labels_y",
                                   label = "Show y-axis labels",
                                   value = FALSE),
                    # checkboxInput(inputId = "indicate_stim2",
                    #               label = "Indicate Baseline/Stimulus",
                    #               value = FALSE),
                    # 
                    # conditionalPanel(
                    #   condition = "input.indicate_stim2 == true",
                    #   textInput("stim_range", "Range of grey box (from,to,from,to,...)", value = "46,146")),
                    # checkboxInput(inputId = "no_grid",
                    #               label = "Remove gridlines",
                    #               value = FALSE),
                    # 
  
                    numericInput("heatmap_height", "Height (# pixels): ", value = 480),
                    numericInput("heatmap_width", "Width (# pixels):", value = 600),
  
                  
                  
                    checkboxInput(inputId = "change_scale2",
                                  label = "Adjust scale", value=FALSE),
                    conditionalPanel(
                      condition = "input.change_scale2 == true",
                      actionButton('range_lineplot','Copy range from lineplot'),

                      textInput("range_x2", "Range temporal axis (min,max)", value = "")
                      
                    ),
                    conditionalPanel(
                      condition = "input.change_scale2 == true",
                      textInput("range_y2", "Range of the signal (min,max)", value = "")
                      
                    ),


                  NULL  ####### End of heatmap UI#######
  
              ),
                    
              conditionalPanel(condition = "input.tabs=='Plot' || input.tabs=='Heatmap'",
                   
                   h4("Labels"),
                   
                   
                   
                   ########
                   checkboxInput(inputId = "indicate_stim",
                                 label = "Indicate Baseline/Stimulus",
                                 value = FALSE),
                   
                   conditionalPanel(
                     condition = "input.indicate_stim == true",
                     
                     radioButtons(inputId = "stim_shape",
                                  label = NULL, choices = list("Bar on top" = "bar", "Box in plot" = "box", "Bar&Box"="both"), selected = "bar"),
                     
                     textInput("stim_range", "Range of grey box (from,to,from,to,...)", value = "46,146"),
                     
                     textInput("stim_text", "Text (condition1, condition2,...)", value = ""),
                     textInput("stim_colors", "Colors (condition1, condition2,...)", value = ""),                     
                     
                     
                     NULL),
                   
                   
                   ###############
                   
                   
                   
                   
                   
                   
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
                     numericInput("fnt_sz_title", "Plot title:", value = 24),
                     numericInput("fnt_sz_labs", "Axis titles:", value = 24),
                     numericInput("fnt_sz_ax", "Axis labels:", value = 18),
                     numericInput("fnt_sz_stim", "Condition labels:", value = 8)
                     
                   ),
                   conditionalPanel(
                     condition = "input.color_data == true || input.color_stats == true || input.tabs=='Heatmap'",
                     checkboxInput(inputId = "add_legend",
                                   label = "Add legend",
                                   value = FALSE))
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
                  tabPanel("Plot", downloadButton("downloadPlotPDF", "Download pdf-file"), downloadButton("downloadPlotPNG", "Download png-file"), 
                           
                           actionButton("settings_copy", icon = icon("clone"),
                                        label = "Clone current setting"),
                           
                           
                           div(`data-spy`="affix", `data-offset-top`="10", plotOutput("coolplot", height="100%")),
 #                              htmlOutput("LegendText", width="200px", inline =FALSE),

 #                          plotOutput("coolplot"),
                             NULL
                           
                           

                  ),
                  tabPanel("Heatmap", downloadButton("downloadHeatmapPDF", "Download pdf-file"), downloadButton("downloadHeatmapPNG", "Download png-file"),
#                           h4("UNDER DEVELOPMENT"), 
div(`data-spy`="affix", `data-offset-top`="10", plotOutput("plot_heatmap", height="100%")),
NULL
                           ),
##### Uncomment for interactive graph panel
#                  tabPanel("Plot-interactive", plotlyOutput("plot_interact")
#                  ), 
                  tabPanel("Data Summary", tableOutput('data_summary')),

                  tabPanel("About", includeHTML("about.html")
                  )
                  
      )
    )
  )         
)


server <- function(input, output, session) {

##################### Synchronize scales between tabs ##################  
  
observeEvent(input$change_scale, {  
  if (input$change_scale==TRUE)  {
    updateCheckboxInput(session, "change_scale2", value = TRUE)
  } else if (input$change_scale==FALSE)   {
    updateCheckboxInput(session, "change_scale2", value = FALSE)
  }
})
 
observeEvent(input$change_scale2, {  
  if (input$change_scale2==TRUE)  {
    updateCheckboxInput(session, "change_scale", value = TRUE)
  } else if (input$change_scale2==FALSE)   {
    updateCheckboxInput(session, "change_scale", value = FALSE)
  }
})
  

observeEvent(input$range_lineplot, {
  updateTextInput(session, "range_x2", value = input$range_x)
  updateTextInput(session, "range_y2", value = input$range_y)
  
})


###### DATA INPUT ###################
  
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
          observe({ print(names(df_input_list)) })             
          
          df_input <- bind_rows(df_input_list, .id = "ids")
          
          #If there is no id columns add it
          if(!"id" %in% colnames(df_input)) {
            df_input <- df_input %>% rename(id =ids)
          }
          #If the uploaded has an id column, use it
          else if("id" %in% colnames(df_input)) {
            df_input <- df_input %>% select(-one_of("ids"))            
          }

          #Force the first column from the csv file to be labeled as "Time" if this columns is absent          
          if(input$tidyInput == FALSE && !"Time" %in% colnames(df_input)) {
          names(df_input)[2] <- "Time"
          }
          data <- df_input
          
        })
      }
    } else if (input$data_input == 4) {
      if (input$data_paste == "" || input$submit_data_button == 0) {
        data <- data.frame(x = "Copy your data into the textbox,
                           select the appropriate delimiter, and
                           press 'Submit data'", Time="1", id="1")

        
        } else {
          isolate({
            data <- read_delim(input$data_paste,
                               delim = input$text_delim,
                               col_names = TRUE)
            
            #Check that data has at least 2 columns (1st is Time) and 2 rows (upper row is header)
            if (ncol(data)<2 || nrow(data)<1) {return(data.frame(x = "Number of columns and rows should be >2", Time="1", id="1"))}
            #The first column is defined as Time, id is added for compatibility            
            names(data)[1] <- "Time"
            data$id <- "1"
          })
        }
      }
#  }
  
  columns_to_remove <- names(data)
  #Remove column Time
  columns_to_remove <- columns_to_remove[columns_to_remove != "Time"]
  #Remove last column (id)
  columns_to_remove <- columns_to_remove[columns_to_remove != "id"]

  
#  columns_to_remove <- columns_to_remove[-1]
  
#  columns_to_remove <- columns_to_remove[-length(columns_to_remove)]
  
  #Show the columns that can be removed
  updateSelectInput(session, "data_remove", choices = columns_to_remove)
  
    return(data)
})

################## DISPLAY UPLOADED DATA (exactly as provided ##################

output$data_uploaded <- renderDataTable({
  
  #    observe({ print(input$tidyInput) })
  df_filtered()
})

################ REMOVE SELECTED COLUMNS #########
df_filtered <- reactive({     
  
  if (!is.null(input$data_remove)) {
    columns = input$data_remove
    df <- df_upload() %>% select(-one_of(columns))
  } else if (is.null(input$data_remove)) {
    df <- df_upload()}
  
})


##### CONVERT TO TIDY DATA ##########

#Need to tidy the data?!
#Untidy data will be converted to long format with two columns named 'Condition' and 'Value'
#The input for "Condition" will be taken from the header, i.e. first row
#Tidy data will be used as supplied

df_upload_tidy <- reactive({
  
  koos <- df_upload()

  if(input$tidyInput == FALSE ) {
    koos <- df_filtered()
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

##### Get Variables from the input ##############

observe({ 
  var_names  <- names(df_upload_tidy())
  var_list <- c("none", var_names)
  #        updateSelectInput(session, "colour_list", choices = var_list)
  updateSelectInput(session, "y_var", choices = var_list, selected="Value")
  updateSelectInput(session, "x_var", choices = var_list, selected="Time")
  updateSelectInput(session, "c_var", choices = var_list, selected="id")
  updateSelectInput(session, "g_var", choices = var_list, selected="Sample")

})

########### GET INPUT VARIABLEs FROM HTML ##############

observe({
  
  
  ############ ?data ################
  
  query <- parseQueryString(session$clientData$url_search)
  if (!is.null(query[['data']])) {
    presets_data <- query[['data']]
    presets_data <- unlist(strsplit(presets_data,";"))
    observe(print((presets_data)))
    
    updateRadioButtons(session, "data_input", selected = presets_data[1])    
    updateCheckboxInput(session, "tidyInput", value = presets_data[2])

    updateCheckboxInput(session, "normalization", value = presets_data[3])

    updateRadioButtons(session, "norm_type", selected = presets_data[4])    
    updateTextInput(session, "base_range", value= presets_data[5])
    
  }
  
  ############ ?vis ################
  
  if (!is.null(query[['vis']])) {
    
    presets_vis <- query[['vis']]
    presets_vis <- unlist(strsplit(presets_vis,";"))
    observe(print((presets_vis)))
    
    updateRadioButtons(session, "data_form", selected = presets_vis[1])
    updateSliderInput(session, "alphaInput", value = presets_vis[2])
    updateRadioButtons(session, "summaryInput", selected = presets_vis[3])
    updateCheckboxInput(session, "add_CI", value = presets_vis[4])
    updateSliderInput(session, "alphaInput_summ", value = presets_vis[5])
#    updateRadioButtons(session, "ordered", selected = presets_vis[6])
    #  updateTabsetPanel(session, "tabs", selected = "Plot")
  }
  
  ############ ?layout ################
  
  if (!is.null(query[['layout']])) {
    
    presets_layout <- query[['layout']]
    presets_layout <- unlist(strsplit(presets_layout,";"))
    observe(print((presets_layout)))
    
    updateCheckboxInput(session, "no_grid", value = (presets_layout[2]))
    
    updateCheckboxInput(session, "change_scale", value = presets_layout[3])

    updateTextInput(session, "range_y", value= presets_layout[5])
    updateCheckboxInput(session, "color_data", value = presets_layout[6])
    updateCheckboxInput(session, "color_stats", value = presets_layout[7])
    updateRadioButtons(session, "adjustcolors", selected = presets_layout[8])    
#    updateCheckboxInput(session, "add_description", value = presets_layout[9])
    if (length(presets_layout)>10) {
      updateNumericInput(session, "plot_height", value= presets_layout[10])
      updateNumericInput(session, "plot_width", value= presets_layout[11])
    }
    #  updateTabsetPanel(session, "tabs", selected = "Plot")
  }
  
  ############ ?color ################
  
  if (!is.null(query[['color']])) {
    
    presets_color <- query[['color']]
    presets_color <- unlist(strsplit(presets_color,";"))
    
    updateSelectInput(session, "colour_list", selected = presets_color[1])
    updateTextInput(session, "user_color_list", value= presets_color[2])
  }
  
  ############ ?label ################
  
  if (!is.null(query[['label']])) {
    
    presets_label <- query[['label']]
    presets_label <- unlist(strsplit(presets_label,";"))
    observe(print((presets_label)))
    
    
    updateCheckboxInput(session, "add_title", value = presets_label[1])
    updateTextInput(session, "title", value= presets_label[2])
    
    updateCheckboxInput(session, "label_axes", value = presets_label[3])
    updateTextInput(session, "lab_x", value= presets_label[4])
    updateTextInput(session, "lab_y", value= presets_label[5])
    
    updateCheckboxInput(session, "adj_fnt_sz", value = presets_label[6])
    updateNumericInput(session, "fnt_sz_labs", value= presets_label[7])
    updateNumericInput(session, "fnt_sz_ax", value= presets_label[8])
#    updateCheckboxInput(session, "add_description", value = presets_label[9])
  }
  
#   ############ ?url ################
#   
#   if (!is.null(query[['url']])) {
#     updateRadioButtons(session, "data_input", selected = 5)  
#     updateTextInput(session, "URL", value= query[['url']])
#     observe(print((query[['url']])))
#     updateTabsetPanel(session, "tabs", selected = "Plot")
#   }

  
  
  
  
   })
 

########### RENDER URL ##############

output$HTMLpreset <- renderText({
  url()
})

######### GENERATE URL with the settings #########

url <- reactive({
  
  base_URL <- paste(sep = "", session$clientData$url_protocol, "//",session$clientData$url_hostname, ":",session$clientData$url_port, session$clientData$url_pathname)
  
  data <- c(input$data_input, input$tidyInput, input$normalization, input$norm_type, input$base_range, "")
  
  vis <- c(input$data_form, input$alphaInput, input$summaryInput, input$add_CI, input$alphaInput_summ, "")
  layout <- c(" ", input$no_grid, input$change_scale, " ", input$range_y, input$color_data, input$color_stats,
              input$adjustcolors, "X", input$plot_height, input$plot_width)
  
  #Hide the standard list of colors if it is'nt used
  if (input$adjustcolors != "5") {
    color <- c(input$colour_list, "none")
  } else if (input$adjustcolors == "5") {
    color <- c(input$colour_list, input$user_color_list)
  }
  
  label <- c(input$add_title, input$title, input$label_axes, input$lab_x, input$lab_y, input$adj_fnt_sz, input$fnt_sz_labs, input$fnt_sz_ax, input$add_description)
  
  #replace FALSE by "" and convert to string with ; as seperator
  data <- sub("FALSE", "", data)
  data <- paste(data, collapse=";")
  data <- paste0("data=", data) 
  
  vis <- sub("FALSE", "", vis)
  vis <- paste(vis, collapse=";")
  vis <- paste0("vis=", vis) 
  
  layout <- sub("FALSE", "", layout)
  layout <- paste(layout, collapse=";")
  layout <- paste0("layout=", layout) 
  
  color <- sub("FALSE", "", color)
  color <- paste(color, collapse=";")
  color <- paste0("color=", color) 
  
  label <- sub("FALSE", "", label)
  label <- paste(label, collapse=";")
  label <- paste0("label=", label) 
  
  if (input$data_input == "5") {url <- paste("url=",input$URL,sep="")} else {url <- NULL}
  
  parameters <- paste(data, vis,layout,color,label,url, sep="&")
  
  preset_URL <- paste(base_URL, parameters, sep="?")
  
  observe(print(parameters))
  observe(print(preset_URL))  
  return(preset_URL)
})


############# Pop-up that displays the URL to 'clone' the current settings ################

observeEvent(input$settings_copy , {
  showModal(urlModal(url=url(), title = "Use the URL to launch PlotsOfData with the current setting"))
})

# observeEvent(input$legend_copy , {
#   showModal(urlModal(url=Fig_legend(), title = "Legend text"))
# })


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
  observe({ print(head(koos)) })
  return(koos)
})

######## Binning x-axis #########

df_binned <- reactive ({

  max_bin <- df_normalized() %>% group_by(unique_id) %>% summarise(n=n()) %>% summarise(max(n)) %>% unlist() %>% as.numeric()
  updateSliderInput(session, "binning", min=1, max=max_bin)

  bin_factor <- as.numeric(input$binning)
  
  if (bin_factor != 1) {

    #add a coumn with number to assign the bins
    df <- df_normalized() %>% group_by(unique_id) %>% mutate(bin_id = trunc((row_number(Time)-1)/bin_factor))
    
    #use the column with bins to generate a summary, effectively combining the data
    df_binned <- df %>% group_by(unique_id, bin_id,id) %>% summarise(Value=mean(Value), Time=mean(Time)) %>% ungroup()

# Remove the last bin if it is not completely filled (needed for correct visualization of the heatmap)
    number_of_x_values <- trunc(max_bin/bin_factor)
    df_binned <- df_binned %>% group_by(unique_id,id) %>% slice(1:number_of_x_values) %>% ungroup()

  } else {df_binned <- df_normalized()}
  
  observe({print(head(df_binned))})
  
  return(df_binned)
})


######## Several options ofr normalization of the data #########

df_normalized <- reactive ({
  
#  observe({ print(baseline) })
  if (input$normalization==TRUE) {

    #Get the values that determine the baseline
    baseline <- input$base_range
    baseline_range <- as.numeric(strsplit(input$base_range,",")[[1]])
    base_start <- baseline_range[1]

    #In case only one value is given for normalization   
    if (length(baseline_range) ==1) {
      base_end <- base_start
    
    #In case a range is given for normalization
    } else {
      base_end <- baseline_range[2]
    }

    #Now get started with the actual normalization    
    
#    observe({ print(base_end) })
    
    #Divided by baseline (baseline set to 1)
    if (input$norm_type == "fold"){
      koos <- df_selected() %>%
        group_by(unique_id) %>% 
        mutate(Value=Value/mean(Value[base_start:base_end])) %>% ungroup()
 
    #Divide by baseline and subtract 1 (baseline set to 0)  
    } else if (input$norm_type == "perc"){
      koos <- df_selected() %>%
        group_by(unique_id) %>% 
        mutate(Value= (Value/mean(Value[base_start:base_end]))-1) %>% ungroup()

    #Subtract baseline (baseline set to 0)      
    } else if (input$norm_type == "diff"){
      koos <- df_selected() %>%
        group_by(unique_id) %>% 
        mutate(Value=Value-mean(Value[base_start:base_end])) %>% ungroup()

    #divide by maximal value
     } else if (input$norm_type == "max"){
      koos <- df_selected() %>%
        group_by(unique_id) %>% 
        mutate(Value=Value/max(Value)) %>% ungroup()
      
    #divide by minimum value
     } else if (input$norm_type == "min"){
       koos <- df_selected() %>%
         group_by(unique_id) %>% 
         mutate(Value=Value/min(Value)) %>% ungroup()

    #Scale between zero and one      
     } else if (input$norm_type == "zero_one"){
       koos <- df_selected() %>%
         group_by(unique_id) %>% 
         mutate(Value=(Value-min(Value))/(max(Value)-min(Value))) %>% ungroup()
       
    #Z-score (relative to baseline)
     } else if (input$norm_type == "z-score"){
       koos <- df_selected() %>%
         group_by(unique_id) %>% 
         mutate(Value=(Value-mean(Value[base_start:base_end]))/sd(Value[base_start:base_end]) ) %>% ungroup()
       
    #divide by integrated response
     } else if (input$norm_type == "integral"){
       koos <- df_selected() %>%
         group_by(unique_id) %>% 
         mutate(Value=Value/sum(Value)) %>% ungroup()
       
     }

  #No Normalization# 
  } else {
    koos <- df_selected()}
  return(koos)

  
})

######## Determine and set the order of Conditions #######  

ordered_list <- reactive({
  
  klaas <-  df_binned()
  
  if(input$ordered == "max_int") {
    reordered_list <- reorder(klaas$unique_id, klaas$Value, max, na.rm = TRUE)
    
  } else if (input$ordered == "none") {
    reordered_list <- factor(klaas$unique_id, levels=unique(klaas$unique_id))
    
  } else if (input$ordered == "int_int") {
    reordered_list <- reorder(klaas$unique_id, klaas$Value, sum, na.rm = TRUE)
    
  }  else if (input$ordered == "amplitude") {

    #Determine a ranking based on amplitude = max-min
    df_rank <- klaas %>% group_by(unique_id) %>% summarise(amplitude=max(Value)-min(Value)) %>% mutate(rank=percent_rank(amplitude))
    reordered_list <- reorder(df_rank$unique_id, df_rank$rank)
    
  } else if (input$ordered == "hc") {
    #Convert to wide format
    df_wide <- klaas %>% select(unique_id, Value,Time)  %>% spread(key=unique_id, value=Value)
    #Remove Time info
    df_wide <- df_wide %>% select(-Time)
    #hierarchical clustering
    hc <- hclust(dist(t(df_wide)))
    #Column order from clustering
    col.order <- hc$order
    #Reorder the dataframe dat according to the column order determined by clustering
    df_clustered <- df_wide[, col.order]
    
    #Get the ordered column names from the clustered dataframe
    reordered_list <- colnames(df_clustered)
    observe({ print(reordered_list) })
  }
  

  ordered_list <- levels(reordered_list)
  observe({ print(ordered_list) })
  
  return(ordered_list)
  
})


#### Caluclate Summary of the DATA for the MEAN ####

df_summary_mean <- reactive({
  koos <- df_binned() %>%
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

######### DEFINE DOWNLOAD BUTTONS FOR ORDINARY PLOT ###########

output$downloadPlotPDF <- downloadHandler(
  filename <- function() {
    paste("PlotTwist_", Sys.time(), ".pdf", sep = "")
  },
  content <- function(file) {
    pdf(file, width = input$myWidth/72, height = input$myHeight/72)
    plot(plot_data())
    dev.off()
  },
  contentType = "application/pdf" # MIME type of the image
)

output$downloadPlotPNG <- downloadHandler(
  filename <- function() {
    paste("PlotTwist", Sys.time(), ".png", sep = "")
  },
  content <- function(file) {
    png(file, width = input$plot_width*4, height = input$plot_height*4, res=300)
    plot(plot_data())
    dev.off()
  },
  contentType = "application/png" # MIME type of the image
)

######### DEFINE DOWNLOAD BUTTONS FOR HEATMAP ###########

output$downloadHeatmapPDF <- downloadHandler(
  filename <- function() {
    paste("PlotTwist_", Sys.time(), ".pdf", sep = "")
  },
  content <- function(file) {
    pdf(file, width = input$heatmap_width/72, height = input$heatmap_height/72)
    plot(plot_map())
    dev.off()
  },
  contentType = "application/pdf" # MIME type of the image
)

output$downloadHeatmapPNG <- downloadHandler(
  filename <- function() {
    paste("PlotTwist", Sys.time(), ".png", sep = "")
  },
  content <- function(file) {
    png(file, width = input$heatmap_width*4, height = input$heatmap_height*4, res=300)
    plot(plot_map())
    dev.off()
  },
  contentType = "application/png" # MIME type of the image
)

############## GENERATE PLOT LAYERS FOR ORDINARY PLOT #############      
        
plot_data <- reactive({

    #Define how colors are used
    klaas <- df_binned()
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
 
    if (input$fnt_sz_stim == "") {
      fnt_sz_stim <- 6
    } else {
      fnt_sz_stim <- input$fnt_sz_stim
    }
    
    
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
           
    ########## Define how color is mapped onto the data
    #    observe({ print(class(input$colour_list)) })
    if (input$color_stats == FALSE) {
      kleur_stats <- NULL
    } else if (input$color_stats == TRUE) {
      kleur_stats <- "id"
    } 
    
    #### Command to prepare the plot ####
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

    #### plot stats ####
    
    if (input$summaryInput == TRUE  && input$add_CI == FALSE) {
      p <- p + geom_line(data=koos, aes_string(x="Time", y="mean", group="id", color=kleur_stats),size=2,alpha=input$alphaInput_summ)
    } else if (input$summaryInput == TRUE  && input$add_CI == TRUE) {
      p <- p + geom_ribbon(data=koos, aes_string(x="Time", ymin="ci_lo", ymax="ci_hi", group="id", fill=kleur_stats), alpha=input$alphaInput_summ/2)
      p <- p + geom_line(data=koos, aes_string(x="Time", y="mean", group="id", color=kleur_stats),size=2,alpha=input$alphaInput_summ)
    } else if (input$summaryInput == FALSE  && input$add_CI == TRUE) {
      p <- p + geom_ribbon(data=koos, aes_string(x="Time", ymin="ci_lo", ymax="ci_hi", group="id", fill=kleur_stats), alpha=input$alphaInput_summ/2)
    }
    
    
    # This needs to go here (before annotations)
    p <- p+ theme_light(base_size = 16)
    
    ############## Adjust scale if necessary ##########

    
    #Adjust scale if range for x (min,max) is specified
    if (input$range_x != "" &&  input$change_scale == TRUE) {
      rng_x <- as.numeric(strsplit(input$range_x,",")[[1]])
      observe({ print(rng_x) })
      #If min>max invert the axis
        if (rng_x[1]>rng_x[2]) {p <- p+ scale_x_reverse()}

      #Autoscale if rangeis NOT specified
    } else if (input$range_x == "" ||  input$change_scale == FALSE) {
      rng_x <- c(NULL,NULL)
 #     observe({ print(rng_x) })
    }


    #Adjust scale if range for y (min,max) is specified
    if (input$range_y != "" &&  input$change_scale == TRUE) {
      rng_y <- as.numeric(strsplit(input$range_y,",")[[1]])

      #If min>max invert the axis
      if (rng_y[1]>rng_y[2]) {p <- p+ scale_y_reverse()}
      
      upper_y <- rng_y[2]
      lower_y <- rng_y[1]
      

      #Autoscale if rangeis NOT specified
    } else if (input$range_y == "" ||  input$change_scale == FALSE) {

      
      #Read out the current range, this is necessary for annotation of stimulus
      rng_y <- c(NULL,NULL)
      upper_y <- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range[2]
      lower_y <- ggplot_build(p)$layout$panel_scales_y[[1]]$range$range[1]
      
    }
    range_y <- upper_y-lower_y

    #################### Add labels for perturbations #####################
    
    rang <- as.numeric(strsplit(input$stim_range,",")[[1]])
    
    stimText <- c("","","","","")
    
    if (input$indicate_stim == TRUE && input$stim_text !="") {
      stimText <- gsub("\\s","", strsplit(input$stim_text,",")[[1]])
    }

    
    if (input$indicate_stim == TRUE && input$stim_colors !="") {
      stimColors <- gsub("\\s","", strsplit(input$stim_colors,",")[[1]])
      
    } else if (input$indicate_stim == TRUE && input$stim_colors =="") {
      stimColors <- "black"
    }
      
  
    # if a stimulus is applied
    if (input$indicate_stim == TRUE) {
      
      p <- p  +  theme(plot.margin = unit(c(3,1,1,1), "lines"))
      p <- p + coord_cartesian(xlim=c(rng_x[1],rng_x[2]),ylim=c(lower_y,upper_y),clip = 'off')
      

      
      #If only one number is entered, a vertical line is added
      if (length(rang) ==1) {
        p <- p + geom_vline(xintercept=rang[1], black="orange", size=1)
      }
      

      nsteps = floor(length(rang)/2)
      #Repeat the colors if needed
      if(length(stimColors) < nsteps) {
        stimColors<-rep(stimColors,times=(round(nsteps/length(stimColors)))+1)
      }
      
      
      if(input$stim_shape == "bar") {
          for (i in 0:nsteps) {
             p <- p + annotate("rect", xmin=rang[(i*2)+1], xmax=rang[(i*2)+2], ymin=upper_y+.02*range_y, ymax=upper_y+.05*range_y, alpha=0.8, fill=stimColors[i+1])
             p <- p + annotate("text", x=rang[(i*2)+1]+0.5*(rang[(i*2)+2]-rang[(i*2)+1]), y=upper_y+.1*range_y, alpha=1, color=stimColors[i+1], size=fnt_sz_stim,label=paste(stimText[i+1]))
          }
      } else if (input$stim_shape == "box") {
        
          for (i in 0:nsteps) {
              p <- p + annotate("rect", xmin=rang[(i*2)+1], xmax=rang[(i*2)+2], ymin=-Inf, ymax=Inf, alpha=0.1, fill=stimColors[i+1])
              p <- p + annotate("text", x=rang[(i*2)+1]+0.5*(rang[(i*2)+2]-rang[(i*2)+1]), y=upper_y+.1*range_y, alpha=1, color=stimColors[i+1], size=fnt_sz_stim,label=paste(stimText[i+1]))
          }
          
      } else if (input$stim_shape == "both") {
          for (i in 0:nsteps) {
            p <- p + annotate("rect", xmin=rang[(i*2)+1], xmax=rang[(i*2)+2], ymin=-Inf, ymax=Inf, alpha=0.1, fill=stimColors[i+1])
            p <- p + annotate("rect", xmin=rang[(i*2)+1], xmax=rang[(i*2)+2], ymin=upper_y+.02*range_y, ymax=upper_y+.05*range_y, alpha=0.8, fill=stimColors[i+1])
            p <- p + annotate("text", x=rang[(i*2)+1]+0.5*(rang[(i*2)+2]-rang[(i*2)+1]), y=upper_y+.1*range_y, alpha=1, color=stimColors[i+1], size=fnt_sz_stim,label=paste(stimText[i+1]))
          }
        
      }
    }
    

    
########## Do some formatting of the lay-out ##########
    


    # if title specified
    if (input$add_title == TRUE) {
      #Add line break to generate some space
      title <- paste(input$title, "\n",sep="")
      p <- p + labs(title = title)
    }

    # # if labels specified
    if (input$label_axes)
      p <- p + labs(x = input$lab_x, y = input$lab_y)

    # # if font size is adjusted
    if (input$adj_fnt_sz) {
      p <- p + theme(axis.text = element_text(size=input$fnt_sz_ax))
      p <- p + theme(axis.title = element_text(size=input$fnt_sz_labs))
      p <- p + theme(plot.title = element_text(size=input$fnt_sz_title))
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
    
    if (input$multiples == TRUE) {
      if (number_of_conditions == 1) {
                  p <- p+ facet_wrap(~unique_id)
                  #Remove the strip above the individual panels
                  p <- p + theme(strip.background = element_blank(), strip.text = element_blank(), panel.spacing.y = unit(.5, "lines"),panel.spacing.x = unit(.5, "lines"))
      } else if (number_of_conditions > 1) {
                    p <- p+ facet_grid(id~.)
      }
                    
    }

    #Remove upper and right axis

        p <- p + theme(panel.border = element_blank())
        p <- p + theme(axis.line.x  = element_line(colour = "black"), axis.line.y  = element_line(colour = "black"))

    return(p)
    
  }) #close output$coolplot

# Uncomment for interactive graph panel
# output$plot_interact <- renderPlotly({
#   ggplotly(plot_data(), height=as.numeric(input$plot_height), width=as.numeric(input$plot_width))
# })


############# GENERATE PLOT LAYERS FOR HEATMAP #############  

plot_map <- reactive({
  
  ####### Read the order from the ordered list #############  
  custom_order <- ordered_list()
  #  observe({ print(custom_order) })  
  
  klaas <- df_binned()
  koos <- df_summary_mean()
  
  number_of_conditions <- nlevels(as.factor(klaas$id))
  
  #Get the maximum number of traces
  max_n <- max(koos$n)
  #observe({print(n)})
  
  klaas <- klaas %>% mutate(id = as.factor(id), unique_id = as.character(unique_id))
  koos <- koos %>% mutate(id = as.factor(id))
  
  # Set default font size
  if (input$fnt_sz_stim == "") {
    fnt_sz_stim <- 6
  } else {
    fnt_sz_stim <- input$fnt_sz_stim
  }
  
  #### Command to prepare the plot ####
  p <- ggplot(data=klaas, aes_string(x="Time"))
  
  #geom_raster is faster than geom_tile
  p <- p + geom_raster(data=klaas, aes_string(x="Time", y="unique_id", fill="Value"))+ scale_fill_viridis_c()

  # Setting the order of the x-axis
  p <- p + scale_y_discrete(limits=custom_order)
  
  if (input$change_scale == TRUE) {
    rng_x <- as.numeric(strsplit(input$range_x2,",")[[1]])
    p <- p + xlim(rng_x[1],rng_x[2])  
    
    rng_y <- as.numeric(strsplit(input$range_y2,",")[[1]])
    p <- p+ scale_fill_viridis_c(limits=c(rng_y[1],rng_y[2]))
    
  } 
  
  # This needs to go here (before annotations)
  p <- p+ theme_light(base_size = 16)
  
  #################### Add labels for perturbations #####################
  
  rang <- as.numeric(strsplit(input$stim_range,",")[[1]])
  
  stimText <- c("","","","","")
  
  if (input$indicate_stim == TRUE && input$stim_text !="") {
    stimText <- gsub("\\s","", strsplit(input$stim_text,",")[[1]])
  }
  
  if (input$indicate_stim == TRUE && input$stim_colors !="") {
    stimColors <- gsub("\\s","", strsplit(input$stim_colors,",")[[1]])
    
  } else if (input$indicate_stim == TRUE && input$stim_colors =="") {
    stimColors <- "black"
  }
  nsteps = floor(length(rang)/2)
  
#  observe({print(nsteps)})
  
  if (input$indicate_stim ==TRUE) {

    p <- p  +  theme(plot.margin = unit(c(3,1,1,1), "lines"))
    p <- p + coord_cartesian(clip = 'off')
  
    #Repeat the colors if needed
    if(length(stimColors) < nsteps) {
      stimColors<-rep(stimColors,times=(round(nsteps/length(stimColors)))+1)
    }  
    
    if(input$stim_shape == "bar") {
      for (i in 0:nsteps) {
        p <- p + annotate("rect", xmin=rang[(i*2)+1]-0.5, xmax=rang[(i*2)+2]+0.5, ymin=max_n+0.8, ymax=max_n+1.3, fill=stimColors[i+1])
        p <- p + annotate("text", x=rang[(i*2)+1]+0.5*(rang[(i*2)+2]-rang[(i*2)+1]), y=max_n+2, alpha=1, alpha=1, color=stimColors[i+1], size=fnt_sz_stim,label=paste(stimText[i+1]))
      }
    } else if (input$stim_shape == "box") {
      
      for (i in 0:nsteps) {
        p <- p + annotate("rect", xmin=rang[(i*2)+1], xmax=rang[(i*2)+2], ymin=0.5, ymax=max_n+0.5, fill=NA, color=stimColors[i+1],size=1)
        p <- p + annotate("text", x=rang[(i*2)+1]+0.5*(rang[(i*2)+2]-rang[(i*2)+1]), y=max_n+1.2, alpha=1, alpha=1, color=stimColors[i+1], size=fnt_sz_stim,label=paste(stimText[i+1]))
      }
      
    } else if (input$stim_shape == "both") {
      for (i in 0:nsteps) {
        p <- p + annotate("rect", xmin=rang[(i*2)+1]-0.5, xmax=rang[(i*2)+2]+0.5, ymin=max_n+0.8, ymax=max_n+1.3, fill=stimColors[i+1])
        p <- p + annotate("rect", xmin=rang[(i*2)+1], xmax=rang[(i*2)+2], ymin=0.5, ymax=max_n+0.5, fill=NA, color=stimColors[i+1],size=1)
        p <- p + annotate("text", x=rang[(i*2)+1]+0.5*(rang[(i*2)+2]-rang[(i*2)+1]), y=max_n+2, alpha=1, alpha=1, color=stimColors[i+1], size=fnt_sz_stim,label=paste(stimText[i+1]))
      }
      
    }
}
  
  ########## Do some formatting of the lay-out ##########
  
  #remove legend (if selected)
  if (input$add_legend == FALSE) {  
    p <- p + theme(legend.position="none")
  }
  
  #remove gridlines
  p <- p+ theme(panel.border = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.ticks = element_line(colour = "grey20"), 
                #                 axis.line = element_line(size = 0.1, linetype = "solid", colour = "black"),
                NULL)
  
  #remove labels and ticks on y-axis
  if (input$show_labels_y == FALSE)
  p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  
  # if title specified
  if (input$add_title == TRUE) {
    #Add line break to generate some space
    title <- paste(input$title, "\n",sep="")
    p <- p + labs(title = title)
  }
  
  # # if labels specified
  if (input$label_axes)
    p <- p + labs(x = input$lab_x, y = input$lab_y)
  
  # # if font size is adjusted
  if (input$adj_fnt_sz) {
    p <- p + theme(axis.text = element_text(size=input$fnt_sz_ax))
    p <- p + theme(axis.title = element_text(size=input$fnt_sz_labs))
    p <- p + theme(plot.title = element_text(size=input$fnt_sz_title))
  }
  
  
  # Facetting for heatmap - requires optimization
  # if (input$multiples == TRUE && number_of_conditions > 1) {
  #     p <- p+ facet_grid(id~.)
  # }
    

  
  
  return(p)
  
})



############# Set width and height of the plot area ###############
width <- reactive ({ input$plot_width })
height <- reactive ({ input$plot_height })

output$coolplot <- renderPlot(width = width, height = height, {     
  plot(plot_data())
}) #close output$coolplot

############# Set width and height of the heatmap area ################
heatmap_width <- reactive ({ input$heatmap_width })
heatmap_height <- reactive ({ input$heatmap_height })

output$plot_heatmap <- renderPlot(width = heatmap_width, height = heatmap_height, {     
  plot(plot_map())
}) #close output$heatmap


############## Render the data summary as a table ###########
  
  output$data_summary <- renderTable({
    
    df_out <- message <- data.frame(Note = "Showing selected data in tidy format")
    df_summary_mean()
  })


############## Export the data in tidy format ###########

output$downloadData <- downloadHandler(
  
  filename = function() {
    paste("PlotTwist_tidy_normalized", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(df_normalized(), file, row.names = FALSE)
  }
)


######## The End; close server ########################  
    # End R-session when browser closed
    session$onSessionEnded(stopApp)

  } #close server

shinyApp(ui = ui, server = server)