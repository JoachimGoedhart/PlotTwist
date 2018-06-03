library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

#Confidence level
Confidence_Percentage = 95
Confidence_level = Confidence_Percentage/100

#Read a text file (comma separated values)
df_wide_example <- read.csv("FRET-ratio-wide.csv", na.strings = "")

ui <- fluidPage(
  titlePanel("TimesApp - Plotting Data from Time series"),
  sidebarLayout(
    sidebarPanel(width=3,
                 conditionalPanel(
                   condition = "input.tabs=='Plot'",
                   
                   sliderInput("alphaInput", "Visibility of the data", 0, 1, 0.3),
                   
                   # conditionalPanel(
                   #   condition = "input.adjust_jitter == true",
                   #   sliderInput("jitter_width", "Width:", 0,0.5,0.3),
                   #   checkboxInput(inputId = "random_jitter", label = ("Randomize Jitter"), value = TRUE)
                   # ),
                   
                   checkboxInput("summaryInput", "Show the mean", value=FALSE),
                   #        sliderInput("Input_CI", "Confidence Level", 90, 100, 95),
                   checkboxInput(inputId = "add_CI", label = HTML("Add 95% CI <br/> (unadvisable for n<10)"), value = FALSE),
                   sliderInput("alphaInput_summ", "Visibility of the statistics", 0, 1, 1),
                   
                   
                   
                   h4("Plot Layout"),
                   checkboxInput(inputId = "indicate_stim",
                                 label = "Indicate Stimulus",
                                 value = FALSE),
                   conditionalPanel(
                     condition = "input.indicate_stim == true",
                     textInput("stim_range", "stimulus applied (from,to)", value = "46,146")),
                   
                   numericInput("plot_height", "Height (# pixels): ", value = 480),
                   numericInput("plot_width", "Width (# pixels):", value = 600),
                   
                   checkboxInput(inputId = "adjust_scale",
                                 label = "Adjust scaling",
                                 value = FALSE),
                   conditionalPanel(
                     condition = "input.adjust_scale == true",
                     textInput("range", "Range of values (min,max)", value = "0,2")
                     
                   ),
                   conditionalPanel(
                     condition = "input.colour_list != 'none'",
                     checkboxInput(inputId = "add_legend",
                                   label = "Add legend",
                                   value = FALSE)),
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
                                 value = FALSE)
                 ),
                 
                 
                 conditionalPanel(
                   condition = "input.adj_fnt_sz == true",
                   numericInput("fnt_sz_ttl", "Size axis titles:", value = 24),
                   numericInput("fnt_sz_ax", "Size axis labels:", value = 18)
                   
                 ),
                 
                 conditionalPanel(
                   condition = "input.tabs=='Data upload'",
                   h4("Data upload"),
                   radioButtons(
                     "data_input", "",
                     choices = 
                       list("Example 1 (wide format)" = 1,
#                            "Example 2 (tidy format)" = 2,
                            "Upload file" = 3,
                            "Paste data" = 4)
                     ,
                     selected =  1),
                   conditionalPanel(
                     condition = "input.data_input=='1'"
                     
                   ),
                   conditionalPanel(
                     condition = "input.data_input=='3'",
                     h5("Upload file: "),
                     fileInput("upload", "", multiple = FALSE),
                     selectInput("file_type", "Type of file:",
                                 list("text (csv)" = "text",
                                      "Excel" = "Excel"
                                 ),
                                 selected = "text"),
                     conditionalPanel(
                       condition = "input.file_type=='text'",
                       
                       radioButtons(
                         "upload_delim", "Delimiter",
                         choices = 
                           list("Comma" = ",",
                                "Tab" = "\t",
                                "Semicolon" = ";",
                                "Space" = " "),
                         selected = ",")),
                     
                     actionButton("submit_datafile_button",
                                  "Submit datafile")),
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
                   checkboxInput(inputId = "tidyInput",
                                 label = "These data are Tidy",
                                 value = FALSE),
                   conditionalPanel(
                     condition = "input.tidyInput==true",
                     h5("",
                        a("Click here for more info on tidy data",
                          href = "http://thenode.biologists.com/converting-excellent-spreadsheets-tidy-data/education/")),
                     selectInput("x_var", "Conditions to compare:", choices = ""),
                     selectInput("y_var", "Variables:", choices = "")
                     
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


server <- function(input, output) {

  
  
  #####################################
  ###### READ IN / GET DATA ###########
  #####################################
  
df_upload <- reactive({
    if (input$data_input == 1) {
      data <- df_wide_example
    }  else if (input$data_input == 2) {
#      data <- df_tidy_example 
    } else if (input$data_input == 3) {
      file_in <- input$upload
      # Avoid error message while file is not uploaded yet
      if (is.null(input$upload)) {
        return(data.frame(x = "Select your datafile"))
      } else if (input$submit_datafile_button == 0) {
        return(data.frame(x = "Press 'submit datafile' button"))
      } else {
        isolate({
          if (input$file_type == "text") {
            data <- read_delim(file_in$datapath,
                               delim = input$upload_delim,
                               col_names = TRUE)
          } else if (input$file_type == "Excel") {
            data <- read_excel(file_in$datapath)
          } 
        })
      }
    } else if (input$data_input == 4) {
      if (input$data_paste == "") {
        data <- data.frame(x = "Copy your data into the textbox,
                           select the appropriate delimiter, and
                           press 'Submit data'")
      } else {
        if (input$submit_data_button == 0) {
          return(data.frame(x = "Press 'submit data' button"))
        } else {
          isolate({
            data <- read_delim(input$data_paste,
                               delim = input$text_delim,
                               col_names = TRUE)
          })
        }
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
df_tidy <- reactive({
  if(input$tidyInput == FALSE ) {
    klaas <- gather(df_upload(), Sample, Value, -Time)
  }
  
  else if(input$tidyInput == TRUE ) {
    klaas <- df_upload()

  }
})
 #################################################


##################################################
#### Caluclate Summary of the DATA for the MEAN ####

df_summary_mean <- reactive({
  koos <- df_tidy() %>%
    group_by(Time) %>% 
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
    paste("ComparisonPlot_", Sys.time(), ".pdf", sep = "")
  },
  content <- function(file) {
    ggsave(file, width = input$plot_width/72,
           height = input$plot_height/72, dpi="retina")
  },
  contentType = "application/pdf" # MIME type of the image
)

output$downloadPlotPNG <- downloadHandler(
  filename <- function() {
    paste("ComparisonPlot_", Sys.time(), ".png", sep = "")
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

    #### Command to prepare the plot ####
    p <- ggplot(df_tidy(), aes(x=Time))
      
      #### plot individual measurements ####
    p <- p+ geom_line(data=df_tidy(), aes(x=Time, y=Value, group=Sample), color="black", alpha=input$alphaInput)

    if (input$summaryInput == TRUE  && input$add_CI == FALSE) {
      p <- p + geom_line(data=df_summary_mean(), aes(x=Time, y=mean),size=2,alpha=input$alphaInput_summ)
    } else if (input$summaryInput == TRUE  && input$add_CI == TRUE) {
    p <- p + geom_ribbon(data=df_summary_mean(), aes(x=Time, ymin=ci_lo, ymax=ci_hi),fill="blue", alpha=0.2)
    p <- p + geom_line(data=df_summary_mean(), aes(x=Time, y=mean),size=2,alpha=input$alphaInput_summ)
    } else if (input$summaryInput == FALSE  && input$add_CI == TRUE) {
      p <- p + geom_ribbon(data=df_summary_mean(), aes(x=Time, ymin=ci_lo, ymax=ci_hi),fill="blue", alpha=0.2)
    }
    
    ########### Do some formatting of the lay-out
    
    p <- p+ theme_light(base_size = 16)
    
    # if a stimulus is applied
    if (input$indicate_stim == TRUE) {
      rang <- as.numeric(strsplit(input$stim_range,",")[[1]])
      p <- p + annotate("rect", xmin=rang[1], xmax=rang[2], ymin=-Inf, ymax=Inf, alpha=0.1, fill="black")
      
    }
    
    
    # # if the range of values is specified
    if (input$adjust_scale == TRUE) {
      rng <- as.numeric(strsplit(input$range,",")[[1]])
      p <- p + ylim(rng[1],rng[2])
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

    # #remove legend (if selected)
    # if (input$add_legend == FALSE) {
    #   p <- p + theme(legend.position="none")
    # }
    
    p
    
  }) #close output$coolplot

 ###############################################
  
  

  
  
    
 ################################################
#### Render the data summary as a table ###########
  
  output$data_summary <- renderTable({
    
    df_out <- message <- data.frame(Note = "Not implemented yet")
  })
################################################
  
  } #close server

shinyApp(ui = ui, server = server)