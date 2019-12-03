# PlotTwist
A Shiny App for plotting continuous data

### About PlotTwist
  
PlotTwist (Plotting data from Time-lapse experiments With Indicators of conditions at Set Times) is an app for visualizing the data and statistics of continuous data, with a focus on time-dependent measurements. The philosophy of the approach is that plotting the raw data (instead of a summary) improves transparency and interpretation. To facilitate the visualization of individual measurements, the data can be shown as 'small mutliples' or in 'heatmap-style'. To further facilitate the comparison, the mean and 95% CI can be added. The user has full control over the visibility of the raw data and statistics by adjustment of the transparency (alpha). Several color blind friendly palettes are available to label the data. 
The comparison of multiple conditions is enabled by multiple file upload (csv or xls format), where each file represents a condition.

The app accepts both wide (spreadsheet-style) data and tidy data. Deselection of user-defined columns or conditions is possible. Several methods for normalizing the data are implemented. There are several options for annotating the plot, e.g. to indicate the time and nature of a perturbation. This will overlay a transparant grey box, add a bar on top of the graph or both.

An option for clustering of the data is available on a separate tab. Several basic clustering algorithms are implemented. The result of the clustering is displayed as  plots for each of the clusters. The data can be downloaded in tidy CSV format. To evaluate the optimal number of clusters, a number of Cluster Validation Indices are available. 

For more information on the underlying code and #dataViz styles see these blogs:

[Visualizing data with R/ggplot2 - It's about time](http://thenode.biologists.com/visualizing-data-with-r-ggplot2/education/)

[Visualizing data with R/ggplot2 - One more time](http://thenode.biologists.com/visualizing-data-one-more-time/education/)

[Normalization methods](http://thenode.biologists.com/data-normalization/research/)

[Visualizing heterogeneity](http://thenode.biologists.com/visualizing-heterogeneity-of-imaging-data/research/)


### Running the App

The web-tool runs from a shiny server, and can be accessed at: [https://huygens.science.uva.nl/PlotTwist/](https://huygens.science.uva.nl/PlotTwist/)

Alternatively, the app can run from R/Rstudio.

#### Preparations
Note that the app depends on several R packages that need to be installed (shiny, ggplot2, dplyr, tidyr, readr, readxl, magrittr, ggrepel, DT, dtw, NbClust, gridExtra, shinycssloaders, RCurl). 
Run this command in R/Rstudio to download and install all the packages (only needs to be done once):
```
install.packages("shiny", "ggplot2", "dplyr", "tidyr", "readr", "readxl", "magrittr", "ggrepel", "DT", "dtw", "NbClust", "gridExtra", "shinycssloaders", "RCurl")
```
o The first option is running it directly from Github. In the command line (in R or Rstudio) type:
```
shiny::runGitHub('PlotTwist', 'JoachimGoedhart')
```
o The second option is download the app and to use it offline:

-download the `app.R` and csv files (`Data_wide_example_time_single.csv` and `Data_tidy_example_time_multi.csv`) with example data.

-Run RStudio and load `app.R`

-Select 'Run All' (shortcut is command-option-R on a Mac) or click on "Run App" (upper right button on the window)

This should launch a web browser with the Shiny app.


### Credits

The code for the shiny app is partially derived from [ggplotGUI](https://github.com/gertstulp/ggplotgui) by [Gert Stulp](https://www.gertstulp.com)  
The colorblind safe palettes were developed by [Paul Tol](https://personal.sron.nl/~pault/) and [Okabe and Ito](http://jfly.uni-koeln.de/color/).

PlotTwist is created and maintained by Joachim Goedhart ([@joachimgoedhart](https://twitter.com/joachimgoedhart))and Marten Postma

### Example output

Data from a single file with stimulation indicated by a grey box:

![alt text](https://github.com/JoachimGoedhart/PlotTwist/blob/master/Timeseries_example1.png "Output")

Data in a heatmap presentation with annotation of the different perturbations:

![alt text](https://github.com/JoachimGoedhart/PlotTwist/blob/master/Timeseries_heatmap.png "Output")

Data from multiple files, means (and 95% CI) of each condition have different colors:

![alt text](https://github.com/JoachimGoedhart/PlotTwist/blob/master/Timeseries_example2.png "Output")

The data for the x-axis does not need to be time. Also other continuous data can be visualized. This is an example with spectral data (shaded region indicates a region of 460-500 nm, a possible CFP emission filter):

![alt text](https://github.com/JoachimGoedhart/PlotTwist/blob/master/Spectra_example1.png "Output")

  
