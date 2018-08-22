# PlotTwist
A Shiny App for plotting Time dependent data

### About PlotTwist
  
PlotTwist (Plotting Time-dependent data With Indication of the STimulus) is an app for visualizing the data and statistics of time-dependent measurements (or other continuous data, such as spectra). The philosophy of the approach is that plotting the raw data (instead of a summary) improves transparency and interpretation. To further facilitate the comparison, the mean and 95% CI can be added. The user has full control over the visibility of the raw data and statistics by adjustment of the transparency (alpha).
The comparison of multiple conditions is enabled by multiple file upload (csv or xls format), where each file represents a condition.
There is an option to indicate the time window during which a stimulus was applied. This will overlay a transparant grey box.
For more information on the underlying code see these blogs:
[Visualizing data with R/ggplot2 ??? It???s about time](http://thenode.biologists.com/visualizing-data-with-r-ggplot2/education/)
[isualizing data with R/ggplot2 ??? One more time](http://thenode.biologists.com/visualizing-data-one-more-time/education/)

### Running the App

Currently, the app can only run from R/Rstudio.

Give it a quick try by running it directly from Github. In the command line (in R or Rstudio) type:
shiny::runGitHub('PlotTwist', 'JoachimGoedhart')

Or download it to use it offline:

-download the app.R and csv files with example data.

-Run RStudio and load app.R

-Select 'Run All' (shortcut is command-option-R on a Mac) or click on "Run App" (upper right button on the window)

This should launch a web browser with the Shiny app.
Note that the app depends on several R packages that need to be installed (shiny, ggplot2, dplyr, tidyr, readr, readxl)


### Credits

The code for the shiny app is partially derived from [ggplotGUI](https://github.com/gertstulp/ggplotgui) by [Gert Stulp](https://www.gertstulp.com)  
The colorblind safe palettes were developed by [Paul Tol](https://personal.sron.nl/~pault/).

PlotTwist is created and maintained by Joachim Goedhart ([@joachimgoedhart](https://twitter.com/joachimgoedhart))and Marten Postma

### Example output

![alt text](https://github.com/JoachimGoedhart/PlotTwist/blob/master/Timeseries_example1.png "Output")

  
### Potential improvements/additions:

* Plot individual traces in heatmap-style