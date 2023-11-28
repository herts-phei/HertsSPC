# SPC_package

The HertsSPC package looks to bring together the box:: functionality developed for HPFT, which includes code for producing SPC charts, summary tables and narratives. Functionality also includes SPC processing for the purpose of using with plotly and crosstalk. The reasoning behind creating the package to allow for use on Rshiny applications, as box() does not currently allow for this. This allows for the mass production of SPC charts/tables.

External parties may use the package, with awareness that not all data is SPC applicable, and the outputs are currently tailored to that used within HPFT Performance (although customisation is possible user-end). Although SPC's can be produced automatically on mass, not all data will be applicable and should be thoroughly inspected/discussed by teams using before disseminating to colleagues.

The package has the following aims:

-   Apply statistical process control methodology (in line with NHS Making Data Count)

-   Produce charts, with ability to rebase, baseline and apply icons. Charts can be static, or done interactively with either plotly or echarts4r

-   Produce summary table with icons

-   Produce narratives for individual indicators

## Installation

Download the current version of the package by running:

``` R
devtools::install_github("herts-phei/HertsSPC")

library(HertsSPC)
```

## Setup

Read in unprocessed data for indicators/performance metrics. The input to any of the functionality should begin with a dataframe with one row per date per indicator. If there is duplication in this, then the plots/tables will produce unexpected results. If duplicate rows exist for a date and no indicator column is identified, then the processing will assume two indicators exist in the dataframe, and will apply dummy indicators to the processing (Indicator 1, Indicator 2 etc.). If this is undesired, append an indicator column to your dataframe.

## Using the package

For more examples, please see vignettes:

```         
?HertsSPC::spc_ouput() 
```

Everything can be produced through the spc_output() function alone. However, if you are producing multiple plots, spc_output() needs to be run first to produce the processed dataframe with multiple indicators. The charts can be then produced in a loop using spc_chart(). To note, facetting graphs is not possible within the charting functionality.

As an example:

```         

  library(dplyr)

  tooth_data <- force(ToothGrowth) %>%
    filter(supp == "VC") %>% slice(-15:-27) %>%
    mutate(Date = seq.Date(as.Date("2021-01-01"), as.Date("2021-01-17"), by = "days"),
           supp = "Indicator 1",
           polarity = "up",
           greater_than_hundred = FALSE,
           less_than_zero = FALSE,
           unit = "count")

  tooth_data2 <- force(ToothGrowth) %>%
    filter(supp == "VC") %>% slice(-15:-27) %>%
    mutate(Date = seq.Date(as.Date("2021-01-01"), as.Date("2021-01-17"), by = "days"),
           supp = "Indicator 2",
           polarity = "up",
           greater_than_hundred = FALSE,
           less_than_zero = FALSE,
           unit = "count") %>%
    bind_rows(tooth_data)

spc_data <- HertsSPC::spc_output(data = tooth_data2, 
                                time_field = "Date",
                                indicator = "supp",
                                value = "len",
                                output = "data")
                     
graph_list <- list()

for(i in unique(spc_data$indicator)){

# If you are running a ggplot with icons, the chunk of ggplot processing needs to be wrapped in () before the pipe, see below. If using plotly or echarts, you do not need to wrap

 graph <- ( HertsSPC::spc_chart(.data = filter(spc_data, indicator == i),
                              .plot_title = i,
                              .base_date_range = NULL, 
                              .package = "ggplot") + 
                              labs(title = "abc") +
        ggplot2::theme(axis.line = ggplot2::element_line(color = 'white')) ) %>%
        spc_add_icons()
        
                              
 graph_list[[paste(i)]] <- graph

}

graph_list[1]

graph_list[2]
                   
```
