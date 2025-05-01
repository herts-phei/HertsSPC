# SPC_package

The HertsSPC package includes code for producing SPC charts, summary tables and narratives. Functionality also includes SPC processing for the purpose of using with plotly and crosstalk. The reasoning behind creating the package to allow for use on Rshiny applications, as box() does not currently allow for this. This allows for the mass production of SPC charts/tables.

External parties may use the package, with awareness that not all data is SPC applicable, and the outputs are currently tailored to that used within HPFT Performance (although customisation is possible user-end). Although SPC's can be produced automatically on mass, not all data will be applicable and should be thoroughly inspected/discussed by teams using before disseminating to colleagues.

The package has the following aims:

-   Apply statistical process control methodology (in line with NHS Making Data Count)

-   Produce charts, with ability to rebase, baseline and apply icons. Charts can be static, or done interactively with either plotly or echarts4r

-   Produce summary table with icons (along with the data frame behind it)

-   Produce narratives for individual indicators

-   Return the status for the indicators

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

Everything can be produced through the spc_output() function alone. However, if you are producing multiple plots, spc_output() needs to be run in the first instance to produce the processed dataframe with multiple indicators. The charts can be then produced in a loop using spc_chart(). To note, facetting graphs is not possible within the charting functionality.

As an example:

```         

  library(dplyr)
  
  start_date <- as.Date("2025-01-01")
  end_date <- as.Date("2025-01-20")
  date_sequence <- seq.Date(from = start_date, to = end_date, by = "day")
  
  
  indicator_data_1 <- 
     data.frame(Date = date_sequence,
                kpi = "Indicator 1",
                indicator_value = c(45,48,44,43,45,
                                    65,45,46,46,44,
                                    43,42,41,40,39,
                                    46,47,56,52,50),
                target = 60)
                
  indicator_data_2 <- 
     data.frame(Date = date_sequence,
                kpi = "Indicator 2",
                indicator_value = c(45,48,44,43,45,
                                    45,45,47,46,44,
                                    43,44,43,28,44,
                                    45,47,45,43,46),
                target = 45)


  all_indicator_data <- bind_rows(indicator_data_1, indicator_data_2) %>% 
      dplyr::mutate(polarity = "up",
                    greater_than_hundred = FALSE,
                    less_than_zero = FALSE,
                    unit = "count")

  spc_data <- HertsSPC::spc_output(
                  data = all_indicator_data,
                  time_field = "Date",
                  indicator = "kpi",
                  value = "indicator_value",
                  output = "data",
                  target = "target"
                  )
                     
graph_list <- list()

for(i in unique(spc_data$indicator)){

# If you are running a ggplot with icons, the chunk of ggplot processing needs to be wrapped in () before the pipe, see below. If using plotly or echarts, you do not need to wrap

 graph <- ( HertsSPC::spc_chart(.data = filter(spc_data, indicator == i),
                                .plot_title = i,
                                .base_date_range = NULL,
                                .package = "ggplot",
                                .chart_theme = spc_chart_options(x_breaks = "1 day",
                                                                 x_label_format = "%b-%d")
                               ) +
                                
        ggplot2::labs(title = "abc") +
        ggplot2::theme(axis.line = ggplot2::element_line(color = 'white'))
        
        ) %>%
        
        HertsSPC::spc_add_icons()
        
        
 graph <- HertsSPC::spc_chart(.data = filter(spc_data, indicator == i),
                              .plot_title = i,
                              .base_date_range = NULL,
                              .package = "plotly") %>%
        HertsSPC::spc_add_icons()
 #        
                              
 graph_list[[paste(i)]] <- graph

}

graph_list[1]

graph_list[2]

spc_summary_table(.data = spc_data,
                  .mode = "interactive",
                  .time_field = "Day",
                  .indicator = "Metric",
                  .value = "Value",
                  .time_unit = "day",
                  .nad = FALSE)
                   
```
