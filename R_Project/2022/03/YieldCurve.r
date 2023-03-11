# Visualising Australian Bank Bill and Government Bond Yield
# Eddy Zhang
# 11 March 2023

library(readrba)
library(lubridate)
library(tidyverse)

# Get Yield Data for Short Term and Long Term Instruments from RBA
rba_st_yield <- read_rba(table_no = "F1.1")
rba_lt_yield <- read_rba(table_no = "F2.1")

rba_1y_yield <- read.csv("1yAusBondYield.csv") #RBA does not provide 1-year Bond data, this is downloaded from investing.com


                              

# Data Clean

rba_st_yield_wip <- rba_st_yield %>%
                    filter(units == "Per cent") %>%
                    select(date, series, value) %>%
                    pivot_wider(names_from = series, values_from = value) %>%
                    arrange(date) %>%
                    select(c("date","Interbank Overnight Cash Rate","1-month BABs/NCDs", "3-month BABs/NCDs", "6-month BABs/NCDs")) %>%
                    drop_na()


rba_lt_yield_wip <- rba_lt_yield %>%
                    filter(units == "Per cent per annum") %>%
                    select(date, series, value) %>%
                    pivot_wider(names_from = series, values_from = value) %>%
                    arrange(date) %>%
                    select(1:5) %>%
                    drop_na()

rba_1y_yield_wip <- rba_1y_yield %>%
                    mutate(date = as.Date(rba_1y_yield$Date, "%d/%m/%Y")) %>%
                    relocate(date = date, "1-year" = Aus.1.Year.Yield) %>%
                    select(date, "1-year") %>%
                    arrange(date)


rba_all_yield <- rba_lt_yield_wip %>%
                 left_join(rba_st_yield_wip, by = "date") %>%
                 left_join(rba_1y_yield_wip, by = "date")

rba_all_yield <- rba_all_yield %>%
                 relocate(
                   date = date,
                   Overnight = "Interbank Overnight Cash Rate",
                   "1-month" = "1-month BABs/NCDs",
                   "3-month" = "3-month BABs/NCDs",
                   "6-month" = "6-month BABs/NCDs",
                   "1-year" = "1-year",
                   "2-year" = "Australian Government 2 year bond",
                   "3-year" = "Australian Government 3 year bond",
                   "5-year" = "Australian Government 5 year bond",
                   "10-year" = "Australian Government 10 year bond"
                 )

rba_col_names <- colnames(rba_all_yield)


#load Plotly package to produce visuals

library(plotly)

trace1 <- list(
  type = "surface",
  x = as.vector(rba_col_names[2:length(rba_col_names)]),
  y = rba_all_yield$date,
  z = as.matrix(rba_all_yield[2:length(rba_all_yield)], nrow=dim(rba_all_yield)[1], ncol=dim(rba_all_yield)[2]-1),
  contours = list(
    x = list(
      highlight = TRUE, 
      highlightcolor = "#FF0C00",
      highlightwidth = 1
    ), 
    y = list(highlight = TRUE,
             highlightcolor = "#FF0C00", 
             highlightwidth = 1), 
    
    z = list(highlight = FALSE) 
  ), 
  lighting = list(
    ambient = 0.9, 
    diffuse = 0.9, 
    specular = 0
  ), 
  showscale = FALSE, 
  colorscale = "lightblue", 
  reversescale = TRUE
)


layout <- list(
  scene = list(
    xaxis = list(
      title = list(text = "Aust BB/Govt Bond Maturity"), 
      tickfont = list(size = 10), 
      tickmode = "linear",
      titlefont = list(size = 8),
      showgrid = FALSE
    ), 
    yaxis = list(title = list(text = ""),
                 tickfont = list(size = 10)), 
    zaxis = list(
      title = list(text = "Yield",
                   tickfont = list(size = 10),
                   titlefont = list(size = 8)), 
      showgrid = TRUE 
      #showticklabels = TRUE
    ), 
    aspectmode = "manual", 
    aspectratio = list(
      x = 7, 
      y = 12, 
      z = 5
    ),
    camera = list(eye = list(
      x = 1.8, 
      y = 1.1, 
      z = 0.05
    ))
  ), 
  title = list(text = "Australian Bank Bill/Goverment Bond Yield Curve"), 
  width = 1000, 
  height = 600, 
  margin = list(
    b = 40, 
    l = 60, 
    r = 10, 
    t = 40
  ), 
  hoverlabel = list(bgcolor = "#FFFFFF"),
  annotations = list(
    list(
      x = 1, 
      y = 0, 
      font = list(
        size = 10, 
        family = "Arial"
      ), 
      text = "Produced by Eddy Zhang, Data Sourced from RBA and Investing.Com",  
      showarrow = FALSE
    )
  )
)
p <- plot_ly(width=layout$width, height=layout$height)
p <- add_trace(p, type=trace1$type, x=trace1$x, y=trace1$y, z=trace1$z, contours=trace1$contours, lighting=trace1$lighting, showscale=trace1$showscale, colorscale=trace1$colorscale, reversescale=trace1$reversescale)
p <- layout(p, scene=layout$scene, title=layout$title,margin=layout$margin,xaxis=layout$xaxis, yaxis=layout$yaxis, yzaxis=layout$zaxis,hoverlabel=layout$hoverlabel,titlefont=layout$titlefont,annotations=layout$annotations)
