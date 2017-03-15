library(shiny)
library(shinythemes)
library(leaflet)
library(data.table)
library(tidyr)
library(dplyr)
library(dtplyr)
library(RColorBrewer)
library(scales)
library(lubridate)
library(stringi)
library(ggplot2)
library(viridis)
library(plotly)

shinyUI(
  navbarPage("2015空气质量数据可视化",
             theme = shinytheme('cyborg'),
             fluid = TRUE,
             #### 站点 ####
             tabPanel('站点数据',
                      div(class='outer',
                          tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                          # 地图output
                          leafletOutput("map", width = "100%", height = "100%"),
                          absolutePanel(top = 10,right = 30, # 固定位置
                                        # 时间textoutput
                                        h5(textOutput('output_slider_time')),
                                        # 时间选择器slider
                                        sliderInput("slider_time", "时间选择:",
                                        min = as.POSIXct(0*60*60, origin = "2015-01-02"),
                                        max = as.POSIXct(0*60*60, origin = "2015-12-30"),
                                        value = as.POSIXct(0*60*60, origin = "2015-01-02"),
                                        step = 24*60*60,
                                        timeFormat = '%y-%m-%d',
                                        timezone = "GMT"),
                                        # 时间选择calendars
                                        # dateInput("date", 
                                        #           label = h5("Date input"),
                                        #           startview = "month",
                                        #           value = "2015-01-01"),
                                        # 类别选择器select
                                        selectInput("select_air", 'AQI及空气污染物选择:',
                                                    c("AQI",'PM2.5','PM10','SO2',
                                                      'NO2','O3','CO'),
                                                    selectize = FALSE),
                                        # 前10污染站点绘图output
                                        h5(style="font-family:'STHeiti';font-size:11pt;color:#999",
                                           "高值站点"),
                                        plotlyOutput("top",height = '400px')
                          )
                      ) 
              ),
             #### 城市 ####
             tabPanel('时间尺度',
                      div(class="outer",
                          tags$style(type = "text/css", ".outer {position: fixed; top: 50px; left: 0;right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                          plotlyOutput("ggscatter_plot", width = "100%", height="50%"),
                          plotOutput("gghot_plot", width = "100%", height="50%")
                      )
             )
             # tabPanel('空间尺度',
             #          div(class="outer",
             #              tags$style(type = "text/css", ".outer {position: fixed; top: 50px; left: 0;right: 0; bottom: 0; overflow: hidden; padding: 0}"),
             #              plotOutput("ggploygon_plot", width = "100%", height="100%")
             #          )
             # )
             
  )
)