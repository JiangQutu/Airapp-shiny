library(shiny)
library(shinythemes)
library(leaflet)
library(data.table)
library(tidyr)
library(dplyr)
library(scales)
library(RColorBrewer)
library(lubridate)
library(stringi)
library(ggplot2)
library(viridis)
library(plotly)
library(leafletCN)
airdata <- fread('data/airdata_wide.csv')
geo <- fread('data/geo_clean.csv')
city <- read.csv('data/citygeo.csv')
province <- fread('data/provincegeo.csv')
#### 第一页站点数据与经纬度清洗合并 ####
#geo$province <- gsub('省|市|壮族自治区|回族自治区|维吾尔自治区|自治区','',geo$province)
#geo$city <- gsub('市', '', geo$city)
# geo[geo=='广西壮族自治区'] <- '广西'
airdata <- airdata %>% gather(station_code,value, 4:1500)
airdata <- as.data.table(left_join(airdata,geo,by='station_code')) 
airdata <- airdata[complete.cases(lon),]
# 初始leaflet图
china <- leaflet() %>% 
  fitBounds(86.33,24.51,145.05,42.33)
  #setView(lng=117.38,lat=36.9,zoom=4) 
#### 第二页gghot_plot data ####
## hot data
hotdata <- airdata[complete.cases(value),] %>% 
  group_by(province,month,type) %>% 
  summarise(value=mean(value)) %>% 
  arrange(month,value) %>% 
  as.data.table()
## scatter data
citydata <- airdata[complete.cases(value),] %>%
  select(date=date,type=type,city=city,value=value) %>% 
  subset(city %in% c('北京','石家庄','海口','舟山','成都','武汉','上海','杭州','广州')) %>% 
  group_by(date,type,city) %>% 
  summarise(value=mean(value)) %>%
  as.data.table()
citydata$date <- as.Date(citydata$date)
## polygon data
# polydata <- airdata[complete.cases(value),] %>%
#   select(month=month,type=type,region=city,value=value) %>% 
#   group_by(month,type,region) %>% 
#   summarise(value=mean(value)) %>%
#   as.data.table()
# choropleth <- as.data.table(merge(city, polydata, by = 'region',all.x=TRUE))
# choropleth <- choropleth[complete.cases(month),] %>% 
#   subset(type=='AQI') %>% 
#   arrange(order)
# choropleth$month_CN <- factor(choropleth$month, labels = c('一月','二月','三月','四月','五月','六月','七月','八月','九月','十月','十一月','十二月'))
# polytheme <- function(){
#   theme(text = element_text(family = 'STHeiti',color='white'),
#         axis.text = element_blank(),
#         axis.title = element_blank(),
#         plot.title = element_text(hjust = 0.5),
#         plot.background = element_rect(fill="#0e0e0e"),
#         panel.background = element_rect(fill="#0e0e0e"),
#         panel.border = element_rect(fill=NA, color="#0e0e0e", size=0.5, linetype="solid"),
#         panel.grid = element_blank(),
#         axis.line = element_blank(),
#         axis.ticks = element_blank(),
#         legend.background = element_rect(fill="#0e0e0e"),
#         legend.position = 'bottom',
#         legend.key.width = unit(2.5,'cm'), # 图例长宽
#         legend.key.height = unit(0.3,'cm'),
#         legend.title.align = 0.5,
#         strip.background = element_blank(),
#         strip.placement = 'outside',
#         strip.text = element_text(color = 'white'))
# }

#### ggplot_mytheme ####
mytheme <- function(...,background='#0e0e0e'){
  theme(text = element_text(family = 'STHeiti',color='white'),
        axis.text = element_text(color = 'white'),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill="#0e0e0e"),
        panel.background = element_rect(fill="#0e0e0e"),
        panel.border = element_rect(fill=NA,color="#0e0e0e", size=0.5, linetype="solid"),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_rect(fill="#0e0e0e"),
        legend.position = 'none')
}
#### shinyServer ####
shinyServer(function(input, output, session) {
  #### 站点Leaflet绘图 ####
  # 选择器时间，type
  output$output_slider_time  <- renderText({
    paste("Time:", stri_datetime_format(input$slider_time,'uuuu-MM-dd'),input$select_air)
  })
  # 选择器返回数据
  filteredData <- reactive({
    if(input$select_air=="AQI"){
      airdata %>%
        subset(type=='AQI') %>% 
        filter(date==stri_datetime_format(input$slider_time,'uuuu-MM-dd'))
    }else{
      airdata %>%
        subset(type==input$select_air) %>%
        filter(date==stri_datetime_format(input$slider_time,'uuuu-MM-dd'))
    }
  })
  # 输出选择地图
  output$map <- renderLeaflet({
    china %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      clearMarkerClusters() %>%
      clearMarkers()
  })
  # 反应更新
  observe({
    # 不同污染物不同图例
    airtype <- input$select_air
    if(airtype=='AQI'){
      pal <- colorBin(
        palette = viridis(12),
        domain = filteredData()$value,
        bins=c(0,50,100,150,200,300,400,500)
      )
    }else if(airtype=='PM2.5'){
      pal <- colorBin(
        palette = rev(brewer.pal(8,"RdYlGn")),
        domain = filteredData()$value,
        bins=c(0,15,35,75,115,150,250,350,1100)
      )
    }
    else if(airtype=='PM10'){
      pal <- colorBin(
        palette = rev(brewer.pal(7,"RdYlGn")),
        domain = filteredData()$value,
        bins=c(0,50,150,250,350,420,500,1100)
      )
    }
    else if(airtype=='NO2'){
      pal <- colorBin(
        palette = rev(brewer.pal(7,"RdYlBu")),
        domain = filteredData()$value,
        bins=c(0,10,20,30,40,80,180,400)
      )
    }
    else if(airtype=='SO2'){
      pal <- colorBin(
        palette = rev(brewer.pal(7,"BrBG")),
        domain = filteredData()$value,
        bins=c(0,15,25,35,45,50,150,500,2620)
      )
    }
    else if(airtype=='O3'){
      pal <- colorBin(
        palette = rev(brewer.pal(6,"RdYlBu")),
        domain = filteredData()$value,
        bins=c(0,20,40,60,90,215,1000)
      )
    }
    else if(airtype=='CO'){
      pal <- colorBin(
        palette = rev(brewer.pal(7,"RdBu")),
        domain = filteredData()$value,
        bins=c(0,0.2,0.4,0.8,1.2,2,4,10,70)
      )
    }
    # 重新绘图
    leafletProxy('map') %>% 
      clearMarkerClusters() %>%
      clearMarkers() %>%
      clearControls() %>% 
      addCircleMarkers(filteredData()$lon,filteredData()$lat,
                fillColor = pal(filteredData()$value),
                color = '#ffffff', weight = 0.1,
                radius=3,
                fillOpacity = 0.7,
                popup = paste(filteredData()$province,
                              filteredData()$city,
                              filteredData()$station,
                              filteredData()$value, 
                              sep = "<br>")) %>% 
      addLegend(position='bottomleft',
                pal=pal,values=filteredData()$value,
                title=input$select_air)
  })
  #### top站点及绘图 ####
  top <- reactive({
    filteredData() %>% 
      arrange(desc(value)) %>%
      head(22) %>%
      as.data.frame()
  })
  # top_plot
  output$top <- renderPlotly({
    plot_ly(data=top(),name='',
            x = ~value,
            y = ~reorder(station_code,value),
            type = 'bar',orientation = 'h',
            text = ~paste(province,city,station),
            marker = list(color='rgba(83, 139, 198,0.9)',line=list(color='rgba(83, 139, 198,1)',width=1))) %>% 
      layout(yaxis = list(showgrid = FALSE, showline = TRUE,
                          color='#999',showticklabels = TRUE,
                          title=''),
             xaxis = list(zeroline = FALSE, showline = TRUE,
                          showticklabels = TRUE,gridcolor='#999',
                          color='#999',side='top',
                          title=''),
             plot_bgcolor='rgba(1,1,1,0)',
             paper_bgcolor='rgba(1,1,1,0)',
             margin = list(l = 42, r = 0, t = 48, b = 40))
  })
  #### gghot_plot ####
  output$gghot_plot <- renderPlot({
    ggplot(hotdata[hotdata$type=='AQI',], aes(province, month, fill = value)) + 
      geom_tile(colour="white", size=0.1, stat="identity") + 
      scale_fill_viridis(option="D",name='legend') +
      scale_y_continuous(expand = c(0,0),breaks = 1:12,
                         labels = c('一月','二月','三月','四月','五月','六月','七月','八月','九月','十月','十一月','十二月')) +
      xlab('')+ylab('') +
      mytheme()+theme(axis.text = element_text(size = 15))
  })
  #### ggscatter_plot ####
  output$ggscatter_plot <- renderPlotly({
    p <- ggplot(citydata[type=='AQI'],aes(date,value,colour=value)) + 
      geom_point(alpha=0.7)+
      scale_x_date(date_breaks = "4 month",
                   date_labels = '%m-%d') +
      scale_y_continuous(breaks = c(0,100,200,300,400,500))+
      facet_wrap(~city,nrow = 1,labeller = label_value)+
      scale_color_viridis(name='aqi index') + 
      xlab('') + ylab('') +
      #geom_hline(yintercept = 75,color='red')+
      mytheme() + theme(axis.line.x = element_line(colour = 'gray'),
                        axis.text.y = element_text(angle = 90),
                        axis.ticks = element_line(color = 'gray',size = 0.01),
                        strip.background = element_rect(fill = '#481668',
                                                        color = '#e0e4eb'),
                        strip.text = element_text(color = 'white'))
    ggplotly(p,tooltip=c('x','y'))
  })
  #### ggploygon_plot ####
  output$ggploygon_plot <- renderPlot({
    ggplot(choropleth)+
      geom_polygon(aes(long,lat,group=group,fill=value),
                   colour = alpha("gray",1/2),size=0.1)+
      geom_polygon(data = province, aes(long,lat,group=group),
                   colour = alpha('gray90',1/2), size=0.2,
                   fill = NA)+
      #coord_map()+ # 墨卡托投影
      polytheme()+
      facet_wrap(~month_CN)+
      scale_fill_distiller(palette = "Spectral",name='城市AQI')
  })
})
