library(shiny)

if(!require(data.table)){
  install.packages("data.table")
  library(data.table)
}
if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}
if(!require(tidyr)){
  install.packages("tidyr")
  library(tidyr)
}
if(!require(stringi)){
  install.packages("stringi")
  library(stringi)
}
if(!require(rtweet)){
  install.packages("rtweet")
  library(rtweet)
}
if(!require(lubridate)){
  install.packages("lubridate")
  library(lubridate)
}
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require(dygraphs)){
  install.packages("dygraphs")
  library(dygraphs)
}
if(!require(magick)){
  install.packages("magick")
  library(magick)
}

Unzip <- function(...) rbind(data.frame(), ...)

# Define UI for application that draws a histogram
#UI####
ui <- fluidPage(
  # Application title
  titlePanel("検索ワードのリツイート数の多い画像"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    sidebarPanel(
      h4(strong(column(1,"検索ワード"))),
      h4(column(2,
             textInput(inputId = "wd",
                          label = NULL,
                          value = "大雨"))),
      # column(1,h4("画像順序"),
      #        offset = 1),
      h4(column(2,
                radioButtons(inputId = "sort",
                             label = "画像順序",
                             choices = c("出現頻度"=1,"最新投稿"=2),
                             selected = 1,
                             inline = T))),
      h4(column(2,
                actionButton(inputId = "button",
                             label = "ツイートの抽出開始&更新"))),
      br(),
      br(),
      width = 12
    ),
    
    # Show a plot of the generated distribution
    
    mainPanel(
      dygraphOutput("Hdy",height="110px"),
      plotOutput("RTweet",height = "100%",width = "100%",hover = hoverOpts(id ="plot_hover")),
      verbatimTextOutput("info"),
      width = 12
    ),
    tags$style(type="text/css",
               "#info  {font-size: 2vh !important;}",
               "#RTweet{height:  67.5vh !important;
                        width:   120vh !important;
                        margin-left: auto;
                        margin-right: auto;}")
  )
)

#SERVER####
# Define server logic required to draw a histogram
server <- function(input, output) {
  refreshPlot0 <- reactiveTimer(intervalMs = 60000)
  
  wd="大雨"
  sort=2
  
  WD <- eventReactive(input$button,{
    return(input$wd)
  })

  SORT <- eventReactive(input$button,{
    return(input$sort)
  })
  
  observe({
    output$TM <- renderText({
      as.character(Sys.time())
    })
    refreshPlot0()
    wd=WD()
    sort=SORT()
    print(c(wd,sort))
    td <- search_tweets(paste(wd,"filter:media"),lang = "ja",n = 1000,include_rts = T)
    
    tds <-
      td %>%
      arrange(desc(status_id)) %>%
      mutate(JTime=as.POSIXct(format(created_at, tz="Japan"))) %>%
      mutate(YMD_HM=format(JTime,"%Y%m%d_%H%M")) %>%
      mutate(YMD_HM=ymd_hm(YMD_HM)) %>%
      mutate(RT=is_quote|is_retweet) %>%
      ungroup()
    
    TDC <-
      tds %>%
      count(YMD_HM,RT) %>%
      filter(YMD_HM<max(YMD_HM))
    
    print(head(TDC %>% arrange(desc(YMD_HM))))
    
    output$Hdy <- renderDygraph({
      Comp <- 
        data.frame(YMD_HM=rep(seq(min(TDC$YMD_HM,na.rm = T),max(TDC$YMD_HM,na.rm=T),60),each=2),
                   RT=c(F,T))
      TDCS <-
        Comp %>%
        left_join(TDC) %>%
        complete(YMD_HM,RT,fill=list(n=0)) %>%
        mutate(RTs=factor(RT,labels = c("Origin","Retweet"))) %>%
        select(YMD_HM,RTs,n) %>%
        spread(RTs,n) %>%
        select(Retweet,Origin)
      
      rownames(TDCS) <- unique(Comp$YMD_HM)
      
      dygraph(TDCS,main = paste0("ツイート数 1分ごとの推移 ",min(TDC$YMD_HM),"～",max(TDC$YMD_HM))) %>% #
        dyOptions(stackedGraph = T, drawPoints = T, pointSize = 1, strokeWidth = 2,fillAlpha = 0.5,colors = c("red","blue"),
                  axisLabelFontSize = 20,axisLabelWidth = 100,titleHeight = 30,labelsKMB = T) %>%
        dyRangeSelector(height = 10,keepMouseZoom = T,
                        dateWindow = c(min(min(TDC$YMD_HM),max(TDC$YMD_HM)-20*60)-9*60*60,max(TDC$YMD_HM)-9*60*60)) %>%
        dyLegend(width = 150)
    })
    
    RTD <-
      tds %>%
      distinct(status_id,emu=ext_media_url)
    
    TDP <-
      tds %>%
      left_join(RTD %>% rename(retweet_status_id=1)) %>%
      mutate(ext_media_url=ifelse(!is.na(ext_media_url),ext_media_url,emu)) %>%
      filter(grepl("http",ext_media_url))
    
    PD <-
      do.call(Unzip, TDP$ext_media_url)
    colnames(PD) = paste0("Photo",1:ncol(PD))
    
    TDPS <- 
      TDP %>%
      cbind(PD) %>%
      mutate(RID=ifelse(retweet_status_id==""|is.na(retweet_status_id),ifelse(quoted_status_id==""|is.na(quoted_status_id),status_id,quoted_status_id),retweet_status_id)) %>%
      mutate(RTime=ifelse(retweet_status_id==""|is.na(retweet_status_id),ifelse(quoted_status_id==""|is.na(quoted_status_id),created_at,quoted_created_at),retweet_created_at)) %>%
      mutate(RTime=as.POSIXct(RTime,origin="1970-01-01")) %>%
      select(status_id,screen_name,JTime,RID,RTime,text,favorite_count,retweet_count,starts_with("Photo")) %>%
      gather(PNo,Purl,starts_with("Photo")) %>%
      arrange(status_id) %>%
      distinct(status_id,Purl,.keep_all = T)
    
    TDPC <-
      TDPS %>%
      mutate(Purl=factor(Purl,levels = unique(Purl))) %>%
      group_by(Purl) %>%
      summarise(n=n(),nf=max(favorite_count),nr=max(retweet_count)) %>%
      ungroup() %>%
      mutate(Rank=frank(-n,ties.method = "max")) %>%
      arrange(Rank,desc(nf),Purl) %>%
      filter(nf>0 | nr>0) %>%
      mutate(Purl=as.character(Purl)) %>%
      mutate(Ps=ifelse(grepl("img/",Purl),regexpr("g/",Purl),regexpr("[ab]/",Purl))) %>%
      mutate(Pl=nchar(Purl)) %>%
      mutate(Pjpg=substr(Purl,Ps+2,Pl)) %>%
      left_join(TDPS %>% distinct(Purl,.keep_all=T) %>% select(Purl,text,JTime,RTime)) %>%
      filter(!grepl("おはよう",text)) %>%
      mutate(JTime=as.POSIXct(JTime))
    
    if(sort==1){
      TDPC <-
        TDPC %>%
        arrange(Rank) %>%
        filter(Rank<=20)
    }
    
    if(sort==2){
      TDPC <-
        TDPC %>%
        arrange(desc(RTime)) %>%
        filter(1:n()<=20)
    }
    
    path="Photo/"
    
    n=nrow(TDPC)
    r=round(sqrt(n))
    co=ceiling(n/r)
    ro=r
    p=ggplot() +
      scale_x_continuous(limits = c(0,co),expand = c(0,0)) +
      scale_y_continuous(limits = c(0,r),expand = c(0,0)) +
      theme(axis.title = element_blank()) +
      theme(axis.text = element_blank()) +
      theme(axis.ticks = element_blank())
    i=1
    XY=data.frame(Purl=TDPC$Purl,sx=0,lx=0,sy=0,ly=0)
    for (i in 1:n) {
      XY[i,-1] = c(floor((i-1)/r),floor((i-1)/r)+1,ro,ro-1)
      
      JPG <-
        image_read(TDPC$Purl[i])
      
      p <-
        p +
        annotation_raster(JPG,XY$sx[i],XY$lx[i],XY$sy[i],XY$ly[i])
      # plot(p)
      ro=ro-1
      if(ro==0)
        ro=r
    }
    
    output$RTweet <- renderPlot({
      plot(p)
    })
    
    output$info <- renderPrint({
      if(!is.null(input$plot_hover)){
        hover=input$plot_hover
        hover$x=hover$x*co
        hover$y=hover$y*r
        w=which(hover$x>XY$sx&hover$x<XY$lx&hover$y<XY$sy&hover$y>XY$ly)
        paste(TDPC$RTime[w],paste0(TDPC$n[w],"リツイート"),paste0(TDPC$nf[w],"いいね"),TDPC$text[w])
      }
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




