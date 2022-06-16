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

#UI####
ui <- fluidPage(
  # Application title
  titlePanel("検索ワードによる画像付きツイートの抽出"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    sidebarPanel(
      h4(strong(column(1,"検索ワード"))),
      h4(column(2,
                textInput(inputId = "wd",
                          label = NULL,
                          value = "大雨"))),
      h4(strong(column(1,"ツイート数"))),
      h4(column(2,
                numericInput(inputId = "num",
                             label = NULL,
                             value = 100,
                             min = 10,
                             max = 1000,
                             step = 10))),
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
server <- function(input, output) {
  refreshPlot0 <- reactiveTimer(intervalMs = 60000)
  
  WD <- eventReactive(input$button,{
    return(input$wd)
  })
  
  SORT <- eventReactive(input$button,{
    return(input$sort)
  })
  
  NUM <- eventReactive(input$button,{
    return(input$num)
  })
  
  wd="大雨"
  sort=2
  num=10
  
  observe({
    refreshPlot0()
    wd=WD()
    sort=SORT()
    num=NUM()
    
    print(c(wd,sort))
    tm=Sys.time()
    print(tm)
    td <- search_tweets(paste(wd,"filter:media"),lang = "ja",n = num,include_rts = T)
    
    # rID=sort(td$retweet_status_id[!td$retweet_status_id %in% td$status_id])
    # if(length(rID)>0){
    #   for (id in rID) {
    #     td0 <- search_tweets(paste(wd,"filter:media"),lang = "ja",n = 1,include_rts = T,max_id = id)
    #     td <-
    #       td %>%
    #       rbind(td0)
    #   }
    # }
    
    tds <-
      td %>%
      distinct(status_id,.keep_all = T) %>%
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
      distinct(status_id,.keep_all = T) %>%
      group_by(RID) %>%
      summarise(n=n(),nf=max(favorite_count),nr=max(retweet_count)) %>%
      ungroup() %>%
      mutate(n=ifelse(RID %in% rID,n-1,n)) %>%
      left_join(TDPS %>% distinct(Purl,.keep_all=T) %>% select(RID,Purl,text,JTime,RTime)) %>%
      filter(nf>0 | nr>0) %>%
      filter(!grepl("おは",text)) %>%
      filter(!grepl("^@",text)) %>%
      mutate(Rank=frank(-n,ties.method = "max")) %>%
      arrange(Rank,desc(nf),desc(nr),RID) %>%
      ungroup() 
    
    if(sort==1){
      TDPC0 <-
        TDPC %>%
        arrange(Rank) %>%
        filter(Rank<=20|n==max(n))
    }
    
    if(sort==2){
      TDPC0 <-
        TDPC %>%
        arrange(desc(RTime)) %>%
        filter(RID %in% unique(RID)[1:20])
    }
    
    ID=unique(TDPC0$RID)
    ID=ID[1:min(20,length(ID))]
    XY=data.frame(RID=ID,sx=0,lx=0,sy=0,ly=0)
    n=nrow(XY)
    ro=round(sqrt(n))
    co=ceiling(n/ro)
    r=ro
    p=ggplot() +
      scale_x_continuous(limits = c(0,co),expand = c(0,0)) +
      scale_y_continuous(limits = c(0,ro),expand = c(0,0)) +
      theme(axis.title = element_blank()) +
      theme(axis.text  = element_blank()) +
      theme(axis.ticks = element_blank())
    
    i=1
    for (i in 1:n) {
      id=ID[i]
      XY[i,-1] = c(floor((i-1)/ro),floor((i-1)/ro)+1,r,r-1)
      
      (JPG <-
          try(image_scale(
            image_read(TDPC$Purl[which(TDPC$RID==id)]), geometry = 960)))
      if(sum(class(JPG)=="try-error"))
        next
      
      GIF <-
        image_append(JPG, stack = F)
      if(length(JPG)>2){
        GIF12 <-
          image_append(JPG[1:2], stack = T)
        GIF34 <-
          image_append(JPG[-(1:2)], stack = T)
        GIF <-
          image_append(image_scale(c(GIF12,GIF34)), stack = F)
      }
      
      
      p <-
        p +
        annotation_raster(GIF,XY$sx[i],XY$lx[i],XY$sy[i],XY$ly[i])
      # plot(p)
      r=r-1
      if(r==0)
        r=ro
    }
    
    output$RTweet <- renderPlot({
      plot(p)
    })
    
    TDPCS <-
      TDPC0 %>%
      distinct(RID,.keep_all = T) %>%
      left_join(XY)
    
    output$info <- renderPrint({
      if(!is.null(input$plot_hover)){
        hover=input$plot_hover
        hover$x=hover$x*co
        hover$y=hover$y*ro
        w=which(hover$x>XY$sx&hover$x<XY$lx&hover$y<XY$sy&hover$y>XY$ly)
        paste(TDPCS$RTime[w],paste0(TDPCS$n[w],"ツイート"),paste0(TDPCS$nf[w],"いいね"),TDPCS$text[w])
      }
    })
    print(Sys.time())
    print(Sys.time()-tm)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




