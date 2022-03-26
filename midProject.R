#package use 
install.packages("psych")
install.packages("bslib")
install.packages("ggplot2")
install.packages("caret")
install.packages("naivebayes")
library(psych)
library(bslib)
library(shiny)
library(ggplot2)
library(plotly)
library(caret)
library(naivebayes)

#file use
fitNb<-readRDS("nb3group.RDS")
dat6322<-read.csv("studentData.csv")
fitKnn<-readRDS("knn2gfinal.RDS")


#user interface part
ui<-fluidPage(theme = bs_theme(version = 4, bootswatch = "united"),
              tabsetPanel(id="inTabset",
                          tabPanel("HOME",
                               column(width = 4, offset =0 ,imageOutput("chula", width = "425px", height = "141px")),
                               br(),
                               br(),
                               br(),
                                   fluidRow(column(12,align="center",
                                   strong(h1("Academic Recomender Program for Student"),style="color:#eb591e" ),
                                   h3("โปรแกรมแนะนำทางวิชาการสำหรับนิสิต รายวิชา การวิจัยเพื่อพัฒนาการเรียนรู้"),
                                   br(),
                                   br(),
                                   textInput("idst", h4("Please input student ID")),
                                   tags$style(type="text/css", "#idst {text-align:center;color:#eb591e;}"),
                                   actionButton('top2', 'Next')))
                          ),
                          tabPanel("MIDTERM SCORE",
                                   strong(h1("EXPLORE MIDTERM SCORE")),
                                   sidebarLayout(
                                     sidebarPanel(
                                       verticalLayout(
                                         h3("Profile"),
                                         textOutput("student"),
                                         textOutput("idstudent"),
                                         h3("Your midterm score"),
                                         strong(h2(textOutput("point"))),
                                         tags$style(type="text/css", "#point {color:#eb591e;}"),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br())),
                                     mainPanel(verticalLayout(tabsetPanel(tabPanel("Summary",
                                                                                   h3("Statistic Table"),
                                                                                   tableOutput("des"),
                                               ),
                                               tabPanel("Histogram",
                                                        p("\b"),
                                                        sliderInput("bins",h4("Adjust bins"),
                                                                    min = 1,
                                                                    max = 30,
                                                                    value = 5,
                                                                    step = 1),
                                                        plotOutput("plot", width = "700px",height = "500px")
                                               ),
                                               tabPanel("Boxplot",
                                                        flowLayout(
                                                          radioButtons("pattern",h4("Dot patterns"),
                                                                       choices = c("jitter", "overplot", "stack"),
                                                          ),
                                                          sliderInput("dotsizeb",h4("Adjust dot size"),
                                                                      min = 0,
                                                                      max = 3,
                                                                      value = 0.8,
                                                                      step = 0.1, width = "300px"),
                                                          div(h4("Where are you?"),
                                                              actionButton("getdotb", "Show"))),          
                                                        
                                                        plotOutput("bplot",width = "800px",height = "500px" )               
                                               ),
                                               tabPanel("Dotplot",
                                                        flowLayout(
                                                          sliderInput("dotsize",h4("Adjust dot size"),
                                                                      min = 0,
                                                                      max = 3,
                                                                      value = 0.8,
                                                                      step = 0.1, width = "300px"),
                                                          div(h4("Where are you?"),
                                                              actionButton("getdot", "Show"))),
                                                        plotOutput("dplot",width = "800px",height = "500px")
                                                        
                                               ))
                                               
                                               ))),
                                   value = "panel2"),
                          tabPanel("RECOMMENDER", h1("RISK ASSESSMENT"),
                                   sidebarLayout(
                            sidebarPanel(
                                         h3("Profile"),
                                         textOutput("student2"),
                                         textOutput("idstudent2"),
                                         h3("Your midterm score"),
                                         strong(h2(textOutput("point2"))),
                                         tags$style(type="text/css", "#point2 {color:#eb591e;}"),
                                         h4("Please input your information"),
                                         sliderInput("allatd", "ขณะนี้มีการเรียนการสอนมาแล้วกี่ครั้ง",min=0,max = 20, value = 1,step = 1,width = "80%"),
                                         sliderInput("atd", "จำนวนครั้งที่นิสิตเข้าเรียน",min=0,max = 20, value = 1,step = 1,width = "80%"),
                                         sliderInput("allwork","จำนวนแบบฝึกหัดที่ได้รับมอบหมาย",min=0,max = 20, value = 1,step = 1,width = "80%"),
                                         sliderInput("work", "จำนวนแบบฝึกหัดที่นิสิตส่ง",min=0,max = 20, value = 1,step=1,width = "80%")
                            ),  
                            mainPanel(
                                      verticalLayout(tabsetPanel(
                                        tabPanel("STATUS NOW",
                                                 fluidRow(column(10,
                                                 br(),
                                                 h4(strong(("คำแนะนำ"))),
                                                 p(h5(strong("Red",style="color:red")),": มีความเสี่ยงระดับสูงที่ผลการเรียนจะไม่ผ่าน"),
                                                 p(h5(strong("Yellow",style="color:#E6A102")), ": มีความเสี่ยงระดับปานกลางที่ผลการเรียนจะไม่ผ่าน"),
                                                 p(h5(strong("Green",style="color:#05A24A")), ": มีความเสี่ยงระดับต่ำที่ผลการเรียนจะไม่ผ่าน"),
                                                 p(strong("หมายเหตุ"), ": สถานะดังกล่าวเป็นเพียงการคาดการณ์จากข้อมูลผลการเรียนย้อนหลังจำนวน 4 ภาคการศึกษาเท่านั้น ไม่ใช่การตัดสินผลการเรียนของนิสิต ขอให้นิสิตใช้ข้อมูลดังกล่าวให้เป็นประโยชน์สำหรับการวางแผนเพื่อให้ผลการเรียนเป็นตามที่คาดหวัง"),
                                                 ),
                                                 br()
                                                 ),
                                                 actionButton("go", "Calculate"),
                                                 h1(strong(verbatimTextOutput("SR"), style="color:#F58C43"))),                    
                                                 
                                        
                                        
                                        tabPanel("HOW MANY MORE POINTS?",
                                                 fluidRow(column(10,
                                                 h4(strong(("คำแนะนำ"))),
                                                 p(h5(strong("Safe",style="color:#05A24A")),": ความเสี่ยงต่อการมีผลการเรียนไม่ผ่านระดับต่ำ เมื่อได้คะแนนสอบปลายภาคตามที่คาดการณ์"),
                                                 p(h5(strong("Risk",style="color:red")),": ความเสี่ยงต่อการมีผลการเรียนไม่ผ่านระดับสูง เมื่อได้คะแนนสอบปลายภาคตามที่คาดการณ์"),
                                                 p(strong("หมายเหตุ"), ": สถานะดังกล่าวเป็นเพียงการคาดการณ์จากข้อมูลผลการเรียนย้อนหลังจำนวน 4 ภาคการศึกษาเท่านั้น ไม่ใช่การตัดสินผลการเรียนของนิสิต ขอให้นิสิตใช้ข้อมูลดังกล่าวให้เป็นประโยชน์สำหรับการวางแผนเพื่อให้ผลการเรียนเป็นตามที่คาดหวัง"),
                                                 br(),
                                                 sliderInput("final", "Assume your final score (0-25 points)",min=0,max = 25, value = 1 ,step=1, width = "50%"),
                                                 actionButton("go2", "Calculate"),
                                                 h1(strong(verbatimTextOutput("MP"))))
                                                 ))
    
                            )

                                      ))   
                            
                          ) 
                         ) )  )


#Server part
server<-function(input, output,session){

  
  output$chula <- renderImage({
        list(
        src = "logo.png",
        filetype = "image/png",
        alt = "chula",
        width = "425px",
        height = "141px"
      )
  }, deleteFile = FALSE)

  mid.s<-reactive({ 
    s<-filter(dat6322, id==input$idst)
    paste0(unlist(s$mid))
  })
  filterid<-reactive({ 
    filter(dat6322, id==input$idst)
    
  })
  
  observeEvent(input$top2,{updateTabsetPanel(session, "inTabset", selected = "panel2")
  })
  
  output$student<-renderText({
    s<-filter(dat6322, id==input$idst)
    paste("ชื่อนิสิต : ", unlist(s$name),unlist(s$lname))
  })
  output$student2<-renderText({
    s<-filter(dat6322, id==input$idst)
    paste("ชื่อนิสิต : ", unlist(s$name),unlist(s$lname))
  })
  
  output$idstudent<-renderText({
    paste("รหัสประจำตัวนิสิต : ", input$idst)
  })
  output$idstudent2<-renderText({
    paste("รหัสประจำตัวนิสิต : ", input$idst)
  })
  
  output$plot<-renderPlot({
    x<-dat6322$mid
    bins<-seq(min(x), max(x), length.out=input$bins +1)
    hist(x, breaks=bins, xlim=c(0,25), col="#F58C43",xlab = "Midterm score", ylab = "Frequency",main = NA, cex.lab=1.5)
  })
  
  output$des<-renderTable({A<-describe(dat6322$mid)
  A[c(-1,-6,-7,-10)]
  })
  
  output$point<-renderText({
    mid.s()
  })
  output$point2<-renderText({
    mid.s()
  })
  
  
  output$dplot<-renderPlot({
    if(input$getdot>0){ggplot(dat6322) + geom_dotplot(aes(mid) ,
                                                      alpha=1.5, 
                                                      binwidth = input$dotsize, 
                                                      fill=ifelse(dat6322$id==input$idst, "Red", "#F58C43")) + labs(x="Midterm score", y="Count")+theme(axis.text=element_text(size=15),
                                                        axis.title=element_text(size=20),axis.text.y=element_blank())}
    else{ggplot(dat6322) + geom_dotplot(aes(mid) ,
                                        alpha=1.5, 
                                        binwidth = input$dotsize, 
                                        fill="#F58C43") + labs(x="Midterm score",y="Count")+theme(axis.text=element_text(size=15),axis.title=element_text(size=20),axis.text.y=element_blank())}
    
  })
  output$bplot<-renderPlot({
    if(input$getdotb>0){
      bp<-dat6322$mid
      boxplot(bp,horizontal = TRUE, col="#F58C43", xlab = "Midterm score",cex.lab=2)
      stripchart(bp, 
                 method = input$pattern, 
                 cex = input$dotsizeb,
                 pch  = 19,
                 col =  "#9A928E", 
                 add=TRUE)
      stripchart(filterid()$mid, 
                 method = input$pattern,
                 pch  = 4,
                 cex  = 4,
                 lwd  = 4,
                 col  = "red", 
                 add = TRUE)}
    else{
      bp<-dat6322$mid
      boxplot(bp,horizontal = TRUE, col="#F58C43", xlab = "Midterm score",cex.lab=2)
      stripchart(bp, 
                 method = input$pattern,
                 cex = input$dotsizeb,
                 pch  = 19,
                 col =  "#9A928E", 
                 add=TRUE)}
  })
  #Page 3
  
  stData<-reactive({
    data.frame(
      att = (input$atd/input$allatd)*5,
      prac = (input$work/input$allwork)*5,
      mid = filterid()$mid,
      stringsAsFactors = FALSE)
    
    
  })
  
  result<-reactive({
    as.character(predict(fitNb,newdata = stData()))})
  
  output$SR<-renderText({
    if (input$go>0) { 
      isolate(paste("result : " ,result())) 
    }       
  })

  stData2<-reactive({
    data.frame(
      att = (input$atd/input$allatd)*5,
      prac = (input$work/input$allwork)*5,
      mid = filterid()$mid,
      score = input$final,
      stringsAsFactors = FALSE)
    
    
  })
  
  result2<-reactive({
    as.character(predict(fitKnn,newdata = stData2()))})
  output$MP<-renderText({
    if (input$go2>0) { 
      isolate(paste("result : " ,result2())) 
    }       
  })
  

}



#create Shiny app
shinyApp(ui=ui, server=server)
