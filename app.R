library(shiny)

ui<- fluidPage(
 
    titlePanel("Hypothesis testing"),
    sidebarLayout(
    sidebarPanel(
      wellPanel(
        radioButtons("dist", "Choose a distribution", choices = c("Z","T"), selected = "Z"),
        numericInput("sig","Significance level",value = 0.05, min=0.0001, max =0.1, step = 0.005)
        
      ),
      wellPanel(
        radioButtons("tails","Tails type",choices = c("Two-tailed"='two',"Left-tailed"='left',"Right-tailed"='right'))
      ),
      wellPanel(
        textInput("tail","Tail Colour",value = "red"),
        textInput("fence","Fence Colour",value = "blue")
      ),
      
      conditionalPanel(
        condition = "input.dist=='T'",
        numericInput("freedom","Degree of Freedom",15)
      ),
      wellPanel(
        downloadButton("download","Download Plot")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot")),
        tabPanel("Debug",verbatimTextOutput("debugger"))
        
      )
    )
  )

)
server<- function(input, output){
  dist<- reactive({
    return(input$dist)
  })
  sig<- reactive({
    return(input$sig)
  })
  tails<- reactive({
    return(input$tails)
  })
  tailcol<- reactive({
    return(input$tail)
  })
  fencecol<- reactive({
    return(input$fence)
  })
  freedom<- reactive({
    return(input$freedom)
  })
  
  dplot<- function(){
    x<- seq(-5,5,0.01)
    if(tails()=="two"){
      alpha<- sig()/2
      tails<- 2
    } else {
      alpha<- sig()
      if(tails()=="left"){
        tails<- -1
      } else {
        tails<- 1
      }
    }
  
  
  if(input$dist=="T"){
    pdf<- dt(x, input$freedom)
    critical<- qt(alpha, input$freedom)
    plottitle<- paste("Rejection region based on",dist(), " distribution and alpha=", formatC(alpha,3), sep = "")
  } else if (input$dist=="Z") {
    pdf<- dnorm(x)
    critical<- qnorm(alpha)
    plottitle<- paste("Rejection region based on",dist(), " distribution and alpha=", formatC(alpha,3), sep = "")
  } else {
    pdf<- dnorm(x)
    critical<- qnorm(aplha)
    plottitle<- paste("Rejection region based on",dist(), " distribution and alpha=", formatC(alpha,3), sep = "")
  }
  
  dd<- data.frame(x,pdf)
  library(ggplot2)
  finalplot<- ggplot(dd, aes(x,pdf))
  finalplot<- finalplot + geom_line() + ggtitle(plottitle)
  
  if(tails==-1||tails==2){
    finalplot<- finalplot + geom_ribbon(data = subset(dd, x<= critical),aes(ymax=pdf),ymin=0,fill=tailcol(), alpha=0.5) + 
      geom_vline(xintercept = critical, colour= fencecol(), linetype="dashed", size=1 )
    plotlabel<- paste("Reject H0 if","\n", "test stat < ",formatC(critical,4),".")
    finalplot<- finalplot + annotate("text", x= -3.75, y=0.10, label= plotlabel)
  }
  
  if(tails==1||tails==2){
    finalplot<- finalplot + geom_ribbon(data = subset(dd, x >= -critical),aes(ymax=pdf),ymin=0,fill=tailcol(), alpha=0.5) + 
      geom_vline(xintercept = -critical, colour= fencecol(), linetype="dashed", size=1 )
    plotlabel<- paste("Reject H0 if","\n", "test stat < ",formatC(-critical,4),".")
    finalplot<- finalplot + annotate("text", x= 3.75, y=0.10, label= plotlabel)
  }

    return(finalplot)
  }
  output$plot<- renderPlot({
    p<- dplot()
    print(p)
  })
  output$debugger<- renderText({
    paste("Dist:",dist(),"Sig Level:",sig(),"Num Tails:",tails())
  })
  
  output$download<- downloadHandler(
    filename = function(){paste(input$dist,'_',input$tails,'_',sig(),'.png',sep = '')},
    content = function(file){
      ggsave(file, plot=doPlot(), device=png, width=800, height=800, limitsize=FALSE)
    }
  )
}

shinyApp(ui =ui, server= server)

