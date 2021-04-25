
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
kitchk<-function(pa,pb,pc){
  if (length(pa)==1){            #length of sides as value a,b,c
    a=pa
    b=pb
    c=pc
  }else{                        #position(xa,ya),(xb,yb),...
    a=sqrt(sum((pc-pb)^2))
    b=sqrt(sum((pa-pc)^2))
    c=sqrt(sum((pa-pb)^2))
  }
  if ((a+b)<=c | abs(a-b)>=c){
    Return("Not a Triangle")
  }
  pe=a+b+c                          #!!!perimetre
  p=pe/2
  area=sqrt(p*(p-a)*(p-b)*(p-c))       #!!!area
  if(pe>7.9){
    pchk="perimeter too large"
  }else if (pe<4.0){
    pchk="perimeter too small"
  }else{
    pchk="perimeter OK"
  }
  si=c(a,b,c)
  name=c("a","b","c")
  schk=NULL
  for (i in (1:3)){
    if (si[i]>(2.7)){
      schk[i]="too long"
    }else if(si[i]<1.2){
      schk[i]="too short"
    }else{
      schk[i]="OK"
    }
    schk[i]=paste("side",name[i],schk[i])
  }
  ds=1.95-si#Indicator of difference from the average equilateral
  est=sqrt(sum(ds)^2)#Deviation
  trilist=list("Side"=si,"Side.Check"=schk,"Perimeter"=pe,
               "Perimeter.Check"=pchk)
  geolist=list("Indicator.From.Avg"=ds,"Deviation.From.Avg"=est,"Working.Area"=area)
  return(list("SidesInfo"=trilist,"OtherInfo"=geolist))
}
#-----
sL<-function(pa,pb,pc){
  sLa<<-sqrt(sum((pc-pb)^2))
  sLb<<-sqrt(sum((pa-pc)^2))
  sLc<<-sqrt(sum((pa-pb)^2))
}
#sLa<-function(pa,pb,pc) sLa<<-sqrt(sum(pc-pb)^2)
#sLb<-function(pa,pb,pc) sLb<<-sqrt(sum(pa-pc)^2)
#sLc<-function(pa,pb,pc) sLc<<-sqrt(sum(pa-pb)^2)

pL<-function(pa,pb,pc){
  pLo<<-sum(sqrt(sum((pc-pb)^2)),sqrt(sum((pa-pc)^2)),sqrt(sum((pa-pb)^2)))
}

#triValid<-function(pa,pb,pc){
#  if (sizeLength(pa,pb,pc)[1]+sizeLength[2][3])
#}

KtriValid_c<-function(pa,pb,pc){
  sL(pa,pb,pc)
  pL(pa,pb,pc)
  ifelse(sLa<2.7&sLb<2.7&sLa>1.2&sLb>1.2&sLc>1.2&sLc<2.7
         &sLa+sLb>sLc&abs(sLa-sLb)<sLc&pLo>4.0&pLo<7.9,TRUE,FALSE)
}
#plot(c(0,5),c(0,5),type = 'n')

PlotValid<-function(pa,pb){
  #points(c(pa[1],pb[1]),c(pa[2],pb[2]),col = 'dark red')
  #abline(v=c(pa[1],pb[1]),untf = F)
  #abline(h=c(pa[2],pb[2]),untf=F)
  for (x in seq(from = 0, to = 5, by = .1)){
    for (y in seq(from = 0, to = 5, by = .1)){
      if (KtriValid_c(pa,pb,c(x,y))==TRUE){
        points(x,y,col = 'green')
      }
    }
  }
}

#PlotValid(c(0.3,2.1),c(1.5,1.5))
plotValid<-function(pa,pb,pc,target){
  if(target == '--'){
    title('Kitchen Layout')
  }
  if(target == 'A'){
    title('Kitchen Layout with Valid Range of Point A')
    PlotValid(pb,pc)
    abline(h=pa[2])
    abline(v=pa[1])
  }
  if(target == 'B'){
    title('Kitchen Layout with Valid Range of Point B')
    PlotValid(pa,pc)
    abline(h=pb[2])
    abline(v=pb[1])
  }
  if(target == 'C'){
    title('Kitchen Layout with Valid Range of Point C')
    PlotValid(pa,pb)
    abline(h=pc[2])
    abline(v=pc[1])
  }
}
#-------
shinyServer(function(input, output) {
  
  output$KitchenInfo <- renderTable({
    print(kitchk(c(input$Xa/1000,input$Ya/1000),
                 c(input$Xb/1000,input$Yb/1000),c(input$Xc/1000,
                                                  input$Yc/1000
                 ))[[1]])
  })
  output$KitchenInfo2 <- renderTable({
    print(kitchk(c(input$Xa/1000,input$Ya/1000),
                 c(input$Xb/1000,input$Yb/1000),
                 c(input$Xc/1000,input$Yc/1000))[[2]])
  })
  
  output$IntPlot <- renderPlot({
    plot(c(0,5),c(0,5),asp=1,type="n")
    points(c(input$Xa/1000,input$Xb/1000,input$Xc/1000),
         c(input$Ya/1000,input$Yb/1000,input$Yc/1000),
         type="b",col="red")
    points(c(input$Xa/1000,input$Xc/1000),
           c(input$Ya/1000,input$Yc/1000),type="l",
           col="red")
    plotValid(c(input$Xa/1000,input$Ya/1000),
              c(input$Xb/1000,input$Yb/1000),
              c(input$Xc/1000,input$Yc/1000),input$target_point)
  })
  #output$IntPlot2 <- renderPlot({
   # kitRan(kitchk(c(input$Xa/1000,input$Ya/1000),
    #              c(input$Xb/1000,input$Yb/1000),
     #             c(input$Xc/1000,input$Yc/1000))$SidesInfo$Side[input$base],input$base)
  #})
  
  
  
  #--Select&download datas--------
  datasetInput <- reactive({
    switch(input$dataset,
           "Basic Geom" = kitchk(c(input$Xa/1000,input$Ya/1000),
                                 c(input$Xb/1000,input$Yb/1000),
                                 c(input$Xc/1000,input$Yc/1000))[[1]],
           "Rating" = kitchk(c(input$Xa/1000,input$Ya/1000),
                             c(input$Xb/1000,input$Yb/1000),
                             c(input$Xc/1000,input$Yc/1000))[[2]],
           "All" = kitchk(c(input$Xa/1000,input$Ya/1000),
                          c(input$Xb/1000,input$Yb/1000),
                          c(input$Xc/1000,input$Yc/1000)))
  })
  # Table of selected dataset ----
  output$table <- renderTable({
    datasetInput()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$concept_name,' ',input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
})
