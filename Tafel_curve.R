#Initializing the script
require(readr,readxl)->requeriments
if(requeriments==F){
  var.control<-readline("Install miising packages for run the script?[y/n] ")
  if(var.control=="y"){
    install.packages("readr","readxl")
  }
}
rm(requeriments)
print("R routine for analysing ciclic voltametric data for tafel curves")
print("Created for MafiusKity (my)")
print("Version beta0.3")
print("Press any key to continue...")
readline("")
print("This algorithm assumes that the potential and current are in columns")
print("Disable Capslok")

#Importing data
data_import<-NA;E<-NA;i<-NA
repeat{
  import_method<-menu(c(".xls",".csv or .txt"),
                      title="What is te archive format of the data? (0 to exit)")
  if(import_method==0){break}
  path_data<-readline("File path/URL: ")
  colnames<-readline("First row as names?[y/n] ")
  if(colnames=="n"){coolnames=F}
  else{coolnames=T}
  rm(colnames)
  #Importing .xls
  if(import_method==1){
    shet<-readline("sheet of data: ")
    range_data<-readline("Range of data (Ex. A1:B10): ")
    data_import<-read_excel(path_data,sheet=shet,range = range_data,col_names = 
                              coolnames)
  }
  #Importing .csv and .txt
  if(import_method==2){
    sepa<-readline("What is the separator?: ")
    data_import<-read.csv(path_data,header=coolnames,sep=sepa)
  }
  #Confering import
  ifelse(length(data_import[,1])<=100, print(data_import),print(data_import[1:100,]))
  
  var.control=readline("Has the data been correctly imported?[y/n] ")
  if(var.control=="y"){break}
}
repeat{
  data_import[,as.numeric(readline("potential column number: "))]->E
  data_import[,as.numeric(readline("current column number: "))]->i
  plot(E,i)
  parameters_correctly<-menu(c("Yes","No"),title=
                                 "Have the current and potential parameters been
                               properly set?")
  if(parameters_correctly==1){break}
  else{View(data_import)}
}
data_f<-data.frame(E,i)
rm(E,i)

#Data pre-processing
modify_prepro=function(target,type,data_f){
  if(type==1){
    add<-as.numeric(readline("Constant for add: "))
   data_f[,target]<-data_f[,target]+add 
  }
  if(type==2){
    mult<-as.numeric(readline("Multiplication constant: "))
    data_f[,target]<-data_f[,target]*mult
  }
  return(data_f)
}
repeat{
  prepro<-menu(c("Yes","No"),title="Will the data be modified?")
  if(prepro==1){
    prepro_target<-menu(c("Potential","Current"))
    prepro_type<-menu(c("Add constant","Multiply by constant"),
                title="Type of modification")
    modify_prepro(prepro_target,prepro_type,data_f)->data_f
    print(data_f)
  }
    else {break}
}

#Tafel
tafel_plot<-NA
tafel_data<-NA
data_f2<-NA
repeat{
  plot(data_f[,1],data_f[,2])
  mult_cycle<-menu(c("Yes","No"),title="Does the voltammogram show multiple cycles?")
  if(mult_cycle==1){
    p_ref<-as.numeric(readline("Select the potential start of the voltametric cyclic: "))
    print(which(round(data_f[,1],2)==round(p_ref,2)))
    a<-as.numeric(readline("Select the position of the potential start of the studied cycle: "))
    b<-as.numeric(readline("Select the position of the potential end of the studied cycle: "))
    plot(data_f[a:b,1],data_f[a:b,2])
    c<-readline("Is the correct cycle on the graph? [y/n]")
    if(c=="y"){
      data_f2<-cbind(data_f[a:b,1],data_f[a:b,2]);break}
  }
  else{
    data_f2<-data_f
    break
  }
}
rm(data_f)
  #Saving voltammetric cycle
readline("Do you want to save this graphic?[y/n]")->svc
if(svc=="y"){
  repeat{
    gm<-readline("Graphic title:")
    xl<-readline("Name of x-axis:")
    yl<-readline("Name of y-axis:")
    plot(data_f2[,1],data_f2[,2],xlab=xl,ylab=yl,main=gm)
    a<-readline("The graph is ok?[y,n]")
    if(a=="y"){
      tiff(filename=gm)
        plot(data_f2[,1],data_f2[,2],xlab=xl,ylab=yl,main=gm)
      dev.off()
      print("Check your directory letter:");system("pwd")
      break}
  }
}
rm(svc)
data_f3<-NA
print("Selecting the cycle interval to perform tafel...")
Sys.sleep(2)
repeat{
  nofp<-menu(c("Oxidative","Reductive"),title="Nature of the process")
  if(nofp==1){
    b<-as.numeric(which.max(data_f2[,2]))
    interval_t<-1:b
  }
  if(nofp==2){
    b<-as.numeric(which.min(data_f2[,2]))
    interval_t<-1:b
  }
  plot(data_f2[interval_t,1],data_f2[interval_t,2])
  z<-as.numeric(readline("Potential start of the process: "))
  if(nofp==1){
    a<-head(which(round(data_f2[,1],1)==round(z,1)),1)}
  if(nofp==2){
    a<-tail(which(round(data_f2[,1],1)==round(z,1)),1) #If the cycle are bad selected, here you will have same problems
  }
  interval_t=a:b
  plot(log10(data_f2[interval_t,2]),data_f2[interval_t,1],main="Tafel_curve")
  c<-readline("Is the graphic good?[y/n]")
  if(c=="y"){
    data_f3<-cbind(data_f2[interval_t,1],log10(data_f2[interval_t,2]))
    #data_f3[,1]==E
    #data_f3[,2]==log(i)
    break
  }
  plot(data_f2[,1],data_f2[,2])
}
  #Derivates?
deriv_pelo<-function(y,x){
  z<-2
  dy<-NA
  xm<-NA
  repeat{
    dy[z-1]<-(y[z]-y[z-1])/(x[z]-x[z-1])
    xm[z-1]<-(x[z]+x[z-1])/2
    z<-z+1
    if(z>length(y)){break}
  }
  return(data.frame(xm,dy))
}
a<-readline("Do you want to derive the graph?[y/n]")
if(a=="y"){
 d1<-deriv_pelo(data_f3[,1],data_f3[,2])
 plot(d1[,1],d1[,2],main="1°Deriv.")
 b<-readline("Do you want to derive the graph again?[y/n]")
 if(b=="y"){
   d2<-deriv_pelo(d1[,2],d1[,1])
   plot(d2[,1],d2[,2],main="2°Deriv.")
 }
}
  #The processes interval:
a<-readline("do you want to do linear regressions?[y/n]")
if(a=="y"){
  j<-as.numeric(readline("How many regridings?: "))
  #data_f3[,1]==E
  #data_f3[,2]==log(i)
  plot(data_f3[,2],data_f3[,1])
  #multiples ablines
  i<-1
  collors<-rainbow(j)
  t.coef=matrix(NA,nrow = j,ncol=5)
  colnames(t.coef)<-c("Collor",'Interseption','Inclination','R2','Interval(in E)')
  abline_interval=as.list(NA)
  repeat{
    t.coef[i,1]<-collors[i]
    Ei<-as.numeric(readline("select the start potential of the line(y axis): "))
    Ee<-as.numeric(readline("select the end potential of the line(y axis): "))
    i_position<-as.numeric(head(which(round(Ei,2)==round(data_f3[,1],2)),1))
    e_position<-as.numeric(tail(which(round(Ee,2)==round(data_f3[,1],2)),1))
    abline_interval[[i]]=i_position:e_position
    rl<-lm(data_f3[abline_interval[[i]],1]~data_f3[abline_interval[[i]],2])
    t.coef[i,2]<-round(rl$coefficients[1],2)
    t.coef[i,3]<-round(rl$coefficients[2],2)
    t.coef[i,4]<-round(summary(rl)$r.square,4)
    t.coef[i,5]<-paste("[",Ei,":",Ee,"]")
    plot(data_f3[,2],data_f3[,1])
    abline(rl,col=collors[i])
    print(t.coef)
    qualy<-readline("Redo the regression?[y/n]")
    if(qualy=="n"){
      i=i+1
      if(j<i){break}
      Sys.sleep(1)
      print("Next regrecion: ")
      }
    if(qualy=="y"){
      plot(data_f3[,2],data_f3[,1])
      print("Redo the regression:")
    }
  }
}
plot(data_f3[,2],data_f3[,1])
if(a=="y"){
  i=1
  while(i<=j){
    rl<-lm(data_f3[abline_interval[[i]],1]~data_f3[abline_interval[[i]],2])
    abline(rl,col=collors[i])
    i=i+1
  }
  print(t.coef)
}
#Saving
save_choice<-menu(c("Graphc","Regressions parameters","All","No"),title="do you want to save any data?")
  #Graph
if(save_choice==1||3){
  graph_name<-readline("What will the graphic be called?")
  xl<-readline("Name of x-axis:")
  yl<-readline("Name of y-axis:")
  i=1
  tiff(filename=graph_name)
    plot(data_f3[,2],data_f3[,1],main=graph_name,xlab=xl,ylab=yl)
    while(i<=j){
      rl<-lm(data_f3[abline_interval[[i]],1]~data_f3[abline_interval[[i]],2])
      abline(rl,col=collors[i])
      i=i+1
    }
  dev.off()
  print("Check your directory:")
  system("pwd")
}
stopifnot(1||3!=save_choice)
  #A table
if(save_choice==2||3){
  print(t.coef)
  print("How do you want to call each row in the table?")
  i=1
  rname=NA
  while(i<=j){
    rn<-readline(paste("Row",i,":"))
    rn->rname[i]
    i=i+1
  }
  rownames(t.coef)<-rname
  tab_name<-readline("Name of archive:")
  write.table(t.coef,file=tab_name,sep="\t")
  print("Check your directory:")
  system("pwd")
}
rm(list=ls())
q(save="no")

