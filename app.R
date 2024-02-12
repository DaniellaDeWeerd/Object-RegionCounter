library(shiny)
library(readr)
library(stringr)
library(dplyr)
library(tidyverse)
library(zip)
 
options(shiny.maxRequestSize = 300 * 1024^2)

ui <- fluidPage(
  titlePanel("Object Counter"),
  sidebarLayout(
    sidebarPanel(
      style = "height: 95vh; overflow-y: auto;", 
      fileInput("left","Please input all Reference Atlas files for the left side:",multiple = TRUE),
      fileInput("right","Please input all Reference Atlas files for the right side:",multiple = TRUE),
      #load in tree
      tags$p("Load both right and left before clicking this please"),
      actionButton("create","Files Loaded"),
      tags$h1(""),
      #download to get blank annotation file
      downloadButton("blankAnnotation","Download blank annotation file"),
      #checkpoint download
      tags$h1(""),
      tags$p("Download all 3 files for the Checkpoint"),
      downloadButton("checkpt1","Download checkpt1"),
      downloadButton("checkpt2","Download checkpt2"),
      downloadButton("checkpt3","Download checkpt3"),
      tags$h1(""),
      #input checkpoint
      fileInput("reLoad","Input all checkpoint files here",multiple = TRUE),
      #load in annotation file
      fileInput("annotation","Please input the csv file that has the annotation information"),
      # selectizeInput("search","Please choose the marker you would like to create your data on",choices=c("Don't select yet"),multiple = F,options = list(create = FALSE)),
      # actionButton("run","Marker is Above"),
      # downloadButton("ZipPercentage","Download Zip file of all percentage outputs"),
      
      #Raw Counts of all combinations of markers (Grab markers by getting unique of ones without seperator)
      tags$h1(""),
      tags$h1(""),
      actionButton("allCombo","Get counts of all combinations"),
      downloadButton("ZipAll","Download Zip file of all count outputs"),
      
      #Raw Area of all all combinations of markers
      tags$h1(""),
      tags$h1(""),
      actionButton("allArea","Get region area of all combinations"),
      downloadButton("ZipArea","Download Zip file of all region area outputs"),
      
      #Region percent
    ),
    mainPanel(
      textOutput("print"),
      tableOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  vals <- reactiveValues(
    right = data.frame(matrix(nrow = 0, ncol = 13)),
    left = data.frame(matrix(nrow = 0, ncol = 13)),
    both = data.frame(matrix(nrow = 0, ncol = 13)),
  )
  
  observeEvent(input$left,{
    the_files <- input$left
    names <- the_files$name
    paths <- the_files$datapath
    for (i in 1:length(paths)){
      toNormal <- read_delim(paths[i],delim = ";", escape_double = FALSE, trim_ws = TRUE)
      toNormal <- toNormal[,1:10]
      name <- names[i]
      for (j in 2:length(toNormal[,1])) {
        name <- rbind(name,name)
      }
      side <- rep("left", length(toNormal[,1]))
      toAdd <- cbind(name,side,toNormal)
      vals$both <- rbind(vals$both,toAdd)
      vals$left <- rbind(vals$left,toAdd)
    }
    initialLeft <<- vals$left
    output$print <- renderText({
      print("Done Left")
    })
  })
  
  observeEvent(input$right,{
    the_files <- input$right
    names <- the_files$name
    paths <- the_files$datapath
    for (i in 1:length(paths)){
      toNormal <- read_delim(paths[i],delim = ";", escape_double = FALSE, trim_ws = TRUE)
      toNormal <- toNormal[,1:10]
      name <- names[i]
      for (j in 2:length(toNormal[,1])) {
        name <- rbind(name,name)
      }
      side <- rep("right", length(toNormal[,1]))
      toAdd <- cbind(name,side,toNormal)
      vals$both <- rbind(vals$both,toAdd)
      vals$right <- rbind(vals$right,toAdd)
    }
    
    output$print <- renderText({
      print("Done Right")
    })
  })
  
  observeEvent(input$create,{
    afterLoad <<- vals$both
    # both <<- afterLoad
    
    keepCols <- c("name","side","Region ID","Region Name","Object count","Region area")
    vals$both <- vals$both[,keepCols]
    keepRows <- c(320,943,648,844,882,656,962,767,1021,1085,935,211,1015,919,927,810,819,588,296,772)
    vals$both <- vals$both[vals$both$`Region ID` %in% keepRows,]
    layer6 <- list(c(844,882),c(1021,1085),c(919,927),c(810,819))
    names <- unique(vals$both$name)
    check1 <<- vals$both
    for (name in names){
      current <- vals$both[vals$both$name == name,]
      for (i in 1:length(layer6)){
        pair <- layer6[[i]]
        #Count
        Lsegment <- current[current$`Region ID` %in% pair & current$side == "left",]
        if(all(is.na(Lsegment$`Object count`)) == F){
          Lsum <- sum(Lsegment$`Object count`)
          vals$both[vals$both$name == name & vals$both$`Region ID` %in% pair & vals$both$side == "left",'Object count'] <- Lsum
        }
        Rsegment <- current[current$`Region ID` %in% pair & current$side == "right",]
        if(all(is.na(Rsegment$`Object count`)) == T){
          Rsum <- sum(Rsegment$`Object count`)
          vals$both[vals$both$name == name & vals$both$`Region ID` %in% pair & vals$both$side == "right",'Object count'] <- Rsum
        }
        #Area
        Lsegment <- current[current$`Region ID` %in% pair & current$side == "left",]
        if(all(is.na(Lsegment$`Region area`)) == F){
          Lsum <- sum(Lsegment$`Region area`)
          vals$both[vals$both$name == name & vals$both$`Region ID` %in% pair & vals$both$side == "left",'Region area'] <- Lsum
        }
        Rsegment <- current[current$`Region ID` %in% pair & current$side == "right",]
        if(all(is.na(Rsegment$`Region area`)) == F){
          Rsum <- sum(Rsegment$`Region area`)
          vals$both[vals$both$name == name & vals$both$`Region ID` %in% pair & vals$both$side == "right",'Region area'] <- Rsum
        }
      }
    }
    removeRows <- c(882,1085,927,819)
    vals$both <- vals$both[!vals$both$`Region ID` %in% removeRows,]
    check2 <<- vals$both
    match <- list(c(935,588),c(211,296),c(1015,772),c(919,810))
    for (name in names){
      current <- vals$both[vals$both$name == name,]
      for (i in 1:length(match)){
        pair <- match[[i]]
        #Count
        Lsegment <- current[current$`Region ID` %in% pair & current$side == "left",]
        if(all(is.na(Lsegment$`Object count`)) == F){
          Lsum <- sum(Lsegment$`Object count`)
          vals$both[vals$both$name == name & vals$both$`Region ID` %in% pair & vals$both$side == "left",'Object count'] <- Lsum
        }
        Rsegment <- current[current$`Region ID` %in% pair & current$side == "right",]
        if(all(is.na(Rsegment$`Object count`)) == F){
          Rsum <- sum(Rsegment$`Object count`)
          vals$both[vals$both$name == name & vals$both$`Region ID` %in% pair & vals$both$side == "right",'Object count'] <- Rsum
        }
        #Area
        Lsegment <- current[current$`Region ID` %in% pair & current$side == "left",]
        if(all(is.na(Lsegment$`Region area`)) == F){
          Lsum <- sum(Lsegment$`Region area`)
          vals$both[vals$both$name == name & vals$both$`Region ID` %in% pair & vals$both$side == "left",'Region area'] <- Lsum
        }
        Rsegment <- current[current$`Region ID` %in% pair & current$side == "right",]
        if(all(is.na(Rsegment$`Region area`)) == F){
          Rsum <- sum(Rsegment$`Region area`)
          vals$both[vals$both$name == name & vals$both$`Region ID` %in% pair & vals$both$side == "right",'Region area'] <- Rsum
        }
      }
    }
    removeVentral <- c(810,588,296,772)
    vals$both <- vals$both[! vals$both$`Region ID` %in% removeVentral & ! vals$both$`Region ID` %in% removeRows,]
    vals$both <- as.data.frame(sapply(vals$both,gsub,pattern="6a",replacement="6"))
    vals$both <- as.data.frame(sapply(vals$both,gsub,pattern="dorsal part,",replacement=""))
    check3 <<- vals$both
    both <<- vals$both
    
    #LEFT
    beforeSeg <<- vals$left
    keepCols <- c("name","side","Region ID","Region Name","Object count","Region area")
    vals$left <- vals$left[,keepCols]
    keepRows <- c(320,943,648,844,882,656,962,767,1021,1085,935,211,1015,919,927,810,819,588,296,772)
    vals$left <- vals$left[vals$left$`Region ID` %in% keepRows,]
    layer6 <- list(c(844,882),c(1021,1085),c(919,927),c(810,819))
    names <- unique(vals$left$name)
    checkLeft <<- vals$left
    left <- vals$left
    for (name in names){
      current <- left[left$name == name,]
      for (i in 1:length(layer6)){
        pair <- layer6[[i]]
        #Count
        Lsegment <- current[current$`Region ID` %in% pair & current$side == "left",]
        if(all(is.na(Lsegment$`Object count`)) == F){
          Lsum <- sum(Lsegment$`Object count`)
          left[left$name == name & left$`Region ID` %in% pair & left$side == "left",'Object count'] <- Lsum
        }
        #Area
        Lsegment <- current[current$`Region ID` %in% pair & current$side == "left",]
        if(all(is.na(Lsegment$`Region area`)) == F){
          Lsum <- sum(Lsegment$`Region area`)
          left[left$name == name & left$`Region ID` %in% pair & left$side == "left",'Region area'] <- Lsum
        }
      }
    }
    removeRows <- c(882,1085,927,819)
    left <- left[!left$`Region ID` %in% removeRows,]
    vals$left <- left
    left <- vals$left
    checkLeft2 <<- vals$left
    match <- list(c(935,588),c(211,296),c(1015,772),c(919,810))
    name <- names[45]
    for (name in names){
      current <- left[left$name == name,]
      for (i in 1:length(match)){
        pair <- match[[i]]
        #Count
        Lsegment <- current[current$`Region ID` %in% pair & current$side == "left",]
        if(all(is.na(Lsegment$`Object count`)) == F){
          Lsum <- sum(Lsegment$`Object count`)
          left[left$name == name & left$`Region ID` %in% pair & left$side == "left",'Object count'] <- Lsum
        }
        #Area
        Lsegment <- current[current$`Region ID` %in% pair & current$side == "left",]
        if(all(is.na(Lsegment$`Region area`)) == F){
          Lsum <- sum(Lsegment$`Region area`)
          left[left$name == name & left$`Region ID` %in% pair & left$side == "left",'Region area'] <- Lsum
        }
      }
    }
    removeVentral <- c(810,588,296,772)
    Left1 <<- vals$left
    vals$left <- left
    vals$left <- vals$left[! vals$left$`Region ID` %in% removeVentral & ! vals$left$`Region ID` %in% removeRows,]
    Left2 <<- vals$left
    vals$left <- as.data.frame(sapply(vals$left,gsub,pattern="6a",replacement="6"))
    Left3 <<- vals$left
    vals$left <- as.data.frame(sapply(vals$left,gsub,pattern="dorsal part,",replacement=""))
    checkLeft3 <<- vals$left
    left <<- vals$left
    
    #RIGHT
    keepCols <- c("name","side","Region ID","Region Name","Object count","Region area")
    vals$right <- vals$right[,keepCols]
    keepRows <- c(320,943,648,844,882,656,962,767,1021,1085,935,211,1015,919,927,810,819,588,296,772)
    vals$right <- vals$right[vals$right$`Region ID` %in% keepRows,]
    layer6 <- list(c(844,882),c(1021,1085),c(919,927),c(810,819))
    names <- unique(vals$right$name)
    check1 <<- vals$right
    for (name in names){
      current <- vals$right[vals$right$name == name,]
      for (i in 1:length(layer6)){
        pair <- layer6[[i]]
        #Count
        Rsegment <- current[current$`Region ID` %in% pair & current$side == "right",]
        if(all(is.na(Rsegment$`Object count`)) == F){
          Rsum <- sum(Rsegment$`Object count`)
          vals$right[vals$right$name == name & vals$right$`Region ID` %in% pair & vals$right$side == "right",'Object count'] <- Rsum
        }
        #Area
        Rsegment <- current[current$`Region ID` %in% pair & current$side == "right",]
        if(all(is.na(Rsegment$`Region area`)) == F){
          Rsum <- sum(Rsegment$`Region area`)
          vals$right[vals$right$name == name & vals$right$`Region ID` %in% pair & vals$right$side == "right",'Region area'] <- Rsum
        }
      }
    }
    removeRows <- c(882,1085,927,819)
    vals$right <- vals$right[!vals$right$`Region ID` %in% removeRows,]
    check2 <<- vals$right
    match <- list(c(935,588),c(211,296),c(1015,772),c(919,810))
    for (name in names){
      current <- vals$right[vals$right$name == name,]
      for (i in 1:length(match)){
        pair <- match[[i]]
        #Count
        Rsegment <- current[current$`Region ID` %in% pair & current$side == "right",]
        if(all(is.na(Rsegment$`Object count`)) == F){
          Rsum <- sum(Rsegment$`Object count`)
          vals$right[vals$right$name == name & vals$right$`Region ID` %in% pair & vals$right$side == "right",'Object count'] <- Rsum
        }
        #Area
        Rsegment <- current[current$`Region ID` %in% pair & current$side == "right",]
        if(all(is.na(Rsegment$`Region area`)) == F){
          Rsum <- sum(Rsegment$`Region area`)
          vals$right[vals$right$name == name & vals$right$`Region ID` %in% pair & vals$right$side == "right",'Region area'] <- Rsum
        }
      }
    }
    removeVentral <- c(810,588,296,772)
    vals$right <- vals$right[! vals$right$`Region ID` %in% removeVentral & ! vals$right$`Region ID` %in% removeRows,]
    vals$right <- as.data.frame(sapply(vals$right,gsub,pattern="6a",replacement="6"))
    vals$right <- as.data.frame(sapply(vals$right,gsub,pattern="dorsal part,",replacement=""))
    
    
    check3 <<- vals$both
    both <<- vals$both
    left <<- vals$left
    right <<- vals$right
    
    output$checkpt1 <- downloadHandler(
      filename = function() {
        "both.rds"
      },
      content = function(file) {
        saveRDS(both, file = file)
      }
    )
    output$checkpt2 <- downloadHandler(
      filename = function() {
        "left.rds"
      },
      content = function(file) {
        saveRDS(left, file = file)
      }
    )
    output$checkpt3 <- downloadHandler(
      filename = function() {
        "right.rds"
      },
      content = function(file) {
        saveRDS(right, file = file)
      }
    )
    
    table = vals$both %>% group_by(name,side)  %>%
      summarise()
    table1 <- cbind (table, mouse=NA,sex = NA,treatment=NA,mpi=NA,genotype=NA,marker=NA,include="Y")
    output$blankAnnotation <- downloadHandler(
      filename = function() {
        paste0("annotation", ".csv")
      },
      content = function(file) {
        write.csv(table1, file ,row.names = FALSE)
        print("Done Downloading Blank Annotation")
      }
    )
    output$print <- renderText({
      print("Done modifying data and creating Blank Annotation")
    })
    
  })
  
  #Load AfterLoad.RData
  observeEvent(input$reLoad,{
    the_files <- input$reLoad
    paths <- the_files$datapath
    names <- the_files$name
    if (length(paths) != 3) {
      output$print <- renderText({
        print("You didn't upload the three files required. Please try again")
      })
    }
    else {
      for (i in 1:length(paths)) {
        path <- paths[i]
        name <- names[i]
        print(name)
        if (grepl("both", name) == T | grepl("checkpt1", name) == T) {
          both <<- readRDS(path)
        }
        else if (grepl("left", name) == T | grepl("checkpt2", name) == T) {
          left <<- readRDS(path)
        }
        else if (grepl("right", name) == T| grepl("checkpt3", name) == T) {
          right <<- readRDS(path)
        }
        
        else {
          print("wrong files!!")
        }
      }
      output$print <- renderText({
        print("Done uploading checkpoint files here")
      })
    }
    checkL <<- left
    checkR <<- right
    checkB <<- both
  })
  
  observeEvent(input$annotation,{
    #read in annotation file
    file <- input$annotation
    annotation <<- read.csv(file$datapath)
    both1 <<- both
    left1 <<- left
    right1 <<- right
    marker <- c()
    mouse <- c()
    treatment <- c()
    mpi <- c()
    genotype <- c()
    include <- c()
    sex <- c()
    #clean up the file
    colnames(annotation) <- c("name","side","mouse","sex","treatment","mpi","genotype","marker","include")
    annotation <- annotation[annotation[,1] !=  "name",]
    
    for (i in 1:length(both$name)){
      name <- both$name[i]
      side <- both$side[i]
      ### MARKER ###
      mark <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "marker"]
      if (length(mark) == 0) {mark <- NA}
      marker <- c(marker,mark)
      ### MOUSE ###
      mou <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "mouse"]
      if (length(mou) == 0) {mou <- NA}
      mouse <- c(mouse,mou)
      ### TREATMENT ###
      treat <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "treatment"]
      if (length(treat) == 0) {treat <- NA}
      treatment <- c(treatment,treat)
      ### SEX ###
      se <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "sex"]
      if (length(treat) == 0) {treat <- NA}
      sex <- c(sex,se)
      ### MPI ###
      mp <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "mpi"]
      if (length(mp) == 0) {mp <- NA}
      mpi <- c(mpi,mp)
      ### GENOTYPE ###
      geno <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "genotype"]
      if (length(geno) == 0) {geno <- NA}
      genotype <- c(genotype,geno)
      ### INCLUDE ###
      inc <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "include"]
      if (length(inc) == 0) {inc <- NA}
      include <- c(include,inc)
    }
    both$marker <- marker
    both$mouse <- mouse
    both$sex <- sex
    both$treatment <- treatment
    both$mpi <- mpi
    both$genotype <- genotype
    both$include <- include
    both <- both[both$include == "Y",]
    Check5 <<- both
    
    print("here1")
    #LEFT
    marker <- c()
    mouse <- c()
    treatment <- c()
    mpi <- c()
    genotype <- c()
    include <- c()
    sex <- c()
    colnames(annotation) <- c("name","side","mouse","sex","treatment","mpi","genotype","marker","include")
    annotation <- annotation[annotation[,1] !=  "name",]
    for (i in 1:length(left$name)){
      name <- left$name[i]
      side <- left$side[i]
      ### MARKER ###
      mark <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "marker"]
      if (length(mark) == 0) {mark <- NA}
      marker <- c(marker,mark)
      ### MOUSE ###
      mou <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "mouse"]
      if (length(mou) == 0) {mou <- NA}
      mouse <- c(mouse,mou)
      ### TREATMENT ###
      treat <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "treatment"]
      if (length(treat) == 0) {treat <- NA}
      treatment <- c(treatment,treat)
      ### SEX ###
      se <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "sex"]
      if (length(treat) == 0) {treat <- NA}
      sex <- c(sex,se)
      ### MPI ###
      mp <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "mpi"]
      if (length(mp) == 0) {mp <- NA}
      mpi <- c(mpi,mp)
      ### GENOTYPE ###
      geno <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "genotype"]
      if (length(geno) == 0) {geno <- NA}
      genotype <- c(genotype,geno)
      ### INCLUDE ###
      inc <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "include"]
      if (length(inc) == 0) {inc <- NA}
      include <- c(include,inc)
    }
    left$marker <- marker
    left$mouse <- mouse
    left$sex <- sex
    left$treatment <- treatment
    left$mpi <- mpi
    left$genotype <- genotype
    left$include <- include
    left <- left[left$include == "Y",]
    
    #RIGHT
    marker <- c()
    mouse <- c()
    treatment <- c()
    mpi <- c()
    genotype <- c()
    include <- c()
    sex <- c()
    colnames(annotation) <- c("name","side","mouse","sex","treatment","mpi","genotype","marker","include")
    annotation <- annotation[annotation[,1] !=  "name",]
    for (i in 1:length(right$name)){
      name <- right$name[i]
      side <- right$side[i]
      ### MARKER ###
      mark <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "marker"]
      if (length(mark) == 0) {mark <- NA}
      marker <- c(marker,mark)
      ### MOUSE ###
      mou <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "mouse"]
      if (length(mou) == 0) {mou <- NA}
      mouse <- c(mouse,mou)
      ### TREATMENT ###
      treat <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "treatment"]
      if (length(treat) == 0) {treat <- NA}
      treatment <- c(treatment,treat)
      ### SEX ###
      se <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "sex"]
      if (length(treat) == 0) {treat <- NA}
      sex <- c(sex,se)
      ### MPI ###
      mp <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "mpi"]
      if (length(mp) == 0) {mp <- NA}
      mpi <- c(mpi,mp)
      ### GENOTYPE ###
      geno <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "genotype"]
      if (length(geno) == 0) {geno <- NA}
      genotype <- c(genotype,geno)
      ### INCLUDE ###
      inc <- annotation[annotation$name == name & annotation$side == side,colnames(annotation) == "include"]
      if (length(inc) == 0) {inc <- NA}
      include <- c(include,inc)
    }
    right$marker <- marker
    right$mouse <- mouse
    right$sex <- sex
    right$treatment <- treatment
    right$mpi <- mpi
    right$genotype <- genotype
    right$include <- include
    right <- right[right$include == "Y",]
    
    options <- unique(both[!str_detect(both$marker, "_"),"marker"])
    if (is.list(options)) {
      options <- options[[1]]
    }
    updateSelectizeInput(session, 'search', "Please choose the marker you would like to create your data on", choices = options,options = list(create = F))
    
    both <<- both
    left <<- left
    right <<- right
    
    output$print <- renderText({
      print("Done merging in Annotation")
    })
    
  })
  
  observeEvent(input$allCombo,{
    both$`Object count` <- as.numeric(both$`Object count`)
    left$`Object count` <- as.numeric(left$`Object count`)
    right$`Object count` <- as.numeric(right$`Object count`)
    
    check <<- both
    markers <- unique(check$marker)
    i <- 1
    for (i in 1:length(markers)){
      mark <- markers[i]
      part <- check[check$marker == mark,c("Region Name","mouse","Object count")]
      attempt <- part %>%
        pivot_wider(names_from=c(`Region Name`), 
                    values_from= `Object count`,
                    values_fn = sum)
      attempt <- data.frame(attempt)
      rownames(attempt) <- attempt$mouse
      attempt <- attempt[,-1]
      write.csv(attempt, paste0(mark,"-both-count.csv",sep = ""))
    }
    
    #LEFT
    check <<- left
    markers <- unique(check$marker)
    i <- 3
    for (i in 1:length(markers)){
      mark <- markers[i]
      part <- check[check$marker == mark,c("Region Name","mouse","Object count")]
      attempt <- part %>%
        pivot_wider(names_from=c(`Region Name`), 
                    values_from= `Object count`,
                    values_fn = sum)
      attempt <- data.frame(attempt)
      rownames(attempt) <- attempt$mouse
      attempt <- attempt[,-1]
      write.csv(attempt, paste0(mark,"-left-count.csv",sep = ""))
    }
    
    #RIGHT
    check <<- right
    markers <- unique(check$marker)
    i <- 1
    for (i in 1:length(markers)){
      mark <- markers[i]
      part <- check[check$marker == mark,c("Region Name","mouse","Object count")]
      attempt <- part %>%
        pivot_wider(names_from=c(`Region Name`), 
                    values_from= `Object count`,
                    values_fn = sum)
      attempt <- data.frame(attempt)
      rownames(attempt) <- attempt$mouse
      attempt <- attempt[,-1]
      write.csv(attempt, paste0(mark,"-right-count.csv",sep = ""))
    }
    
    
    Zip_Files <<- list.files(pattern = "count.csv$")
    output$ZipAll <- downloadHandler(
      filename = function() {
        "CountsZip.zip"
      },
      content = function(file) {
        zip::zip(zipfile = file, files = Zip_Files)
        do.call(file.remove, list(Zip_Files))
        output$print <- renderText({
          print("done downloading counts zip")
        })
      }
    )
    
    output$print <- renderText({
      print("Done creating all counts files and zip")
    })
    
  })
  
  observeEvent(input$allArea,{
    both$`Region area` <- as.numeric(both$`Region area`)
    left$`Region area` <- as.numeric(left$`Region area`)
    right$`Region area` <- as.numeric(right$`Region area`)
    print("here1")
    
    check1 <<- both
    markers <<- unique(check1$marker)
    for (i in 1:length(markers)){
      mark <- markers[i]
      part <- check1[check1$marker == mark,c("Region Name","mouse","Region area")]
      print("here1.5")
      attempt <- part %>%
        pivot_wider(names_from=c(`Region Name`), 
                    values_from= `Region area`,
                    values_fn = sum)
      attempt <- data.frame(attempt)
      print("here1.75")
      rownames(attempt) <- attempt$mouse
      attempt <- attempt[,-1]
      write.csv(attempt, paste0(mark,"-both-region_area.csv",sep = ""))
    }
    print("here2")
    
    #LEFT
    check1 <<- left
    markers <- unique(check1$marker)
    i <- 1
    for (i in 1:length(markers)){
      mark <- markers[i]
      part <- check1[check1$marker == mark,c("Region Name","mouse","Region area")]
      attempt <- part %>%
        pivot_wider(names_from=c(`Region Name`), 
                    values_from= `Region area`,
                    values_fn = sum)
      attempt <- data.frame(attempt)
      rownames(attempt) <- attempt$mouse
      attempt <- attempt[,-1]
      write.csv(attempt, paste0(mark,"-left-region_area.csv",sep = ""))
    }
    print("here3")
    
    #RIGHT
    check1 <<- right
    markers <- unique(check1$marker)
    i <- 1
    for (i in 1:length(markers)){
      mark <- markers[i]
      part <- check1[check1$marker == mark,c("Region Name","mouse","Region area")]
      attempt <- part %>%
        pivot_wider(names_from=c(`Region Name`), 
                    values_from= `Region area`,
                    values_fn = sum)
      attempt <- data.frame(attempt)
      rownames(attempt) <- attempt$mouse
      attempt <- attempt[,-1]
      write.csv(attempt, paste0(mark,"-right-region_area.csv",sep = ""))
    }
    
    Zip_Files <<- list.files(pattern = "region_area.csv$")
    
    output$ZipArea <- downloadHandler(
      filename = function() {
        "AreaZip.zip"
      },
      content = function(file) {
        zip::zip(zipfile = file, files = Zip_Files)
        do.call(file.remove, list(Zip_Files))
        output$print <- renderText({
          print("done downloading area zip")
        })
      }
    )
    
    output$print <- renderText({
      print("Done creating all Area files and zip")
    })
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
