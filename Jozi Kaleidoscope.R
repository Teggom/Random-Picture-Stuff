# install/load packages
if(!("png" %in% installed.packages())){
  install.packages("png")
  library("png")
} else {
  library("png")
}
if(!("jpeg" %in% installed.packages())){
  install.packages("jpeg")
  library("jpeg")
} else {
  library("jpeg")
}
if(!("abind" %in% installed.packages())){
  install.packages("abind")
  library("abind")
} else {
  library("abind")
}


# folder logic
setwd("~/../Desktop/")
if(!(dir.exists("JoziKaleidoscope"))){
  dir.create("JoziKaleidoscope")
  setwd("JoziKaleidoscope")
} 
setwd("JoziKaleidoscope")
if(!(dir.exists("SourcePictures"))){
  dir.create("SourcePictures")
}
if(!(dir.exists("OutputPictures"))){
  dir.create("OutputPictures")
}
# set folder
setwd("SourcePictures/")

# get files
picture_names <- dir()

# Optional: Prune non correct format pictures

# open pictures
for(each in picture_names){
  if(grepl("\\.jpeg$|\\.jpg$", each)){
    Open_Pic <- readJPEG(each)
  } else if(grepl("\\.png$", each)){
    Open_Pic <- readPNG(each)
  } else{
    warning(paste("Error BADFORMAT:", each))
  }
  print(paste("Opened:", each))
  cols <- ncol(Open_Pic)
  rows <- nrow(Open_Pic)
  
  Zerod <- Open_Pic
  Zerod[,,] <- 0
  min <- min(rows, cols)
  rows <- min
  cols <- min
  Zerod <- Zerod[1:min, 1:min, ]
  Holdcell <- rows/10
  WHoldcell <- Holdcell
  for(row in 1:rows){
    for(col in 1:row){
      Zerod[row,col,] <- Open_Pic[row,col,]
      Zerod[col,row,] <- Open_Pic[row,col,]
    }
    if(each>WHoldcell){
      cat("0")
      WHoldcell <- WHoldcell + Holdcell
    }
  }
  cat("\n")
  

  Flipped <- Zerod
  for(row in 1:rows){
    Flipped[rows-row+1,,] <- Zerod[row,,]
  }
  Tall <- abind(Zerod, Flipped, along = 1)
  Flipped <- Tall
  for(col in 1:cols){
    Flipped[,cols-col+1,] <- Tall[,col,]
  }
  NewPic <- abind(Tall, Flipped, along = 2)
  writePNG(image = NewPic, target = paste("../OutputPictures/", each, ".png", sep = ""))
}


