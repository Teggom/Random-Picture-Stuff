if(!("png" %in% installed.packages())){install.packages("png");library("png")}else{library("png")}
setwd("~/R Pic script")
On <- 1
Pic <- readPNG("1.png")
Backup <- Pic
for(x in 1:200){
  print(paste("Reading", paste(x, paste = "png", sep = ".")))
  Pic <- readPNG(paste(x, "png", sep = "."))
  Backup <- Pic
  for(Row in 4:(length(Pic[,1,1])-3)){
    for(Col in 4:(length(Pic[1,,1])-3)){
      averageR <- sum(Backup[(Row-3):(Row+3), (Col-3):(Col+3), 1])/49
      averageG <- sum(Backup[(Row-3):(Row+3), (Col-3):(Col+3), 2])/49
      averageB <- sum(Backup[(Row-3):(Row+3), (Col-3):(Col+3), 3])/49
      Pic[Row, Col, 1] <- averageR
      Pic[Row, Col, 2] <- averageG
      Pic[Row, Col, 3] <- averageB
    }
  }
  print(paste("Writing to", paste(x+1, paste = "png", sep = ".")))
  writePNG(target = paste(x+1, paste = "png", sep = "."),image =  Pic)
}



#Blotchy
offset <- 0
for(x in 1:100){
  print(paste("Reading", paste(x+offset, paste = "png", sep = ".")))
  Pic <- readPNG(paste(x+offset, "png", sep = "."))
  Backup <- Pic
  for(Row in 2:(length(Pic[,1,1])-1)){
    for(Col in 2:(length(Pic[1,,1])-1)){
      averageR <- max(Backup[(Row-1):(Row+1), (Col-1):(Col+1), 1])
      averageG <- max(Backup[(Row-1):(Row+1), (Col-1):(Col+1), 2])
      averageB <- max(Backup[(Row-1):(Row+1), (Col-1):(Col+1), 3])
      Pic[Row, Col, 1] <- averageR
      Pic[Row, Col, 2] <- averageG
      Pic[Row, Col, 3] <- averageB
    }
    print(paste("On", Row, "Of", length(Pic[,1,1])))
  }
  print(paste("Writing to", paste(x+1+offset, paste = "png", sep = ".")))
  writePNG(target = paste(x+1+offset, paste = ".png", sep = ""),image =  Pic)
}



#Not working as intended
for(x in 1:20){
  print(paste("Reading", paste(x, paste = "png", sep = ".")))
  Pic <- readPNG(paste(x, "png", sep = "."))
  Backup <- Pic
  for(Row in 1:(length(Pic[,1,1]))){
    for(Col in 1:(length(Pic[1,,1]))){
      averageR <- (Backup[Row,1,1]+sum(Backup[(Row):(length(Pic[,1,1])-Row), (Col):(Col), 1])/length((Row):(length(Pic[,1,1])-Row)))/2
      averageG <- (Backup[Row,1,2]+sum(Backup[(Row):(length(Pic[,1,2])-Row), (Col):(Col), 2])/length((Row):(length(Pic[,1,2])-Row)))/2
      averageB <- (Backup[Row,1,3]+sum(Backup[(Row):(length(Pic[,1,3])-Row), (Col):(Col), 3])/length((Row):(length(Pic[,1,3])-Row)))/2
      Pic[Row, Col, 1] <- averageR
      Pic[Row, Col, 2] <- averageG
      Pic[Row, Col, 3] <- averageB
    }
  }
  print(paste("Writing to", paste(x+1, paste = "png", sep = ".")))
  writePNG(target = paste(x+1, paste = "png", sep = "."),image =  Pic)
}



#Blends photos together
if(!("abind" %in% installed.packages())){install.packages("abind");library("abind")}else{library("abind")}
PicA <- readPNG("PictureA.png")
PicB <- readPNG("PictureB.png")
if(length(PicA[1,,1])!=length(PicB[1,,1]) || length(PicA[,1,1])!=length(PicB[,1,1])){
  print("Error, Dimension Mismatch")
  Max_Row <- min(length(PicA[1,,1]), length(PicB[1,,1]))
  Max_Col <- min(length(PicA[,1,1]), length(PicB[,1,1]))
  PicA <- PicA[1:Max_Col, 1:Max_Row,]
  PicB <- PicB[1:Max_Col, 1:Max_Row,]
}
PicB <- PicB[,,1:3]
PicA <- PicA[,,1:3]
Pic <- (PicA*2+PicB)/3
writePNG(image = Pic, target = "Blended.png")




#Takes the ceiling
if(!("abind" %in% installed.packages())){install.packages("abind");library("abind")}else{library("abind")}
PicA <- readPNG("PictureA.png")
PicB <- readPNG("PictureB.png")
if(length(PicA[1,,1])!=length(PicB[1,,1]) || length(PicA[,1,1])!=length(PicB[,1,1])){
  print("Error, Dimension Mismatch")
  Max_Row <- min(length(PicA[1,,1]), length(PicB[1,,1]))
  Max_Col <- min(length(PicA[,1,1]), length(PicB[,1,1]))
  PicA <- PicA[1:Max_Col, 1:Max_Row,]
  PicB <- PicB[1:Max_Col, 1:Max_Row,]
}
print(paste("Reading", paste(x, paste = "png", sep = ".")))
Backup <- PicA
for(Row in 1:(length(PicA[,1,1]))){
  for(Col in 1:(length(PicA[1,,1]))){
    for(Color in 1:3){
      Backup[Row, Col, Color] <- max(PicA[Row,Col,Color], PicB[Row,Col,Color])
    }
  }
  print(paste("On", Row, "Of", length(PicA[,1,1])))
}
writePNG(image = Backup, target = "Smush.png")



#Weird Distortion
PicA <- readPNG("PictureA.png")
PicA <- PicA[,,1:3]
Pic <- PicA[,,c(3,2,1)]
writePNG(image = Pic, target = "Disto3.png")


#Funny
PicA <- readPNG("PictureA.png")
PicA <- (PicA*255)^2
writePNG(image = PicA, target = "ColdCut.png")

#Splices Pictures together
if(!("abind" %in% installed.packages())){install.packages("abind");library("abind")}else{library("abind")}
PicA <- readPNG("PictureA.png")
PicB <- readPNG("PictureB.png")
if(length(PicA[1,,1])!=length(PicB[1,,1]) || length(PicA[,1,1])!=length(PicB[,1,1])){
  print("Error, Dimension Mismatch")
  Max_Row <- min(length(PicA[1,,1]), length(PicB[1,,1]))
  Max_Col <- min(length(PicB[,1,1]), length(PicB[,1,1]))
  PicA <- PicA[1:Max_Col, 1:Max_Row,]
  PicB <- PicB[1:Max_Col, 1:Max_Row,]
}
PicB <- PicB[,,1:3]
PicA <- PicA[,,1:3]
Use_A <- FALSE
Empty_Picture <- PicA[,1:2,]
for(Col in 2:length(PicA[,1,1])){
  using <- PicA
  if(!Use_A){
    using <- PicB
  }
  Empty_Picture <- abind(Empty_Picture, using[,Col,],along = 2)
  Use_A <- !Use_A
}
writePNG(image = Empty_Picture, target = "Spliced.png")





#Break a picture into a bunch of pieces
path <- "~/../Desktop/SuperAids/"
Picture <- readPNG(paste(path, "Phil.png", sep = ""))
x_cut <- 20
y_cut <- 20
dropped <- Picture[1:(length(Picture[,1,1])-(length(Picture[,1,1])%%x_cut)), 1:(length(Picture[1,,1])-(length(Picture[1,,1])%%y_cut)), 1:3]
number_x <- length(dropped[,1,1])/x_cut
number_y <- length(dropped[1,,1])/y_cut
for(x in 1:number_x){
  for(y in 1:number_y){
    break_Pic <- dropped[((x-1)*x_cut+1):((x)*x_cut), ((y-1)*y_cut+1):((y)*y_cut),]
    writePNG(image = break_Pic, target = paste(path, "CELL- ( ", x,", ", y, ").png", sep = ""))
  }
}
