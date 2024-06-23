library(ggplot2)

JuanSoto <- c(Contact = 86.57, Power = 73.45, Luckiness = 43.22, `Strikeout Prone` = 17.95, 
              `Run Potential` = 95.56, Patience = 91.56, `Well-Rounded` = 95.89, Efficiency = 78.14)


JamesWood <- c(Contact = 83.37, Power = 78.76, Luckiness = 55.71, `Strikeout Prone` = 19.43, 
               `Run Potential` = 90.90, Patience = 68.14, `Well-Rounded` = 92.89, Efficiency = 42.14)


data <- rbind(JuanSoto, JamesWood)
data

Attributes = colnames(data)
AttNo = length(Attributes)

data <- cbind(data, data[,1])

# create a empty plot with a size of x -120,120 and y of -120,150 and save it to object 'p'
ggplot() + xlim(c(-120, 120)) + ylim(c(-120, 150))

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

circle1 <- circleFun(c(0,0),200,npoints = 100)
circle2 <- circleFun(c(0,0),150,npoints = 100)
circle3 <- circleFun(c(0,0),100,npoints = 100)
circle4 <- circleFun(c(0,0),50,npoints = 100)

# add the radar background
ggplot() + xlim(c(-120, 120)) + ylim(c(-120, 150)) + 
# add circles
  geom_polygon(data = circle1, aes(x=x,y=y),fill = "#F0F0F0", colour = "#969696") + geom_polygon(data = circle2, aes(x=x,y=y),fill = "#FFFFFF", colour = "#d9d9d9") + geom_polygon(data = circle3, aes(x=x,y=y),fill = "#F0F0F0", colour = "#d9d9d9") + geom_polygon(data = circle4, aes(x=x,y=y),fill = "#FFFFFF", colour = "#d9d9d9")

## Add the radar background
ggplot() + xlim(c(-120, 120)) + ylim(c(-120, 150)) + 
  ## Add circles
  geom_polygon(data = circle1, aes(x=x,y=y),fill = "#F0F0F0", colour = "#969696") + geom_polygon(data = circle2, aes(x=x,y=y),fill = "#FFFFFF", colour = "#d9d9d9") + geom_polygon(data = circle3, aes(x=x,y=y),fill = "#F0F0F0", colour = "#d9d9d9") + geom_polygon(data = circle4, aes(x=x,y=y),fill = "#FFFFFF", colour = "#d9d9d9") +
  ## Change the theme to void 
  theme_void()

angle_spilt <- (2*pi) / (AttNo)
angle_spilt_seq <- seq(0,(2*pi),angle_spilt)
angle_spilt_seq

LineData <- data.frame(x = numeric(), y = numeric(), stringsAsFactors = FALSE)
TitlePositioning <- data.frame(title = character(), x = numeric(), y = numeric(), stringsAsFactors = FALSE)


# create plot background construction data  
for (i in 1:NCOL(data)) {
  angle_multiplier <- if(i < NCOL(data)){i}else{1}
  radians_for_segment <- angle_spilt_seq[i]
  
  x <- 100 * cos(radians_for_segment)
  y <- 100 * sin(radians_for_segment)
  temp <- data.frame(x = x, y = y, stringsAsFactors = F)
  LineData <- rbind(temp, LineData)
  
  x <- 112 * cos(radians_for_segment)
  y <- 112 * sin(radians_for_segment)
  title <- colnames(data)[i]
  temp <- data.frame(title = title, x = x, y = y, stringsAsFactors = F)
  TitlePositioning <- rbind(temp, TitlePositioning)
}

## create the value labellings data 
values <- c(25,50,75)
radian_for_values <- angle_spilt / 2
x <- values * cos(radian_for_values)
y <- values * sin(radian_for_values)
ValuePositioning <- data.frame(values = values, x = x, y = y, stringsAsFactors = F)

## Add the origin values for the lines 
LineData$x2 <- 0
LineData$y2 <- 0

## check the data output 
LineData

## Add the radar background
ggplot() + xlim(c(-120, 120)) + ylim(c(-120, 150)) + 
  ## Add circles
  geom_polygon(data = circle1, aes(x=x,y=y),fill = "#F0F0F0", colour = "#969696") + geom_polygon(data = circle2, aes(x=x,y=y),fill = "#FFFFFF", colour = "#d9d9d9") + geom_polygon(data = circle3, aes(x=x,y=y),fill = "#F0F0F0", colour = "#d9d9d9") + geom_polygon(data = circle4, aes(x=x,y=y),fill = "#FFFFFF", colour = "#d9d9d9") +
  ## Change the theme to void 
  theme_void() +
  ## Add the segment lines and attribute/value titles 
  geom_segment(data=LineData, aes(x = LineData$x, y = LineData$y, xend = LineData$x2, yend = LineData$y2),colour = "#d9d9d9", linetype = "dashed") + 
  annotate("text", x = TitlePositioning$x , y = TitlePositioning$y, label = TitlePositioning$title, size= 2.5) +  
  annotate("text", x = ValuePositioning$x , y = ValuePositioning$y, label = ValuePositioning$values, size= 2.5, colour = "#969696")

# empty dataframe to catch result 
polydata <- data.frame(player = character(), value = numeric(), radians = numeric(), x = numeric(), y = numeric(), stringsAsFactors = FALSE)

## create polygon data for the players 
for (i in 1:NCOL(data)) {
  
  for (p in 1:NROW(data)) {
    
    player2calc <- data[p,]
    angle_multiplier <- if(i < NCOL(data)){i}else{1}
    radians_for_segment <- angle_spilt_seq[i]
    x <- player2calc[i] * cos(radians_for_segment)
    y <- player2calc[i] * sin(radians_for_segment)
    player <- rownames(data)[p]
    temp <- data.frame(player = player, value = player2calc[i], radians = radians_for_segment, x = x, y = y, stringsAsFactors = F)
    polydata <- rbind(temp, polydata)
  }
}
head(polydata)

## split the data up into player 1 and 2
playersDB <- unique(polydata$player)
player1 <- polydata[which(polydata$player == playersDB[1]),]
player2 <- polydata[which(polydata$player == playersDB[2]),]

## Add the radar background
ggplot() + xlim(c(-120, 120)) + ylim(c(-120, 150)) + 
  ## Add circles
  geom_polygon(data = circle1, aes(x=x,y=y),fill = "#F0F0F0", colour = "#969696") + geom_polygon(data = circle2, aes(x=x,y=y),fill = "#FFFFFF", colour = "#d9d9d9") + geom_polygon(data = circle3, aes(x=x,y=y),fill = "#F0F0F0", colour = "#d9d9d9") + geom_polygon(data = circle4, aes(x=x,y=y),fill = "#FFFFFF", colour = "#d9d9d9") +
  ## Change the theme to void 
  theme_void() +
  ## Add the segment lines and attribute/value titles 
  geom_segment(data=LineData, aes(x = LineData$x, y = LineData$y, xend = LineData$x2, yend = LineData$y2),colour = "#d9d9d9", linetype = "dashed") + 
  annotate("text", x = TitlePositioning$x , y = TitlePositioning$y, label = TitlePositioning$title, size= 2.5) +  
  annotate("text", x = ValuePositioning$x , y = ValuePositioning$y, label = ValuePositioning$values, size= 2.5, colour = "#969696") +
  ## Add player 1 data 
  geom_polygon(data = player1, aes(x=x,y=y),fill = "#AB0003", colour = "#AB0003", alpha = 0.3) + geom_point(data = player1, aes(x = x, y = y),size=0.3, colour= "#A30845")

## create the title string for player 1
Player1_title <- gsub('([[:upper:]])', ' \\1', playersDB[1])
Player1_title <- trimws(Player1_title)

## Add the radar background
ggplot() + xlim(c(-120, 120)) + ylim(c(-120, 150)) + 
  ## Add circles
  geom_polygon(data = circle1, aes(x=x,y=y),fill = "#F0F0F0", colour = "#969696") + geom_polygon(data = circle2, aes(x=x,y=y),fill = "#FFFFFF", colour = "#d9d9d9") + geom_polygon(data = circle3, aes(x=x,y=y),fill = "#F0F0F0", colour = "#d9d9d9") + geom_polygon(data = circle4, aes(x=x,y=y),fill = "#FFFFFF", colour = "#d9d9d9") +
  ## Change the theme to void 
  theme_void() +
  ## Add the segment lines and attribute/value titles 
  geom_segment(data=LineData, aes(x = LineData$x, y = LineData$y, xend = LineData$x2, yend = LineData$y2),colour = "#d9d9d9", linetype = "dashed") + 
  annotate("text", x = TitlePositioning$x , y = TitlePositioning$y, label = TitlePositioning$title, size= 2.5) +  
  annotate("text", x = ValuePositioning$x , y = ValuePositioning$y, label = ValuePositioning$values, size= 2.5, colour = "#969696") +
  ## Add player 1 data 
  geom_polygon(data = player1, aes(x=x,y=y),fill = "#AB0003", colour = "#AB0003", alpha = 0.3) + geom_point(data = player1, aes(x = x, y = y),size=0.3, colour= "#A30845") + 
  ## Add Chart Title
  annotate("text", x = -110 , y = 130, label = Player1_title, size= 5, colour = "#AB0003", family = "Helvetica", fontface = "bold", hjust = 0) + 
  annotate("text", x = 110 , y = 130, label = "Hitting Metrics", size= 4, colour = "#969696", family = "Helvetica", fontface = "bold", hjust = 1)

## Create Title Strings for Player 2
Player2_title <- gsub('([[:upper:]])', ' \\1', playersDB[2])
Player2_title <- trimws(Player2_title)

## Add the radar background
ggplot() + xlim(c(-120, 120)) + ylim(c(-120, 150)) + 
  ## Add circles
  geom_polygon(data = circle1, aes(x=x,y=y),fill = "#F0F0F0", colour = "#969696") + geom_polygon(data = circle2, aes(x=x,y=y),fill = "#FFFFFF", colour = "#d9d9d9") + geom_polygon(data = circle3, aes(x=x,y=y),fill = "#F0F0F0", colour = "#d9d9d9") + geom_polygon(data = circle4, aes(x=x,y=y),fill = "#FFFFFF", colour = "#d9d9d9") +
  ## Change the theme to void 
  theme_void() +
  ## Add the segment lines and attribute/value titles 
  geom_segment(data=LineData, aes(x = LineData$x, y = LineData$y, xend = LineData$x2, yend = LineData$y2),colour = "#d9d9d9", linetype = "dashed") + 
  annotate("text", x = TitlePositioning$x , y = TitlePositioning$y, label = TitlePositioning$title, size= 2.5) +  
  annotate("text", x = ValuePositioning$x , y = ValuePositioning$y, label = ValuePositioning$values, size= 2.5, colour = "#969696") +
  ## Add player 1 data 
  geom_polygon(data = player1, aes(x=x,y=y),fill = "#AB0003", colour = "#AB0003", alpha = 0.3) + geom_point(data = player1, aes(x = x, y = y),size=0.3, colour= "#AB0003") + 
  ## Add Chart Title
  annotate("text", x = -110 , y = 130, label = Player1_title, size= 5, colour = "#AB0003", family = "Helvetica", fontface = "bold", hjust = 0) + 
  annotate("text", x = 110 , y = 130, label = "Player Comp: 93.1% ", size= 4, colour = "#969696", family = "Helvetica", fontface = "bold", hjust = 1) +
  ## Add the player 2 polygon and data points
  geom_polygon(data = player2, aes(x=x,y=y),fill = "navy", colour = "navy", alpha = 0.3) +
  geom_point(data = player2, aes(x = x, y = y),size=0.3, colour= "navy") +
  ## Add the titles for player 2
  annotate("text", x = -110 , y = 116, label = Player2_title, size= 5, colour = "navy", family = "Helvetica", fontface = "bold", hjust = 0) + 
  annotate("text", x = -110 , y = 123 , label = "vrs", size= 3, colour = "#969696", family = "Helvetica", hjust = 0) + 
  labs(caption = "@Riordan5J | Inspired by @FC_rstats")