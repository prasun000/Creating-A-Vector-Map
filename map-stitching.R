require("imager")
require("svDialogs")

a<-readline(prompt = "Enter file type of screen-shots : png / jpeg  --- ")

# Assume SS are named image_01,image_02,...,image_17 (say , at max 30 images ) 
# saved at the file location  : file

#Suitably modify s1,s2
s1 = "C:\\Users\\INDIA\\Desktop\\ss\\image_0"
s2 = "C:\\Users\\INDIA\\Desktop\\ss\\image_"

num_of_ss = 12  # suitably modify

#'new_data.csv' contains the data

dat = read.csv('new_data.csv')
dat$ScreenshotNo = as.character(dat$ScreenshotNo)
z = list(x = c() , y = c())
list_of_places = c()
list_of_ss = c()
list_of_ss = dat$ScreenshotNo
list_of_places = dat$Landmark
z$x = dat$xcoordinate
z$y = dat$ycoordinate


for (i in 1:num_of_ss) {
  if(i<10){
    ss = load.image(paste(paste(paste(s1,i,sep=''),'.',sep=''),a,sep = ''))
  }
  else{
    ss = load.image(paste(paste(paste(s2,i,sep=''),'.',sep=''),a,sep = ''))
  }
  plot(ss)
  user <- dlgInput("Is this ss needed ? : y/n", Sys.info()["Requirement"])$res
  if (user == 'y'){
    ####################aassume same zoom level#####################################
    cat('\n Now select landmark(s) \n')
    q = 0
    while(!is.null(q)){
      
      cat("Left click to store  instance of a landmark \n")
      q = locator(1, type = "o", pch = 3, col = "magenta")
      place <- dlgInput("Enter name of place : ", Sys.info()["Place"])$res
      list_of_places = append(list_of_places,tolower(place))
      list_of_ss = append(list_of_ss,as.character(i))
      
      z$x = append(z$x,q$x)
      z$y = append(z$y,q$y)
      
      cat(paste("The next few clicks will store the approx position of",place))
      cat("\n Right-click  after you have clicked atleast three times \n")
      q = locator( type = "o", pch = 3, col = "blue")
      k = length(q$x)
    
      list_of_places = append(list_of_places,rep(tolower(place) , k))
      list_of_ss = append(list_of_ss,rep( as.character(i) , k) )
      
      z$x = append(z$x,q$x)
      z$y = append(z$y,q$y)
      
      cat(paste("Numer of clicks :",k))
      
      cat("\n Left click to move to next landmark on same ss \n")
      cat("\n Right click  to move to next ss \n")
      
      q = locator(1)
   
    }
    cat(paste("Completed SS number",i))
    cat("\n Places covered : \n")
    print(unique(list_of_places))
    print("\n")
  }
  
}

dat=data.frame("xcoordinate"=z$x,"ycoordinate"=z$y,"Landmark"=list_of_places,"Screenshot.No"=list_of_ss)
write.csv(dat,'new_data.csv')

lx<-lm(xcoordinate~Landmark+Screenshot.No-1,dat)
ly<-lm(ycoordinate~Landmark+Screenshot.No-1,dat)

summary(lx)
plot(lx$res)

summary(ly)
plot(ly$res)

plot(lx)
plot(ly)

num_of_landmarks = length(unique(dat$Landmark))

# Condition for map to be made 
if(lx$rank < num_of_landmarks + num_of_ss -1){
	print('Not connected ! Map cannot be made ')
}


lab = rep(0,num_of_landmarks)
for(i in 1:num_of_landmarks){
	lab[i] = as.character(i)
}
 
#text(ly$coef[1:num_of_landmarks],lx$coef[1:num_of_landmarks],labels = lab)
################################Plot#####################################
checkPlot = function(x,lab){
	plot(x)
	identify(x,lab = lab)
}
places =checkPlot(ly$coef[1:num_of_landmarks]~lx$coef[1:num_of_landmarks],lab)
print(lx$coef[places])
print(ly$coef[places])
###########################################################################

############################Influence measures############################
tmp = influence.measures(lx)
dat[which(apply(tmp$is.inf,1,sum) > 0),]
###########################################################################

################Crude outlier detection######################################


plot(lx$res)
abline(h=0)

plot(ly$res)
abline(h=0)

outlier_x=c()
q1_x= quantile(lx$res,0.25)[[1]]
q3_x= quantile(lx$res,0.75)[[1]]
ran_x=3*IQR(lx$res)
l=length(lx$res)
for(i in 1:l){
	if(lx$res[i]<(q1_x-ran_x)|lx$res[i]>(q3_x+ran_x)){
		outlier_x=c(outlier_x,i)
	}
}

outlier_y=c()
q1_y= quantile(ly$res,0.25)[[1]]
q3_y= quantile(ly$res,0.75)[[1]]
ran_y=3*IQR(ly$res)
l=length(ly$res)
for(i in 1:l){
	if(ly$res[i]<(q1_y-ran_y)|ly$res[i]>(q3_y+ran_y)){
	outlier_y=c(out_y,i)
	}
}

