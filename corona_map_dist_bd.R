library(tidyverse)
library(tmap)
library(tabulizer)
library(sf)
library(rmapshaper)

## Reading table
corona_bd <- extract_tables("District_wise_case_no_update - web.pdf",output="matrix",method = "decide")


## Data Manipulation

d1 <- corona_bd[[1]][-1,-c(1,4,5)]
d1[28,] <-c("Sunamganj",1170) 
d1[29,] <- c("Habiganj",899)

d2 <- corona_bd[[2]][,-c(1,4,5)]

d3 <- corona_bd[[3]][-c(1,8),-c(3,4,5)]
d3[1,] <- c("Chapainawabganj",227)
d3 <- rbind(d3,c("Pabna",627))


tab <- rbind(d1,d2,d3)
colnames(tab) <- tab[1,]
tab <- tab[-1,]
tab <- as_tibble(tab)
tab <- tab%>%
    transform(total=as.numeric(`Total No of Cases`))
tab <- tab%>%
    select(c(District.City,total))
tab[1,] <- c("Dhaka",tab$total[1]+tab$total[2])
tab <- tab[-2,]
tab <- tab%>%
    transform(total=as.numeric(total))
tab <- tab%>%
    mutate(District.City=recode(District.City,"Coxâ€™s bazar"="Cox's Bazar"))


## Creating map

shape <- st_read(file.choose())
shape <- shape%>%
    mutate(District.City=DISTNAME%>%
                            recode("Munshiganj"="Munshigonj",
                                   "Narsingdi"="Narshingdi",
                                   "Chittagong"="Chattogram",
                                   "Comilla"="Cumilla",
                                   "Brahmanbaria"="B. Baria",
                                   "Lakshmipur"="Laksmipur",
                                   "Panchagarh"="Panchagar",
                                   "Barisal"="Barishal",
                                   "Nawabganj"="Chapainawabganj",
                                   "Khagrachhari"="Khagrachari",
                                   "Maulvibazar"="Moulovi Bazar",
                                   "Netrakona"="Netrokona",
                                   "Jhalokati"="Jhalokathi",
                                   "Bogra"="Bogura"))
final <- inner_join(shape,tab)
final <- final%>%
    group_by(District.City)%>%
    summarise(total=sum(total))

breaks <- c(0,2,5,7,10,15,20,50,120,200,500)*100
final%>%tm_shape()+
    tm_fill(col = "total",breaks = breaks,title = "Covid19 Cases",,palette = "Blues")+
    tm_borders()+tm_style("cobalt")+
    tm_layout(
        main.title = "Corona Distribution in Bangladesh till July 17",
        main.title.position = c("center"),
        main.title.size = 2,
        legend.position = c("right","bottom"),
        legend.outside = T,
        legend.text.size = .8,
        legend.frame = T,
        legend.bg.color = T
        
    )
