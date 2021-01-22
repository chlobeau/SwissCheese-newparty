#Get count of parties and total individuals per party code:

input <- read.csv("Q:/Blecha/OperationSwissCheese/DataEntry/PhotoWarehouse/NewPartyCode/FlatDetectionFile_20201028b.csv")
head(input, 50)

#######Assign unique party IDs to NonWildMammalianParty#######
input$NonWildMammalParty <- 0
unique(input$CommonName)
head(input)
input.test <- subset(input, LastName = "Bevan" & LocationID == "172_0")
unique(input.test$CommonName)
input$NonWildMammalParty[input$CommonName == "On-foot non-mechanized" | 
                           input$CommonName == "Mountain Bike" |
                           input$CommonName == "Motorcycle" |
                           input$CommonName == "Car/Truck/SUV" |  
                           input$CommonName == "Quad-bike" |  
                           input$CommonName == "Side-by-side OHV" |   
                           input$CommonName == "Dog Off Leash" |   
                           input$CommonName == "Dog On Leash" | 
                           input$CommonName == "Dog Off Leash\nDog Off Leash" | 
                           input$CommonName == "Horseback"  
                           
] <- 1
input$PartyID <- -9999

input <- input[with(input, order(LocationName, ImageNum)),]
Observer.recompiled <- data.frame()
Observer.list <- unique(input$LastName)
for(O in 1:NROW(Observer.list)){
  input.sub1 <- subset(input, LastName == Observer.list[O])
  Location.recompiled <- data.frame()
  Location.list <- unique(input.sub1$LocationName)
  for(L in 1:NROW(Location.list)){
    input.sub2 <- subset(input.sub1, LocationName == Location.list[L])
    PartyID <- 1
    Type.list <- unique(input.sub2$CommonName)
    Type.recompiled <- data.frame()
    for(t in 1:NROW(Type.list)){
      input.sub3 <- subset(input.sub2, CommonName == Type.list[t])
      for(i in 1:NROW(input.sub3)){
        if(input.sub3$NonWildMammalParty[i] == 1 & input.sub3$DetailText[i] == "New party"){
          input.sub3$PartyID[i] <- PartyID
          PartyID <- PartyID + 1}
        if(input.sub3$NonWildMammalParty[i] == 1){  
          input.sub3$PartyID[i] <- PartyID}
      }
      
      Type.recompiled <- rbind(Type.recompiled, input.sub3)
    }
    Location.recompiled <- rbind(Location.recompiled, Type.recompiled)
  }
  Observer.recompiled <- rbind(Observer.recompiled, Location.recompiled) 
}

#write.csv(Observer.recompiled, "Q:/Blecha/OperationSwissCheese/DataEntry/PhotoWarehouse/NewPartyCode/Observer.recompiled.20201020a.csv")


#######now conduct count of parties and individuals#######
Individual.count.by.site <- data.frame()
Location.list <- unique(Observer.recompiled$LocationName)
for(L in 1:NROW(Location.list)){
  sub1 <- subset(Observer.recompiled, LocationName == Location.list[L] & NonWildMammalParty == 1)
  
  LocationName <- Location.list[L]
  sub2.rbind <- data.frame()
  observer.list <- unique(sub1$LastName)
  for(o in 1:NROW(observer.list)){
    sub2 <- subset(sub1, LastName == observer.list[o])
    Type.list <- unique(sub2$CommonName)
    Observer <- sub2$LastName[1]
    Total.Parties <- NROW(unique(sub2$PartyID))
    max.per.party <- aggregate(Individuals ~ PartyID, data = sub2, max)
    total.individuals <- sum(max.per.party$Individuals)
    cbound <- data.frame(LocationName, Observer, Total.Parties, total.individuals)
    sub2.rbind <- rbind(sub2.rbind, cbound)
  }
  Individual.count.by.site <- rbind(Individual.count.by.site, sub2.rbind)
}
Individual.count.by.site


#######now conduct count of parties and individuals separated by user type#######
Individual.count.by.site.type.observer <- data.frame()
Location.list <- unique(Observer.recompiled$LocationName)
for(L in 1:NROW(Location.list)){
  sub1 <- subset(Observer.recompiled, LocationName == Location.list[L] & NonWildMammalParty == 1)
  LocationName <- Location.list[L]
  observer.list <- unique(sub1$LastName)
  sub2.rbind <- data.frame()
  for(o in 1:NROW(observer.list)){
    sub2 <- subset(sub1, LastName == observer.list[o])
    Type.list <- unique(sub2$CommonName)
    sub3.rbind <- data.frame()
    for(t in 1:NROW(Type.list)){
      sub3 <- subset(sub2, CommonName == Type.list[t])
      UserType <- Type.list[t]
      Observer <- sub3$LastName[1]
      Total.Parties <- NROW(unique(sub3$PartyID))
      max.per.party <- aggregate(Individuals ~ PartyID, data = sub3, max)
      raw.detections.per.party <- aggregate(Individuals ~ PartyID, data = sub3, NROW)
      total.individuals <- sum(max.per.party$Individuals)
      total.detections <- sum(raw.detections.per.party$Individuals)
      cbound <- data.frame(LocationName, UserType, Observer, Total.Parties, total.individuals, total.detections)
      sub3.rbind <- rbind(sub3.rbind, cbound)
      }
    sub2.rbind <- rbind(sub3.rbind, sub2.rbind)
  }
  Individual.count.by.site.type.observer <- rbind(Individual.count.by.site.type.observer, sub2.rbind)
  
}
Individual.count.by.site.type.observer
#aggregate(data = Individual.count.by.site.type, total.individuals ~ LocationName+Observer, FUN = sum)
unique(Individual.count.by.site.type.observer$UserType)
write.csv(Individual.count.by.site.type.observer, "Individual.count.by.site.type.observer_20201028.csv")


#calculate differences between observers:
library(tidyr)
separate(Individual.count.by.site.type.observer, c("LocationName"))
melt(Individual.count.by.site.type.observer, id = (c("LocationName", "UserType")))
#
head(input)
input.chloe.113 <- subset(input, LocationName == "113_0")
unique(input.chloe.113$CommonName)
