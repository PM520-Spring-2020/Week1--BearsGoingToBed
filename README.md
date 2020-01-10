# Week1-BearsGoingToBed
Code to simulate the 538 question about bears going to bed
This document simulates the 7 dwarves problem from 538.com. 
It is probably simplest to work from the Rmd file directly, rather than this readme.
The Rmd file omits the description of the problem and presents a partial solution.

Set up some global variables

```{r globals, echo=TRUE}
HowManySims<-1000   # the number of dwarf bedtimes to simulate
LowestNumberOfDwarves<-7   # the smallest number of dwarves going to bed
HighestNumberOfDwarves<-7
FirstDwarfMustPickWrongBed<-1   # If set to 1, always picks wrong bed; =0 then picks a random bed
```

Now we simulate the dwarves going to bed.
```{r bedtime}
 
for (iDwarves in LowestNumberOfDwarves:HighestNumberOfDwarves){
  HowManyDwarfsSleptInWrongBedThisTime<-NULL # This will store the number of dwarves that ended up in the wrong bed each time
  for (j in 1:HowManySims){
    DoesLastDwarfSleepInCorrectBed<-0  # This will indicate whether the last dwarf sleeps in the correct bed in this particular realization
    for (j in 1:HowManySims){
      AvailableBeds<-1:iDwarves
      HowManyInWrongBed<-0
      # Put the first drawf to bed
      if (FirstDwarfMustPickWrongBed){
        ChosenBed<-sample(2:iDwarves,1,replace=FALSE)
      }else{
         ChosenBed<-sample(2:iDwarves,1,replace=FALSE)
      }
      AvailableBeds<-AvailableBeds[-ChosenBed]
    }
    # Did he/she sleep in the wrong bed?
    if (ChosenBed!=1){
      HowManyInWrongBed<-HowManyInWrongBed+1
    }
      
    # now put the rest to bed
    for (k in 2:iDwarves){
      if (any(AvailableBeds==k)){
        # she sleeps in her own bed
        AvailableBeds<-AvailableBeds[-which(AvailableBeds==k)]
      }else{
        # she picks a bed at random from those available
        ChosenBed<-sample(AvailableBeds,1)
        AvailableBeds<-AvailableBeds[-which(AvailableBeds==ChosenBed)]
        HowManyInWrongBed<-HowManyInWrongBed+1
      }
    }
    
    # did the last dwarf sleep in the correct bed
    if (ChosenBed==iDwarves)
      DoesLastDwarfSleepInCorrectBed<-1+DoesLastDwarfSleepInCorrectBed    
    # store the number of dwarves that slept in the wrong bed
    HowManyDwarfsSleptInWrongBedThisTime <- append(HowManyDwarfsSleptInWrongBedThisTime,HowManyInWrongBed,after=length(HowManyDwarfsSleptInWrongBedThisTime))

  }
  
  print(table(HowManyDwarfsSleptInWrongBedThisTime))
}
```
