###############################################
## PS 5625 - Applied Statistical Programming
## Problem Set 3
## Let's Make a Deal
## Author: Luwei YING

# First, remove existing objects from the global environment.
rm(list=ls()) 

## Task 1 Define "door" with numeric value: 1, 2, or 3.

# I create a function to make sure the chosen door number is only from 1, 2 or 3.
Door_choose <- function(x){
  if (!(x %in% 1:3)) {
    stop(x, ' is not a valid door number', call.=FALSE)
  }
  class(x) <- 'door'
  return(x)
}

# test
Door_choose(5) # This returns an error message
Door_choose(3)

## Task 2 Create "PlayGame" with S3 system

PlayGame<-function(x){
  Door_chosen <- as.numeric(Door_choose(x))
  Door_car <- sample(1:3, 1)
  # define two objects: the door chosen by the player and the door with a car
  if (Door_chosen ==  Door_car) {
    print("Congratulations! You win!")
  } else {
    print("Sorry, the car is not here.")
  }
  # the result depends on whether the two objects are the same 
}

# test
PlayGame(3)

## Task 3 Do it with the S4 system

# Again, remove existing objects from the global environment, in case it calls
# the S3 method by accident.
rm(list=ls())

# set up a class
setClass(Class="door",
         slots=c(number='numeric')
)

# I create a construction function that allows the user to create a door 
# object. Basically, it changes the class of x from "number" to "door".
Construct.door<-function(x){
  class(x)<-"door"
  return(x)
}

# Here is a validation function that checks whether the value stored in 
# door is actually an integer
setValidity("door", 
            function(object){
  if(!(object@number %in% 1:3)) {
    return("@number is not a valid door number")
    }
 }
)

# Initialize
setMethod("initialize", "door", function(.Object, ...) {
  value = callNextMethod()
  validObject(value)
  return(value)
})

# Create a new generic method PlayGame.
setGeneric("PlayGame", function(x){
  standardGeneric("PlayGame")
})

# Set method with the above generic method.
setMethod(
  "PlayGame",
  "numeric",
  PlayGame<-function(x){
    Door_choose <- as.numeric(x)
    Door_car <- sample(1:3, 1)
    if (Door_choose ==  Door_car) {
      print("Congratulations! You win!")
    } else {
      print("Sorry, the car is not here.")
    }
  }
)

# test
Choice1 <- Construct.door(3)
class(Choice1) #Just to ckeck if "Choice1" is classed as a door object.
PlayGame(Choice1)