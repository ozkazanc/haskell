module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn Mercury s = s / (31557600 * 0.2408467)
ageOn Venus   s = s / (31557600 * 0.61519726)
ageOn Earth   s = s / (31557600 * 1)
ageOn Mars    s = s / (31557600 * 1.8808158)
ageOn Jupiter s = s / (31557600 * 11.862615) 
ageOn Saturn  s = s / (31557600 * 29.447498)
ageOn Uranus  s = s / (31557600 * 84.016846)
ageOn Neptune s = s / (31557600 * 164.79132) 
