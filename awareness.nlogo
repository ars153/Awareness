;; This version of the awareness model uses only a square lattice (no long range links), and uses Adam's
;; suggestion for the two controls (reducing the number of contacts, or reducing p-infect). In addition,
;; there is a notion of cost and benefit to allow tradeoffs to be explored. Benefit is derived from making
;; contact, and the details are explained in the procedure count-contacts. Cost comes from becoming infected
;; and is simply a multiple of the final number of removeds.
;;
;; Control B reduces benefit by reducing the number of contacts. All contacts give rise to equal benefit.
;;
;; Control C does not affect the number of contacts, but instead reduces the benefit from contact involving
;; susceptibles who have a reduced p-infect. This is meant to reflect the cost associated with protective
;; actions such as handwashing or using antiseptic gels or condoms.
;;
;; This version is an SIR model, but it might be useful to explore a similar question with an SIS model too


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Breeds, turtle variables, and global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

globals [SS-contacts
         SI-contacts
         SR-contacts
         II-contacts
         IR-contacts
         RR-contacts]

breed [susceptibles susceptible]
breed [infecteds infected]
breed [removeds removed]
breed [the-dead dead]

susceptibles-own [to-become-infected?
                  p-infect]


infecteds-own [to-remove?
               to-die?
               time-left
               i-modified-contact?]

turtles-own [contact-chance
             modified-contact?]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  setup-globals
  setup-turtles
  reset-ticks
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup procedure: setup-globals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to setup-globals
  set SS-contacts 0
  set SI-contacts 0
  set SR-contacts 0
  set II-contacts 0
  set IR-contacts 0
  set RR-contacts 0
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup procedure: setup-turtles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We first create a susceptible on a certain number of patches depending on the population density
;; Then we convert initial-inf randomly chosen susceptibles to infecteds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to setup-turtles
  set-default-shape turtles "person"

  ask patches
     [set pcolor white]

  ask n-of (density * count patches) patches[

          sprout-susceptibles 1
          [set color green
           set contact-chance default-contact-chance
           set to-become-infected? false
           set modified-contact? false
           set p-infect p-infect-init]
        ]

   ask n-of initial-inf turtles
     [
       set breed infecteds
       set color red
       set i-modified-contact? false
       set time-left (countdown - 2 + random (countdown / 5)) ;; The infected will have between 80% and 120% of the value countdown before they die or recover
     ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  ifelse (ticks < max-ticks) and (count infecteds > 0)
  [
    modify-contact
    count-contacts
    infect-susceptibles
    remove-infecteds
    update-breeds
    tick
    ;; update-plots
  ][
    stop
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go procedure: count contacts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We add to the total contacts so far the count of the number of contacts made at the current step.
;; Which contacts do we count?
;; SI contacts: For each susceptible, we count the number of infecteds within its z-infection radius. Each contact is given the weight
;;      p-infect / p-infect-init in order to capture the reduced benefit of contact when the susceptible is being cautious.
;; SR contacts: For each susceptible, we count the number of removeds within its z-infection radius. Each contact is given the weight
;;      p-infect / p-infect-init, as above.
;; SS contacts: For each susceptible, we count the number of other susceptibles within its z-infection radius and having the original
;;      susceptible within their own z-infection radius. Each contact is given the weight p-infect / p-infect-init.
;;      The total count is then halved so that each SS pair is counted only once. The effect of this is that the weight of an SS
;;      contact is the average of the p-infect / p-infect-init ratios of the two susceptibles.
;; IR contacts: For each infected, we count the number of removeds within its z-infection radius.
;; II contacts: For each infected, we count the number of other infecteds within its z-infection radius. This total is then halved.
;; RR contacts: For each removed, we count the number of other removeds within its z-infection radius. This total is then halved.
;; Each contact is then multiplied by the contact chance to account for randomness in people contacting each other in their z-infection radii
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to count-contacts

  let SI 0
  let SR 0
  let SS 0
  let IR 0
  let II 0
  let RR 0

  ask susceptibles [
    set SI (SI + ((count infecteds in-radius z-infection-init) * (p-infect / p-infect-init)))
    set SR (SR + ((count removeds in-radius z-infection-init) * (p-infect / p-infect-init)))
    set SS (SS + ((count other susceptibles in-radius z-infection-init) * (p-infect / p-infect-init)))
    set SS (SS / 2)
    set SI round (SI * contact-chance)
    set SR round (SR * contact-chance)
    set SS round (SS * contact-chance)
  ]


  ask infecteds [
    set IR round (IR + (count removeds in-radius z-infection-init))
    set II round (II + (count other infecteds in-radius z-infection-init))
    set II (II / 2)
    set IR (IR * contact-chance)
    set II (II * contact-chance)
  ]


  ask removeds [
    set RR round (RR + (count other removeds in-radius z-infection-init))
    set RR (RR / 2)
    set RR (RR * contact-chance)
  ]

  set SI-contacts (SI-contacts + SI)
  set SR-contacts (SR-contacts + SR)
  set SS-contacts (SS-contacts + SS)
  set IR-contacts (IR-contacts + IR)
  set II-contacts (II-contacts + II)
  set RR-contacts (RR-contacts + RR)

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go procedure: infect-susceptibles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Each susceptible makes contact with a certain number of infecteds within its infection neighbourhood (of radius z-infection), depending on the contact chance.
;; Each of these contacts may result in it becoming infected, with probability p-infect
;;
;; Note: in this model I am assuming that in each time unit a susceptible will make contact with *all* of its possible contacts. (Not anymore)
;; This is different from the models used in the work with Carron, where it was assumed that in a single time unit only one contact
;; would take place. Does this difference matter? It may be that implicitly this model is using a larger time unit.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to infect-susceptibles
   ask susceptibles [

     let infected-contacts                              ;; this is the number of infecteds contacted by this susceptible, and is the sum of
        (count infecteds in-radius z-infection-init)         ;; the number of infecteds within the radius z-infection

     set infected-contacts (infected-contacts * contact-chance)

     let infection-prob 1 - ((1 - p-infect) ^ infected-contacts)   ;; probability of at least one of these contacts causing infection is
                                                                   ;; 1 - the probability that none of them cause infection

     let p (random 1000) + 1             ;; This works. Not sure if the multiplying by 1000 is really necessary.
     if (p <= infection-prob * 1000) [ set to-become-infected? true ]
 ]
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go procedure: remove-infecteds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; When an infected is at the end of its illness, it either dies or is removed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to remove-infecteds
  ask infecteds with [time-left = 0] [ ;; Any infected whose time is up will either recover or die
    let recovery (random 1000) + 1 ;; Recovery chance

    ifelse recovery <= p-recover * 1000 ;; If the infected recovers it is set to be removed, if not, it is set to be killed
    [set to-remove? true]
    [set to-die? true]
  ]
 end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go procedure: update-breeds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; all changes of breed or resetting of turtle variables take effect
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to update-breeds

  ask susceptibles with [to-become-infected? = true][   ; infected susceptibles become infected
     set breed infecteds
     set color red
     set i-modified-contact? false
     set time-left (countdown - 2 + random (countdown / 5))
  ]

  ask infecteds with [to-remove? = true] [ ; infecteds marked for removal are removed. Not sure I need to do it this way...
     set breed removeds
     set color 8   ;; light gray
  ]

  ask infecteds with [to-die? = true] [
    set breed the-dead
    set color black
  ]

  ask infecteds [set time-left (time-left - 1)] ;; Update time left for infecteds
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go procedure: modify contact
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If social distancing is applied, a certain proportion of all turtles will reduce their contact chance when a threshold of affected people is reached
;; Susceptibles will reduce their contacts to the default contact chance multiplied by sd-contact-modifier with some randomness
;; The probability that a turtle will follow social distancing is sd-chance
;; If social distancing, we only modify contacts of people who have not modified their contacts already
;; Infected people will self isolate even before the threshold is reached if infected-isolation? is on, and the effects will be more severe than social distancing
;; Infected people will reduce their contact chance to a value between 0 and 5 percent
;; Susceptibles who follow social distancing are coloured cyan and infecteds who follow infected isolation are coloured pink
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to modify-contact

  ask turtles with [modified-contact? = false] [
    if social-distancing? [
      let been-infected count infecteds + count removeds + count the-dead ;; Total number of people who have been infected

      if been-infected > sd-threshold * count turtles [ ;; If the total number of victims is higher than the population threshold
        let p random 1000 + 1 ;; This will be used in the coin toss to determine whether a turtle will comply with social distancing

        if sd-chance * 1000 >= p [
          set contact-chance (default-contact-chance * (sd-contact-modifier - 0.05 + random-float 0.1))
          if breed = susceptibles [set color cyan]]

        set modified-contact? true
        ]
      ]
    ]

  ask infecteds with [i-modified-contact? = false] [
    if infected-isolation? [
      let p random 1000 + 1 ;; This will be used in the coin toss to determine whether a turtle will comply with social distancing

      if sd-chance * 1000 >= p [
        set contact-chance (default-contact-chance * (random-float 0.05))
        set color pink
      ]

      set i-modified-contact? true
      ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
490
10
1098
619
-1
-1
12.0
1
10
1
1
1
0
0
0
1
0
49
0
49
1
1
1
ticks
30.0

BUTTON
142
364
212
399
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
233
365
304
401
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
15
102
229
135
p-infect-init
p-infect-init
0.0
1.0
0.4
0.01
1
NIL
HORIZONTAL

SLIDER
14
145
227
178
p-recover
p-recover
0.0
1.0
0.8
0.01
1
NIL
HORIZONTAL

SLIDER
88
12
281
45
initial-inf
initial-inf
0
2500
10.0
1
1
NIL
HORIZONTAL

INPUTBOX
15
11
79
71
max-ticks
1.0
1
0
Number

PLOT
19
434
462
734
Simulation populations
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"susceptible" 1.0 0 -10899396 true "" "plot count susceptibles"
"infected" 1.0 0 -2674135 true "" "plot count infecteds"
"removed" 1.0 0 -16777216 true "" "plot count removeds"

SLIDER
16
196
188
229
z-infection-init
z-infection-init
0
71
1.0
1
1
NIL
HORIZONTAL

SLIDER
88
54
260
87
countdown
countdown
1
100
100.0
1
1
NIL
HORIZONTAL

SLIDER
218
197
390
230
sd-threshold
sd-threshold
0
1
0.01
0.01
1
NIL
HORIZONTAL

SWITCH
270
99
421
132
social-distancing?
social-distancing?
0
1
-1000

SWITCH
272
54
428
87
infected-isolation?
infected-isolation?
0
1
-1000

SLIDER
14
242
191
275
default-contact-chance
default-contact-chance
0
1
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
216
244
388
277
sd-chance
sd-chance
0
1
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
215
290
387
323
density
density
0
1
1.0
0.01
1
NIL
HORIZONTAL

SLIDER
238
153
410
186
sd-contact-modifier
sd-contact-modifier
0.05
1
0.05
0.01
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

## HOW TO USE IT
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

transmitter
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105
Line -955883 false 210 30 285 15
Line -955883 false 210 60 285 75
Line -955883 false 90 30 15 15
Line -955883 false 90 60 15 75

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count the-dead</metric>
    <enumeratedValueSet variable="aware-of-removeds?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ticks">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-infect-init">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-inf">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-infection-init">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-z-infection?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-p-infect?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-aware">
      <value value="2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="p-remove" first="0" step="0.02" last="1"/>
    <enumeratedValueSet variable="risk-attitude">
      <value value="0.5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="aware-of-dead" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count the-dead</metric>
    <enumeratedValueSet variable="risk-attitude">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-inf">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-aware">
      <value value="2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="p-remove" first="0" step="0.02" last="1"/>
    <enumeratedValueSet variable="countdown">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="aware-of-dead?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="aware-of-removeds?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-infection-init">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ticks">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-p-infect?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-z-infection?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-infect-init">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="aware-of-removeds" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count the-dead</metric>
    <enumeratedValueSet variable="risk-attitude">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-inf">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-aware">
      <value value="2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="p-remove" first="0" step="0.02" last="1"/>
    <enumeratedValueSet variable="countdown">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="aware-of-dead?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="aware-of-removeds?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-infection-init">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ticks">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-p-infect?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-z-infection?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-infect-init">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="aware-of-removeds(r)" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count removeds</metric>
    <enumeratedValueSet variable="risk-attitude">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-inf">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-aware">
      <value value="2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="p-remove" first="0" step="0.02" last="1"/>
    <enumeratedValueSet variable="countdown">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="aware-of-dead?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="aware-of-removeds?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-infection-init">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-z-infection?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ticks">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-p-infect?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-infect-init">
      <value value="0.4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="social-distancing-sonly" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count removeds</metric>
    <metric>count the-dead</metric>
    <enumeratedValueSet variable="risk-attitude">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-inf">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-aware">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-remove">
      <value value="0.2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="sd-threshold" first="0" step="0.02" last="1"/>
    <enumeratedValueSet variable="aware-of-removeds?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ticks">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-p-infect?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-infect-init">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="countdown">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-distancing?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="aware-of-dead?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-infection-init">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-z-infection?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sd-test" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count the-dead</metric>
    <metric>count removeds</metric>
    <enumeratedValueSet variable="risk-attitude">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-inf">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-aware">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-remove">
      <value value="0.8"/>
    </enumeratedValueSet>
    <steppedValueSet variable="sd-threshold" first="0" step="0.02" last="1"/>
    <enumeratedValueSet variable="aware-of-removeds?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ticks">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-p-infect?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-infect-init">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="countdown">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-distancing?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="aware-of-dead?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-infection-init">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-z-infection?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sd-test-2" repetitions="20" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count the-dead</metric>
    <enumeratedValueSet variable="risk-attitude">
      <value value="0"/>
      <value value="0.5"/>
      <value value="1"/>
      <value value="2"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-inf">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-aware">
      <value value="2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="p-remove" first="0" step="0.2" last="1"/>
    <steppedValueSet variable="sd-threshold" first="0" step="0.2" last="1"/>
    <enumeratedValueSet variable="aware-of-removeds?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ticks">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-p-infect?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="p-infect-init" first="0.1" step="0.3" last="1"/>
    <enumeratedValueSet variable="countdown">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-distancing?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="aware-of-dead?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-infection-init">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-z-infection?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="isolation" repetitions="20" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count the-dead</metric>
    <metric>count removeds</metric>
    <enumeratedValueSet variable="risk-attitude">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-inf">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-aware">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-remove">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-threshold">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="removed-isolation?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="aware-of-removeds?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ticks">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-p-infect?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-infect-init">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infected-isolation?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="countdown">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-distancing?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="aware-of-dead?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-infection-init">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-z-infection?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="default-test" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count removeds</metric>
    <enumeratedValueSet variable="risk-attitude">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-inf">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-aware">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-remove">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-threshold">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="removed-isolation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="aware-of-removeds?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ticks">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-p-infect?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-infect-init">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infected-isolation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="countdown">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-distancing?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="aware-of-dead?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-infection-init">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-z-infection?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="sdtest" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles with [z-infection = 0]</metric>
    <enumeratedValueSet variable="risk-attitude">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="density">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-inf">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-aware">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-remove">
      <value value="0.8"/>
    </enumeratedValueSet>
    <steppedValueSet variable="sd-chance" first="0.1" step="0.1" last="1"/>
    <enumeratedValueSet variable="sd-threshold">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="removed-isolation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="aware-of-removeds?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-ticks">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-p-infect?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p-infect-init">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="contact-chance">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="infected-isolation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="countdown">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-distancing?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="aware-of-dead?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="z-infection-init">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="modify-z-infection?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
