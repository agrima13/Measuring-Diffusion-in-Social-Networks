;;social-capital consists of trust,  reciprocity,  follow-norm,  and network
;;social-capital is that people have value through society that people belong to
;;I want to simulate value of social-capital
;;through assuming social-capital makes people be cooperative.

globals[ social-capital trust  reciprocity  follow-norm  tie-level]

turtles-own
[ 
  cooperative               ;; if a turtles has SC, the turtle is non-cooperative
  non-cooperative           ;; if a turtlesdoesn't have SC, the turtle is non-cooperative 
  benefit-from-SC           ;; 
  cost-from-SC              ;;   
  social-benefit            ;; 
]


to setup1
  clear-all
  setup-nodes
  setup-social-network
  ask n-of inital-SC turtles  ;; inital trust of society turtles living in 
    [ attend-social-relation  ]
  ask links [ set color white ]
 
end

to setup-nodes
  set-default-shape turtles "person"
  crt num-people
  [ set size 1.5
    setxy (random-xcor * 1.0) (random-ycor * 1.0)
    not-attend-social-relation 
  ]
end

to setup-social-network
let num-links (num-network * num-people) / 2  ;; haw many network a turtle has averagely 
  while [count links < num-links ]
  [ask one-of turtles
    [let choice (min-one-of (other turtles with [not link-neighbor? myself])
                   [distance myself])
      if choice != nobody [ create-link-with choice ] ]
  ]
end

to setup-social-capital  ;; component of social-capital
  set social-capital 10
  set trust 2
  set reciprocity 2
  set follow-norm 2
  set tie-level 2
  set social-capital trust + reciprocity + follow-norm + tie-level
 end

to go ;; turtles' behavior
  have-relationshop
  ask turtles 
  [ set label total-benefit-from-SC + cost-from-SC ] 
  create-society
  tick
  if total-benefit-from-SC = 100 [ stop] 
end

to attend-social-relation   ;; turtle procedure
  set cooperative true
  set non-cooperative false
  set color sky
  set benefit-from-SC (social-capital + transaction-cost)
  set cost-from-SC (social-capital - benefit-from-SC)
end

to not-attend-social-relation   ;; turtle procedure
  set cooperative false
  set non-cooperative true
  set color pink
end

to have-relationshop
  ask turtles with [cooperative]
    [ ask link-neighbors with [not cooperative]
        [ if random-float 100 < altruistic-tendency
            [ attend-social-relation ] ] ]
end

to-report total-benefit-from-SC
  report (sum [benefit-from-SC] of (turtles with [cooperative]))
end

to-report total-benefit-from-SC-person
  report (sum [benefit-from-SC] of (turtles with [cooperative] )
     / ((count turtles with [cooperative]) + ( count turtles with [non-cooperative])))
end

to-report each-benefit ;;each person's benefit in the same society
  report (sum ([total-benefit-from-SC + cost-from-SC] of (turtles with [cooperative])) 
    / (count turtles with [cooperative]))
end

to-report cost-from-total-SC ;; this is same meaning with free-riders' benefit
  report ((sum ([cost-from-SC ] of (turtles with [cooperative])) ) / (count turtles with [non-cooperative]))
  - ((sum ([cost-from-SC ] of (turtles with [cooperative])) ) / (count turtles with [cooperative]))
end

to-report benefit-from-free-ride ;; this is same meaning with social-benefit
  report ((sum ([cost-from-SC ] of (turtles with [cooperative])) ) / (count turtles with [non-cooperative]))
end

;; how good place people live in
;; I think the higher benefit-from-SC a society has, 
;; the better place people live in.

to  create-society 
  ask patches [
    if total-benefit-from-SC >= 0 and total-benefit-from-SC <= 4 [ set pcolor black ]
    if total-benefit-from-SC >= 5 and total-benefit-from-SC <= 9 [ set pcolor 41 ]
    if total-benefit-from-SC >= 10 and total-benefit-from-SC <= 14 [ set pcolor 42 ]
    if total-benefit-from-SC >= 15 and total-benefit-from-SC <= 19 [ set pcolor 43 ]
    if total-benefit-from-SC >= 20 and total-benefit-from-SC <= 24 [ set pcolor 44 ]
    if total-benefit-from-SC >= 25 and total-benefit-from-SC <= 29 [ set pcolor 45 ]
    if total-benefit-from-SC >= 30 and total-benefit-from-SC <= 34 [ set pcolor 46 ]
    if total-benefit-from-SC >= 35 and total-benefit-from-SC <= 39 [ set pcolor 47 ]
    if total-benefit-from-SC >= 40 and total-benefit-from-SC <= 44 [ set pcolor 48 ]
    if total-benefit-from-SC >= 45 and total-benefit-from-SC <= 49 [ set pcolor 49 ]
    if total-benefit-from-SC >= 50 and total-benefit-from-SC <= 54 [ set pcolor 50 ]
    if total-benefit-from-SC >= 55 and total-benefit-from-SC <= 59 [ set pcolor 61 ]
    if total-benefit-from-SC >= 60 and total-benefit-from-SC <= 64 [ set pcolor 62 ]
    if total-benefit-from-SC >= 65 and total-benefit-from-SC <= 69 [ set pcolor 63 ]
    if total-benefit-from-SC >= 70 and total-benefit-from-SC <= 74 [ set pcolor 64 ]
    if total-benefit-from-SC >= 75 and total-benefit-from-SC <= 79 [ set pcolor 65 ]
    if total-benefit-from-SC >= 80 and total-benefit-from-SC <= 84 [ set pcolor 66 ]
    if total-benefit-from-SC >= 85 and total-benefit-from-SC <= 89 [ set pcolor 67 ]
    if total-benefit-from-SC >= 90 and total-benefit-from-SC <= 94 [ set pcolor 68 ]
    if total-benefit-from-SC >= 95 and total-benefit-from-SC <= 99 [ set pcolor 69 ]
    if total-benefit-from-SC >= 100 [ set pcolor 125.9 ]
    ]
end    

@#$#@#$#@
GRAPHICS-WINDOW
377
10
816
470
16
16
13.0
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks

BUTTON
11
13
85
46
NIL
setup1
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
80
13
143
46
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
12
50
143
83
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

SLIDER
7
95
193
128
num-people
num-people
0
1000
386
1
1
person
HORIZONTAL

SLIDER
8
153
190
186
num-network
num-network
0
30
4
1
1
NIL
HORIZONTAL

TEXTBOX
14
136
216
162
각 사람들이 가진 네트워크의 수
12
0.0
1

SLIDER
9
208
194
241
inital-SC
inital-SC
0
10
6
1
1
level
HORIZONTAL

TEXTBOX
11
190
161
208
초기 사회적 자본의 정도
12
0.0
1

TEXTBOX
15
248
192
274
한 사회에서의 거래비용의 정도
12
0.0
1

SLIDER
9
264
193
297
transaction-cost
transaction-cost
0
100
18
1
1
NIL
HORIZONTAL

TEXTBOX
11
304
198
330
이해관계와 관계없는 이타적 성향
12
0.0
1

SLIDER
10
322
194
355
altruistic-tendency
altruistic-tendency
0
100
62
1
1
NIL
HORIZONTAL

MONITOR
219
15
375
64
Social-Capital-Value
total-benefit-from-SC
17
1
12

MONITOR
218
66
374
115
benefit-from-SC
each-benefit
17
1
12

MONITOR
228
170
386
219
cost-from-SC
cost-from-total-SC
17
1
12

MONITOR
226
316
383
365
each person' benefit
total-benefit-from-SC-person
17
1
12

MONITOR
224
238
378
287
NIL
benefit-from-free-ride
17
1
12

TEXTBOX
228
286
378
312
each person's benefit from free ride
12
0.0
1

TEXTBOX
227
220
422
246
무임승차로 인한 사회전체의 손실
12
0.0
1

TEXTBOX
251
142
401
168
사회적자본의 확충을 위한 사회적 비용
12
0.0
1

@#$#@#$#@
WHAT IS IT?
-----------
사회적 관계 속에서 사회적 자본의 가치 측정


HOW IT WORKS
------------
이 모형은 네트워크 형성에서 초기 사회적자본이 사회적관계 속에서 협력과 비협력의 가정을 통해 사회적 자본의 가치를 측정한다.


HOW TO USE IT
-------------
num-people : 초기 사람의 수
num-network : 사람들이 가진 네트워크의 수
inital-SC : 사회에 존재하는 초기 사회적 자본의 양, 이 사회적 자본의 양은 turtle을 통해 나타난다.
transaction-cost: 사회에 존재하는 거래 비용의 정도, 이는 사회적자본이 풍부하다면 사용되지 않아도 되는 가치의 소비를 의미한다.ex) 계약시 공증비용?
altruistic-trendancy : 측정되기 어렵지만 기본적 사회에 존재하는 이타적 성향


THINGS TO NOTICE
----------------
This section could give some ideas of things for the user to notice while running the model.


THINGS TO TRY
-------------
This section could give some ideas of things for the user to try to do (move sliders, switches, etc.) with the model.


EXTENDING THE MODEL
-------------------
This section could give some ideas of things to add or change in the procedures tab to make the model more complicated, detailed, accurate, etc.


NETLOGO FEATURES
----------------
This section could point out any especially interesting or unusual features of NetLogo that the model makes use of, particularly in the Procedures tab.  It might also point out places where workarounds were needed because of missing features.


RELATED MODELS
--------------
This section could give the names of models in the NetLogo Models Library or elsewhere which are of related interest.


CREDITS AND REFERENCES
----------------------
This section could contain a reference to the model's URL on the web if it has one, as well as any other necessary credits or references.
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

sheep
false
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

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
NetLogo 4.1.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
