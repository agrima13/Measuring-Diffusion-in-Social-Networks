globals [total ;;total number of informed people
  n ;;previous number of informed people
  m-current  ;;number of negatively informed strong ties
  j-current  ;;number of negatively informed weak ties
  mx-current ;;number of positively informed strong ties
  jx-current ;;number of positively informed weak ties
  a ;;the power of negative w-o-m over positive w-o-m
  x ;;recording the highlighted x-coordinate
  y ;;recording the highlighted y-coordinate
  tickx] ;;tick numebr when the observed patch change opinion
  
patches-own [state ;;state of patch: not informed (0)/ informed (1)
   threshold ;;threshold for each patch: randomly assigned
   alpha ;;advertising strength
   beta-s ;;the strengh of strong ties
   beta-w ;;the strengh of weak ties
   prob ;;probability of changing into negatively informed state
   m ;;number of negatively informed strong ties
   j ;;number of negatively informed weak ties
   content ;;positive (-1) or negative (1)
   jx ;;number of positively informed weak ties
   mx ;;number of positively informed strong ties
   probx] ;;probibility of changing into positively informed state
   
to setup
  ;;preparing the world
  ca 
  reset-ticks
 
  watch-one ;;recording which patch is being highlighted
  ask patches [ set probx 0 set jx 0 set mx 0 set content 0 set state 0 set threshold random-float 1 set alpha advertising set beta-s strong-ties set beta-w weak-ties]
  set tickx 0 ;;setting the starting point of the tick count for the highlighted patch
  set a 1.133 ;;setting the power of negative w-o-m over positive w-o-m
end

to watch-one ;;recording which patch is being highlighted
  ask one-of patches [ 
    set x pxcor
    set y pycor
    watch-me]
end

to go
  set n count patches with [state = 1] ;;counting the total informed pacth(es)
  check ;;change into informed or not
  recolor ;;recoloring the patch(es) according to their state and content
  reportwatch ;;preparing the value of highlighted patch to be reported to user
  set total count patches with [state = 1] ;;recount the new total number of informed patch(es)
  ifelse (n != total) [tick] [stop] ;;stop if the number of informed people is not changing
end


to check ;;change into informed or not and what the content of the patch is
  
  if (ticks = 2) ;;When the tick = 2 (after there are people who are informed through advertisement, assign the content value to them)
      [ask n-of (%-positive-rumor / 100 * n) patches with [state = 1 and content = 1] [set content -1]]
  
  ask patches with [state = 0 or ( pxcor = x and pycor = y)] ;;start checking the uninformed patches one by one for potential power to change into informed
     
     [set m count neighbors with [state = 1 and content = 1] ;;setting up the value for m, mx j and jx
      set mx count neighbors with [state = 1 and content = -1]   
      set j (n - m - count patches with [content = -1 and state = 1])
      set jx (n - mx - count patches with [content = 1 and state = 1])
      
      ifelse (ticks < 2) ;;when the ticks < 2, no content difference yet so different formula will be used
          [set prob (1 - ((1 - alpha) * ((1 - beta-w) ^ j) * ((1 - beta-s) ^ m)))]
          [ifelse (count patches with [content = -1 and state = 1] > 0) ;;when ticks > 2, there will be two different content so probility will be counted seperately
            [set prob (1 - ((1 - alpha) * ((1 - beta-w) ^ j) * ((1 - beta-s) ^ m)))
             set probx (1 - ((1 - alpha) * ((1 - beta-w) ^ jx) * ((1 - beta-s) ^ mx)))
             set probx probx * a]
            [set prob (1 - ((1 - alpha) * ((1 - beta-w) ^ j) * ((1 - beta-s) ^ m)))]]
      
      ifelse (probx > 0 and prob > 0) ;;checking whether there are a probability to change state
        [if ( abs (prob - probx) >= threshold) ;;checking whether the value is still able to exceed threshold after taking other content into consideration
         [set state 1 ;;changing state to informed
          ifelse (prob > probx) ;;if the probability for negative content is bigger than the positive content, change content to negative and the other way round
           [set content 1]
           [set content -1]]]
        [ifelse (probx = 0) ;;if no positive content 
            [if (prob >= threshold) [ ;;change into negative once the probability reached the threshold
              set state 1
              set content 1]]
            [if (probx >= threshold) [ ;;change into postive once the probability reached the threshold
              set state 1
              set content -1]]]]
end

to recolor
  ask patches with [state = 1] [ ifelse (content = 1) [ set pcolor red ] [set pcolor blue]]
end

to reportwatch ;;preparing the value of highlighted patch to be reported to user
  ask patches
  [if (pxcor = x) and (pycor = y) [
    set m-current count neighbors with [state = 1 and content = 1]
    set mx-current count neighbors with [state = 1 and content = -1]
    set j-current (count patches with [state = 1] - m-current - count patches with [content = -1 and state = 1])
    set jx-current (count patches with [state = 1] - mx-current - count patches with [content = 1 and state = 1])
    if ((tickx = 0) and [state] of patch-at x y = 1) [set tickx ticks]] ] ;;counting when the highlighted patch change state
end
@#$#@#$#@
GRAPHICS-WINDOW
440
11
860
452
20
20
10.0
1
10
1
1
1
0
1
1
1
-20
20
-20
20
0
0
1
ticks
30.0

SLIDER
23
56
220
89
weak-ties
weak-ties
0
0.1
0.015
0.001
1
NIL
HORIZONTAL

TEXTBOX
32
38
182
56
Strength
13
0.0
1

SLIDER
23
89
220
122
strong-ties
strong-ties
0
0.1
0.071
0.001
1
NIL
HORIZONTAL

SLIDER
23
122
220
155
advertising
advertising
0
0.01
0.0090
0.001
1
NIL
HORIZONTAL

BUTTON
259
59
326
92
setup
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
337
58
400
91
go
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

MONITOR
257
116
404
169
total informed people
total
0
1
13

MONITOR
257
169
404
222
total uninformed people
world-height * world-width - total
0
1
13

SLIDER
23
164
221
197
%-positive-rumor
%-positive-rumor
0
100
64
1
1
%
HORIZONTAL

MONITOR
74
251
229
304
Total negative w-o-m
count patches with [content = 1]
17
1
13

MONITOR
228
251
377
304
Total positive w-o-m
count patches with [content = -1]
17
1
13

MONITOR
88
317
174
370
Strong-ties
m-current + mx-current
0
1
13

MONITOR
39
370
129
423
(-) strong-ties
m-current
0
1
13

MONITOR
129
370
222
423
(+) strong-ties
mx-current
0
1
13

MONITOR
271
317
350
370
Weak-ties
j-current + jx-current
0
1
13

MONITOR
222
370
313
423
(-) weak-ties
j-current
0
1
13

MONITOR
313
370
408
423
(+) weak-ties
jx-current
0
1
13

MONITOR
188
317
257
370
tick-stop
tickx
0
1
13

@#$#@#$#@
## WHAT IS IT?

In brief, this simulation is built on as a continuation to the previous model on Talk of the Network (Goldenberg et al., 2001) with an add-on feature.

This simulation will let user observe how information is being spread in the community. For this simulation, members of the community will be stationary. If the collection of information coming from others with strong and weak ties is strong enough to pass their threshold, they will turn from the state of “uninformed” to “informed”. Each member of the community will have its own unique threshold of getting into the informed state. And just as in real life, members will put higher value in the information spread by someone close to them than those who are not directly connected to them.

In general Word-of-Mouth (w-o-m) can be defined as the sharing of information (opinions or recommendations) through communication among people about products and services (Westbrook, 1987). In business area, a lot of companies are aiming to increase their brands knowledge through various word-of-mouth methods – offline (e.g. face-to-face) as well as online (e.g. through SNS). Of course, advertisement also plays a role in the sharing of information by being the initial contact as well as one of the influencers in getting people to know a brand. But, contrary to popular belief, in long-term, advertisement is not as powerful as w-o-m in contributing to the spread of information (Buttle, 1998; Keller & Berry, 2003; Laborie, 2006). Not only brand knowledge, researchers also found that w-o-m influenced almost 70% of customer’s decision to buy (Balter, 2008). 

For this simulation, there will be two kinds of relationship defined – strong ties and weak ties. The differentiation of the relationship is based on “the strength of weak ties” theory by Granovetter (1973). By observing the micro-level differences, a lot of differences can be observed on the macro-level phenomena. According to Granovetter, as contrary to weak ties, strong ties are characterized with a more stable, frequent and intimate interaction. Using this logic, in this simulation, strong ties are understood as those who are closely connected to the individual while weak ties are understood as those who are not directly connected to the individual.

As a add-on feature, current simulation will also be observing the effect of different valance of the information (e.g. positive or negative) towards the rate and end-result of the spreading of information. Moreover, according to the result collected by Deitz and Cakim (2005), the number of negative w-o-m overpower positive w-o-m by 1.133 times.

The add-on feature in this simulation is based on a journal article titled "On Braggarts and Gossips: A Self-Enhancement Account of Word-of-Mouth Generation and Transmission" by Angelis M.D. et al. (2012).

## HOW IT WORKS

As have been mentioned before, each member has its TRESHOLD. To start off, user can adjust a set of variables which will be constant throughout the simulation - STRONG-TIES, WEAK-TIES, ADVERTISING and %-OF-POSITIVE-RUMOR.

Besides that, each member also has its own STATE - informed "1" or not "0"). With the implementation of watch-one procedure, user can observe and learn more about the detail of the process from one of the randomly selected member which is being put into the spotlight. From this member, user can track the NO. OF WEAK TIES which can tell us the number of "informed" others in the community which are not directly connected to the member. The other one is to track the NO. OF STRONG TIES which is the number of "informed" others in the surrounding which are directly connected to the member (neighbors).

The probability of a member to move from not knowing to knowing is defined using a formula stated in the journal article (Goldenberg et al., 2001):

	prob = (1 - ((1 - alpha) * ((1 - beta-w) ^ j) * ((1 - beta-s) ^ m)))

alpha: ADVERTISING
beta-w: WEAK-TIES
beta-s: STRONG-TIES
j: number of "informed" members in the community who are not directly connected to the highlighted member
m:  number of "informed" members who are directly connected to the highlighted member
The member need to have a prob >= threshold to move from uninformed to informed.

As there are two kinds of rumor being spread, the probability for both negative (PROB) and positive rumor (PROBX) will be counted seperately. As been mentioned above, negative w-o-m will have more power over positive w-o-m. So, PROBX = PROBX * A with a = the power of negative w-o-m. After that, members can only move from "uninformed" to "informed" only if:
			abs (prob - probx) >= treshold 


## HOW TO USE IT

As a starter, user can use the sliders provided to adjust the WEAK-TIES, STRONG-TIES, ADVERTISING and %-POSITIVE-RUMOR. 

STRONG-TIES: This is to set how strong the effect of others with a strong connection to the member knowing the information towards the probability of the member knowing the information.
WEAK-TIES: This is to set how strong the effect of others who the member is not directly connected to knowing the information towards the probability of the member knowing the information.
ADVERTISING: This is to set how strong the effect of the advertisement towards the initial as well as the add-on probability of the member knowing the information.
%-POSITIVE-RUMOR: Using this, user can set the specific percentage of positive rumor in the early stage of information transmission.

After that, user can simply click on the SETUP button then the GO button to begin the simulation.

Members who have moved from "uninformed" to "informed" will be given a blue color code (positive) and red color code (negative). This will not change until the end of the simulation after the division of the percentage of positive rumor.


## THINGS TO NOTICE

User can observe how the number of TOTAL INFORMED PEOPLE and TOTAL UNINFORMED PEOPLE changes throughout the simulation. A "slower" speed is recommended to fully observe the changes as this simulation is quite fast. 

Furthermore, user also can see how different percentage of positive rumor can affect the number of people informed. Another thing to observe is the TICK-STOP which will tell user when the highlighted member changes from "uninformed" to "informed".


## THINGS TO TRY

User can try to play around with the sliders. As mentioned before, user can observe how different percentage of positive rumor can affect the end number of people being informed. Besides that, user can also change the power of negative w-o-m over positive w-o-m by changing the setting for a at the code bar.

User may also try out the BehaviorSpace prepared to help them to see the complete quantitative data in Microsoft Excel format. The settings are adjusted according to Goldenberg et al. (2001) except for the percentage of positive rumor. User may also be interested to change the settings to match with the user's needs.


## EXTENDING THE MODEL

One of the important things to be upgraded is the count of ticks. If it's possible to make it a larger count, maybe user will be able to observe the differences in different conditions.
And if researchers manage to find a fix number of power for negative vs. positive w-o-m, it will be very useful and user just need to change the a.
For future simulation, can add "self-enhancement" as been stated in article by Angelis et al. (2012) as an extra variable.

## RELATED MODELS

The previous model of similar topic titled "Talk of the Network" which is based on a journal article titled "Talk of the Network: A Complex Systems Look at the Underlying Process of Word-of-Mouth" by Goldenberg et al. (2001).

## CREDITS AND REFERENCES

Angelis M.D., Bonezzi A., Peluso A. M., Rucker D.D. & Costabile M. "On Braggarts and Gossips: A Self-Enhancement Account of Word-of-Mouth Generation and Transmission". Journal of Marketing Research Vol. XLIX, 551-563. 2012.

Balter, Dave (2008), The Word of Mouth Manual, Vol. 2. Boston: Bzz Pubs.

Buttle FA. (1998). “Word-of-Mouth: Understanding and Managing Referral Marketing,” Journal of Strategic Marketing, 6, 241-254.

Deitz, Sarah, and Idil Cakim. "Online Influence and the Tech-fluentials," July 2005: [URL: http://www.efluentials.com/documents/wommaconferencepaperjuly 132005.pdf].

Goldenberg J., Libai B. & Muller E. "Talk of the Network: A Complex Systems Look at the Underlying Process of Word-of-Mouth". Marketing Letters 12:3, 211-223. 2001. Kluwer Academic Publishers, The Netherlands.

Granovetter MS. (1973). “The Strength of Weak Ties,” American Journal of Sociology, 78(May), 1360-1380.

Keller, Ed, and Jon Berry. The Influentials: One American in Ten Tells the Other Nine How to Vote, Where to Eat, and What to Buy. New York: Free Press, 2003.

Laborie, Jean-Louis. "The Theory Behind Engagement and Integration's Early Experience Across Media." Paper presented at ReThink: 52nd Annual Advertising Research Foundation Annual Conference and Expo, March 20-22, 2006: [URL: http://mail.thearf.org/roymorgan/ Engagement/2006.rethink.ARRThe%20Theory.pres.Laborie.pdf].

Westbrook, Robert A. (1987), “Product/Consumption-Based Affective Responses and Postpurchase Processes,” Journal of Marketing Research, 24 (August), 258–70.
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
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count patches with [state = 1]</metric>
    <metric>count patches with [state = 0]</metric>
    <metric>count patches with [state = 1 and content = 1]</metric>
    <metric>count patches with [state = 1 and content = -1]</metric>
    <enumeratedValueSet variable="strong-ties">
      <value value="0.01"/>
      <value value="0.04"/>
      <value value="0.07"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="advertising">
      <value value="5.0E-4"/>
      <value value="0.00525"/>
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="weak-ties">
      <value value="0.0050"/>
      <value value="0.01"/>
      <value value="0.015"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%-positive-rumor">
      <value value="0"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
