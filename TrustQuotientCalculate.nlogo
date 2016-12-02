extensions [ nw ]



links-own [strength]

directed-link-breed [ dirlinks dirlink ]
undirected-link-breed [ unlinks unlink ]

turtles-own
[
  node-id
  infected?           ;; if true, the turtle is infectious
  resistant?          ;; if true, the turtle can't be infected
  gossip-check-timer   ;; number of ticks since this turtle's last gossip-check
  node-clustering-coefficient
  distance-from-other-turtles
  chance?
  haspq?
  haspqd?
  pq
  pqd
  tqlabel
  pqlabel
  tq1label
  tq2label
  hastq?
  tq
  tq1
  tq2
  interaction
  k
  pfactor
]

globals
[
  avg
  ;pq
  clustering-coefficient               ;; the clustering coefficient of the network; this is the                                      ;; average of clustering coefficients of all turtles
  average-path-length                  ;; average path length of the network
  clustering-coefficient-of-lattice    ;; the clustering coefficient of the initial lattice
  average-path-length-of-lattice       ;; average path length of the initial lattice
  infinity                          ;; a very large number.
  links-list
  pv
  pqv
  lab
  m
]


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;



to infect
   
  let ex random count turtles
  ask turtle ex
  [become-infected]
 
end
;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;



to import-network
  clear-all
  set-default-shape turtles "circle"
;;import-attributes
;;  layout-circle (sort turtles) (max-pxcor - 1)
import-links
layout-one
end

;; This procedure reads in a files that contains node-specific attributes
;; including an unique identification number
to import-attributes
  ;; This opens the file, so we can use it.
  file-open "attributes.txt"
  ;; Read in all the data in the file
  ;; data on the line is in this order:
  ;; node-id attribute1 attribute2
  while [not file-at-end?]
  [
    ;; this reads a single line into a three-item list
    let items read-from-string (word "[" file-read-line "]")
    crt 1 [
      set node-id item 0 items
      set size    item 1 items
      set color   item 2 items
    ]
  ]
  file-close
end

;; This procedure reads in a file that contains all the links
;; The file is simply 3 columns separated by spaces.  In this
;; example, the links are directed.  The first column contains
;; the node-id of the node originating the link.  The second
;; column the node-id of the node on the other end of the link.
;; The third column is the strength of the link.

to import-links
  ;; This opens the file, so we can use it.
  let node1 one-of turtles
  let node2 one-of turtles
 ;; file-open "links.txt"
 file-open "zero1.txt"
 let i 0
  ;; Read in all the data in the file
  while [not file-at-end?] ;; and i < 20000]
  [
    ;; this reads a single line into a three-item list
    
    let items read-from-string (word "[" file-read-line "]")
    set node1 get-node (item 0 items)
    set node2 get-node (item 1 items)
    ask node1
    [
      if (node1 != node2) [ create-link-with node2 ]
    ]
    set i i + 1
  ]
  file-close
  go
end

;; Helper procedure for looking up a node by node-id.
to-report get-node [id]
  let i 0
  set i count turtles with [ node-id = id ]
  if (i = 0) [ crt 1 [set node-id id ] ]
  report one-of turtles with [node-id = id]
end





to go
  ;; new edge is green, old edges are gray
  ask links [ set color blue ]
  reset-ticks
  ask turtles[
    set color green
    become-susceptible
       set gossip-check-timer random gossip-check-frequency
       set clustering-coefficient-of-lattice clustering-coefficient
       set pv 1 - ( .99 ^ ((count links * 2 ) / ( count turtles )))
       set avg (count links * 2 / count turtles)
       set haspq? false
       set haspqd? false
       set chance? false
       set pq random-normal-in-bounds 0.6 0.2 0 1 
       set interaction random-exponential-in-bounds 0.6 0 1 
       set haspq? true
       set pqd random-normal 0.5 0.2 
       set haspqd? true
       set hastq? false
    ]
 
  
end

;; used for creating a new node

to-report random-normal-in-bounds [mid dev mmin mmax]
  let result random-normal mid dev
  if result < mmin or result > mmax
    [ report random-normal-in-bounds mid dev mmin mmax ]
  report result
end


to-report random-exponential-in-bounds [mid mmin mmax]
  let result random-exponential mid 
  if result < mmin or result > mmax
    [ report random-exponential-in-bounds mid mmin mmax ]
  report result
end


to-report in-neighborhood? [ hood ]
  report ( member? end1 hood and member? end2 hood )
end


to set-victim-p
  ;ask n-of initial-outbreak-size turtles
  ask turtles with [tqlabel = "P"]
  [set infected? true
  set color orange]
  
end

to set-victim-u
  ;ask n-of initial-outbreak-size turtles
  
  ask turtles with [tqlabel = "U"]
  [set infected? true
  set color orange]
  
end


to set-victim-f
  ;ask n-of initial-outbreak-size turtles
  ask turtles with [tqlabel = "F"]
  [set infected? true
  set color orange]
  
end

to set-trustworthy
  
end


to find-clustering-coefficient
  ifelse all? turtles [count link-neighbors <= 1]
  [
    ;; it is undefined
    ;; what should this be?
    set clustering-coefficient 0
  ]
  [
    let total 0
    ask turtles with [ count link-neighbors <= 1]
      [ set node-clustering-coefficient "undefined" ]
    ask turtles with [ count link-neighbors > 1]
    [
      let hood link-neighbors
      set node-clustering-coefficient (2 * count links with [ in-neighborhood? hood ] /
                                         ((count hood) * (count hood - 1)) )
      ;; find the sum for the value at turtles
      set total total + node-clustering-coefficient
    ]
    ;; take the average
    set clustering-coefficient total / count turtles with [count link-neighbors > 1]
  ]
end

to setup-spatially-clustered-network
  let num-links (6 * count turtles) / 2
  while [count links < num-links ]
  [
    ask one-of turtles
    [
      let choice (min-one-of (other turtles with [not link-neighbor? myself])
                   [distance myself])
      if choice != nobody [ create-link-with choice ]
    ]
  ]
  ; make the network look a little prettier
  repeat 10
  [
    layout-spring turtles links 0.3 (world-width / (sqrt count turtles)) 1
  ]
end

to assign-random
  if all? turtles [haspq?][stop]
  ask turtle 1
  [
set pq random-float 1 
set haspq? true
print pq
  ]

end


to assign-trust

  
  if all? turtles [hastq?][stop]
  ask turtle 1
  [
    set tq pq  
    set hastq? true
    print tq
  ]

end

to assign-pqd
  if all? turtles [haspqd?][stop]
  ask turtles
  [
set pqd random-float 1 
set haspqd? true
print pqd
  ]

end


to become-infected-seed  ;; turtle procedure
  set infected? true
  ;set resistant? false
  set color orange
  
  
end

to become-infected  ;; turtle procedure
  set infected? true
  ;set resistant? false
  set color red
  set chance? true

end

to become-susceptible  ;; turtle procedure
  set infected? false
  ;set resistant? false
  set color green
end





to spread
  if all? turtles [not infected?]
    [ stop ]
  ask turtles
  [
     set gossip-check-timer gossip-check-timer + 1
     if gossip-check-timer >= gossip-check-frequency
       [ set gossip-check-timer 0 ]
  ]
  spread-gossip
  tick
 end


to spread-gossip
   
   ask turtles with [infected? and not chance?]
  
    [ 
    
     become-infected
     if tqlabel = "U" [set pqv random-normal-in-bounds 0.9 0.2 0 1]
     if tqlabel = "P" [set pqv random-normal-in-bounds 0.6 0.2 0 1]
     if tqlabel = "F" [set pqv random-normal-in-bounds 0.1 0.2 0 1] 
       ask link-neighbors with [not infected? ]
      [ 
        
            
        if pqv >= pq
            [ become-infected-seed ]  ] ;]
       
      ] 
     
 end      
           

to spread-gossip-2
   
   ask turtles with [infected? and not chance?]
  
    [ 
    
     become-infected
     if tq1label = "U" [set pqv random-normal-in-bounds 0.9 0.2 0 1]
     if tq1label = "P" [set pqv random-normal-in-bounds 0.6 0.2 0 1]
     if tq1label = "F" [set pqv random-normal-in-bounds 0.1 0.2 0 1] 
       ask link-neighbors with [not infected? ]
      [ 
        
            
        if pqv >= pq
            [ become-infected-seed ]  ] ;]
       
      ] 
     
 end   
   
   
   
to tie-1
ask turtles with [color = orange] 
[
   set pfactor (pq) * (1 - 0.4)
   set tq2 tq1 * pfactor
   
   
]
end 
   
   

   
to tie-2
ask turtles 
[
   let j count link-neighbors
   set tq1 ( interaction / j )
]

;write-tq1
end 
   
   
   
   
      

 to strong-tie
   let m1 ceiling ((20 * (count turtles))/ 100)
let m2 count turtles - m1
let i m2

loop
[
let val item i sort [tq] of turtles 

ask turtles 
[
ifelse tq = val 

[
  set infected? true
  set color orange
  ;print tq
  ;print val
  ;print j
  ;print pfactor
  ]
[
   
 ]  ]
set i i + 1

if i = count turtles [ stop ]]

 end   





to normalize
 ask turtles
 [
   let max1 max [tq1] of turtles
   let min1 min [tq1] of turtles 
   set tq1 (tq1 - min1) / (max1 - min1)
   ;print tq
 ] 
end



to strong-tie-2
   let m1 ceiling ((20 * (count turtles))/ 100)
let m2 count turtles - m1
let i m2

loop
[
let val item i sort [tq1] of turtles 

ask turtles 
[
ifelse tq1 = val 

[
  set infected? true
  set color orange
  ;print tq
  ;print val
  ;print j
  ;print pfactor
  ]
[
   
 ]  ]
set i i + 1

if i = count turtles [ stop ]]

 end  


to normalize2 
 ask turtles 
 [
   let max1 max [tq1] of turtles
   let min1 min [tq1] of turtles
   set tq1 (tq1 - min1) / (max1 - min1)
   
 ] 
end



 

to model-select
  ask turtles with [color = orange ]
[
ifelse tq2label = "U" or tq2label = "NS"

[set infected? false
set color green]
[]
]




end


to refine-tie
  
  
end

;*************************************************************** SET LABEL ************************************************************************************
;***************************************************************************************************************************************************************





to set-strength-2
  
  ask turtles [
    if tq1 = 0 [set tq1label "NS"]
    if tq1 > 0 and tq1 <= 0.04044343 [set tq1label "U"] 
    if tq1 > 0.04044343 and tq1 <= 0.41128014 [set tq1label "P"]
    if tq1 > 0.41128014 [set tq1label "F"] 
     
  ]

  
end


to set-strength
  
  ask turtles [
    if tq2 = 0 [set tq2label "NS"]
    if tq2 > 0 and tq2 <= 0.03720099 [set tq2label "U"] 
    if tq2 > 0.03720099 and tq2 <= 0.27707763 [set tq2label "P"]
    if tq2 > 0.27707763 [set tq2label "F"] 
     
  ]

  
end










;**************************************************************************************************************************************************************************************
;**************************************************************************************************************************************************************************************
;**************************************************************************************************************************************************************************************


;; This code is the heart of the "preferential attachment" mechanism, and acts like
;; a lottery where each node gets a ticket for every connection it already has.
;; While the basic idea is the same as in the Lottery Example (in the Code Examples
;; section of the Models Library), things are made simpler here by the fact that we
;; can just use the links as if they were the "tickets": we first pick a random link,
;; and than we pick one of the two ends of that link.
to-report find-partner
  report [one-of both-ends] of one-of links
  
end


 



;;;;;;;;;;;;;;
;;; Layout ;;;
;;;;;;;;;;;;;;

;; resize-nodes, change back and forth from size based on degree to a size of 1
to resize-nodes
  ifelse all? turtles [size <= 1]
  [
    ;; a node is a circle with diameter determined by
    ;; the SIZE variable; using SQRT makes the circle's
    ;; area proportional to its degree
    ask turtles [ set size sqrt count link-neighbors ]
  ]
  [
    ask turtles [ set size 1 ]
  ]
end

;to-report get-links-to-use
;  report ifelse-value (links-to-use = "directed")
;    [ dirlinks ]
;    [ unlinks ] 
;end



to betweenness
  centrality task nw:betweenness-centrality
end

to eigenvector
  centrality task nw:eigenvector-centrality
end

to closeness
  ;centrality task nw:closeness-centrality
  ask turtles [set label nw:closeness-centrality]
end

;; Takes a centrality measure as a reporter task, runs it for all nodes
;; and set labels, sizes and colors of turtles to illustrate result
to centrality [ measure ]
  ;nw:set-context turtles get-links-to-use
  
  ask turtles [
    let res (runresult measure) ;; run the task for the turtle
    ifelse is-number? res [
      let c ( ( count turtles - 1 ) * ( count turtles - 2 ) * 0.5 )
      let res1 res / c
      set label precision res1 2
      set size res ;; this will be normalized later
      
    ]
    [ ;; if the result is not a number, it is because eigenvector returned false (in the case of disconnected graphs
      
      set label res
      set size 1
    ]
  ]
  normalize-sizes-and-colors
end


to normalize-sizes-and-colors
  if count turtles > 0 [
    let sizes sort [ size ] of turtles ;; initial sizes in increasing order
    let delta last sizes - first sizes ;; difference between biggest and smallest
    ifelse delta = 0 [ ;; if they are all the same size
      ask turtles [ set size 1 ]
    ]
    [ ;; remap the size to a range between 0.5 and 2.5
      ask turtles [ set size ((size - first sizes) / delta) * 2 + 0.5 ]
    ]
    ask turtles [ set color scale-color red size 0 5 ] ; using a higher range max not to get too white...
  ]
end


to layout-one
  ;; the number 3 here is arbitrary; more repetitions slows down the
  ;; model, but too few gives poor layouts
  repeat 3 [
    ;; the more turtles we have to fit into the same amount of space,
    ;; the smaller the inputs to layout-spring we'll need to use
    let factor sqrt count turtles
    ;; numbers here are arbitrarily chosen for pleasing appearance
    layout-spring turtles links (1 / factor) (7 / factor) (1 / factor)
    display  ;; for smooth animation
  ]
  ;; don't bump the edges of the world
  let x-offset max [xcor] of turtles + min [xcor] of turtles
  let y-offset max [ycor] of turtles + min [ycor] of turtles
  ;; big jumps look funny, so only adjust a little each time
  set x-offset limit-magnitude x-offset 0.1
  set y-offset limit-magnitude y-offset 0.1
  ask turtles [ setxy (xcor - x-offset / 2) (ycor - y-offset / 2) ]
end

to-report limit-magnitude [number limit]
  if number > limit [ report limit ]
  if number < (- limit) [ report (- limit) ]
  report number
end

to reset
  ask turtles [become-susceptible
   ;set pq 1
   ;set haspq? false
   set chance? false 
    ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layouts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to redo-layout [ forever? ]
  if layout = "radial" and count turtles > 1 [
    layout-radial turtles links ( max-one-of turtles [ count my-links + count my-out-links + count my-in-links ] )
  ]
  if layout = "spring" [
    let factor sqrt count turtles
    if factor = 0 [ set factor 1 ]
    repeat ifelse-value forever? [ 1 ] [ 50 ] [
      layout-spring turtles links (1 / factor) (14 / factor) (1.5 / factor)
      display
      if not forever? [ wait 0.005 ]
    ]
  ]
  if layout = "circle" [
    layout-circle sort turtles max-pxcor * 0.9
  ]
  if layout = "tutte" [
    layout-circle sort turtles max-pxcor * 0.9
    repeat 10 [
      layout-tutte max-n-of (count turtles * 0.5) turtles [ count my-links ] links 12
    ]
  ]
end

to layout-once
  redo-layout false
end

to spring-forever
  set layout "spring" 
  redo-layout true
end



to write-trust-file
 file-open "trust1.txt"
 ifelse tq2 = 0 [][
 file-write tq2
 file-type "\n" ]
 file-close
 end





to write-tq1
 file-open "trust2.txt"
 ifelse tq1 = 0 [][
 file-write tq1
 file-type "\n" ]
 file-close
 end
@#$#@#$#@
GRAPHICS-WINDOW
519
82
931
515
25
25
7.9
1
10
1
1
1
0
0
0
1
-25
25
-25
25
1
1
1
ticks
60.0

PLOT
8
330
219
517
Degree Distribution (log-log)
log(degree)
log(# of nodes)
0.0
0.3
0.0
0.3
true
false
"" ""
PENS
"default" 1.0 0 -10899396 true "" "if not plot? [ stop ]\nlet max-degree max [count link-neighbors] of turtles\n;; for this plot, the axes are logarithmic, so we can't\n;; use \"histogram-from\"; we have to plot the points\n;; ourselves one at a time\nplot-pen-reset  ;; erase what we plotted before\n;; the way we create the network there is never a zero degree node,\n;; so start plotting at degree one\nlet degree 1\nwhile [degree <= max-degree] [\n  let matches turtles with [count link-neighbors = degree]\n  if any? matches\n    [ plotxy log degree 10\n             log (count matches) 10 ]\n  set degree degree + 1\n]"

PLOT
8
143
220
329
Degree Distribution
degree
# of nodes
1.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -13345367 true "" "if not plot? [ stop ]\nlet max-degree max [count link-neighbors] of turtles\nplot-pen-reset  ;; erase what we plotted before\nset-plot-x-range 1 (max-degree + 1)  ;; + 1 to make room for the width of the last bar\nhistogram [count link-neighbors] of turtles"

SWITCH
2
10
92
43
plot?
plot?
0
1
-1000

SWITCH
2
44
92
77
layout?
layout?
0
1
-1000

MONITOR
1201
17
1268
62
# of nodes
count turtles
3
1
11

SLIDER
1
79
93
112
gossip-check-frequency
gossip-check-frequency
1
20
1
1
1
NIL
HORIZONTAL

BUTTON
1157
482
1220
515
clear
clear-all\n
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
298
343
429
376
Select Strong Ties
strong-tie
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
298
104
417
137
spread gossip
spread\n\n\n
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
1111
167
1203
212
Average Node Degree
(count links * 2 ) / ( count turtles )
3
1
11

MONITOR
1022
167
1108
212
Gossipers
count turtles with [color = red or color = orange]
17
1
11

BUTTON
247
52
311
85
Reset
reset
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
315
54
412
87
layout
layout-once
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
328
10
466
55
layout
layout
"spring" "circle" "tutte"
0

BUTTON
94
11
157
44
write
write-trust-file
NIL
1
T
TURTLE
NIL
NIL
NIL
NIL
1

BUTTON
259
11
325
44
import
import-network
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
1020
20
1077
65
U
count turtles with [tqlabel = \"U\"]
17
1
11

MONITOR
1080
19
1137
64
P
count turtles with [tqlabel = \"P\"]
17
1
11

MONITOR
1140
18
1197
63
F
count turtles with [tqlabel = \"F\"]
17
1
11

MONITOR
958
21
1015
66
NS
count turtles with [pqlabel = \"NS\"]
17
1
11

MONITOR
1021
68
1078
113
U1
count turtles with [color = red or color = orange and tq2label = \"U\"]
17
1
11

MONITOR
1081
68
1138
113
P2
count turtles with [color = red or color = orange and tq2label = \"P\"]
17
1
11

MONITOR
1141
67
1198
112
F1
count turtles with [color = red or color = orange and tq2label = \"F\"]
17
1
11

MONITOR
1022
117
1079
162
U2
(count turtles with [color = red or color = orange and tqlabel = \"U\"]/ count turtles with [color = red or color = orange])* 100
17
1
11

MONITOR
1083
117
1140
162
P2
(count turtles with [color = red or color = orange and tqlabel = \"P\"] / count turtles with [color = red or color = orange]) * 100
17
1
11

MONITOR
1144
117
1201
162
F2
(count turtles with [color = red or color = orange and tqlabel = \"F\"]/ count turtles with [color = red or color = orange]) * 100
17
1
11

BUTTON
420
103
495
136
setcolor
ask turtles [ if pqlabel = \"U\"[set color violet] ]\nask turtles [ if pqlabel = \"P\"[set color yellow] ]\nask turtles [ if pqlabel = \"F\"[set color magenta] ]
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
467
10
654
43
SpreadWithoutUnconcerned
\n\nlet i 0\nlet cg 0\nlet newcg 0\nloop\n[\nreset\nstrong-tie\n;strong-tie1\nmodel-select\nspread-gossip\nset newcg count turtles with [color = red or color = orange]\nset cg (cg + newcg)\nprint cg / 10\nset i i + 1\nif i = 10 [ stop ]\n\n]\n\n
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
655
10
824
43
SpreadWithUnconcerned
\n\nlet i 0\nlet cg 0\nlet newcg 0\nloop\n[\nreset\nstrong-tie\n\nspread-gossip\nset newcg count turtles with [color = red or color = orange]\nset cg (cg + newcg)\nprint cg / 10\nset i i + 1\nif i = 10 [ stop ]\n\n]\n\n
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
262
164
462
314
Interaction
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
"default" 1.0 0 -16777216 true "" "set-plot-y-range 0 count turtles / 2\nlet ys sort [ interaction ] of turtles\nlet y-min precision first ys 3\nlet y-max precision last ys 3\nif y-max > y-min [\n  set-plot-pen-interval (y-max - y-min) / 10\n  set-plot-x-range y-min y-max\n  histogram ys\n]"

BUTTON
299
383
400
416
Model Select
model-select
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
344
448
427
481
NIL
normalize
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
1048
308
1119
341
Spread
set-strength-2\nlet i 0\nlet cg 0\nlet newcg 0\nloop\n[\nreset\nstrong-tie-2\nspread-gossip-2\nset newcg count turtles with [color = red or color = orange]\nset cg (cg + newcg)\nprint cg \nset i i + 1\n\nif i = 10 [ stop ]\n\n]\n\n
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
97
45
175
78
writetq1
write-tq1
NIL
1
T
TURTLE
NIL
NIL
NIL
NIL
1

BUTTON
955
222
1018
255
First
tie-2
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
1115
374
1210
407
Refinement
strong-tie-2\ntie-1\n\n
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
1142
320
1242
353
Spread-New
\nset-strength\nlet i 0\nlet cg 0\nlet newcg 0\nloop\n[\nreset\nstrong-tie-2\nmodel-select\nspread-gossip-2\nset newcg count turtles with [color = red or color = orange]\nset cg (cg + newcg)\nprint cg\nset i i + 1\n\nif i = 10 [ stop ]\n\n]\n\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

In some networks, a few "hubs" have lots of connections, while everybody else only has a few.  This model shows one way such networks can arise.

Such networks can be found in a surprisingly large range of real world situations, ranging from the connections between websites to the collaborations between actors.

This model generates these networks by a process of "preferential attachment", in which new network members prefer to make a connection to the more popular existing members.

## HOW IT WORKS

The model starts with two nodes connected by an edge.

At each step, a new node is added.  A new node picks an existing node to connect to randomly, but with some bias.  More specifically, a node's chance of being selected is directly proportional to the number of connections it already has, or its "degree." This is the mechanism which is called "preferential attachment."

## HOW TO USE IT

Pressing the GO ONCE button adds one new node.  To continuously add nodes, press GO.

The LAYOUT? switch controls whether or not the layout procedure is run.  This procedure attempts to move the nodes around to make the structure of the network easier to see.

The PLOT? switch turns off the plots which speeds up the model.

The RESIZE-NODES button will make all of the nodes take on a size representative of their degree distribution.  If you press it again the nodes will return to equal size.

If you want the model to run faster, you can turn off the LAYOUT? and PLOT? switches and/or freeze the view (using the on/off button in the control strip over the view). The LAYOUT? switch has the greatest effect on the speed of the model.

If you have LAYOUT? switched off, and then want the network to have a more appealing layout, press the REDO-LAYOUT button which will run the layout-step procedure until you press the button again. You can press REDO-LAYOUT at any time even if you had LAYOUT? switched on and it will try to make the network easier to see.

## THINGS TO NOTICE

The networks that result from running this model are often called "scale-free" or "power law" networks. These are networks in which the distribution of the number of connections of each node is not a normal distribution --- instead it follows what is a called a power law distribution.  Power law distributions are different from normal distributions in that they do not have a peak at the average, and they are more likely to contain extreme values (see Albert & Barabási 2002 for a further description of the frequency and significance of scale-free networks).  Barabási and Albert originally described this mechanism for creating networks, but there are other mechanisms of creating scale-free networks and so the networks created by the mechanism implemented in this model are referred to as Barabási scale-free networks.

You can see the degree distribution of the network in this model by looking at the plots. The top plot is a histogram of the degree of each node.  The bottom plot shows the same data, but both axes are on a logarithmic scale.  When degree distribution follows a power law, it appears as a straight line on the log-log plot.  One simple way to think about power laws is that if there is one node with a degree distribution of 1000, then there will be ten nodes with a degree distribution of 100, and 100 nodes with a degree distribution of 10.

## THINGS TO TRY

Let the model run a little while.  How many nodes are "hubs", that is, have many connections?  How many have only a few?  Does some low degree node ever become a hub?  How often?

Turn off the LAYOUT? switch and freeze the view to speed up the model, then allow a large network to form.  What is the shape of the histogram in the top plot?  What do you see in log-log plot? Notice that the log-log plot is only a straight line for a limited range of values.  Why is this?  Does the degree to which the log-log plot resembles a straight line grow as you add more node to the network?

## EXTENDING THE MODEL

Assign an additional attribute to each node.  Make the probability of attachment depend on this new attribute as well as on degree.  (A bias slider could control how much the attribute influences the decision.)

Can the layout algorithm be improved?  Perhaps nodes from different hubs could repel each other more strongly than nodes from the same hub, in order to encourage the hubs to be physically separate in the layout.

## NETWORK CONCEPTS

There are many ways to graphically display networks.  This model uses a common "spring" method where the movement of a node at each time step is the net result of "spring" forces that pulls connected nodes together and repulsion forces that push all the nodes away from each other.  This code is in the `layout-step` procedure. You can force this code to execute any time by pressing the REDO LAYOUT button, and pressing it again when you are happy with the layout.

## NETLOGO FEATURES

Nodes are turtle agents and edges are link agents. The `layout-spring` primitive places the nodes, as if the edges are springs and the nodes are repelling each other.

Though it is not used in this model, there exists a network extension for NetLogo that you can download at: https://github.com/NetLogo/NW-Extension.

## RELATED MODELS

See other models in the Networks section of the Models Library, such as Giant Component.

See also Network Example, in the Code Examples section.

## CREDITS AND REFERENCES

This model is based on:  
Albert-László Barabási. Linked: The New Science of Networks, Perseus Publishing, Cambridge, Massachusetts, pages 79-92.

For a more technical treatment, see:  
Albert-László Barabási & Reka Albert. Emergence of Scaling in Random Networks, Science, Vol 286, Issue 5439, 15 October 1999, pages 509-512.

Barabási's webpage has additional information at: http://www.nd.edu/~alb/

The layout algorithm is based on the Fruchterman-Reingold layout algorithm.  More information about this algorithm can be obtained at: http://citeseer.ist.psu.edu/fruchterman91graph.html.

For a model similar to the one described in the first extension, please consult:  
W. Brian Arthur, "Urban Systems and Historical Path-Dependence", Chapt. 4 in Urban systems and Infrastructure, J. Ausubel and R. Herman (eds.), National Academy of Sciences, Washington, D.C., 1988.


## HOW TO CITE

If you mention this model in a publication, we ask that you include these citations for the model itself and for the NetLogo software:

* Wilensky, U. (2005).  NetLogo Preferential Attachment model.  http://ccl.northwestern.edu/netlogo/models/PreferentialAttachment.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.
* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 2005 Uri Wilensky.

![CC BY-NC-SA 3.0](http://i.creativecommons.org/l/by-nc-sa/3.0/88x31.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.
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
NetLogo 5.0.5
@#$#@#$#@
set layout? false
set plot? false
setup repeat 300 [ go ]
repeat 100 [ layout ]
@#$#@#$#@
@#$#@#$#@
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
