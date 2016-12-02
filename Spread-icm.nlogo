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
  pq
  pqlabel
  betlabel
  cenlabel
  
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
]


;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  set-default-shape turtles "circle"
  ;; make the initial network of two turtles and an edge
  make-node nobody        ;; first node, unattached
  make-node turtle 0      ;; second node, attached to first node
  ask links [ set color blue ]
  reset-ticks
  
end


to printbet
  ;let ys sort [ nw:betweenness-centrality ] of turtles
  ask turtles [set label nw:betweenness-centrality]
    normalize-sizes-and-colors
end


to printeigen
  ;let ys sort [ nw:betweenness-centrality ] of turtles
  ask turtles [set label  nw:eigenvector-centrality]
    normalize-sizes-and-colors
end


to infect
   
  let ex random count turtles
  ask turtle ex
  [become-infected]
 
end


;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;

to go
  ;; new edge is green, old edges are gray
  ask links [ set color blue ]
  make-node find-partner         ;; find partner & use it as attachment
                                ;; point for new node
  tick
  ;if layout? [ layout ]
  find-clustering-coefficient
end

;; used for creating a new node
to make-node [old-node]
  crt 1
  [
    set color green
    if old-node != nobody
      [ create-link-with old-node [ set color yellow ]
        ;; position the new node near its partner
        move-to old-node
        fd 8
      ]
       become-susceptible
       set gossip-check-timer random gossip-check-frequency
       set clustering-coefficient-of-lattice clustering-coefficient
       set pv 1 - ( .99 ^ ((count links * 2 ) / ( count turtles )))
       set avg (count links * 2 / count turtles)
       set haspq? false
       set chance? false
       set pq random-normal 0.5 0.2 
       set haspq? true
      ]
 
 
end

to-report in-neighborhood? [ hood ]
  report ( member? end1 hood and member? end2 hood )
end


to set-victim
  ;ask n-of initial-outbreak-size turtles
  ask turtle id-num
  [set infected? true
  set color orange]  
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


;to spread-gossip
;   ask turtles with [infected? and not chance?]
;  
;    [ 
;     become-infected
;      ask link-neighbors with [not infected? ]
;        [ if random-float 1 >= pq
;            [ become-infected-seed ]
;            
;        ] 
;       
;         
;     ]
;   
;    
;end


;to spread-gossip
;   ask turtles with [infected? and not chance?]
;  
;    [ 
;     become-infected
;     ifelse sq = "high"
;       [
;      ask link-neighbors with [not infected?]
;      [ 
;        if pqlabel = "high" and betlabel = "low" and cenlabel = "low" and random-float 1 >= pq
;            [ become-infected-seed ]  ]
;       
;      ]
;       
;      [ ask link-neighbors with [not infected?]
;      [ 
;        if random-float 1 >= pq
;            [ become-infected-seed ]  ]
;       
;      ] ]  
;     
;    
;end       
      
      

to spread-gossip
   ask turtles with [infected? and not chance?]
  
    [ 
     become-infected
     if sq = "high"
       [
      ask link-neighbors with [not infected?]
      [ 
        ifelse ( pqlabel = "Low" and betlabel = "High" and cenlabel = "High" ) or ( pqlabel = "High" and betlabel = "High" and cenlabel = "High" )
        or ( pqlabel = "Low" and betlabel = "High" and cenlabel = "Low" ) or ( pqlabel = "Low" and betlabel = "Low" and cenlabel = "High" )
        
            [ ] [become-infected]   
        
      ]
       ] 
       
       if sq = "average"
       [
      ask link-neighbors with [not infected?]
      [ 
        ifelse ( pqlabel = "Low" and betlabel = "High" and cenlabel = "High" )
        
            [ ] [become-infected]   
        
      ]
       ] 
       
       if sq = "low"
       [
       ask link-neighbors with [not infected?]
      [ 
        if random-float 1 >= pq
            [ become-infected-seed ]  ]
       
      ] ]
       
        
     
    
end      
      
      
      
 
      
      
      
           
     
   
    


;*************************************************************** SET LABEL ************************************************************************************
;***************************************************************************************************************************************************************

to setstrength
  
  ask turtles [ifelse pq >= 0.5 [set pqlabel "High"][set pqlabel "Low"]]
  let d ( ( count turtles - 1 ) * ( count turtles - 2 ) * 0.5 )
  ask turtles [ifelse nw:betweenness-centrality  >= ( max [nw:betweenness-centrality]  of turtles / ( 2 * d )) [set betlabel "High"][set betlabel "Low"]]
  ask turtles [ifelse nw:closeness-centrality  >= ( ( max [nw:closeness-centrality]  of turtles ) / 2 )  [set cenlabel "High"][set cenlabel "Low"]]
  ;ask turtles [if pqlabel = "Low" and betlabel = "High" [set sq .9 ] ]
  ;ask turtles [if pqlabel = "Low" and betlabel = "Low" [set sq .4 ] ]
  ;ask turtles [if pqlabel = "High" and betlabel = "High" [set sq .6 ] ]
  ;ask turtles [if pqlabel = "High" and betlabel = "Low" [set sq .2 ] ]
  ;ask turtles [set label pqlabel]
  write-to-file
end

;************************************************************************************************************************************************
;*********************************************************************************BETWEENESS*****************************************************
;************************************************************************************************************************************************


to highpqlowbetlowcen
  ask turtles with [infected? and not chance?]
  
    [ 
     become-infected
     
      ask link-neighbors with [not infected?]
      [ 
        ifelse pqlabel = "High" and betlabel = "Low" and cenlabel = "Low"
        
            []
            
      [ 
        if random-float 1 >= pq
            [ become-infected-seed ]  ]
       
      ]   
        
      ]
        
       
       
  end



to lowpqlowbethighcen
   ask turtles with [infected? and not chance?]
  
    [ 
     become-infected
     
      ask link-neighbors with [not infected?]
      [ 
        ifelse pqlabel = "Low" and betlabel = "Low" and cenlabel = "High"
        
            []
            
      [ 
        if random-float 1 >= pq
            [ become-infected-seed ]  ]
       
      ]   
        
      ]
        
  end



to highpqhighbetlowcen
   ask turtles with [infected? and not chance?]
  
    [ 
     become-infected
     
      ask link-neighbors with [not infected?]
      [ 
        ifelse pqlabel = "High" and betlabel = "High" and cenlabel = "Low"
        
            []
            
      [ 
        if random-float 1 >= pq
            [ become-infected-seed ]  ]
       
      ]   
        
      ]
        
  end


to lowpqhighbethighcen
   ask turtles with [infected? and not chance?]
  
    [ 
     become-infected
     
      ask link-neighbors with [not infected?]
      [ 
        ifelse pqlabel = "Low" and betlabel = "High" and cenlabel = "High"
        
            []
            
      [ 
        if random-float 1 >= pq
            [ become-infected-seed ]  ]
       
      ]   
        
      ]
        
  end


;**************************************************************************************************************************************************************
;********************************************************************CLOSENESS*********************************************************************************
;**************************************************************************************************************************************************************

to highpqhighcenlowbet
   ask turtles with [infected? and not chance?]
  
    [ 
     become-infected
     
      ask link-neighbors with [not infected?]
      [ 
        ifelse pqlabel = "High" and betlabel = "High" and cenlabel = "Low"
        
            []
            
      [ 
        if random-float 1 >= pq
            [ become-infected-seed ]  ]
       
      ]   
        
      ]
        
  end



to lowpqlowcenhighbet
  ask turtles with [infected? and not chance?]
  
    [ 
     become-infected
     
      ask link-neighbors with [not infected?]
      [ 
        ifelse pqlabel = "Low" and betlabel = "Low" and cenlabel = "High"
        
            []
            
      [ 
        if random-float 1 >= pq
            [ become-infected-seed ]  ]
       
      ]   
        
      ]
        
  end



to highpqhighcenhighbet
  ask turtles with [infected? and not chance?]
  
    [ 
     become-infected
     
      ask link-neighbors with [not infected?]
      [ 
        ifelse pqlabel = "High" and betlabel = "High" and cenlabel = "High"
        
            []
            
      [ 
        if random-float 1 >= pq
            [ become-infected-seed ]  ]
       
      ]   
        
      ]
        
  end


to lowpqlowcenlowbet
   ask turtles with [infected? and not chance?]
  
    [ 
     become-infected
     
      ask link-neighbors with [not infected?]
      [ 
        ifelse pqlabel = "Low" and betlabel = "Low" and cenlabel = "Low"
        
            []
            
      [ 
        if random-float 1 >= pq
            [ become-infected-seed ]  ]
       
      ]   
        
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



to write-to-file
  file-open "pp.txt"
  file-print (word "---------- Tick Number: " ticks "-----------")
  ;; use SORT so the turtles print their data in order by who number,
  ;; rather than in random order
  foreach sort turtles [
    ask ? [
      file-print (pq)
    ]
  ]
  file-print ""  ;; blank line
end
@#$#@#$#@
GRAPHICS-WINDOW
868
-2
1294
445
25
25
8.16
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
333
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
153
333
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

BUTTON
2
10
68
43
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
225
10
302
43
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

BUTTON
139
10
224
43
go-once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
2
46
92
79
plot?
plot?
0
1
-1000

SWITCH
3
80
93
113
layout?
layout?
0
1
-1000

MONITOR
275
79
354
124
# of nodes
count turtles
3
1
11

BUTTON
305
10
415
43
resize nodes
resize-nodes
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
94
46
268
79
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
73
10
136
43
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
417
10
483
43
set pq
assign-random
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
484
10
547
43
start
set-victim
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
613
10
721
43
spread gossip
spread
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
336
152
745
330
Network Status
average degree
probability
0.0
100.0
0.0
1.0
false
true
"" ""
PENS
"prob of adding mal user" 1.0 0 -2674135 true "" "if not plot? [ stop ]\n;plot ((1 - ( .99 ^ (count links * 2 ) / count turtles))    / ((count links * 2 ) / ( count turtles )))\nplotxy (avg * 10) ( pv * 10 )"
"clustering-coefficient" 1.0 0 -8630108 true "" "if not plot? [ stop ]\nplotxy (avg * 10) clustering-coefficient"
"Q" 1.0 0 -10899396 true "" "if not plot? [ stop ]\nplotxy (avg * 10) (clustering-coefficient * pv * 10)"

MONITOR
641
91
756
136
Average Node Degree
(count links * 2 ) / ( count turtles )
3
1
11

MONITOR
552
46
683
91
pv
pv
17
1
11

MONITOR
686
45
743
90
cc
clustering-coefficient
17
1
11

BUTTON
724
10
790
43
setlink
setup-spatially-clustered-network
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
746
43
823
88
Q
pv * clustering-coefficient
5
1
11

SLIDER
95
80
267
113
initial-outbreak-size
initial-outbreak-size
0
10
1
1
1
NIL
HORIZONTAL

MONITOR
758
89
824
134
Gossipers
count turtles with [color = red or color = orange]
17
1
11

BUTTON
549
10
613
43
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
435
342
498
375
HHH
highpqhighcenhighbet
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
494
342
557
375
HHL
highpqhighbetlowcen
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
549
342
612
375
HLH
highpqhighcenlowbet
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
613
341
676
374
HLL
highpqlowbetlowcen
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
337
410
435
443
Betweeness
betweenness
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
479
446
617
491
sq
sq
"high" "average" "low"
1

BUTTON
439
410
536
443
Eigenvector
eigenvector
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
539
409
626
442
Closeness
closeness
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
340
490
437
523
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
339
446
477
491
layout
layout
"spring" "circle" "tutte"
1

PLOT
699
383
878
534
PQ Distribution
NIL
NIL
0.0
1.0
0.0
1.0
true
false
"" ""
PENS
"default" 0.05 1 -2674135 true "" "histogram [pq] of turtles"

BUTTON
273
46
392
79
Assign Strength
setstrength
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
395
44
550
104
id-num
105
1
0
Number

TEXTBOX
348
352
415
370
BETWEENESS
11
0.0
1

BUTTON
435
377
498
410
LHH
lowpqhighbethighcen
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
494
377
557
410
LHL
lowpqlowcenhighbet
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
551
377
614
410
LLH
lowpqlowbethighcen
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
615
375
678
408
LLL
lowpqlowcenlowbet
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
347
383
412
401
CLOSENESS
11
0.0
1

PLOT
1094
383
1294
533
Betweeness Centrality
Betweenness Centrality
Nodes
0.0
10.0
0.0
15.0
true
false
"" ""
PENS
"default" 1.0 1 -8630108 true "" "set-plot-y-range 0 count turtles / 2\nlet ys sort [ nw:betweenness-centrality / ( (count turtles - 1 ) * (count turtles - 2 ) * .5 ) ] of turtles\nlet y-min precision first ys 3\nlet y-max precision last ys 3\nif y-max > y-min [\n  set-plot-pen-interval (y-max - y-min) / 10\n  set-plot-x-range y-min y-max\n  histogram ys\n]"

PLOT
885
384
1085
534
Closeness Centrality
Closeness Centrality
NIL
0.0
5.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -5825686 true "" "set-plot-y-range 0 count turtles / 2\nlet ys sort [ nw:closeness-centrality ] of turtles\nlet y-min precision first ys 3\nlet y-max precision last ys 3\nif y-max > y-min [\n  set-plot-pen-interval (y-max - y-min) / 10\n  set-plot-x-range y-min y-max\n  histogram ys\n]"

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
