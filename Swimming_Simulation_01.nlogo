globals [
  collision-midle-lane          ; Counts collisions in the middle lane
  collisions-over-time          ; Tracks collisions over time
  crashID                       ; Total number of collisions

  secondes                      ; Number of seconds passed in simulation
  delayInPoolUntilNow           ; Total delay accumulated
  delay-average                 ; Average delay per turtle

  pools                         ; List of pool configurations (number of lanes)
  lanes-length                  ; List of lengths of lanes in each pool

  range-swimmer-view            ; Range of visibility for swimmers
  range-swimmer-vision-real

  overtakingInPoolUntilNow      ; Count of overtakes by swimmers
  overtakingInLaneList
  overtakingInLaneUntilNow

  i-speed
  minutes
  lane
  rest-flag

  centroids

  missInPoolUntilNow
  crashAtWall
  previous-collisons

  swimmer1speed
  swimmer2speed
  previous-index-speed

  swimmer-speed
  laneID
  delayInLaneList
  delayInLaneUntilNow

  crashInLaneList
  crashInLaneUntilNow

  numberSwimmersInLaneList
  numberSwimmersInLane

  variabilitySpeedInPool
  variabilitySpeedInLane
  variabilitySpeedInLaneList

  averageSpeedInLane
  averageSpeedInLaneList

  missInLaneUntilNowList
  missInLaneUntilNow




]


;test github

turtles-own [
  next-move-x                   ; X direction for next move
  next-move-y                   ; Y direction for next move
  target                        ; Current target lane (0 or 1)
  top-speed                     ; Maximum speed of the turtle
  collision-flag                ; Flag to indicate collision occurrence
  time-lane                     ; Time spent in current lane
  time-save                     ; Last saved time
  previous-target               ; Previous target lane
  time-lane-plot                ; Time spent in lane for plotting
  number-tours                  ; Number of laps completed
  ybottom                       ; Bottom lane y-coordinate

  lane-ID

  simulation-length             ; Length of the lane for each swimmers in the simulation

  index-speed

  swimmingStyle-index-map ;
]

to setup
  clear-all
  clear-output

  set minutes 0
  print("*******New Run********")

  set laneID 1

  set i-speed 0
  set rest-flag -1
  set previous-index-speed 0

  set overtakingInPoolUntilNow 0

  set collision-midle-lane 0
  set collisions-over-time 0
  set crashID 0

  setup-grid
  create-or-remove-swimmers

  set secondes 0
  set delayInPoolUntilNow 0

  set delayInLaneList [0 0 0 0 0 0 0 0 0 0]
  set overtakingInLaneList [0 0 0 0 0 0 0 0 0 0]
  set crashInLaneList [0 0 0 0 0 0 0 0 0 0]
  set missInLaneUntilNowList [0 0 0 0 0 0 0 0 0 0]
  create-or-remove-swimmers

  reset-ticks
end
to create-or-remove-swimmers

  ; Determine available patches for swimmers (blue patches not in the middle of lanes)
  let available-patches patches with [pcolor != (rgb 0 0 0) and pycor mod 4 != 2]

  ; Current number of turtles
  let current-swimmers count turtles

  ; Add new swimmers if current count is less than desired
  if current-swimmers < numberSwimmersInPool and any? available-patches [
    let num-new-swimmers numberSwimmersInPool - current-swimmers
    create-turtles num-new-swimmers [
      ; Initialize turtle properties
      set size 1
      set shape "circle"
      set next-move-x 0
      set next-move-y 0
      set target 0
      set number-tours 0
      let turtle-id who
      set collision-flag false

      let speed-gumbell sample-from-gumbel
      set top-speed round (100 / speed-gumbell)

      set time-lane 1
      set ybottom -1
      set time-lane-plot 0
      set time-save 0
      set previous-target false
    ]

      let numb-lanes (numberOfLanes + numberLanesPool2)

      foreach sort-on [top-speed] turtles
      [ the-turtle -> ask the-turtle [
      ; type("turtle ") type who type(" :") type(100 / top-speed) print(" ")

      set index-speed i-speed
      set color rgb (9000 / top-speed) 0 0

      if allocation = "random" [
        let random-patch one-of available-patches with [pycor > 0 and pycor <= numb-lanes * 4]
        setxy [pxcor] of random-patch [pycor] of random-patch
        set ybottom 1 + (floor (ycor / 4) )* 4
        set lane-ID 1 + floor ((ybottom - 1) / 4)

        ifelse ybottom > numberOfLanes * 4 [
          set simulation-length round (item 1 lanes-length / 2)
        ] [
          set simulation-length round (item 0 lanes-length / 2)
        ]
      ]

      if allocation = "logic" [

        let swimmers-per-lane ceiling ((count turtles) / (numb-lanes))
        let rest count turtles mod numb-lanes

        if rest != 0 [
          if count turtles with [ybottom > 0] + 1 > (rest * swimmers-per-lane) [
            ifelse rest-flag < 0 [
              set rest-flag swimmers-per-lane - 1
              set i-speed i-speed - rest
            ][
              set swimmers-per-lane rest-flag
            ]
          ]
        ]

        set lane (1 + (floor ((index-speed) / swimmers-per-lane)))

        let random-patch one-of available-patches with [pycor > 0 + (lane - 1) * 4 and pycor <= lane * 4]

        setxy [pxcor] of random-patch [pycor] of random-patch
        set ybottom 1 + (floor (ycor / 4) )* 4
        set lane-ID 1 + floor ((ybottom - 1) / 4)
        ifelse ybottom > numberOfLanes * 4 [
          set simulation-length round (item 1 lanes-length / 2)
        ] [
          set simulation-length round (item 0 lanes-length / 2)
        ]

        set index-speed previous-index-speed
      ]

      set previous-index-speed previous-index-speed + 1
      set i-speed i-speed + 1

      ]
    ]

    if allocation = "clustering" [
      let k-clusters numb-lanes

      set centroids []

      let step floor ((count turtles) / (k-clusters))
      let index 0

      while [length centroids < k-clusters] [
        let centroid-value 0
        ask turtles with [index-speed = index * step] [
          set centroid-value (100 / top-speed)
        ]
        set centroids lput centroid-value centroids
        set index index + 1
      ]

      print centroids

      ask turtles [
        let closest-centroids []
        let final-index 0

        while [length closest-centroids < k-clusters] [
          let index-clos length closest-centroids
          set closest-centroids lput abs ((item index-clos centroids) - (100 / top-speed)) closest-centroids

          let value-final min closest-centroids

          if value-final = item index-clos closest-centroids [
            set final-index index-clos + 1
          ]
        ]

        let random-patch one-of available-patches with [pycor > 0 + (final-index - 1) * 4 and pycor <= final-index * 4]

        setxy [pxcor] of random-patch [pycor] of random-patch
        set ybottom 1 + (floor (ycor / 4) )* 4
        set lane-ID 1 + floor ((ybottom - 1) / 4)
        ifelse ybottom > numberOfLanes * 4 [
          set simulation-length round (item 1 lanes-length / 2)
        ] [
          set simulation-length round (item 0 lanes-length / 2)
        ]
        let lane-swimmer 1 + floor ((ybottom - 1) / 4)

      ]
    ]

    set numberSwimmersInLaneList [0 0 0 0 0 0 0 0 0 0]
    set averageSpeedInLaneList [0 0 0 0 0 0 0 0 0 0]
    set variabilitySpeedInLaneList [0 0 0 0 0 0 0 0 0 0]
    let i 0
    while [i < max [lane-ID] of turtles] [
      set numberSwimmersInLaneList replace-item i numberSwimmersInLaneList (count turtles with [lane-ID = i + 1])
      set averageSpeedInLaneList replace-item i averageSpeedInLaneList ((round ( 1000 * (100 / (mean [top-speed] of turtles with [lane-ID = i + 1])))) / 1000 )
      set variabilitySpeedInLaneList replace-item i variabilitySpeedInLaneList ( abs (round ( 1000 * ((100 / (max [top-speed] of turtles with [lane-ID = i + 1])) - (100 / (min [top-speed] of turtles with [lane-ID = i + 1]) )))) / 1000 )
      set i i + 1
    ]

    set variabilitySpeedInPool ( abs (round ( 1000 * ((100 / (max [top-speed] of turtles)) - (100 / (min [top-speed] of turtles) )))) / 1000 )

  ]

  ; Remove excess turtles if current count is greater than desired
  if current-swimmers > numberSwimmersInPool [
    let num-remove current-swimmers - numberSwimmersInPool
    ask n-of num-remove turtles [
      die
    ]
  ]
end



to setup-grid
  ask patches [
    ifelse (pxcor + pycor) mod 2 = 0 [
      set pcolor blue  ;; Set light blue color for every other patch
    ] [
      set pcolor (rgb 0 0 127) ;; Set dark blue color for the rest
    ]

    set pools list numberOfLanes numberLanesPool2
    set lanes-length list poolLength lengthOfPool2

    let number-pools length pools
    let pycor-pre 0

    let i 0

    while [i < number-pools] [

    if pycor = pycor-pre or pycor mod 4 = 0 or pxcor = 0 or (pxcor > round ((item i lanes-length) / 2) and pycor <= pycor-pre + (item i pools) * 4 and pycor >= pycor-pre) [
      set pcolor (rgb 0 0 0) ;; Set black
    ]
      set pycor-pre pycor-pre + 4 * (item i pools)
      set i i + 1
    ]
    if pycor > pycor-pre [ set pcolor (rgb 0 0 0)]
  ]
end

to go
  ; Create or remove swimmers based on current conditions
  create-or-remove-swimmers
  set previous-collisons crashID

  ; Adjust swimmer view range
  set range-swimmer-view range-swimmer-vision-real / 2
  set delayInLaneUntilNow ((round (1000 * (item (laneID - 1) delayInLaneList))) / 1000)
  set crashInLaneUntilNow item (laneID - 1) crashInLaneList
  set overtakingInLaneUntilNow item (laneID - 1) overtakingInLaneList
  set numberSwimmersInLane item (laneID - 1) numberSwimmersInLaneList
  set variabilitySpeedInLane item (laneID - 1) variabilitySpeedInLaneList
  set averageSpeedInLane item (laneID - 1) averageSpeedInLaneList
  set numberSwimmersInLane item (laneID - 1) numberSwimmersInLaneList
  set missInLaneUntilNow item (laneID - 1) missInLaneUntilNowList
  set crashAtWall 0

  ; Move turtles based on their speed
  ask turtles [
    if ticks mod top-speed = 0 [
      swim
    ]
  ]

  if ticks mod 50 = 0 [
    set secondes secondes + 1
    if secondes mod 60 = 0 [
      set minutes minutes + 1
    ]
    record-collisions
  ]

  ; Calculate average delay per tour if any tours have been completed
  if sum [number-tours] of turtles != 0 [
    set delay-average delayInPoolUntilNow / sum [number-tours] of turtles
  ]

  ; Stop simulation if specified duration is reached
  if secondes >= (simulationDuration * 60 * 60) [
    stop
  ]

  tick
end


to swim
  set next-move-x 0
  set next-move-y 0
  set previous-target target
  let missInPoolUntilNow-flag 0

  if ycor = ybottom + 2 [  ; At the top row
    set target 0
    set next-move-x 1
    ifelse xcor < ( simulation-length - 1 ) [   ; At the top row, move left when reaching the end
    ][
      ifelse xcor = ( simulation-length - 1 ) and not any? turtles-on patch-at 0 -1 [
        set next-move-y -1
      ] [
          if carefulAtWall = false and xcor = simulation-length - 1 [
            set crashAtWall crashAtWall + 1
            find-lane
            set crashID crashID + 1
          ]
      ]
      if xcor = simulation-length [
        set next-move-y -1
        set next-move-x -1
        set target 1
      ]
    ]
    ]

  if ycor = ybottom + 1 [
      if xcor = simulation-length [  ; At the middle row, move left when reaching the end
      set next-move-y -1
      set next-move-x -1
      ]
      if xcor = 1 [  ; At the middle row, move right when reaching the beginning
      set next-move-y 1
      set next-move-x 1
      ]

    if target = 0 [                      ;overtake1
    if xcor < ( simulation-length - 1 ) [
        let turtle-front turtles-on patch-at 1 0
        ifelse not any? turtles-on patch-at 0 1 or ([target] of turtle-front != target and any? turtle-front ) [
          set next-move-x 1
          set next-move-y 1

          set missInPoolUntilNow-flag 1
        ] [
          set next-move-x 1
          set next-move-y 0
        ]
      ]
      if xcor = ( simulation-length - 1 )[
      set next-move-x 1
      set next-move-y 0
      ]
    ]
    if target = 1 [                      ;overtake2
      if xcor > 2 [
        let turtle-front turtles-on patch-at -1 0
        ifelse not any? turtles-on patch-at 0 -1 or ([ target ] of turtle-front != target and any? turtle-front  )[
          set next-move-x -1
          set next-move-y -1

          set missInPoolUntilNow-flag 1
        ] [
          set next-move-x -1
          set next-move-y 0
        ]
     ]
      if xcor = 2[
      set next-move-x -1
      set next-move-y  0
      ]
    ]
  ]

  if ycor = ybottom [
    set target 1
    set next-move-x -1
    ifelse xcor > 2 [  ; At the bottom row, move right when reaching the beginning
    ][
      ifelse xcor = 2 and not any? turtles-on patch-at 0 1 [
        set next-move-y 1
      ] [
          if carefulAtWall = false and xcor = 2 [
            set crashAtWall crashAtWall + 1
            find-lane
            set crashID crashID + 1
          ]
      ]
      if xcor = 1 [
        set next-move-y 1
        set next-move-x 1
        set target 0
      ]
     ]

  ]

  if any? turtles-on patch-at next-move-x next-move-y [
    let my-xcor xcor
    let my-ycor ycor
    let my-ybottom ybottom

    let turtles-on-patch turtles-on patch-at next-move-x next-move-y
    let max-speed-of-others max [top-speed] of turtles-on-patch


    ifelse max-speed-of-others > top-speed + ((patiencePercentage * max-speed-of-others) / 100)  [
    if target = 0 [ ; right direction
        ifelse xcor < ( simulation-length - 2 ) [
      ifelse ycor <= ybottom + 1 [   ; on midle lane
            if ycor = ybottom + 1 [
        set next-move-y 0
        set next-move-x 1
            ]
      ] [                 ; on top lane
          let turtles-in-midle turtles-on patches with [pycor = my-ybottom + 1 and pxcor >= my-xcor and pxcor <= my-xcor + range-swimmer-view ]

          ifelse any? turtles-in-midle [
            ifelse [ target ] of turtles-in-midle != target [
              set next-move-x 0
              set next-move-y 0
          ][
            set next-move-y -1
            set next-move-x 1
          ]
          ] [
            set next-move-y -1
            set next-move-x 1
          ]
      ]
        ][
          ifelse xcor = simulation-length [
        set next-move-x 0
        set next-move-y 0
          ] [
        set next-move-x 1
        set next-move-y 0
          ]
        ]
    ]
    if target = 1 [      ; left direction
       ifelse xcor > 3 [
      ifelse ycor >= ybottom + 1 [  ; on midle lane
            if ycor = ybottom + 1 [
        set next-move-y 0
        set next-move-x -1
            ]
      ] [                ; on bottom lane
          let turtles-in-midle turtles-on patches with [pycor = my-ybottom + 1 and pxcor >= my-xcor and pxcor <= my-xcor - range-swimmer-view ]

          ifelse any? turtles-in-midle [
            ifelse [ target ] of turtles-in-midle != target [
              set next-move-x 0
              set next-move-y 0
          ][
            set next-move-y 1
            set next-move-x -1
          ]
          ][
            set next-move-y 1
            set next-move-x -1
          ]
      ]
        ][
          ifelse xcor = 1 [
        set next-move-x 0
        set next-move-y 0
          ][
        set next-move-x -1
        set next-move-y 0
          ]
        ]
    ]
    ] [
      set next-move-x 0
      set next-move-y 0
    ]
]
  ifelse not any? turtles-on patch-at next-move-x next-move-y [
    if missInPoolUntilNow-flag = 1 and next-move-y != 0 and collision-flag = false [
      if any? turtles-on patch-at next-move-y 0  [
        if collision-flag = [collision-flag] of one-of turtles-on patch-at next-move-y 0  and max [target] of turtles-on patch-at next-move-y 0 != target [
              set missInPoolUntilNow missInPoolUntilNow + 1
              set missInLaneUntilNowList replace-item (lane-ID - 1) missInLaneUntilNowList (item (lane-ID - 1) missInLaneUntilNowList + 1)
        ]
      ]
    ]

    let ycor-previous ycor


    set xcor xcor + next-move-x
    set ycor ycor + next-move-y

    set collision-flag false

    if ycor = ybottom + 1 and ycor-previous != ycor and xcor > 1 and xcor < simulation-length - 1 [
      set overtakingInPoolUntilNow overtakingInPoolUntilNow + 1
      set overtakingInLaneList replace-item (lane-ID - 1) overtakingInLaneList (item (lane-ID - 1) overtakingInLaneList + 1)
    ]

  ] [
    if ycor = ybottom + 1 and max [ target ] of turtles-on patch-at next-move-x next-move-y != target and collision-flag = false and one-of [collision-flag] of turtles-on patch-at next-move-x next-move-y = false[
      find-lane
      set swimmer1speed (round (1000 * (100 / top-speed))) / 1000
      set swimmer2speed (round (1000 * (100 / max [top-speed] of turtles-on patch-at next-move-x next-move-y))) / 1000
      set crashInLaneList replace-item (lane-ID - 1) crashInLaneList (item (lane-ID - 1) crashInLaneList + 1)
      set collision-midle-lane collision-midle-lane + 1
      set collision-flag true
      if (xcor = simulation-length - 1 and target = 1) or (xcor = 2 and target = 0)
      [
        set crashAtWall crashAtWall + 1
      ]
    ]
  ]

  if previous-target != target [
      set time-lane (secondes - time-save) - ((simulation-length * 2) / (100 / top-speed))
    ifelse time-lane < 0 [
    ] [
      set delayInLaneList replace-item (lane-ID - 1) delayInLaneList (item (lane-ID - 1) delayInLaneList + time-lane)
      set time-lane-plot time-lane-plot + time-lane
      set delayInPoolUntilNow (round (1000 * (delayInPoolUntilNow + time-lane))) / 1000
    ]
      set number-tours number-tours + 1
      set time-save secondes
  ]

end

to record-collisions
  ; Record current mid-lane collisions and update counters
  set collisions-over-time collision-midle-lane
  set crashID crashID + collisions-over-time
  set collision-midle-lane 0
end

to find-lane
  set laneID 1 + floor ((ybottom - 1) / 4)
end


to-report sample-from-gumbel
  ; Sample from Gumbel distribution based on configuration
  let mu 0
  let beta 0
  let min_speed 0
  let max_speed 0

  let index 0
  if swimmingStyle = "back" [
    set range-swimmer-vision-real 0
    set index 0
  ]
  if swimmingStyle = "breast" [
    set range-swimmer-vision-real 10
    set index 1
  ]
  if swimmingStyle = "fly" [
    set range-swimmer-vision-real 10
    set index 2
  ]
  if swimmingStyle = "free" [
    set range-swimmer-vision-real 5
    set index 3
  ]
  if swimmingStyle = "IM" [
    set range-swimmer-vision-real 5
    set index 4
  ]

  ifelse gender = "male" [
    let mu_list [0.9472 0.9491 1.1808 1.2098 1.0893] ; [Back Breast Fly Free IM]
    let beta_list [0.2085 0.1710 0.2027 0.2247 0.1748]
    let min_speed_list [0.4082 0.3377 0.5974 0.4191 0.5513]
    let max_speed_list [1.7899 1.4573 1.8155 1.9751 1.7665]
    set mu item index mu_list
    set beta item index beta_list
    set min_speed item index min_speed_list
    set max_speed item index max_speed_list
  ] [
    let mu_list [0.8575 0.8970 1.0149 1.0556 0.9886]
    let beta_list [0.1709 0.1310 0.1483 0.1841 0.1347]
    let min_speed_list [0.4314 0.6066 0.7570 0.4086 0.6156]
    let max_speed_list [1.4345 1.3123 1.4106 1.6787 1.4491]
    set mu item index mu_list
    set beta item index beta_list
    set min_speed item index min_speed_list
    set max_speed item index max_speed_list
  ]

  let y 0
  while [y < min_speed or y > max_speed] [
     let x random-exponential 1
     set y mu - beta * log x 2
  ]
  report y
end
@#$#@#$#@
GRAPHICS-WINDOW
748
35
1296
864
-1
-1
20.0
1
20
1
1
1
0
1
1
1
0
26
0
40
0
0
1
ticks
30.0

BUTTON
69
64
142
97
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
162
64
225
97
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

MONITOR
249
343
426
388
number-of-collisons-total
crashID
17
1
11

SLIDER
69
286
293
319
simulationDuration
simulationDuration
1
5
1.0
1
1
hours
HORIZONTAL

MONITOR
430
283
635
328
sum of delay of all swimmer (secondes)
delayInPoolUntilNow
0
1
11

SLIDER
67
241
267
274
numberOfLanes
numberOfLanes
1
10
1.0
1
1
NIL
HORIZONTAL

SLIDER
61
755
262
788
numberLanesPool2
numberLanesPool2
0
10 - numberOfLanes
0.0
1
1
NIL
HORIZONTAL

SLIDER
286
239
458
272
poolLength
poolLength
25
50
50.0
25
1
NIL
HORIZONTAL

SLIDER
274
755
446
788
lengthOfPool2
lengthOfPool2
25
50
25.0
25
1
NIL
HORIZONTAL

MONITOR
66
343
243
388
NIL
overtakingInPoolUntilNow
17
1
11

CHOOSER
370
175
508
220
allocation
allocation
"random" "logic" "clustering"
1

CHOOSER
217
176
355
221
gender
gender
"male" "female"
0

CHOOSER
66
177
204
222
swimmingStyle
swimmingStyle
"back" "breast" "fly" "free" "IM"
0

PLOT
62
410
690
720
Density of Gumbel Distribution
10 * speed
number of swimmers
0.0
30.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [(100 / top-speed) * 10] of turtles"
"pen-1" 1.0 2 -14454117 true "" "if allocation = \"clustering\" [\nlet index 0\nwhile [index < length centroids] [\n    plotxy 10 * (item index centroids) 1\n    set index index + 1\n    \n    ]\n]"

MONITOR
308
282
417
327
time (minutes)
minutes
17
1
11

INPUTBOX
244
106
405
166
patiencePercentage
0.0
1
0
Number

MONITOR
440
343
536
388
near-misses
missInPoolUntilNow
17
1
11

MONITOR
552
343
649
388
crash-at-wall
CrashAtWall
17
1
11

SWITCH
466
755
623
788
carefulAtWall
carefulAtWall
0
1
-1000

TEXTBOX
65
734
215
752
OPTIONAL
12
0.0
1

INPUTBOX
68
107
232
167
numberSwimmersInPool
4.0
1
0
Number

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="SL_NS" repetitions="2" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>minutes = simulationDuration * 60</exitCondition>
    <metric>minutes</metric>
    <metric>crashID</metric>
    <metric>laneID</metric>
    <metric>CrashAtWall</metric>
    <metric>numberSwimmersInLane</metric>
    <metric>variabilitySpeedInLane</metric>
    <metric>averageSpeedInLane</metric>
    <metric>swimmer1speed</metric>
    <metric>swimmer2speed</metric>
    <metric>delayInLaneUntilNow</metric>
    <metric>crashInLaneUntilNow</metric>
    <metric>missInLaneUntilNow</metric>
    <metric>overtakingInLaneUntilNow</metric>
    <metric>variabilitySpeedInPool</metric>
    <metric>missInPoolUntilNow</metric>
    <metric>overtakingInPoolUntilNow</metric>
    <metric>delayInPoolUntilNow</metric>
    <runMetricsCondition>previous-collisons != crashID</runMetricsCondition>
    <enumeratedValueSet variable="gender">
      <value value="&quot;male&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swimmingStyle">
      <value value="&quot;back&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="poolLength">
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="130" step="1" last="155"/>
    <enumeratedValueSet variable="allocation">
      <value value="&quot;logic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulationDuration">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patiencePercentage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carefulAtWall">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberLanesPool2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lengthOfPool2">
      <value value="25"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SL_S2" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>minutes = simulationDuration * 60</exitCondition>
    <metric>minutes</metric>
    <metric>crashID</metric>
    <metric>laneID</metric>
    <metric>CrashAtWall</metric>
    <metric>numberSwimmersInLane</metric>
    <metric>variabilitySpeedInLane</metric>
    <metric>averageSpeedInLane</metric>
    <metric>swimmer1speed</metric>
    <metric>swimmer2speed</metric>
    <metric>delayInLaneUntilNow</metric>
    <metric>crashInLaneUntilNow</metric>
    <metric>missInLaneUntilNow</metric>
    <metric>overtakingInLaneUntilNow</metric>
    <metric>variabilitySpeedInPool</metric>
    <metric>missInPoolUntilNow</metric>
    <metric>overtakingInPoolUntilNow</metric>
    <metric>delayInPoolUntilNow</metric>
    <runMetricsCondition>((previous-collisons != crashID ) or (ticks = (simulationDuration * 3000 * 60) - 50))</runMetricsCondition>
    <enumeratedValueSet variable="gender">
      <value value="&quot;male&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swimmingStyle">
      <value value="&quot;free&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="poolLength">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulationDuration">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberLanesPool2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lengthOfPool2">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carefulAtWall">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patiencePercentage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation">
      <value value="&quot;logic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberSwimmersInPool">
      <value value="30"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SL_S3" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>minutes = simulationDuration * 60</exitCondition>
    <metric>minutes</metric>
    <metric>crashID</metric>
    <metric>laneID</metric>
    <metric>CrashAtWall</metric>
    <metric>numberSwimmersInLane</metric>
    <metric>variabilitySpeedInLane</metric>
    <metric>averageSpeedInLane</metric>
    <metric>swimmer1speed</metric>
    <metric>swimmer2speed</metric>
    <metric>delayInLaneUntilNow</metric>
    <metric>crashInLaneUntilNow</metric>
    <metric>missInLaneUntilNow</metric>
    <metric>overtakingInLaneUntilNow</metric>
    <metric>variabilitySpeedInPool</metric>
    <metric>missInPoolUntilNow</metric>
    <metric>overtakingInPoolUntilNow</metric>
    <metric>delayInPoolUntilNow</metric>
    <runMetricsCondition>((previous-collisons != crashID ) or (ticks = (simulationDuration * 3000 * 60) - 50))</runMetricsCondition>
    <enumeratedValueSet variable="gender">
      <value value="&quot;male&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swimmingStyle">
      <value value="&quot;free&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="poolLength">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulationDuration">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberLanesPool2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lengthOfPool2">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carefulAtWall">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patiencePercentage">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation">
      <value value="&quot;logic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberSwimmersInPool">
      <value value="50"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SL_S4" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>minutes = simulationDuration * 60</exitCondition>
    <metric>minutes</metric>
    <metric>crashID</metric>
    <metric>laneID</metric>
    <metric>CrashAtWall</metric>
    <metric>numberSwimmersInLane</metric>
    <metric>variabilitySpeedInLane</metric>
    <metric>averageSpeedInLane</metric>
    <metric>swimmer1speed</metric>
    <metric>swimmer2speed</metric>
    <metric>delayInLaneUntilNow</metric>
    <metric>crashInLaneUntilNow</metric>
    <metric>missInLaneUntilNow</metric>
    <metric>overtakingInLaneUntilNow</metric>
    <metric>variabilitySpeedInPool</metric>
    <metric>missInPoolUntilNow</metric>
    <metric>overtakingInPoolUntilNow</metric>
    <metric>delayInPoolUntilNow</metric>
    <runMetricsCondition>((previous-collisons != crashID ) or (ticks = (simulationDuration * 3000 * 60) - 50))</runMetricsCondition>
    <enumeratedValueSet variable="gender">
      <value value="&quot;male&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swimmingStyle">
      <value value="&quot;back&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="poolLength">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulationDuration">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberLanesPool2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lengthOfPool2">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carefulAtWall">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patiencePercentage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation">
      <value value="&quot;logic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberSwimmersInPool">
      <value value="150"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SL_25_02_back" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>minutes = simulationDuration * 60</exitCondition>
    <metric>minutes</metric>
    <metric>crashID</metric>
    <metric>laneID</metric>
    <metric>CrashAtWall</metric>
    <metric>numberSwimmersInLane</metric>
    <metric>variabilitySpeedInLane</metric>
    <metric>averageSpeedInLane</metric>
    <metric>swimmer1speed</metric>
    <metric>swimmer2speed</metric>
    <metric>delayInLaneUntilNow</metric>
    <metric>crashInLaneUntilNow</metric>
    <metric>missInLaneUntilNow</metric>
    <metric>overtakingInLaneUntilNow</metric>
    <metric>variabilitySpeedInPool</metric>
    <metric>missInPoolUntilNow</metric>
    <metric>overtakingInPoolUntilNow</metric>
    <metric>delayInPoolUntilNow</metric>
    <runMetricsCondition>((previous-collisons != crashID ) or (ticks = (simulationDuration * 3000 * 60) - 50))</runMetricsCondition>
    <enumeratedValueSet variable="gender">
      <value value="&quot;male&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swimmingStyle">
      <value value="&quot;back&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="poolLength">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulationDuration">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberLanesPool2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lengthOfPool2">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carefulAtWall">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patiencePercentage">
      <value value="0"/>
      <value value="3"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation">
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="8" step="1" last="50"/>
  </experiment>
  <experiment name="SL_25_02_back (1)" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>minutes = simulationDuration * 60</exitCondition>
    <metric>minutes</metric>
    <metric>crashID</metric>
    <metric>laneID</metric>
    <metric>CrashAtWall</metric>
    <metric>numberSwimmersInLane</metric>
    <metric>variabilitySpeedInLane</metric>
    <metric>averageSpeedInLane</metric>
    <metric>swimmer1speed</metric>
    <metric>swimmer2speed</metric>
    <metric>delayInLaneUntilNow</metric>
    <metric>crashInLaneUntilNow</metric>
    <metric>missInLaneUntilNow</metric>
    <metric>overtakingInLaneUntilNow</metric>
    <metric>variabilitySpeedInPool</metric>
    <metric>missInPoolUntilNow</metric>
    <metric>overtakingInPoolUntilNow</metric>
    <metric>delayInPoolUntilNow</metric>
    <runMetricsCondition>((previous-collisons != crashID ) or (ticks = (simulationDuration * 3000 * 60) - 50))</runMetricsCondition>
    <enumeratedValueSet variable="gender">
      <value value="&quot;male&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swimmingStyle">
      <value value="&quot;back&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="poolLength">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulationDuration">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberLanesPool2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lengthOfPool2">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carefulAtWall">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patiencePercentage">
      <value value="0"/>
      <value value="3"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation">
      <value value="&quot;logic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="8" step="1" last="50"/>
  </experiment>
  <experiment name="SL_25_02_back (2)" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>minutes = simulationDuration * 60</exitCondition>
    <metric>minutes</metric>
    <metric>crashID</metric>
    <metric>laneID</metric>
    <metric>CrashAtWall</metric>
    <metric>numberSwimmersInLane</metric>
    <metric>variabilitySpeedInLane</metric>
    <metric>averageSpeedInLane</metric>
    <metric>swimmer1speed</metric>
    <metric>swimmer2speed</metric>
    <metric>delayInLaneUntilNow</metric>
    <metric>crashInLaneUntilNow</metric>
    <metric>missInLaneUntilNow</metric>
    <metric>overtakingInLaneUntilNow</metric>
    <metric>variabilitySpeedInPool</metric>
    <metric>missInPoolUntilNow</metric>
    <metric>overtakingInPoolUntilNow</metric>
    <metric>delayInPoolUntilNow</metric>
    <runMetricsCondition>((previous-collisons != crashID ) or (ticks = (simulationDuration * 3000 * 60) - 50))</runMetricsCondition>
    <enumeratedValueSet variable="gender">
      <value value="&quot;male&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swimmingStyle">
      <value value="&quot;back&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="poolLength">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulationDuration">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberLanesPool2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lengthOfPool2">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carefulAtWall">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patiencePercentage">
      <value value="0"/>
      <value value="3"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="8" step="1" last="50"/>
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
