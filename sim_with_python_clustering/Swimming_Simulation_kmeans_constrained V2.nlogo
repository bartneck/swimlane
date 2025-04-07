extensions [py]



globals [                       ; Declare global variables
  collision-midle-lane          ; Counts collisions in the middle lane
  collisions-over-time          ; Tracks collisions over time
  crashID                       ; Total number of collisions
  maxNumberOfSwimmers

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

  i-speed                       ; used for organising swimmers by speed
  minutes                       ; time in minutes
  lane                          ; lane we're considering
  rest-flag                     ; used in logical allocation algorithm

  centroids                     ; will contain a list of centroids

  missInPoolUntilNow            ; contains how many near misses we've had
  crashAtWall                   ; counts the number of crashes at the wall
  previous-collisons            ; how many collisions have there been

  swimmer1speed                 ; used to compare two swimmers in a collision
  swimmer2speed
  swimmer1index
  swimmer2index
  previous-index-speed

  swimmer-speed                 ; will contain the speed of the swimmer we're currently considering
  laneID                        ; contains the lane number we're in
  delayInLaneList               ; contains delay information for the lane we're in
  delayInLaneUntilNow

  crashInLaneList               ; list of crashes in the lanes until now
  crashInLaneUntilNow           ; how many crashes have occured in this lane so far

  numberSwimmersInLaneList      ; contains a list of the swimmer distribution amoung lanes (e.g. a 4 lane pool has: lane 1 and 2 with 5 swimmers, and the rest are empty. Then the list is [5, 5, 0, 0])
  numberSwimmersInLane          ; how many swimmers are in the lane we're considering

  swimmerSpeedsInEachLane           ; This is a list where each entry contains a list of swimmer speeds in the lane corrosponding with the index.

  variabilitySpeedInPool        ; contains the range of swimmer speed in the whole pool
  variabilitySpeedInLane        ; what's the range of the swimmer speeds in a given lane
  variabilitySpeedInLaneList    ; List of ranges per lane

  varianceSpeedInPool
  varianceSpeedInLane
  varianceSpeedInLaneList

  averageSpeedInLane            ; average speed in the lane we're on
  averageSpeedInLaneList        ; list of average speeds per lane

  missInLaneUntilNowList        ; near misses in each lane
  missInLaneUntilNow            ; near misses in the lane we're currently considering

  numReallocated                 ; Will contain how many swimmers need to have their lane changed during allcoation with the clustering method. I.E, when there are too many swimmers in one cluster

  setupComplete
]


;test github

turtles-own [
  next-move-x                   ; X direction for next move
  next-move-y                   ; Y direction for next move
  target                        ; Current target side of the pool (0 = righthand side or 1 = lefthand side)
  top-speed                     ; Maximum speed of the turtle
  collision-flag                ; Flag to indicate collision occurrence
  time-lane                     ; Time spent in current lane
  time-save                     ; Last saved time
  previous-target               ; Previous target lane
  time-lane-plot                ; Time spent in lane for plotting
  number-tours                  ; Number of laps completed
  ybottom                       ; Bottom lane y-coordinate

  lane-ID                       ; what lane is the swimmer in

  simulation-length             ; Length of the lane for each swimmers in the simulation

  index-speed                   ; where is the swimmer located in terms of speed, 0 is the slowest, and n is the fastest

  swimmingStyle-index-map ; never used?
]

to setup
  clear-all
  clear-output
  set setupComplete false
  set minutes 0                       ; Set the time_elapsed to be zero
  print("*******New Run********")     ; Print new run, let's us seperate data
                                      ; set up defualt values:
  set laneID 1                        ;

  set i-speed 0                       ;
  set rest-flag -1                    ;
  set previous-index-speed 0          ;

  set overtakingInPoolUntilNow 0      ; Reset count

  set collision-midle-lane 0          ; Reset count
  set collisions-over-time 0          ; Reset count
  set crashID 0                       ;
  ; setup the swimulation
  setup-grid
  create-or-remove-swimmers
  ; set defualt values
  set secondes 0
  set delayInPoolUntilNow 0

  set delayInLaneList [0 0 0 0 0 0 0 0 0 0]
  set overtakingInLaneList [0 0 0 0 0 0 0 0 0 0]
  set crashInLaneList [0 0 0 0 0 0 0 0 0 0]
  set missInLaneUntilNowList [0 0 0 0 0 0 0 0 0 0]

  set maxNumberOfSwimmers 2 *(ceiling (poolLength / 2) * numberOfLanes + ceiling (lengthOfPool2 / 2) * numberLanesPool2)

  create-or-remove-swimmers
  set setupComplete true
  reset-ticks
end


to create-or-remove-swimmers
  set maxNumberOfSwimmers 2 *(ceiling (poolLength / 2) * numberOfLanes + ceiling (lengthOfPool2 / 2) * numberLanesPool2)
  if numberSwimmersInPool > maxNumberOfSwimmers [
    print("Tooooo mannnyyyy swimmmmerrrssss")
    set numberSwimmersInPool maxNumberOfSwimmers
  ]
  if numberLanesPool2 > 10 - numberOfLanes [
    set numberLanesPool2 10 - numberOfLanes
  ]


  ; Determine available patches for swimmers (blue patches not in the middle of lanes)
  let available-patches patches with [pcolor != (rgb 0 0 0) and pycor mod 4 != 2]

  ; Current number of turtles
  let current-swimmers count turtles

  ; Add new swimmers if current count is less than desired
  if current-swimmers < numberSwimmersInPool and any? available-patches [       ; If there are less swimmers than desired, and there are available patches then:
    let num-new-swimmers numberSwimmersInPool - current-swimmers                ; Define the number of swimmers needed as the difference between the desired amount, and current amount
    create-turtles num-new-swimmers [                                           ; Initialise new instances of what is effectively the 'swimmer class'
      ; Initialize turtle properties
      set size 1
      set shape "circle"
      set next-move-x 0                             ; Not initially moving in the X-direction ; if = 0, not moving in X-direction; if = 1, moving right; if = -1, moving left
      set next-move-y 0                             ; Not initially moving in the Y-Direction ; if = 0, not moving in Y-direction; if = 1, moving up, if = -1, moving down
      set target 0                                  ; Initial target is the righthand side of the pool
      set number-tours 0                            ; The swimmer currently hasn't swum any lengths
      let turtle-id who                             ; Grab the turtles Id value, and put this into a variable
      set collision-flag false                      ; The swimmer has not yet had a collision

      let speed-gumbell sample-from-gumbel          ; Get a random speed from the Gumbel distribution
      set top-speed round (100 / speed-gumbell)     ; Create a max speed from this

      set time-lane 1
      set ybottom -1                                ;This parameter will contain the y-coordinates of the bottom of the lane the swimmer is in
      set time-lane-plot 0
      set time-save 0
      set previous-target false                     ; Initially the swimmer doesn't have a previous target, so it has a value of false
    ]

      let numb-lanes (numberOfLanes + numberLanesPool2)                                           ; The total number of lanes

      foreach sort-on [top-speed] turtles                                                         ; Loops through the players from slowest to fastest
      [ the-turtle -> ask the-turtle [
      ; type("turtle ") type who type(" :") type(100 / top-speed) print(" ")

      set index-speed i-speed                                                                     ; The swimmers are ordered from slowest to fastest, so this variable is directly related to the speed of the swimmer
      set color rgb (9000 / top-speed) 0 0                                                        ; sets the colour based off speed
;      if i-speed = numberSwimmersInPool - 10 [ ; this is for debuggin purposes. Used so you can watch a specific swimmer
;        set color rgb 48 252 3
;      ]
      if allocation = "logic" [                                                   ; logically allocates swimmer location

        let swimmers-per-lane ceiling ((count turtles) / (numb-lanes))            ; We want equal numbers of swimmers per lane, so take the ceiling of the number of swimmers, and the number of lanes. This is the number of swimmers per lane
        let rest count turtles mod numb-lanes                                     ; This is the remainder when the number of swimmers is devided by the number of lanes

        if rest != 0 [                                                            ; If there is a remainer then:
          if count turtles with [ybottom > 0] + 1 > (rest * swimmers-per-lane) [   ; If the number of swimmers in ybottom > 0, such that ybottom + 1 > the remained multiplied by the number of swimmers per lane , then:
            ifelse rest-flag < 0 [                                                ; This block of code should only run once. It's purpose is to define what the swimmers-per-lane should be, by defining a value for rest-flag
              set rest-flag swimmers-per-lane - 1                                 ; Reset rest-flag to be one less than the swimmers-per-lane
              set i-speed i-speed - rest                                          ; Reset i-speed to be i-speed - the remainder
            ][                                                                    ; Else
              set swimmers-per-lane rest-flag                                     ; Set swimmers per lane to be rest-flag
            ]
          ]
        ]

        set lane (1 + (floor ((index-speed) / swimmers-per-lane)))       ; Index speed is directly related to swimmer speed, so this allocates the lane based on speed, it lots of 1 [swimmers-per-lane]. Hence, the slowest 1 [swimmers-per-lane] are in lane 1, and the fasest 1 [swimmers-per-lane] are in the final lane
                                                                        ; index-speed is defined at the start of the loop iteration to be i-speed, every loop i-speed increases by 1. So as the loop count increases, this variable increases, each time it incrases by 1 swimmers-per-lane, it increases by 1

        let random-patch one-of available-patches with [pycor > 0 + (lane - 1) * 4 and pycor <= lane * 4 and (count [turtles-at 0 0] of patch pxcor pycor) = 0]   ; Select a random empty patch from the lane selected above
        ;        set available-patches remove-item random-patch available-patches
        setxy [pxcor] of random-patch [pycor] of random-patch                                              ; Alocate the swimmer's to this lane, in a similar way as done in the random allocation method.
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
      set i-speed i-speed + 1                  ; Increments i-speed. This allows it to increase each loop, as the swimmer speed increases each loop, allowing them to be directly proportional

      ]
    ]

    if allocation  = "random" [                                ; allocates the swimmers randomly based on the concept of looping through the lanes, and chosing a random swimmer to go into that lane
      let swimmer-index-speeds [index-speed] of turtles        ; create a list of the index-speeds of the swimmers. This will later allow us to allocate them randomly
      let num-swimmers count turtles                           ; we need to know the number of swimmers
      let num-lanes numberOfLanes + numberLanesPool2           ; We need to know the number of lanes
      let swimmersAdded []                                     ; This list will contain the index-speeds of the swimmers we've added already. This will allow us to reduce the swimer-index-speeds list to only contain swimmers we haven't added yet
      let i 0                                                  ; set the starting index to be zero
      while [i < num-swimmers] [                               ; loop until we've added all the swimmers
        set swimmer-index-speeds filter [index -> not member? index swimmersAdded] swimmer-index-speeds    ; Reset the swimmer-index-speeds list to only contain the index-speed of swimmers that haven't been added yet
        let swimmer-index-speed one-of swimmer-index-speeds                                                ; choose a random swimmer from this list
        set swimmersAdded lput swimmer-index-speed swimmersAdded                                           ; insert the swimmer we've just choosen to the swimmersAdded list
        let lane-number (i mod num-lanes) + 1                                                              ; We're looping through the lanes, so we know the lane-number we're currently on is the loop number modulus num-lanes
        ask turtles with [index-speed = swimmer-index-speed] [                                             ; this block lets us define the parameters of the specific swimmer we selected
          set lane-ID lane-number                                                                                         ; place the swimmer in the lane we're currently on
          set ybottom (lane-ID - 1) * 4 + 1                                                                                ; define the bottom y index based on the lane we're in
          let random-patch one-of available-patches with [pycor > 0 + (lane-number - 1) * 4 and pycor <= lane-number * 4 and (count [turtles-at 0 0] of patch pxcor pycor) = 0]  ; select an empty patch in the lane

          setxy [pxcor] of random-patch [pycor] of random-patch                                                           ; then place the swimmer on that tile
          ifelse ybottom > numberOfLanes * 4 [                          ; If the y-cord of the bottom of the lane is greater than the max y-cord in the first pool
            set simulation-length round (item 1 lanes-length / 2)       ; Then set the simulation length to be the rounded value of (the second item in lanes-length / 2). Basically places the swimmer in the second pool
          ] [                                                           ; Else
            set simulation-length round (item 0 lanes-length / 2)       ; Set the simulation length to be the rounded value of (the first item in lanes-length /2). Basically places the swimmer in the first pool
          ]
        ]
        set i i + 1        ; move on to the next loop iteration
      ]
    ]

    if allocation = "clustering" [                     ; Assigns swimmers to lanes using a clustering algarithm
      clustering-alg
    ]

    set numberSwimmersInLaneList   [0 0 0 0 0 0 0 0 0 0]     ; there can be up to 10 lanes, and they each start off empty
    set averageSpeedInLaneList     [0 0 0 0 0 0 0 0 0 0]
    set variabilitySpeedInLaneList [0 0 0 0 0 0 0 0 0 0]
    set varianceSpeedInLaneList    [0 0 0 0 0 0 0 0 0 0]
    set swimmerSpeedsInEachLane    [[] [] [] [] [] [] [] [] [] []]
    let i 0                                                     ; start at index 0

    while [i < max [lane-ID] of turtles] [                      ; While we haven't added swimmers to every required lane:
      set numberSwimmersInLaneList replace-item i numberSwimmersInLaneList (count turtles with [lane-ID = i + 1])                ; replace the 0 in the list numberSwimmersInLaneList with the number of swimmer currently in this lane
      set averageSpeedInLaneList replace-item i averageSpeedInLaneList ((round ( 1000 * (100 / (mean [top-speed] of turtles with [lane-ID = i + 1])))) / 1000 )   ; Find the mean speed in this lane, and add this to the list of mean swim speeds
      set variabilitySpeedInLaneList replace-item i variabilitySpeedInLaneList ( abs (round ( 1000 * ((100 / (max [top-speed] of turtles with [lane-ID = i + 1])) - (100 / (min [top-speed] of turtles with [lane-ID = i + 1]) )))) / 1000 )   ; Find the variability of the swimmer speed in this lane, and add this to the lsit of variability
      set swimmerSpeedsInEachLane replace-item i swimmerSpeedsInEachLane ([top-speed] of turtles with [lane-ID = i + 1])
      set i i + 1   ; Increment the index we're looking at
    ]
    set variabilitySpeedInPool ( abs (round ( 1000 * ((100 / (max [top-speed] of turtles)) - (100 / (min [top-speed] of turtles) )))) / 1000 )   ; Find the variability of the swimming speeds of all the swimmers
  ]

  if current-swimmers > numberSwimmersInPool [               ; If we have too many swimmers
    let num-remove current-swimmers - numberSwimmersInPool   ; Then find the excess
    ask n-of num-remove turtles [                            ; and remove them
      die
    ]
  ]
end

to clustering-alg
  py:setup "clusteringEnvironment/Scripts/python"
  py:set "swimmers" sort-on [index-speed] turtles                            ; create a list of sweimmers where they are ordered from slowest to fastest
  py:set "num_clusters" numberOfLanes + numberLanesPool2                     ; we need the saem number of clusters as we have lanes available
  py:set "max_size" maxNumberOfSwimmers / (numberOfLanes + numberLanesPool2) ; the max size of a cluster is the max amount of space in a lane, which is the max number of swimmers over the number of lanes.
  py:set "max_size_one" 2 * (ceiling (poolLength / 2) * numberOfLanes)
  py:set "max_size_two" 2 * (ceiling (lengthOfPool2 / 2) * numberLanesPool2)
  py:set "num_lanes_one" numberOfLanes
  py:set "num_lanes_two" numberLanesPool2
  (py:run ; run python for our calculations
;    k_means_clustering requires 2-dimensions, so we will project the 1-D speeds onto a 2-d plane by adding 0 in the y-direction. e.g. make swimmer speed (vi, 0)
    "from k_means_constrained import KMeansConstrained"    ; we are using the k_means_contrained library, see references/credit.
    "import numpy as np"                                   ; we will need numpy so we can create numpy arrays.
    ; Split the swimmers between pool one and pool two
    "num_pool_one = len(swimmers) / 2"                     ; Start by assigning half and half
    "num_pool_two = len(swimmers) / 2"
    "if num_pool_one > max_size_one and num_pool_two > max_size_two:"        ; if both pools are above capacity, then we have too many swimmers
    "    print('!!!!!ILLEGAL INPUT, TOOOOO MANNNYYYY SWIMMMEEERSSSSS!!!!!')" ; this should never call, as we have error checking for this at the start of the simulation
    "elif num_pool_one > max_size_one:"                ; if pool one is above capacity
    "    num_pool_two += num_pool_one - max_size_one"  ; then add the surplus swimmers to pool two
    "    num_pool_one = max_size_one"                  ; and set the number of swimmers in pool one to be the maximun possible
    "elif num_pool_two > max_size_two:"                ; if pool two is above capacity
    "    num_pool_one += num_pool_two - max_size_two"  ; then add the surplus swimmers to pool one
    "    num_pool_two = max_size_two"                  ; and set the number of swimmers in pool two to be the maximun possible
    ; note: if we didn't have error checking at the start of the program that ensured there weren't too many swimmers, and reduces the number ot be the max if we are larger than the max, then after executing the above code, it should not be possilbe to have too many swimmers
    "def cluster(swimmer_list, num_clusters, max_size):"       ; we define a function that performs the actual clustering algorithm ,s icne we need to do this twice (once for each pool)
    "    max_size = min(max_size, len(swimmer_list))"          ; this is needed so that the number of swimmers is not less than the max size. Becuase the kMeansConstrained algorithm requires this, or it crashes
    "    speeds = []"                                          ; speeds will eventually be a 2-d np array where each entry is [vi, 0]
    "    indices = []"                                         ; indices will be a list of all the index-speeds, in ascending order
    "    for swimmer in swimmer_list:"                         ; loop through all of the swimmers
    "        swimmer['TOP-SPEED'] = [swimmer['TOP-SPEED'], 0]" ; change the swimmers top-speed to be in 2-space: i.e. [vi, 0]
    "        speeds.append(swimmer['TOP-SPEED'])"              ; append this 2-d speed to the speeds list
    "        indices.append(swimmer['INDEX-SPEED'])"           ; and add the index we just processed to the indices list.
    "    speeds = np.array(speeds)"                            ; After we complete the for loop, we've processed all the speeds, so can now turn speeds into a numpy array
    "    return KMeansConstrained(n_clusters=num_clusters,size_min=1,size_max=max_size,random_state=0), speeds, indices" ; we then perform the contrained k-means clustering algorithm on this array (again, see refernces/credit), and return the functions results
    ; we don't run this is the number of lanes in a pool is 0!

    "pool_one_info = list(cluster(swimmers[:int(num_pool_one)], num_lanes_one, max_size_one / num_lanes_one))" ; Cluster pool one
    "pool_two_info = list(cluster(swimmers[int(num_pool_one):], num_lanes_two, max_size_two / num_lanes_two))" ; Cluster pool two
  )

  ; note, the centroids of pool two will be offset by the number of lanes in pool 1
  ; note, we need to define a list of all the centroids, so that the gumbel distribution plot works properly
  (py:run ; Note: a cleaner way of doing this is by returning the value of pool_two_info[0].fit_predict(pool_two_info[1]) in the clustering function. I have not done this, because I might have wanted to use other thigns from the kmeans object. However, I did not, so changing it back would make the code nicer, but I don't want to break anything by changing it.
    "centroids_pool_two = pool_two_info[0].fit_predict(pool_two_info[1])" ; Calculate the centroids
    "for i in range(len(centroids_pool_two)):"           ; loop through the centroids
    "    centroids_pool_two[i] += num_lanes_one"         ; offset them by the number of lanes in pool one
;    "centroids = range(num_lanes_one + num_lanes_two)"   ; create the list of centroid indices required for the gumbel distribution plot.
        ; the above definition of centroids is not actually what we want, we want the actuall centroid positions on the speed axis!
    "centroids = []" ; the only point of this appears to be to place dots on the distribution plot to show where the swimmers are clustered around. It is essentially a nice feature, so isn;t that important if it doesn't work perfectly.
;    "print(pool_one_info[0].cluster_centers_)"        ; I have no idea what is different with this, and the same this for pool_two_info[0], since they are both the same this. But supposedly for this instance, it doesn't have that method?
;    "for cen in pool_two_info[0].cluster_centers_:"
;    "    centroids.append(int(cen[0]))"
;    "for cen in pool_two_info[0].cluster_centers_:"
;    "    centroids.append(int(cen[0]))"

  )
  set centroids py:runresult "centroids"

  placeSwimmersClustering py:runresult "pool_one_info[2]" py:runresult "pool_one_info[0].fit_predict(pool_one_info[1])" ; place swimmers in pool 1
  placeSwimmersClustering py:runresult "pool_two_info[2]" py:runresult "centroids_pool_two"                             ; place swimmers in pool 2

end

to placeSwimmersClustering [indices centroid_list]  ; we want a seperate function to place the swimmers for clustering, since we need to do the same thing multiple times
  let available-patches patches with [pcolor != (rgb 0 0 0) and pycor mod 4 != 2] ; we need to define the space of which we can select tiles from. Note this is a static list, and hecne is not updated during the palcement. Hence it is only the 'available tiles' for the very first instance, and after that we reduce the numbe rof pathces we can choose from by making sure the one we select from available patches has no swimmer on it, and is in the correct lane.
  let loop_num 0                               ; We need to keep track of what loop iteration we ar on so we can select the correct centroid values
  foreach indices [[index] ->                   ; We want to loop through the swimmers in a structured order. Indices is a list of index-speeds from 0-max_index_speed
    ask turtles with [index-speed = index] [    ; We place the swimmers in order of their index-speed, so loop through them in that order
      let centroid item loop_num centroid_list  ; note, centroid here is effectively the lane-number - 1. i.e. each swimmer is asigned to a centroid (from 0-k), and each of these centroids reprosents a lane. So this is 0-indexed lanes
      set lane-ID centroid + 1                  ; The lane number of the swimmer is the centroid value + 1, since centroid is just lane-number zero indexed
      let random-patch one-of available-patches with [pycor > 0 + (centroid) * 4 and pycor <= (centroid + 1) * 4 and (count [turtles-at 0 0] of patch pxcor pycor) = 0] ; select an availiable patch from the lane we selected in the above while loop
      setxy [pxcor] of random-patch [pycor] of random-patch             ; Place the swimmer like we did for the previous two methods
      set ybottom 1 + (floor (ycor / 4) )* 4                             ; set the bottom y-value, which is depended on what lane we placed the swimmer into
      ifelse ybottom > numberOfLanes * 4 [ ; probably is what checks if the swimmer is in pool 1 or 2.
        set simulation-length round (item 1 lanes-length / 2)
      ] [
        set simulation-length round (item 0 lanes-length / 2)
      ]
      set loop_num loop_num + 1 ; increment the loop counter
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
  ifelse debugSetup [         ; if we're only testing the setup, then end the program now, if it made it this far!
    print("great success")
    print(swimmer1Speed)
    stop
  ] [                               ; if not, then run the simulation
    set previous-collisons crashID

    ; Update independent variables
    set range-swimmer-view range-swimmer-vision-real / 2 ; Adjust swimmer view range
    set delayInLaneUntilNow ((round (1000 * (item (laneID - 1) delayInLaneList))) / 1000) ; Update the data we're collecting
    set crashInLaneUntilNow item (laneID - 1) crashInLaneList
    set overtakingInLaneUntilNow item (laneID - 1) overtakingInLaneList
    set numberSwimmersInLane item (laneID - 1) numberSwimmersInLaneList
    set variabilitySpeedInLane item (laneID - 1) variabilitySpeedInLaneList
    set averageSpeedInLane item (laneID - 1) averageSpeedInLaneList
    set numberSwimmersInLane item (laneID - 1) numberSwimmersInLaneList
    set missInLaneUntilNow item (laneID - 1) missInLaneUntilNowList
;    set crashAtWall 0

    ; Move turtles based on their speed
    ask turtles [
    if ticks mod top-speed = 0 [
        swim
      ]
    ]

    if ticks mod 50 = 0 [        ; Update how much time has elapsed
      set secondes secondes + 1
      if secondes mod 60 = 0 [
        set minutes minutes + 1
      ]
      record-collisions          ; record the collisions that have occured in the last time step
    ]

    ; Calculate average delay per tour if any tours have been completed
    if sum [number-tours] of turtles != 0 [
      set delay-average delayInPoolUntilNow / sum [number-tours] of turtles
    ]

    ; Stop simulation if specified duration is reached
    if secondes >= (simulationDuration * 60 * 60) [    ; Convert hours into seconds for this comparison, as SimulationDuration is measured in hours
      stop
    ]
    tick
  ]

end



to swim
  set next-move-x 0                            ; Reset motion in the x and y direction
  set next-move-y 0
  set previous-target target                   ; Make the previus target be the current target
  let missInPoolUntilNow-flag 0


;  ifelse collision-flag = true [
;    set color rgb 227 0 0
;  ] [
;    ifelse target = 0 [ ; this is for debugging purposes. Used so you can watch collisions
;      set color rgb 48 252 3
;    ] [
;      set color rgb 3 48 252
;    ]
;  ]
;
  if ycor = ybottom + 2 [  ; At the top row
    set target 0           ; Then the target of the swimmer should be the righthand side of the pool
    set next-move-x 1
    ifelse xcor < ( simulation-length - 1 ) [   ; At the top row, move left when reaching the end
    ][
      if xcor = ( simulation-length - 1 ) and not any? turtles-on patch-at 0 -1 [ ; If there are no swimmers below you, the move down (this is part of the turn around process)
        set next-move-y -1                                                           ; Then tell the swimmer to move down
      ]
      if xcor = simulation-length [                                                   ; If the swimmer has reached the end of the lane, then it:
        set next-move-y -1                                                            ; must move down
        set next-move-x -0    ;Changed this from -1 to 0. If we're at the y-coordinate we need to be in for this block to run, then we don't want to move diagonally, we just want to move down.                                                        ; and turn around
        set target 1                                                                  ; Then it's new target is the other side of the pool (the left side)
      ]
    ]
    ]

  if ycor = ybottom + 1 [ ; middle row
      if xcor = simulation-length [  ; At the middle row, move left and down when the swimmer has reached the end
      set next-move-y -1
      set next-move-x -1
      ]
      if xcor = 1 [  ; At the middle row, move right and up when the swimmer has reached the start (e.g. when the swimmer is swimming from right to left, and has reached left wall)
      set next-move-y 1
      set next-move-x 1
      ]

    if target = 0 [                      ;overtake1  ;  if the player is swimming from left to right
    if xcor < ( simulation-length - 1 ) [                                                                        ; if the player is not at the end of the lane
        let turtle-front turtles-on patch-at 1 0                                                                 ; This variable will contain information on the turtle on the patch infront of the swimmer, if any. WE can use this to see if we're going to collide with someone
        ifelse not any? turtles-on patch-at 0 1 or ([target] of turtle-front != target and any? turtle-front ) [ ; if there isn't a swimmer above, or if there is a swimmer infront of you, that is swimming towards you, then move up into the top row
          set next-move-x 1
          set next-move-y 1
          set missInPoolUntilNow-flag 1            ; Flag that something has gone down
        ] [                 ; else
          ifelse [target] of turtle-front = target and any? turtle-front  [ ; if there is a swimmer in your way, and you can't move back out of the middle row, then we need to stop and wait
            set next-move-x 0 ; Hence stop
            set next-move-y 0
          ][
            set next-move-x 1 ; else, keep moving forward
            set next-move-y 0
          ]

        ]
      ]
      if xcor = ( simulation-length - 1 )[ ; if we've nearly reached the end of the pool, then keep moving forward
      set next-move-x 1
      set next-move-y 0
      ]
    ]
    if target = 1 [                      ;overtake2, i.e. we're swimming to the lefthand side of the pool, in the middle row
      if xcor > 2 [                                                                                                ; if we're not 2 patches or less from the end
        let turtle-front turtles-on patch-at -1 0                                                                  ; this variable contains information on the swimmer on the patch infront of us, if there is one!
        ifelse not any? turtles-on patch-at 0 -1 or ([ target ] of turtle-front != target and any? turtle-front  )[ ; if there is no swimmer directly below us, or there is a swimmer on the patch infront of us, that is swimming towards us, then
          set next-move-x -1                                                                                       ; move down into the bottom row, while swimming forward (from right to left)
          set next-move-y -1

          set missInPoolUntilNow-flag 1       ; flag that something happened
        ] [                    ; else
          ifelse [target] of turtle-front = target and any? turtle-front  [ ; if there is a swimmer in your way, and you can't move back out of the middle row, then we need to stop and wait
            set next-move-x 0 ; Hence stop
            set next-move-y 0
          ][
            set next-move-x -1 ; else, keep moving forward
            set next-move-y 0
          ]
        ]
     ]
      if xcor = 2 [        ; if the swimmer is at the end, then keep swimming towards the lefthandside
      set next-move-x -1
      set next-move-y  0
      ]
    ]
  ]

  if ycor = ybottom [  ; if we're in the bottom row
    set target 1       ; then we're swimming from right to left
    set next-move-x -1 ; so set the swimmer to move from right to left
    ifelse xcor > 2 [  ; At the bottom row, move right when reaching the beginning
    ][
      if xcor = 2 and not any? turtles-on patch-at 0 1 [ ; if we have reached the lefthand side, and there are not any swimmers directly above us, then
        set next-move-y 1                                   ; move up
;        set next-move-x -1
      ]
      if xcor = 1 [       ; if the xcordinate is 1, then we need to make the swimmer turn around
        set next-move-y 1 ; so start swimming from left to right
        set next-move-x 0 ; move up, changed from 1 to 0, as we shouldn't be moving diagonally here
        set target 0      ; and set the target to be the righthand side
      ]
     ]

  ]

  if any? turtles-on patch-at next-move-x next-move-y [              ; if there are any swimmers on the patch that we want to move too, then:
    let my-xcor xcor                                                 ; define variables for your coordinates
    let my-ycor ycor
    let my-ybottom ybottom

    let turtles-on-patch turtles-on patch-at next-move-x next-move-y ; store information on the swimmer(s) infront of you
    let max-speed-of-others max [top-speed] of turtles-on-patch

;     If your patience is 100, then the following should never be true, because for some positive ints say x & y: y + (100 * x) / 100 = y + x > x
;    The function works by taking the speed of the swimmer blocking you, and comparing how much slower it is compared to your patience. Effectively, it takes your speed and adds an offset based on your patience level and the speed of the swimmer in your way, then if this is
    ifelse max-speed-of-others > top-speed + ((patiencePercentage * max-speed-of-others) / 100)  [   ; if the swimmer(s) max speed is greater than your max speed + some function of your patience, essentially, if we lose patience then
    if target = 0 [                                  ; if you're swimming to the righthand side ; Becayse speed is stored inversly, lower is faster. Hence this patiance function does make sense.
        ifelse xcor < ( simulation-length - 2 ) [    ; if you're not at the end of the lane
          ifelse ycor <= ybottom + 1 [               ; and you're in the midle or bottom row
            if ycor = ybottom + 1 [                  ; if you're in the middle row
              set next-move-y 0                      ; then keep moving to the right
              set next-move-x 1
            ]
          ] [                 ; else, we're in the top row
            let turtles-in-midle turtles-on patches with [pycor = my-ybottom + 1 and pxcor >= my-xcor and pxcor <= my-xcor + range-swimmer-view ] ; are there any swimmers on the patchs in the range from you're x-coordinate, and the end of the sight distance, in the middle row

            ifelse any? turtles-in-midle [                      ; if you can see any swimmers in the middle row, then:
              ifelse [ target ] of turtles-in-midle != target [ ; if the their target is not the same as yours, then you must avoid a colision, so cannot move down to overtake, or move forward (because you'd hit the swimmer infron of you)
                set next-move-x 0 ; hence stop
                set next-move-y 0
              ][                   ; else, if the swimmers we can see, are swimming the same direction as us, then
                set next-move-y -1 ; we can move down and attempt an overtaking maneuver
                set next-move-x 1
              ]
            ] [                    ; else if, we can see no swimmers, then:
              set next-move-y -1   ; we can move down and attempt an overtaking maneuver
              set next-move-x 1
            ]
          ]
        ][                                  ; else if the xcor >= simulation-length - 2
          ifelse xcor = simulation-length [ ; if we've reached the end of the lane
            set next-move-x 0                   ; then we must stop
            set next-move-y 0
          ] [                               ; else
            set next-move-x 1                   ; we can keep swimming
            set next-move-y 0
          ]
        ]
    ]
    if target = 1 [                                 ; if we're swimming from right to left, then
       ifelse xcor > 3 [                            ; if we aren't near the end of the lane, then:
          ifelse ycor >= ybottom + 1 [                  ; if we're not in the bottom row, then:
            if ycor = ybottom + 1 [                 ; if we're in the middle row, then
              set next-move-y 0                     ; keep swimming to the lefthandside of the lane
              set next-move-x -1
            ]
          ] [                                                                                                                                    ; else, we're in the bottom row
            let turtles-in-midle turtles-on patches with [pycor = my-ybottom + 1 and pxcor >= my-xcor and pxcor <= my-xcor - range-swimmer-view ] ; collect data on swimmers in the middle row, that you can see

            ifelse any? turtles-in-midle [                      ; if you can see any swimmers in the middle row
              ifelse [ target ] of turtles-in-midle != target [ ; if there are swimmers that aren't swimming the same direction as you, then:
                set next-move-x 0                               ; you can't attempt to overtake, as it would result in a collision, and you can't move forward, because then you'd collide with the swimmer infront of you
                set next-move-y 0
              ][                                                ; if all the swimmers you can see are moving in the same direction as you, then we can attempt an overtaking manevour.
                set next-move-y 1                               ; hence move up, and forward
                set next-move-x -1
              ]
            ][                                                  ; if you can see no swimmers, then we can attempt an overtaking manevour!
              set next-move-y 1
              set next-move-x -1
            ]
          ]
        ][                        ; else, if we're reaching the left handside
          ifelse xcor = 1 [       ; then if we've reached the wall, we must stop
            set next-move-x 0
            set next-move-y 0
          ][                      ; else, keep moving forward
            set next-move-x -1
            set next-move-y 0
          ]
        ]
    ]
    ] [                  ; if we're patient enough, then just stop to avoid colliding, don't attempt to overtake
      set next-move-x 0
      set next-move-y 0
    ]
]
  ifelse not any? turtles-on patch-at next-move-x next-move-y [                           ; if there aren't any swimmers in where we want to move, then:
    if missInPoolUntilNow-flag = 1 and next-move-y != 0 and collision-flag = false [      ; if something is happening, we're changing rows, and haven't collided with anything
      if any? turtles-on patch-at next-move-y 0  [                                        ; then if there are any swimmers on the on the patch next-move-y in the x direction from the swimmer. i.e. if there was a swimmer infront of you when you moved out of the middle row
        if collision-flag = [collision-flag] of one-of turtles-on patch-at next-move-y 0  and max [target] of turtles-on patch-at next-move-y 0 != target [ ; if one of the swimmers on the patch described above, and the largest target of the swimmers on this patch is not our target (as there should't be more than one swimmer on this patch, then these checks should be checking the same swimmer)
              set missInPoolUntilNow missInPoolUntilNow + 1                                                                               ; then we did not colide
              set missInLaneUntilNowList replace-item (lane-ID - 1) missInLaneUntilNowList (item (lane-ID - 1) missInLaneUntilNowList + 1) ; so update the relevant statistics
        ]
      ]
    ]

    let ycor-previous ycor      ; update the previous y-coordinate


    set xcor xcor + next-move-x ; update the swimmers position
    set ycor ycor + next-move-y

    set collision-flag false  ; reset the collision-flag

    if ycor = ybottom + 1 and ycor-previous != ycor and xcor > 1 and xcor < simulation-length - 1 [ ; if we're in the middle row, and we were in a different row in the last timestamp, and we are not at either of the lanes ends
      set overtakingInPoolUntilNow overtakingInPoolUntilNow + 1                                    ; then we have begun overtaking since the last loop, so increment the number of overtaking attempts
      set overtakingInLaneList replace-item (lane-ID - 1) overtakingInLaneList (item (lane-ID - 1) overtakingInLaneList + 1)
    ]

  ] [
    if ycor = ybottom + 1 and max [ target ] of turtles-on patch-at next-move-x next-move-y != target and collision-flag = false and one-of [collision-flag] of turtles-on patch-at next-move-x next-move-y = false[ ; if we're in the middle row, and if there is a swimmer on the patch we're about to move too is swimming the opposite direction to us, and we haven't collided, and they haven't collided yet, then:
      find-lane                                                       ; find the lane they're in
      set swimmer1speed (round (1000 * (100 / top-speed))) / 1000      ; create speed variables for the two colliding swimmers
      set swimmer2speed (round (1000 * (100 / max [top-speed] of turtles-on patch-at next-move-x next-move-y))) / 1000
      set swimmer1index index-speed
      set swimmer2index max [index-speed] of turtles-on patch-at next-move-x next-move-y
      set crashInLaneList replace-item (lane-ID - 1) crashInLaneList (item (lane-ID - 1) crashInLaneList + 1) ; update the number of crashes
      set collision-midle-lane collision-midle-lane + 1
      set collision-flag true                                                       ; flag that a collision has occured

      if (xcor = simulation-length - 1 and target = 1) or (xcor = 2 and target = 0)  ; if they're at the wall, then increment the number of crashes at the wall
      [
        set crashAtWall crashAtWall + 1
      ]
    ]
  ]
  if previous-target != target [                                                          ; if we've turned around in the last time step, then
      set time-lane (secondes - time-save) - ((simulation-length * 2) / (100 / top-speed)) ; state how long the swimmer has been swimming since the last timestampt
    ifelse time-lane < 0 [
    ] [
      set delayInLaneList replace-item (lane-ID - 1) delayInLaneList (item (lane-ID - 1) delayInLaneList + time-lane) ; how much have swimmers been delayed in this lane?
      set time-lane-plot time-lane-plot + time-lane                                       ; increment the time we've been in the lane
      set delayInPoolUntilNow (round (1000 * (delayInPoolUntilNow + time-lane))) / 1000    ; how much have we been delayed
    ]
      set number-tours number-tours + 1 ; increment the number of lanes we've swum
      set time-save secondes            ; save the current time stamp
  ]
;  if collision-flag = true [
;    set color rgb 227 0 0
;  ]
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
  if swimmingStyle = "back" [            ; If swimming backstroke
    set range-swimmer-vision-real 0      ; How far ahead the swimmer can see in reality
    set index 0
  ]
  if swimmingStyle = "breast" [          ; If swimming breaststroke
    set range-swimmer-vision-real 10     ; How far ahead the swimmer can see in reality
    set index 1
  ]
  if swimmingStyle = "fly" [             ; If swimming fly
    set range-swimmer-vision-real 10     ; How far ahead the swimmer can see in reality
    set index 2
  ]
  if swimmingStyle = "free" [            ; If swimming freestyle
    set range-swimmer-vision-real 5      ; How far ahead the swimmer can see in reality
    set index 3
  ]
  if swimmingStyle = "IM" [              ; If swimming meadly
    set range-swimmer-vision-real 5      ; How far ahead the swimmer can see in reality
    set index 4
  ]

  ifelse gender = "male" [                                                       ; If male
    let mu_list [0.9472 0.9491 1.1808 1.2098 1.0893] ; [Back Breast Fly Free IM]
    let beta_list [0.2085 0.1710 0.2027 0.2247 0.1748]
    let min_speed_list [0.4082 0.3377 0.5974 0.4191 0.5513]                      ; minimun speeds for the different strokes
    let max_speed_list [1.7899 1.4573 1.8155 1.9751 1.7665]                      ; maximun speeds for the different strokes
    set mu item index mu_list                                                    ; grabs the mu based on the stroke the swimmer is using
    set beta item index beta_list                                                ; grabs the beta value based on the stroke being used
    set min_speed item index min_speed_list                                      ; grabs the min_speed based on the stroke being used
    set max_speed item index max_speed_list                                      ; grabs the max_speed based on the stroke being used
  ] [                                                                            ; Else: do the same as above, but with female statistics
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
  while [y < min_speed or y > max_speed] [ ; generate a speed based on the gumbel distribution
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
5.0
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
6.0
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
4.0
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
2

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
5.0
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
404.0
1
0
Number

SWITCH
244
64
365
97
debugSetup
debugSetup
0
1
-1000

MONITOR
481
236
635
281
maxNumberOfSwimmers
maxNumberOfSwimmers
17
1
11

MONITOR
535
172
731
217
number of realocated swimmers
numReallocated
17
1
11

@#$#@#$#@
# WHAT IS IT?
- Potentially put the intro to the paper you're writing in here, then credit it in the final section accordingly

# HOW IT WORKS

## Setup

### Swimmer setup
- The swimmer speeds are allocated based on a Gumbel distribution generated from real world data.
- Each swimmer has an 'index-speed' parameter, which is used for swimmer selection in the different allocation methods. This parameter is set as the current loop iteration while looping through the swimmers from the slowest to the fastest (Note: the logical allocation algorithm is done in the same loop as this). Hence this value refers to where the player lies in the population distribution. For example, if there are 10 swimmers, then the slowest swimmer has an index-speed of 0, and the fastest has an index-speed of 9.
- Each swimmer has a 'target' parameter, which is used to tell which direction they are swimming. This is used in the overtaking logic.
- When a swimmer is allocated to a lane, it's placed on a random available patch in said lane.

### Logical allocation method
The idea behind the logical allocation method is to place the swimmers such that the slowest p swimmers are in the first lane, the second slowest p swimmers are in the second lane, ..., and the fastest p swimmers are in the last lane. Where p = number of swimmers / number of lanes.

Because there isn't always a nice number of swimmers, some lanes by necessity, have more swimmers than other lanes. For example, if we have 5 lanes and 12 swimmers, then the distribution of swimmers will be: [3, 3, 2, 2, 2]. So the actual implementation of this idea requires some other complexities, as described in the detailed code comments. 

### Random allocation method
Ideally we want the same number of swimmers in each lane, and there to be no empty lanes between populated lanes. Hence, this program works by looping through the lanes, and each iteration randomly selects a swimmer from the list of not currently added swimmers to be placed into the lane. This swimmer is then removed from the list of available swimmers. If we do this until there are no unallocated swimmers, then we have a random distribution of swimmers in each lane, but a uniform number of swimmers per lane (with the same caveat as the logical method). 
From a lower lever, the swimmers are selected by setting the initial pool of available swimmers to be a list of all the index-speeds, then a random index-speed is grabbed from this list, and then pushed into another list containing added swimmers, which is then used to redefine the list of available swimmers. Then we select the swimmer associated with this index-speed and update its values such that it is in the lane associated with the given iteration.

### Clustering allocation method
This program uses a k-clustering algorithm, where k is the number of clusters, which is the number of lanes. From a high level, this implementation works by defining a step-size to be the ceiling of the number of swimmers over the number of clusters. Then we step through the swimmers using index-speed based on this, and each step define a centroid based on the selected swimmers speed*. 
Then we loop through the swimmers, and add each one to the cluster with the centroid most associated with the swimmer. This is the centroid with the closest value to the swimmer's speed. This method does not usually led to a uniform amount of swimmers per lane.

*This implementation can result in multiple centroids with the same value, which has a rarity inversely proportional to the number of swimmers. This can happen when the swimmer at a given step has the same speed as a swimmer at a different step. When this happens, some swimmers are associated with multiple clusters (since the centroids are the same). Hence they are all added to the first cluster they are associated with, then moved the next, et cetera, until the final cluster they are associated with. This creates empty lanes between populated lanes, so this program checks to make sure new centroids haven't been added before. 

### Lane statistics

After the swimmers have all been allocated as desired, the program then calculates the mean and variation of speed in each lane, as well as the overall speed variability. This section also counts the number of swimmers per lane.

## Running

After the setup has been completed, the simulation can start running (provided it is not in the debug setup mode). From a high level, this works by first updating the simulation's independent variables, then telling the swimmers to swim according to their max speed, before updating the time elapsed, number of collisions and total delay. 
## Swim function
Each swimmer is told to swim when the modulus of the number of loops (ticks), since the simulation started, and their top speed is zero. Which, since their top speed is recorded as 100 / sampled speed, means the faster swimmers will be told to swim more often, as desired. 
After the swimmer is told to swim, the program checks what part of the lane they are in (top, middle or bottom row), and moves them in accordance:

* Top Row: The swimmer is moving to the right, so if it has not reached the end, make it swim to the right. Then when it reaches the end, make it move  to the bottom row and turn around. 

* Bottom Row: The swimmer is moving to the left, so if it has not reached the end, make it swim to the left. Then, when it reaches the end, make it move to the top row, and turn around.

Except, if there is a swimmer on the patch directly in front of the swimmer being updating, and the swimmer has insufficient patience, then it will attempt an overtaking maneuver, and if not, then it will stay behind the slower swimmer.

When an overtaking maneuver is considered, the swimmer checks the patches in the middle row within its sight distance, and if there are no swimmers with an opposing target in sight, then it will move into the middle row, and start overtaking. However, if it does see an opposing swimmer, then it will abort, and stay behind the slower swimmer.

This leads us to need logic for swimmers in the middle row:

* Middle Row: The swimmer is overtaking, so they must keep swimming in the direction allocated by their target parameter, until they have passed the slower swimmer, or there is a swimmer on the patch in front of them, with an opposing target. If either of these conditions are met, then the swimmer will move back into the appropriate row. If this was for the latter reason, then a collision is likely to occur, as the swimmer moves regardless of whether there is a swimmer in the way. Hence, the movement is flagged, so that near misses can be counted later.

### Overtaking logic:
1) Pre overtake

1.1) Swimmer A checks if Swimmer B is in the way

1.2) Compare each swimmer's speed, and if Swimmer B is too slow, and Swimmer A is too impatient, and the swimmers are not at the end of the lane, then swimmer A attempts to overtake. 

1.3) At the end of the lane, the swimmer will attemt to move like the following diagram:  
0:2:0,|,3:0:1						
1:0:3,|,0:2:0
moving diagonally into the middle row, before to the bottom/top row. If 	Swimmer B is at postion two, and swimmer A is at postion 1, then Swimmer A can't overtake. Instead, Swimmer A will continue swimming straight. If instead Swimmer A is at the lanes corner (between position 1 and 2), then if has no where to swim, as Swimmer B is in the way at position 2, so Swimmer A stops swimming for this time step. 

1.4) If Swimmer A is already in the middle row, then just keep swimming. This feels odd, since if Swimmer B is infront, it is in the way, and there should be no space for swimmer A, so it should stop swimming until there is space?

1.5) Check for swimmers on the middle lane within your sight distance. For backstroke swimmers sight distance includes the tile directly next to you. i.e. at the same x-coordinate as you.

1.6) If there is a swimmer within Swimmer A's sightdistance, and it is swimming in the apposite direction as swimmer A, then swimmer A does not overtake. If it is swimming the same direction, or there are no swimmers within Swimmer A's sight distance, then swimmer A will move down into the middle row to overtake.

2) During overtake

2.1) Each time swimmer A is updated, it checks if there is an empty tile above it, and if there is, then it moves up and has finished overtaking. 

2.2) Swimmer A also checks for swimmers on the tile infront of it, and if there are any apposing swimmers, then swimmer A moves out of the middle row regardless of whether there are any swimmers in the way. This can result in collisions occuring in rows other than the middle

2.3) If there is no space above, and there are no apposing swimmers, then Swimmer A continues forward in the middle row.

### Collision logic:
To check for collisions, the program looks at where the swimmer will be on the next tick, based on the movement determined by the above swimming logic, and checks to see if that patch will be empty. If it is empty, then if the swimmer moved out of the middle row to avoid a collision, then there was a near miss.
Otherwise, if the patch is not empty, and the swimmer is initially in the middle row, the swimmers are moving in opposing directions, and have not collided yet, then they must collide. Hence, the number of collisions  in the middle row is incremented. If the collision occurs at either end of the pool, then the number of collisions at the wall is also incremented. These are later used in the record-collisions function, to update the relevant independent variables.

At the end of the swim function, the total delay experienced by the swimmer is tallied. 

# HOW TO USE IT

The simulation setup has twelve different settings you can modify. They are detailed below:

* NumberOfSwimmersInPool: This is an entry box that only accepts positive integers, anything else will make the program crash. This dictates how many swimmers are placed into the pool for the simulation. If there are more swimmers than available patches in the pool, the simulation will crash.
* PatiencePercentage: This dictates how patient the swimmers will be in the simulation, essentially, how much slower they will swim before they lose patience and overtake.
* SwimmingStyle: This lets you choose which swimming style you want to simulate; backstroke, breaststroke, butterfly, freestyle or individual medley.
* Gender: This lets you choose which gender you want to consider in the simulation.
* Allocation: This lets you decide what allocation method to use, Logic, Random or Cluster, which allocates in accordance to the algorithms explained in the swimmer setup section.
* NumberOfLanes: This sets how many lanes will be in the main pool. There is a maximum of 10, unless the second pool is active (in which case there would be a maximum of (10 - number of lanes in the second pool)).
* PoolLength: This lets you choose if the pool is a full 50m, or only 25m.
* SimulationDuration: This lets you decide how long the simulation will go for in the simulated reality (measured in simulation hours).
* NumberOfLanesPool2: This is the number of lanes in pool two. It has a maximum of (10 - number of lanes in the first pool).
* LengthOfPool2: This lets you decide if the second pool is 50m or 25m.
* CarefulAtWall: This lets you decide if the swimmers will be careful at the wall, if they aren't careful, then they can collide with the walls at either end of the pool.
* DebugSetup: This switch was used during debugging. If you only want to run the setup process, set this to 'On', otherwise make sure it is set to ‘Off’. 

When you are happy, press setup to implement the selected settings. Then press go to run the simulation.

# THINGS TO NOTICE
* There is a bug where if the simulation is setup to have two different pools, and then changed back to have one pool, and the second one isn't manually reduced to have zero lanes, then the number of lanes can exceed the amount we want (and is potentially more than 10). This can crash the program.
* If the simulation is not running, check that the debugSetup switch is set to 'Off'.

# Potential additions
* Make it so the simulation can handle both male and female swimmers at the same time (this would likely result in a larger overall variance of swimming speed).


# CREDITS AND REFERENCES

Code initially by: XXXX (XXXX, XXXX)
Further code: Jethro Ware (2024-2025) - see change logs
Documentation and comments by: Jethro Ware (December 2024)
Project leader: Dr Christoph Bartneck


@software{Levy-Kramer_k-means-constrained_2018,
  author = {Levy-Kramer, Josh},
  month = apr,
  title = {{k-means-constrained}},
  url = {https://github.com/joshlk/k-means-constrained},
  year = {2018}
}
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
    <exitCondition>minutes = simulationDuration * 1</exitCondition>
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
    <enumeratedValueSet variable="debugSetup">
      <value value="false"/>
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
      <value value="&quot;clustering&quot;"/>
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
    <exitCondition>setupComplete = true</exitCondition>
    <runMetricsCondition>((previous-collisons != crashID ) or (ticks = (simulationDuration * 3000 * 60) - 50))</runMetricsCondition>
    <enumeratedValueSet variable="gender">
      <value value="&quot;male&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debugSetup">
      <value value="true"/>
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
    <enumeratedValueSet variable="debugSetup">
      <value value="false"/>
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
  <experiment name="SL_50_10_back" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
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
      <value value="50"/>
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
      <value value="&quot;random&quot;"/>
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="40" step="1" last="500"/>
  </experiment>
  <experiment name="SL_50_01_back" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
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
      <value value="50"/>
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
      <value value="&quot;random&quot;"/>
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="4" step="1" last="50"/>
  </experiment>
  <experiment name="SL_50_02_back" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
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
      <value value="50"/>
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
      <value value="&quot;random&quot;"/>
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="8" step="1" last="100"/>
  </experiment>
  <experiment name="SL_50_03_back" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>minutes = simulationDuration * 60</exitCondition>
    <runMetricsCondition>((previous-collisons != crashID ) or (ticks = (simulationDuration * 3000 * 60) - 50))</runMetricsCondition>
    <enumeratedValueSet variable="gender">
      <value value="&quot;male&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swimmingStyle">
      <value value="&quot;back&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="poolLength">
      <value value="50"/>
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
      <value value="&quot;random&quot;"/>
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="12" step="1" last="150"/>
  </experiment>
  <experiment name="SL_50_04_back" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
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
      <value value="50"/>
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
      <value value="&quot;random&quot;"/>
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="4"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="16" step="1" last="200"/>
  </experiment>
  <experiment name="SL_50_05_back" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
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
      <value value="50"/>
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
      <value value="&quot;random&quot;"/>
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="20" step="1" last="250"/>
  </experiment>
  <experiment name="SL_50_06_back" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
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
      <value value="50"/>
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
      <value value="&quot;random&quot;"/>
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="6"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="24" step="1" last="300"/>
  </experiment>
  <experiment name="SL_50_07_back" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
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
      <value value="50"/>
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
      <value value="&quot;random&quot;"/>
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="7"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="28" step="1" last="350"/>
  </experiment>
  <experiment name="SL_50_08_back" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
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
      <value value="50"/>
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
      <value value="&quot;random&quot;"/>
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="8"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="32" step="1" last="400"/>
  </experiment>
  <experiment name="SL_50_09_back" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>minutes = simulationDuration * 60</exitCondition>
    <metric>swimmerSpeedsInEachLane</metric>
    <runMetricsCondition>((previous-collisons != crashID ) or (ticks = (simulationDuration * 3000 * 60) - 50))</runMetricsCondition>
    <enumeratedValueSet variable="gender">
      <value value="&quot;male&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debugSetup">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swimmingStyle">
      <value value="&quot;back&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="poolLength">
      <value value="50"/>
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
      <value value="&quot;random&quot;"/>
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="9"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="36" step="1" last="450"/>
  </experiment>
  <experiment name="SL_25_01_free" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
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
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patiencePercentage">
      <value value="0"/>
      <value value="3"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation">
      <value value="&quot;logic&quot;"/>
      <value value="&quot;random&quot;"/>
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="4" step="1" last="25"/>
  </experiment>
  <experiment name="SL_25_02_free" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
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
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patiencePercentage">
      <value value="0"/>
      <value value="3"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation">
      <value value="&quot;logic&quot;"/>
      <value value="&quot;random&quot;"/>
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="8" step="1" last="50"/>
  </experiment>
  <experiment name="SL_25_03_free" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
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
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patiencePercentage">
      <value value="0"/>
      <value value="3"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation">
      <value value="&quot;logic&quot;"/>
      <value value="&quot;random&quot;"/>
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="12" step="1" last="75"/>
  </experiment>
  <experiment name="SL_25_04_free" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
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
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patiencePercentage">
      <value value="0"/>
      <value value="3"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation">
      <value value="&quot;logic&quot;"/>
      <value value="&quot;random&quot;"/>
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="4"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="16" step="1" last="100"/>
  </experiment>
  <experiment name="SL_25_05_free" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
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
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patiencePercentage">
      <value value="0"/>
      <value value="3"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation">
      <value value="&quot;logic&quot;"/>
      <value value="&quot;random&quot;"/>
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="20" step="1" last="125"/>
  </experiment>
  <experiment name="SL_25_06_free" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
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
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patiencePercentage">
      <value value="0"/>
      <value value="3"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation">
      <value value="&quot;logic&quot;"/>
      <value value="&quot;random&quot;"/>
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="6"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="24" step="1" last="150"/>
  </experiment>
  <experiment name="SL_25_07_free" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
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
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patiencePercentage">
      <value value="0"/>
      <value value="3"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation">
      <value value="&quot;logic&quot;"/>
      <value value="&quot;random&quot;"/>
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="7"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="28" step="1" last="175"/>
  </experiment>
  <experiment name="SL_25_08_free" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
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
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patiencePercentage">
      <value value="0"/>
      <value value="3"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation">
      <value value="&quot;logic&quot;"/>
      <value value="&quot;random&quot;"/>
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="8"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="32" step="1" last="200"/>
  </experiment>
  <experiment name="SL_25_09_free" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
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
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patiencePercentage">
      <value value="0"/>
      <value value="3"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation">
      <value value="&quot;logic&quot;"/>
      <value value="&quot;random&quot;"/>
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="9"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="36" step="1" last="225"/>
  </experiment>
  <experiment name="SL_25_10_free" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
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
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patiencePercentage">
      <value value="0"/>
      <value value="3"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation">
      <value value="&quot;logic&quot;"/>
      <value value="&quot;random&quot;"/>
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="40" step="1" last="250"/>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>setupComplete = true</exitCondition>
    <metric>variabilitySpeedInPool</metric>
    <metric>variabilitySpeedInLaneList</metric>
    <metric>averageSpeedInLane</metric>
    <metric>averageSpeedInLaneList</metric>
    <runMetricsCondition>setupComplete = true</runMetricsCondition>
    <enumeratedValueSet variable="debugSetup">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="poolLength">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gender">
      <value value="&quot;male&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patiencePercentage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulationDuration">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lengthOfPool2">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation">
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberLanesPool2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carefulAtWall">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swimmingStyle">
      <value value="&quot;back&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberSwimmersInPool">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SL_25_10_free (copy)" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <exitCondition>minutes = simulationDuration * 60</exitCondition>
    <metric>swimmerSpeedsInEachLane</metric>
    <runMetricsCondition>minutes = simulationDuration * 60</runMetricsCondition>
    <enumeratedValueSet variable="gender">
      <value value="&quot;male&quot;"/>
      <value value="&quot;female&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swimmingStyle">
      <value value="&quot;IM&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="poolLength">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulationDuration">
      <value value="0"/>
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
      <value value="&quot;random&quot;"/>
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="40" step="1" last="250"/>
  </experiment>
  <experiment name="SL_25_10_free (copy) (1)" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <exitCondition>minutes = simulationDuration * 60</exitCondition>
    <metric>swimmerSpeedsInEachLane</metric>
    <runMetricsCondition>minutes = simulationDuration * 60</runMetricsCondition>
    <enumeratedValueSet variable="gender">
      <value value="&quot;male&quot;"/>
      <value value="&quot;female&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swimmingStyle">
      <value value="&quot;IM&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="poolLength">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="simulationDuration">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberLanesPool2">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lengthOfPool2">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carefulAtWall">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="patiencePercentage">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allocation">
      <value value="&quot;logic&quot;"/>
      <value value="&quot;random&quot;"/>
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="40" step="1" last="250"/>
  </experiment>
  <experiment name="realocation_clustering" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>setupComplete = true</exitCondition>
    <metric>numReallocated</metric>
    <runMetricsCondition>setupComplete = true</runMetricsCondition>
    <enumeratedValueSet variable="gender">
      <value value="&quot;male&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swimmingStyle">
      <value value="&quot;back&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="poolLength">
      <value value="50"/>
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
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="9"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="36" step="1" last="450"/>
    <enumeratedValueSet variable="debugSetup">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SL_50_08_back (copy)" repetitions="5" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>setupComplete = true</exitCondition>
    <metric>swimmerSpeedsInEachLane</metric>
    <runMetricsCondition>setupComplete = true</runMetricsCondition>
    <enumeratedValueSet variable="gender">
      <value value="&quot;male&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="debugSetup">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="swimmingStyle">
      <value value="&quot;back&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="poolLength">
      <value value="50"/>
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
      <value value="&quot;random&quot;"/>
      <value value="&quot;clustering&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numberOfLanes">
      <value value="8"/>
    </enumeratedValueSet>
    <steppedValueSet variable="numberSwimmersInPool" first="32" step="1" last="400"/>
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
