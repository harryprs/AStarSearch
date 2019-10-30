(ns main)


;AF AG AH AI AJ
;BF BG BH BI BJ
;CF CG CH CI CJ
;DF DG DH DI DJ
;EF EG EH EI EJ
;(A*search {:state "AF", :cost 0, :robot "EI"} a*lmg_graph)
;  "Map of node => adjacent node => cost. This could
;   be replaced with any cost function of the shape
;   (node, node') => cost."
;   Node of 10000 cost = wall
(def graph {"AF" {"BF" 10000, "AG" 5},
            "AG" {"AF" 4, "BG" 7, "AH" 2},
            "AH" {"AG" 3, "BH" 1, "AI" 9},
            "AI" {"AH" 6, "BI" 10000, "AJ" 10},
            "AJ" {"AI" 3, "BJ" 7},
            "BF" {"AF" 2, "BG" 5, "CF" 12},
            "BG" {"BF" 10000, "BH" 3, "AG" 5, "CG" 9},
            "BH" {"BG" 1, "BI" 10000, "AH" 11, "CH"5},
            "BI" {"BH" 3, "BJ" 6, "AI" 4, "CI" 9},
            "BJ" {"BI" 10000, "AJ" 2, "CJ" 5},
            "CF" {"CG" 7, "BF" 10000, "DF" 8},
            "CG" {"CF" 2, "CH" 9, "BG" 6, "DG" 4},
            "CH" {"CG" 12, "CI" 10000, "BH" 2, "DH" 10000},
            "CI" {"CH" 8, "CJ" 4, "BI" 10, "DI" 7},
            "CJ" {"CI" 10000, "BJ" 6, "DJ" 3},
            "DF" {"DG" 7, "CF" 11, "EF" 6},
            "DG" {"DF" 7, "DH" 10000, "CG" 7, "EG" 3},
            "DH" {"DG" 4, "DI" 10000, "CH" 8, "EH" 9},
            "DI" {"DH" 10000, "DJ" 5, "CI" 7, "EI" 3},
            "DJ" {"DI" 10000, "CJ" 4, "EJ" 3},
            "EF" {"EG" 1, "DF" 5},
            "EG" {"EF" 8, "EH" 3, "DG" 2},
            "EH" {"EG" 9, "EI" 4, "DH" 10000},
            "EI" {"EH" 12, "EJ" 10, "DI" 10000},
            "EJ" {"EI" 8, "DJ" 9}
            })

;(A*search {:state "A", :cost 0, :robot "G"} a*lmg_circle)
;(time (A*search {:state "A", :cost 0, :robot "G"} a*lmg_circle))
(def circle {"A" {"B" 3, "D" 4}
             "B" {"A" 6, "C" 3}
             "C" {"B" 7, "H" 8}
             "D" {"E" 10, "A" 6}
             "E" {"D" 3, "F" 12}
             "F" {"E" 6, "G" 1}
             "G" {"F" 3, "H" 5}
             "H" {"C" 4, "G" 4}})

;(A*search {:state "A", :cost 0, :robot "H"} a*lmg_branch)
(def branch {"A" {"B" 2, "E" 10}
             "B" {"A" 2, "C" 3, "D" 4, "F" 6}
             "C" {"B" 3, "D" 2}
             "D" {"B" 4, "C" 3, "E" 10}
             "E" {"A" 10, "D" 10}
             "F" {"B" 3, "G" 5}
             "G" {"H" 3, "F" 2}
             "H" {"G", 7}
             })


;target moves to highest cost connected node < 10000
(defn movetarget [g]
  (key (apply max-key val (filter #(< (second %) 10000) (graph g))))
  )

;(A*search {:state "A", :cost 0} "D" a*lmg)
(defn A*search
  [start LMG & {:keys [get-state get-cost get-robot selector debug]
                  :or {get-state :state
                       get-cost :cost
                       get-robot :robot
                       selector :undef
                       debug    false}}]
  (let [member? (fn [lis x] (some (partial = x) lis))
        selector (if-not (= selector :undef)  ;it was a key arg
                   selector                  ; se leave it as is, else set it as default
                   (fn [bag]  (first (sort-by (comp get-cost first) bag))))]


    (loop [queued  `( (~start) )
           visited nil
           g (get-robot (first (selector queued)))
           ]
      (let [goal (fn [x] (= x g))
            goal? (if (fn? goal)
                    #(when (goal %) %)
                    #(when (= % goal) %))]
        (if (empty? queued) nil                      ;; fail if (null queued)
                            (let [next      (selector queued)          ;; select next node
                                  state     (first next)               ;; filter out path
                                  raw-state (get-state state)          ;; filter costs, etc
                                  ]
                              (when debug (println 'selecting next '=> raw-state))
                              (cond
                                (goal? raw-state)                      ;; goal found
                                (reverse next)                         ;; quit with result

                                :else
                                (if (member? visited raw-state)
                                  (recur (remove #(= % next) queued)
                                         visited
                                         g)
                                  (let [
                                        ;g           (movetarget g)     ;;goal moves
                                        queued      (remove #(= % next) queued)
                                        moves       (LMG state g)
                                        new-visited (cons raw-state visited)
                                        new-states  (map #(cons % next)
                                                         (remove #(member? visited (get-state %))
                                                                 moves))
                                        ]
                                    (when debug
                                      (println              ;'exploring state '=> raw-state
                                        'path      next
                                        ;'moves     moves
                                        ))

                                    (recur
                                      (concat queued new-states)
                                      new-visited
                                      g
                                      )
                                    ))
                                ))
                            )))))


;get the cost to move from node x to y in branch
(defn get-cost_branch [x, y]
  (if (contains? (branch x) y)
    (get (branch x) y)
    )
  )

(defn a*lmg_branch [state g]
  (let [n (:state state)
        c (:cost state)
        ]
    (list
      (if (contains? (branch n) "A")
        {:state "A", :cost (+ c (get-cost_branch n "A")), :robot g }
        )
      (if (contains? (branch n) "B")
        {:state "B", :cost (+ c (get-cost_branch n "B")), :robot g }
        )
      (if (contains? (branch n) "C")
        {:state "C", :cost (+ c (get-cost_branch n "C")), :robot g }
        )
      (if (contains? (branch n) "D")
        {:state "D", :cost (+ c (get-cost_branch n "D")), :robot g }
        )
      (if (contains? (branch n) "E")
        {:state "E", :cost (+ c (get-cost_branch n "E")), :robot g }
        )
      (if (contains? (branch n) "F")
        {:state "F", :cost (+ c (get-cost_branch n "F")), :robot g }
        )
      (if (contains? (branch n) "G")
        {:state "G", :cost (+ c (get-cost_branch n "G")), :robot g }
        )
      (if (contains? (branch n) "H")
        {:state "H", :cost (+ c (get-cost_branch n "H")), :robot g }
        )
      (if (contains? (branch n) "I")
        {:state "I", :cost (+ c (get-cost_branch n "I")), :robot g }
        )
      )))


;get the cost to move from node x to y in circle
(defn get-cost_circle [x, y]
  (if (contains? (circle x) y)
    (get (circle x) y)
    )
  )

(defn a*lmg_circle [state g]
  (let [n (:state state)
        c (:cost state)
        ]
    (list
      (if (contains? (circle n) "A")
        {:state "A", :cost (+ c (get-cost_circle n "A")), :robot g }
        )
      (if (contains? (circle n) "B")
        {:state "B", :cost (+ c (get-cost_circle n "B")), :robot g }
        )
      (if (contains? (circle n) "C")
        {:state "C", :cost (+ c (get-cost_circle n "C")), :robot g }
        )
      (if (contains? (circle n) "D")
        {:state "D", :cost (+ c (get-cost_circle n "D")), :robot g }
        )
      (if (contains? (circle n) "E")
        {:state "E", :cost (+ c (get-cost_circle n "E")), :robot g }
        )
      (if (contains? (circle n) "F")
        {:state "F", :cost (+ c (get-cost_circle n "F")), :robot g }
        )
      (if (contains? (circle n) "G")
        {:state "G", :cost (+ c (get-cost_circle n "G")), :robot g }
        )
      (if (contains? (circle n) "H")
        {:state "H", :cost (+ c (get-cost_circle n "H")), :robot g }
        )
      (if (contains? (circle n) "I")
        {:state "I", :cost (+ c (get-cost_circle n "I")), :robot g }
        )
      )))


;get the cost to move from node x to y in graph
(defn get-cost_graph [x, y]
  (if (contains? (graph x) y)
    (get (graph x) y)
    )
  )

(defn a*lmg_graph [state g]
  (let [n (:state state)
        c (:cost state)
        ]
    (list
      (if (contains? (graph n) "AF")
        {:state "AF", :cost (+ c (get-cost_graph n "AF")), :robot g }
        )
      (if (contains? (graph n) "AG")
        {:state "AG", :cost (+ c (get-cost_graph n "AG")), :robot g }
        )
      (if (contains? (graph n) "AH")
        {:state "AH", :cost (+ c (get-cost_graph n "AH")), :robot g }
        )
      (if (contains? (graph n) "AI")
        {:state "AI", :cost (+ c (get-cost_graph n "AI")), :robot g }
        )
      (if (contains? (graph n) "AJ")
        {:state "AJ", :cost (+ c (get-cost_graph n "AJ")), :robot g }
        )
      (if (contains? (graph n) "BF")
        {:state "BF", :cost (+ c (get-cost_graph n "BF")), :robot g }
        )
      (if (contains? (graph n) "BG")
        {:state "BG", :cost (+ c (get-cost_graph n "BG")), :robot g }
        )
      (if (contains? (graph n) "BH")
        {:state "BH", :cost (+ c (get-cost_graph n "BH")), :robot g }
        )
      (if (contains? (graph n) "BI")
        {:state "BI", :cost (+ c (get-cost_graph n "BI")), :robot g }
        )
      (if (contains? (graph n) "BJ")
        {:state "BJ", :cost (+ c (get-cost_graph n "BJ")), :robot g }
        )
      (if (contains? (graph n) "CF")
        {:state "CF", :cost (+ c (get-cost_graph n "CF")), :robot g }
        )
      (if (contains? (graph n) "CG")
        {:state "CG", :cost (+ c (get-cost_graph n "CG")), :robot g }
        )
      (if (contains? (graph n) "CH")
        {:state "CH", :cost (+ c (get-cost_graph n "CH")), :robot g }
        )
      (if (contains? (graph n) "CI")
        {:state "CI", :cost (+ c (get-cost_graph n "CI")), :robot g }
        )
      (if (contains? (graph n) "CJ")
        {:state "CJ", :cost (+ c (get-cost_graph n "CJ")), :robot g }
        )
      (if (contains? (graph n) "DF")
        {:state "DF", :cost (+ c (get-cost_graph n "DF")), :robot g }
        )
      (if (contains? (graph n) "DG")
        {:state "DG", :cost (+ c (get-cost_graph n "DG")), :robot g }
        )
      (if (contains? (graph n) "DH")
        {:state "DH", :cost (+ c (get-cost_graph n "DH")), :robot g }
        )
      (if (contains? (graph n) "DI")
        {:state "DI", :cost (+ c (get-cost_graph n "DI")), :robot g }
        )
      (if (contains? (graph n) "DJ")
        {:state "DJ", :cost (+ c (get-cost_graph n "DJ")), :robot g }
        )
      (if (contains? (graph n) "EF")
        {:state "EF", :cost (+ c (get-cost_graph n "EF")), :robot g }
        )
      (if (contains? (graph n) "EG")
        {:state "EG", :cost (+ c (get-cost_graph n "EG")), :robot g }
        )
      (if (contains? (graph n) "EH")
        {:state "EH", :cost (+ c (get-cost_graph n "EH")), :robot g }
        )
      (if (contains? (graph n) "EI")
        {:state "EI", :cost (+ c (get-cost_graph n "EI")), :robot g }
        )
      (if (contains? (graph n) "EJ")
        {:state "EJ", :cost (+ c (get-cost_graph n "EJ")), :robot g }
        )
      )))


;Unused code designed for D*

; We decided to make maps an atom datatype, so we could call (swap!) to modify costs
;(def branch (atom {"A" {"B" 2, "E" 10}
;                   "B" {"A" 2, "C" 3, "D" 4, "F" 6}
;                   "C" {"B" 3, "D" 2}
;                   "D" {"B" 4, "C" 3, "E" 10}
;                   "E" {"A" 10, "D" 10}
;                   "F" {"B" 3, "G" 5}
;                   "G" {"H" 3, "F" 2}
;                   "H" {"G", 7}
;                   }))

;node x, node y,
;(defn modify-cost [x, y, cval]
;  ; update arc cost with cval
;  ;(get (swap! graph (assoc-in graph [x y] cval)) y)
;  (swap! graph #(assoc-in % [x y] cval))
;  (get-cost_graph x y)
;  ; if
;  )