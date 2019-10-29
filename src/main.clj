(ns main)

;  "Map of node => adjacent node => cost. This could
;   be replaced with any cost function of the shape
;   (node, node') => cost."
;(def graph (atom {"AF" {"BF" 2, "AG" 5},
;                  "AG" {"AF" 4, "BG" 7, "AH" 2},
;                  "AH" {"AG" 3, "BH" 1, "AI" 9},
;                  "AI" {"AH" 6, "BI" 3, "AJ" 10},
;                  "AJ" {"AI" 3, "BJ" 7},
;                  "BF" {"AF" 2, "BG" 5, "CF" 12},
;                  "BG" {"BF" 10, "BH" 3, "AG" 5, "CG" 9},
;                  "BH" {"BG" 1, "BI" 7, "AH" 11, "CH"5},
;                  "BI" {"BH" 3, "BJ" 6, "AI" 4, "CI" 9},
;                  "BJ" {"BI" 4, "AJ" 2, "CJ" 8},
;                  "CF" {"CG" 7, "BF" 1, "DF" 8},
;                  "CG" {"CF" 2, "CH" 9, "BG" 6, "DG" 4},
;                  "CH" {"CG" 12, "CI" 13, "BH" 2, "DH" 9},
;                  "CI" {"CH" 8, "CJ" 4, "BI" 10, "DI" 7},
;                  "CJ" {"CI" 2, "BJ" 6, "DJ" 3},
;                  "DF" {"DG" 7, "CF" 11, "EF" 6},
;                  "DG" {"DF" 7, "DH" 2, "CG" 7, "EG" 3},
;                  "DH" {"DG" 4, "DI" 5, "CH" 8, "EH" 9},
;                  "DI" {"DH" 9, "DJ" 5, "CI" 7, "EI" 3},
;                  "DJ" {"DI" 2, "CJ" 4, "EJ" 3},
;                  "EF" {"EG" 1, "DF" 5},
;                  "EG" {"EF" 8, "EH" 3, "DG" 2},
;                  "EH" {"EG" 9, "EI" 4, "DH" 3},
;                  "EI" {"EH" 12, "EJ" 10, "DI" 1},
;                  "EJ" {"EI" 8, "DJ" 9}
;                  }))

(def graph (atom {"A" {"B" 2, "E" 10}
                  "B" {"A" 2, "C" 3, "D" 4}
                  "C" {"B" 3, "D" 2}
                  "D" {"B" 4, "C" 3, "E" 10}
                  "E" {"A" 10, "D" 10}}))

;target moves to highest cost connected node
(defn movetarget [g]
  (key (apply max-key val (@graph g)))
  )

;(A*search {:state "A", :cost 0} (fn [x] (= x "I")) a*lmg)
(defn A*search
  [start g LMG & {:keys [get-state get-cost selector debug]
                     :or {get-state :state
                          get-cost :cost
                          selector :undef
                          debug    false}}]
  (let [member? (fn [lis x] (some (partial = x) lis))
        selector (if-not (= selector :undef)  ;it was a key arg
                   selector                  ; se leave it as is, else set it as default
                   (fn [bag]  (first (sort-by (comp get-cost first) bag))))]


    (loop [queued  `( (~start) )
           visited nil
           g g                                      ;NEW CODE: GOAL PARAM
           ]
      (let [goal (fn [x] (= x g))
            goal? (if (fn? goal)                            ;NEW CODE: anonymous function, goal param to find
                    #(when (goal %) %)
                    #(when (= % goal) %))]
      (if (empty? queued) nil                      ;; fail if (null queued)
                          (let [next      (selector queued)          ;; select next node
                                state     (first next)               ;; filter out path
                                raw-state (get-state state)          ;; filter costs, etc
                                ]
                            (when debug (println 'selecting next '=> raw-state))
                            (cond
                              ;(and (fn? goal) (goal raw-state))     ;; is goal a predicate & goal found
                              ;(reverse next)                        ;; quit with result

                              (goal? raw-state)                      ;; goal found
                              (reverse next)                         ;; quit with result

                              :else
                              (if (member? visited raw-state)
                                (recur (remove #(= % next) queued)
                                       visited
                                       g)
                                (let [queued      (remove #(= % next) queued)
                                      moves       (LMG state)
                                      new-visited (cons raw-state visited)
                                      new-states  (map #(cons % next)
                                                       (remove #(member? visited (get-state %))
                                                               moves))
                                      ]
                                  (when debug
                                    (println 'exploring state '=> raw-state
                                             'path      next
                                             'moves     moves
                                             ))

                                  (recur
                                    (concat queued new-states)
                                    new-visited
                                    (movetarget g)
                                    ;(fn [x] (= x "D"))
                                    )                       ;NEW CODE: NEW GOAL PARAM
                                  ))
                              ))
                          )))))








;get the cost to move from node x to y
(defn get-cost [x, y]
  (if (contains? (@graph x) y)
    (get (@graph x) y)
    )
  )


;node x, node y,
(defn modify-cost [x, y, cval]
  ; update arc cost with cval
  ;(get (swap! graph (assoc-in graph [x y] cval)) y)
  (swap! graph #(assoc-in % [x y] cval))
  (get-cost x y)
  ; if
  )


(defn update-arc-cost [x, y]
  )



(defn movenode [node]                                       ;node = node to be moved
  (let [m (first (graph node))                              ;m = target of node
        ]
    (for [x graph]
      ((for [y x]
         (if (= y node)                                      ;update y as m
           (println node)
           (if (= y m)                                   ;update y as node
             (println m)
             )
           )
         )
        (if (= x node)                                      ;update x as m
          (println node)
          (if (= x m)                                   ;update x as node
            (println m)
            )
          )
        )

      )



    )
  )

(defn movenodetoo [node]                                       ;node = node to be moved
  (let [m (first (graph node))                              ;m = target of node
        ]
    (for [x graph]
      (for [y x]
        (for [i y]
          (if (= i node)                                      ;update y as m
            (println node)
            (if (= i m)                                   ;update y as node
              (println m)
              ;(println i)
              )
            )
          )
        ;(if (= y node)                                      ;update y as m
        ;  (println node)
        ;  (if (= y m)                                   ;update y as node
        ;    (println m)
        ;    (println y)
        ;    )
        ;  )
        )
      )
    )
  )

(defn a*lmg [state]

  (let [n (:state state)
        c (:cost state)
        ]

    (list
               (if (contains? (@graph n) "A")
                 {:state "A", :cost (+ c (get-cost n "A"))}
                 )
               (if (contains? (@graph n) "B")
                 {:state "B", :cost (+ c (get-cost n "B"))}
                 )
               (if (contains? (@graph n) "C")
                 {:state "C", :cost (+ c (get-cost n "C"))}
                 )
               (if (contains? (@graph n) "D")
                 {:state "D", :cost (+ c (get-cost n "D"))}
                 )
               (if (contains? (@graph n) "E")
                 {:state "E", :cost (+ c (get-cost n "E"))}
                 )
               (if (contains? (@graph n) "F")
                 {:state "F", :cost (+ c (get-cost n "F"))}
                 )
               (if (contains? (@graph n) "G")
                 {:state "G", :cost (+ c (get-cost n "G"))}
                 )
               (if (contains? (@graph n) "H")
                 {:state "H", :cost (+ c (get-cost n "H"))}
                 )
               (if (contains? (@graph n) "I")
                 {:state "I", :cost (+ c (get-cost n "I"))}
                 )
               )))

;(defn a*lmg [state]
;
;  (let [n (:state state)
;        c (:cost state)
;        ]
;
;    (list
;      (if (contains? (@graph n) "AF")
;           {:state "AF", :cost (+ c (get-cost n "AF"))}
;           )
;      (if (contains? (@graph n) "AG")
;        {:state "AG", :cost (+ c (get-cost n "AG"))}
;        )
;      (if (contains? (@graph n) "AH")
;        {:state "AH", :cost (+ c (get-cost n "AH"))}
;        )
;      (if (contains? (@graph n) "AI")
;        {:state "AI", :cost (+ c (get-cost n "AI"))}
;        )
;      (if (contains? (@graph n) "AJ")
;        {:state "AJ", :cost (+ c (get-cost n "AJ"))}
;        )
;      (if (contains? (@graph n) "BF")
;        {:state "BF", :cost (+ c (get-cost n "BF"))}
;        )
;      (if (contains? (@graph n) "BG")
;        {:state "BG", :cost (+ c (get-cost n "BG"))}
;        )
;      (if (contains? (@graph n) "BH")
;        {:state "BH", :cost (+ c (get-cost n "BH"))}
;        )
;      (if (contains? (@graph n) "BI")
;        {:state "BI", :cost (+ c (get-cost n "BI"))}
;        )
;      (if (contains? (@graph n) "BJ")
;        {:state "BJ", :cost (+ c (get-cost n "BJ"))}
;        )
;      (if (contains? (@graph n) "CF")
;        {:state "CF", :cost (+ c (get-cost n "CF"))}
;        )
;      (if (contains? (@graph n) "CG")
;        {:state "CG", :cost (+ c (get-cost n "CG"))}
;        )
;      (if (contains? (@graph n) "AH")
;        {:state "CH", :cost (+ c (get-cost n "CH"))}
;        )
;      (if (contains? (@graph n) "CI")
;        {:state "CI", :cost (+ c (get-cost n "CI"))}
;        )
;      (if (contains? (@graph n) "CJ")
;        {:state "CJ", :cost (+ c (get-cost n "CJ"))}
;        )
;      (if (contains? (@graph n) "DF")
;        {:state "DF", :cost (+ c (get-cost n "DF"))}
;        )
;      (if (contains? (@graph n) "DG")
;        {:state "DG", :cost (+ c (get-cost n "DG"))}
;        )
;      (if (contains? (@graph n) "DH")
;        {:state "DH", :cost (+ c (get-cost n "DH"))}
;        )
;      (if (contains? (@graph n) "DI")
;        {:state "DI", :cost (+ c (get-cost n "DI"))}
;        )
;      (if (contains? (@graph n) "DJ")
;        {:state "DJ", :cost (+ c (get-cost n "DJ"))}
;        )
;      (if (contains? (@graph n) "EF")
;        {:state "EF", :cost (+ c (get-cost n "EF"))}
;        )
;      (if (contains? (@graph n) "EG")
;        {:state "EG", :cost (+ c (get-cost n "EG"))}
;        )
;      (if (contains? (@graph n) "EH")
;        {:state "EH", :cost (+ c (get-cost n "EH"))}
;        )
;      (if (contains? (@graph n) "EI")
;        {:state "EI", :cost (+ c (get-cost n "EI"))}
;        )
;      (if (contains? (@graph n) "EJ")
;        {:state "EJ", :cost (+ c (get-cost n "EJ"))}
;        )
;      )))


