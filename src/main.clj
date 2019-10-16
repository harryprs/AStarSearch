(ns main)

(defn A*search
  [start goal LMG & {:keys [get-state get-cost selector debug]
                     :or {get-state :state
                          get-cost :cost
                          selector :undef
                          debug    false}}]
  (let [goal? (if (fn? goal)
                #(when (goal %) %)
                #(when (= % goal) %))
        member? (fn [lis x] (some (partial = x) lis))
        selector (if-not (= selector :undef)  ;it was a key arg
                   selector                  ; se leave it as is, else set it as default
                   (fn [bag]  (first (sort-by (comp get-cost first) bag))))]

    (loop [queued  `( (~start) )
           visited nil
           ]
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
                                (recur (remove #(= % next) queued) visited)
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
                                    new-visited)
                                  ))
                              ))
                          ))))

(def graph
  {"A" #{"B"}
   "B" #{"A", "C", "D"}
   "C" #{"B", "G"}
   "D" #{"B", "E"}
   "E" #{"D", "F"}
   "F" #{"E", "I"}
   "G" #{"C", "H"}
   "H" #{"G", "I"}
   "I" #{"H", "F"}
   })


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
              (println i)
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
      (if (contains? (graph n) "A")
        {:state "A", :cost (+ c 2)}
        )
      (if (contains? (graph n) "B")
        {:state "B", :cost (+ c 7)}
        )
      (if (contains? (graph n) "C")
        {:state "C", :cost (+ c 3)}
        )
      (if (contains? (graph n) "D")
        {:state "D", :cost (+ c 4)}
        )
      (if (contains? (graph n) "E")
        {:state "E", :cost (+ c 5)}
        )
      (if (contains? (graph n) "F")
        {:state "F", :cost (+ c 12)}
        )
      (if (contains? (graph n) "G")
        {:state "G", :cost (+ c 6)}
        )
      (if (contains? (graph n) "H")
        {:state "H", :cost (+ c 9)}
        )
      (if (contains? (graph n) "I")
        {:state "I", :cost (+ c 3)}
        )
      )))

