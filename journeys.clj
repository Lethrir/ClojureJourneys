(defn forward [x y f]
  (case f
    :N [x (inc y) f]
    :S [x (dec y) f]
    :E [(inc x) y f]
    :W [(dec x) y f]
  )
)

(defn clockwise [f]
  (case f
    :N :E
    :E :S
    :S :W
    :W :N
  )
)

(defn anticlockwise [f]
  (case f
    :N :W
    :E :N
    :S :E
    :W :S
  )
)

(defn move [[x y f] d]
  (case d
    :L [x y (anticlockwise f)]
    :R [x y (clockwise f)]
    :F (forward x y f)
  )
)

; actual vs expected
(defn validate [start [ax ay af] [ex ey ef]]
  (if (every? true? [(== ax ex) (== ay ey) (= af ef)])
    (println "moved ok from" start "to" [ex ey ef])
    (println "failed to move from" start "to" [ex ey ef] "ended at" [ax ay af])
  )
)

(defn process [start steps final]
  (loop [s start o steps]
    (if (empty? o)
      (validate start s final)
      (let [nextposition (move s (first o))]
        (recur nextposition (rest o)))
    )
  )
)

(process [1 1 :E] [:R :F :R :F :R :F :R :F] [1 1 :E]) ; ok
(process [1 1 :E] [:R :F :R :F :R :F] [1 1 :E]) ; fails
(process [3 2 :N] [:F :R :R :F :L :L :F :F :R :R :F :L :L] [3 3 :N]) ; ok
(process [0 3 :W] [:L :L :F :F :F :L :F :L :F :L] [2 4 :S]) ; ok
