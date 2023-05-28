;; clojure_test.clj
(defprotocol P
  (greet [x]))

(defrecord Person [name]
  P
  (greet [x] (str "Hello, " name)))

(defn -main []
  (println (greet (->Person "World"))))
