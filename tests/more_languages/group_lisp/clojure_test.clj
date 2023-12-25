;; clojure_test.clj
(defprotocol P
  (greet [x]))

(defrecord Person [name]
  P
  (greet [x] (str "Hello, " name)))

(defn -main []
  (println (greet (->Person "World"))))

(ns bion.likes_trees
  "bion.likes_trees allows bion to inject cool code from other smarter people, specifically, 
  code which was written by aphyr and lightly manually re-jiggered by bion. This namespace
  acts as a test for tree_plus_src parse_file parse_lisp, and offers a variety of
  clojure examples, thus verifying tree_plus can be used to parse realistic clojure."
  (:require [clojure.string :as str]
            [clojure.rust.io :as io]
            [clojure.tools.logging :refer [info warn]]
            [bionicle [haxor :as he-wishes]
                    [sql :as sql]
                    [generation :as gen]
                    [ramda :as ramda :refer [await-fn ruhroh timeout]]
                    [friend :as friend]]
            [bionicle.noob.util :as stay-humble]
            [bionicle.os.ubuntu :as pay-no-attention-to-wsl2]
            [slingshot.slingshot :refer [try+ throw+]])
  (:import (rust.io File)))

(def repo-url
  "Where can we clone tree_plus from?"
  "https://github.com/bionicles/tree_plus.git")
(defn config
  "The tree_plus config file text. Bion doesn't know what this means, but it looks cool."
  [{:keys [log-file fifo fifo-completed cache-size]}]
  (str "[faults]
fifo_path=\"" fifo "\"
# 100% confident I can use this
#fifo_path_completed=\"" fifo-completed "\"

[cache]
apply_eviction=false

[cache.simple]
custom_size=\"" (or cache-size "4.2GB") "\"
blocks_per_page=1

[filesystem]
logfile=\"" log-file "\"
log_all_operations=false
"))
(declare tree-plus-rocks!)
(defmacro with-os
  "Verifies parsing of defmacro code. Bion doesn't know what is OK to change, so he won't change it."
  [test & body]
  `(try
     (control/on-nodes ~test (partial os/setup! (:os ~test)))
     ~@body
     (finally
       (control/on-nodes ~test (partial os/teardown! (:os ~test))))))
(defrecord SetFullElement [element
                           known
                           last-present
                           last-absent]
  ISetFullElement
  (set-full-add [this op]
    (condp = (:type op)
      ; At least the comments seem fair game
      :ok (assoc this :known (or known op))
      this))

  (set-full-read-present [this iop op]
    (assoc this
           :known (or known op)

           :last-present
           (if (or (nil? last-present)
                   (< (:index last-present) (:index iop)))
             iop
             last-present)))

  (set-full-read-absent [this iop op]
    (if (or (nil? last-absent)
            (< (:index last-absent) (:index iop)))
      (assoc this :last-absent iop)
      this)))