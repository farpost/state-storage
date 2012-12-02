(ns state-storage.test.coding
  (:use
   [state-storage.coding]
   [clojure.test])
  (:import
   [state_storage.coding JSONCoding]))

(deftest JSONCoding-test

  (let [coding (JSONCoding.)]

    (testing "JSONCoding must code JSON string to Clojure collections and back"
      (are [json-str coll]
           (= coll
              (decode coding json-str)
              (decode coding (encode coding coll)))

           "[{\"x\" 1 \"y\" {\"a\" 3}}]"
           [{:x 1 :y {:a 3}}]

           "[{\"x\" 1 \"y\" {\"a\" 3 \"b\" 4}} {\"c\" 5 \"d\" {\"p\" 42}}]"
           [{:x 1 :y {:a 3 :b 4}} {:c 5 :d {:p 42}}]
           ))

    (testing "JSONCoding decode weird scenarions"
      (are [json result-seq]
           (= result-seq (decode coding json))

           "[{}]" [{}]

           "[[] []]" [[] []]

           nil {:ul [] :ol []}
           ))))
