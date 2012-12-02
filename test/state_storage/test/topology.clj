(ns state-storage.test.topology
  (:require [state-storage.topology])
  (:use
   [clojure.test]
   [state-storage.topology])
  (:import
   state_storage.topology.BackwardIncrementalTopology))


(deftest history-to-increments-test
  (are [history increments]
       (= increments (history-to-increments history))

       [{:a 1}]
       [{:a 1}]

       [] []

       [{:a 1} {:b 2}]
       [{:a 1} {:a nil :b 2}]

       [{:a {:x "y"}} {:b "cake"}]
       [{:a {:x "y"}} {:a nil :b "cake"}]

       [{:a {:x "y"}} {:a {:b "cake"}}]
       [{:a {:x "y"}} {:a {:x nil :b "cake"}}]

       [{:a 1} {:a 1 :b 3} {:b 4 :a 1} {:c 5}]
       [{:a 1} {:b 3} {:b 4} {:c 5 :a nil :b nil}]

       ))

(deftest history-merge-test
  (are [x y merge] (= merge (history-merge x y))
       1 2
       2

       1 {:a 4}
       {:a 4}

       {:a 4} 1
       1

       {:a 3} {:b 4}
       {:a 3 :b 4}

       {:a 3} {:a 4 :b 6}
       {:a 4 :b 6}

       {:a 3 :c 7} {:a 4 :b 6}
       {:a 4 :b 6 :c 7}

       {:a 1 :b 7} {:b nil}
       {:a 1}

       {:a 2 :b {:x 3 :y 4}} {:b {:x nil}}
       {:a 2 :b {:y 4}}

       ))


(deftest history-reductions-last-test
  (are [increments reductions]
       (and
        (= reductions
           (history-reductions increments))
        (= (last reductions)
           (take-last-state increments)))

       ;;atom adding example
       [{:a 1} {:b 2}]
       [{:a 1} {:a 1 :b 2}]

       ;;map adding example
       [{:a 1} {:b {:x 42}}]
       [{:a 1} {:a 1 :b {:x 42}}]

       ;;atom merging example
       [{:a 1 :b 2} {:a 42}]
       [{:a 1 :b 2} {:a 42 :b 2}]

       ;;map merging example
       [{:a 1 :b {:x 1}} {:a 1 :b {:x 42}}]
       [{:a 1 :b {:x 1}} {:a 1 :b {:x 42}}]

       ;;atom saving example
       [{:a 1 :b 2} {:c 3}]
       [{:a 1 :b 2} {:a 1 :b 2 :c 3}]

       ;;map saving example
       [{:a {:x 42} :b 2} {:c 3}]
       [{:a {:x 42} :b 2} {:a {:x 42} :b 2 :c 3}]

       ;;long map merging example
       [{:a {:x 31} :b {:a 2 :b 3}} {:b {:a 42}}]
       [{:a {:x 31} :b {:a 2 :b 3}} {:a {:x 31} :b {:a 42 :b 3}}]

       ;;atom nil merging
       [{:a "what" :b "the heck"} {:b nil}]
       [{:a "what" :b "the heck"} {:a "what"}]

       ;;map nil merging
       [{:a "what" :b {:x "the" :y "heck"}} {:b nil}]
       [{:a "what" :b {:x "the" :y "heck"}} {:a "what"}]

       ;;in-map atom nil merging
       [{:a "what" :b {:x "the" :y "heck"}} {:b {:y nil}}]
       [{:a "what" :b {:x "the" :y "heck"}} {:a "what" :b {:x "the"}}]

       ;;in-map map nil merging
       [{:a "what" :b {:x "the" :y {:a "cake" :b "cake"}}} {:b {:y nil}}]
       [{:a "what" :b {:x "the" :y {:a "cake" :b "cake"}}} {:a "what" :b {:x "the"}}]

       ))


(deftest create-page-filter-test
  (testing "create-page-filter creates a function fetching specified page from lazy history list"
    (are [page size history history-page]
         (= history-page
            (filter-page page size history))

         ;;first page
         1 2 [1 2 3 4 5 6] [5 6]

         ;;middle page
         1 3 [1 2 3 4 5 6] [4 5 6]

         ;;exact last page
         2 3 [1 2 3 4 5 6] [1 2 3]

         ;;not enough elements for the last page
         4 2 [1 2 3 4 5 6 7] [1]
         2 5 [1 2 3 4 5 6 7] [1 2]

         ;;not enough elements for the first page
         1 3 [1 2] [1 2]
         1 4 [1] [1]

         ;;no elements for the last page
         2 3 [1 2 3] []
         2 4 [1 2 3] []

         ;;*invalid page or page size values are interpreted as minimal semantically correct integers

         ;;zero page-size
         2 0 [1 2 3 4 5] []

         ;;any page size less than 0 interpreted as 0
         1 -2 [1 2 3 4 5] []

         ;;any page number less than 1 interpreted as 1
         0 2 [1 2 3 4 5] [4 5]
         -2 2 [1 2 3 4 5] [4 5]

         ;;no way to get elements from an empty list
         1 1 [] []
         )))


(deftest state-diff-test
  (testing ""
    (are [old new diff]
         (= diff (state-diff old new))

         ;;removed atom
         {:a 3} {}
         {:a nil}

                                        ;added atom
         {} {:a 3}
         {:a 3}

         ;;saved atom
         {:a 1 :b 2} {:a 1 :b 3}
         {:b 3}

                                        ;updated atom
         {:a 4} {:a 2}
         {:a 2}

         ;;removed map
         {:a {:b 4}} {}
         {:a nil}

         ;;added map
         {} {:a {:x 4}}
         {:a {:x 4}}

         ;;saved map
         {:a {:x 1} :b 2} {:a {:x 1} :b 3}
         {:b 3}

         ;;updated map: removed value
         {:a {:x 4}} {:a {}}
         {:a {:x nil}}

         ;;updated map: added value
         {:a {}} {:a {:y 6}}
         {:a {:y 6}}

         ;;updated map: updated value
         {:a {:x 4}} {:a {:x 5}}
         {:a {:x 5}}

         ;;updated map: saved value
         {:a {:x 4}} {:a {:x 4 :y 6}}
         {:a {:y 6}}

         )))


(deftest append-state-test
  (let [topology (BackwardIncrementalTopology.)]
    (testing ""
      (are [coll new-state new-coll]
           (= new-coll
              (add-object-state topology new-state coll))

           {:ul [{:a 1} {:b 2}]}
           {:a 1 :b 2 :c 3}
           {:ul [{:a 1} {:b 2} {:c 3}]}

           {:ul [{:a {:x 1 :y 2}} {:b 2}]}
           {:a {:x 3 :y 2} :b 2 :c 4}
           {:ul [{:a {:x 1 :y 2}} {:b 2} {:a {:x 3} :c 4}]}

           {:ul [{:a {:x 1}}]}
           {:a {:y 2}}
           {:ul [{:a {:x 1}} {:a {:x nil :y 2}}]}

           ))))



(deftest BackwardIncrementalTopology-read-page-test

  (let [topology (BackwardIncrementalTopology.)]
    (testing "read-page test"
      (are [page size history page-items]
           (= page-items
              (read-page topology page size :ul history))

           1 2 {:ul [{:revision 1}]}
           [{:revision 1}]

           1 2 {:ul [{:revision 1} {:revision 2}]}
           [{:revision 1} {:revision 2}]

           1 2 {:ul [{:revision 1 :state {:published 0}} {:revision 2 :state {:published 1}}]}
           [{:revision 1 :state {:published 0}} {:revision 2 :state {:published 1}}]

           1 2 {:ul [{:revision 1 :state "old"} {:revision 2 :state "new"}]}
           [{:revision 1 :state "old"} {:revision 2 :state "new"}]

           ;;Not enough elements for a page
           2 3 {:ul [{:revision 1 :initial "bla"} {:revision 2} {:revision 3} {:revision 4}]}
           [{:revision 1 :initial "bla"}]

           3 3 {:ul [{:revision 1 :initial "bla"} {:revision 2} {:revision 3} {:revision 4}]}
           []

           ;;Some weird scenarios, illegal or invalid history or page parameters
           -1 2 {:ul [{:revision 1 :initial "bla"} {:revision 2} {:revision 3} {:revision 4}]}
           [{:revision 3 :initial "bla"} {:revision 4 :initial "bla"}]

           0 2 {:ul [{:revision 1 :initial "bla"} {:revision 2} {:revision 3} {:revision 4}]}
           [{:revision 3 :initial "bla"} {:revision 4 :initial "bla"}]

           2 0 {:ul [{:revision 1 :initial "bla"} {:revision 2} {:revision 3} {:revision 4}]}
           []

           2 -1 {:ul [{:revision 1 :initial "bla"} {:revision 2} {:revision 3} {:revision 4}]}
           []

           2 1 {:ul [] :ol []}
           []

           ))))