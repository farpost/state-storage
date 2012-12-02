(ns state-storage.coding
  (:require
   [clojure.data.json :as json]))

(defprotocol Coding
  "Протокол преобразования данных внутреннего формата во внешний и обратно.

Внутренний формат это всегда Clojure структуры данных.
Внешний формат зависит от конкретной реализации Coding протокола.

Метод encode преобразует данные во внутреннем формате (структуры данных Clojure)
во внешний формат конкретной реализации.

Метод decode преобразует данные из внешнего формата конкретной реализации протокола
во внутренний формат - в структуры данных Clojure.
"
  (decode [this encoded])
  (encode [this decoded]))



(def no-history {:ol [] :ul []})

                                        ;Реализация протокола преобразования JSON строк в структуры данных Clojure и обратно.
(deftype JSONCoding []
  Coding

  (encode [this coll]
    (json/json-str coll :escape-unicode false))

  (decode [this json-string]
    (if (seq json-string)
      (json/read-json json-string)
      no-history)))
