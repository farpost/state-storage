(ns state-storage.topology)

(defprotocol Topology
  "Протокол извлечения и записи состояний в репозиторий истории.
Конкретная реализация определяет конкретную топологию хранения.
Метод add-object-state принимает объект ObjectState, который нужно
добавить в историю increments - Clojure список инкрементов. Возвращает
обновленный список инкрементов с корректно уложенным в него новым состоянием.

Метод read-page читает страницу номер page размером page-size по протоколу
protocol (:ol или :ul - с порядком или без учета порядка) из инкрементом
increments и возвращает Clojure список состояний.
"

  (add-object-state [this object-state increments])
  (read-page [this page page-size protocol increments]))



(defn get-protocol [state]
  "Определяет протокол хранения состояния"
  (if (-> :user-space-revision state) :ol :ul))

(defn state-diff [a b]
  "Вычисляет дельту между двумя состояниями"
  (if (every? (partial instance? java.util.Map) [a b])
    (reduce
     (fn [diff key]
       (let [av (get a key) bv (get b key)]
         (if (= av bv)
           diff
           (assoc diff key (if (and av bv) (state-diff av bv) bv)))))
     (array-map) (concat (keys a) (keys b)))
    b))

(defmulti history-merge
  "Соединяет состояние с дельтой, возвращает новое состояние"
  (fn [x y] (every? map? [x y])))
(defmethod history-merge false [x y] y)
(defmethod history-merge true [x y]
  (->>
   (merge-with history-merge x y)
   (filter (comp not nil? val))
   (into {})))

(defn history-reductions [increments]
  "По списку инкрементов создает список состояний (историю)"
  (->> increments
       (reductions history-merge {})
       rest))

(defn take-last-state [increments]
  "По списку инкрементов получает последнее состояние объекта"
  (reduce history-merge {} increments))

(defn empty-history [history]
  (if (seq history) history []))

(defn filter-page [page size history]
  "Извлекает страницу номер page размером size из истории history"
  (let [skip (-> page dec (* size))]
    (->>
     history
     (drop-last skip)
     (take-last size)
     empty-history)))

(defn display [x]
  "Для отладки операторов -> и ->>"
  (println x) x)

(defn history-to-increments [history]
  "Преобразует список состояний (историю) в список инкрементов"
  (loop [prev {}, history history, increments []]
    (if (empty? history) increments
        (recur
         (first history)
         (rest history)
         (->> history first (state-diff prev) (conj increments))))))


(defmulti conjoin-history
  "Метод записи состояния new-state во множество инкрментов.
Диспатчиниг вызовов на основе значения protocol (ordered или unordered запись."
  (fn [protocol new-state increments] protocol))
(defmethod conjoin-history :ol [protocol new-state increments]
  (->>
   increments
   history-reductions
   (split-with #(->> [% new-state] (map :user-space-revision) (apply <)))
   (interpose [new-state])
   flatten
   history-to-increments))
(defmethod conjoin-history :ul [protocol new-state increments]
  (conj increments
        (->
         increments
         take-last-state
         (state-diff new-state))))

                                        ; Реализация топологии хранения данных в виде списка, в котором:
                                        ; 1. Первый элемент списка - изначальное состояние объекта
                                        ; 2. Все последующие элементы - дельты относительно первого состояния
                                        ;
                                        ; На основе факта наличия у объекта user_space_id топология самостоятельно определяет,
                                        ; по какому протоколу (в какое множество) записывать инкремент:
                                        ; ordered :ol или unordered :ul - влияет на логику изменения топологии истории при записи
                                        ;
                                        ; Чтение же происходит наоборот, по указанному снаружи протоколу :ol и :ul,
                                        ; который внешняя система определяет на основе типа объекта ("bulletin", "auction").
(deftype BackwardIncrementalTopology []
  Topology

  (read-page [this page page-size protocol history]
    (->>
     protocol
     history
     history-reductions
     (filter-page page page-size)))

  (add-object-state [this new-state history]
    (let [protocol (get-protocol new-state)]
      (->>
       protocol
       history
       (conjoin-history protocol new-state)
       (assoc history protocol)))))