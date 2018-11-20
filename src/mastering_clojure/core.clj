(ns mastering-clojure.core
  (:require [cats.builtin :as cb]
            [cats.context :as cct]
            [cats.core :as cc]
            [cats.monad.exception :as cme]
            [cats.monad.identity :as cmi]
            [cats.monad.maybe :as cmm]
            [clojure.core.async :as a]
            [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [clojure.pprint :as pprint]
            [clojure.repl :refer [doc]]))

;; for every cat.monad.* there is a `context` variable defined in it


(cc/mappend "12" "34" "56")
;; => "123456"

(cc/mappend [1 2] [3 4] [5 6])
;; => [1 2 3 4 5 6]

@(cc/mappend (cmm/just "123"))
;; => "123"

(cc/mappend (cmm/just "123") (cmm/just "456"))
;; => #<Just "123456">

(cmm/nothing)
;; => #<Nothing>

(cc/mappend (cmm/just "123") (cmm/nothing) (cmm/just "456"))
;; => #<Just "123456">

(cc/fmap inc [0 1 3])
;; => [1 2 4]

(time (dotimes [_ 100000] (map inc [1 2 3 4 5 6 7 8 9 10 11 12])))
"Elapsed time: 15.990684 msecs"
;; => nil

(time (dotimes [_ 100000] (cc/fmap inc [1 2 3 4 5 6 7 8 9 10 11 12])))
"Elapsed time: 711.402284 msecs"
;; => nil

cme/context
;; => #<Exception>

cmi/context
;; => #<Identity>

cmm/context
;; => #<Maybe>


;; (doc cc/pure) =>
(cct/with-context cmm/context
  (cc/pure 1))
;; => #<Just 1>
(cc/pure cmi/context 1)
;; => #<Identity 1>

(cc/pure cme/context 1)
;; => #<Success 1>

;; cc/<$> is the same as cc/fmap
(cc/fmap inc (cmm/just 1))
;; => #<Just 2>

(cc/<$> inc (cmm/just 1))
;; => #<Just 2>

(cc/<$> inc [0 1 2 3])
;; => [1 2 3 4]

(cc/<$> (partial + 2) (cc/<$> inc [0 1 2 3]))
;; => [3 4 5 6]

;; cc/fapply
(cc/fapply (cmm/just inc) (cmm/just 1))
;; => #<Just 2>

(cmm/just inc)
;; => #<Just #function[clojure.core/inc]>

@(cmm/nothing)
;; => nil

(cc/pure cmm/context 1)
;; => #<Just 1>

;; alet
(cc/alet [a (cmm/just [1 2 3]) b (cmm/just [4 5 6])] (cc/mappend a b))
;; => #<Just [1 2 3 4 5 6]>

;; <*> is the same as cc/fapply


;; morphism

;; must define identity function
;; must obey communative law
;; must obey composition law

;; applicative functors must also obey homomorphism and interchange laws


;; monad trasnforms the context of a contained value into another context
;; monads are data structures used to represent computations that are defined in steps.
;; Each step is represented by an operation on a monad, and several of these steps
;; can be chained together.
;; Essentially, a monad is a composable abstraction of a step of any computation
;; A distinct feature of monads is that they allow us to model impure side effects,
;; which may be performed in the various steps of a given computation, using pure functions


;; Formally, a monad is an algebraic structure that implements two functions: `bind` and `return`

;; bind : (Monad A a, [A -> Monad B] f) -> Monad B
;; return : (A a) -> Monad A

;; monads are the only way to model side effects in pure functional programming languages.
;; they allow us to write imperative style code, which is all about mutation of state,
;; in a pure functional programming language.

;; cc/>>= alias of cc/bind

(cc/bind (cmm/just 1) inc)
;; => 2
(cc/bind (cmm/nothing) inc)
;; => #<Nothing>

;; this is because the standard `inc` function does not return a monad

(cc/bind (cmm/just 1) #(-> % inc cc/return))
;; => #<Just 2>
(cc/bind (cmm/nothing) #(-> % inc cc/return))
;; => #<Nothing>


;; lift-m can be used to `lift` a function that returns a type A to return a monad containing a type A

((cc/lift-m inc) (cmm/just 1))
;; => #<Just 2>
((cc/lift-m inc) (cmm/nothing))
;; => #<Nothing>

(cc/>>= (cc/>>= (cmm/just 1) #(-> % inc cmm/just)) #(-> % dec cmm/just))
;; => #<Just 1>

(cc/bind (cc/bind (cmm/just 1) #(-> % inc cmm/just)) #(-> % dec cmi/identity))
;; => #<Identity 1>


;; cc/mlet

(defn process-with-maybe [x]
  (cc/mlet [a (if (even? x)
                (cmm/just x)
                (cmm/nothing))
            b (do
                (println (str "Incrementing " a))
                (-> a inc cmm/just))]
           b))


(cme/success 1)
;; => #<Success 1>
(cme/failure {:error (Exception.)})
;; => #<Failure #error {:cause "" :data {:error #error {:cause nil :via [{:type java.lang.Exception :message nil :at [...] :trace [...]}]}}}>
(cme/try-on 1)
;; => #<Success 1>
@(cme/try-on 1)
;; => 1
(cme/try-on (/ 1 0))
;; => #<Failure #error {:cause "Divide by zero" :via ...}>
(cme/try-on (-> 1 (/ 0) inc))
;; => #<Failure #error {:cause "Divide by zero" :via ...}>
(cc/bind (cme/try-on (/ 1 1)) #(-> % inc cc/return))
;; => #<Success 2>
(cc/bind (cme/try-on (/ 1 0)) #(-> % inc cc/return))
;; => #<Failure #error {:cause "Divide by zero" :via ...}>
(cme/try-or-else (/ 1 0) 0)
;; => #<Success 0>
(cme/try-or-recover (/ 1 0) (fn [e] (if (instance? ArithmeticException e) 0 :error)))
;; => 0

;; logic relations in clojure.core.logic have the name end with o
;; multiple logic relations, end with e


;; l/== : unification
(l/run* [x] (l/== x 1))
;; => (1)

(l/run* [x] (l/== 0 1))
;; => ()

;; /!= : disequality
(l/run* [x] (l/!= 1 1))
;; => ()

;; _0 : unbound logical value
;; also _1, _2 ... in the context of a `run*` form
(l/run* [x] (l/== 1 1))
;; => (_0)

;; l/conso
(l/run* [x] (l/conso 1 [2 x] [1 2 3]))
;; => (3)

(l/run* [x y] (l/== x y) (l/== y 1))
;; => ([1 1])

(l/run* [x] (l/fresh [y] (l/== x y) (l/== y 1)))
;; => (1)

(l/run 5 [x]
  (fd/in x (fd/interval 0 100))
  (fd/> x 10))
;; => (11 12 13 14 15)

(l/run 1 [v a b x]
  (l/== v [a b])
  (fd/in a b x (fd/domain 0 1 2))
  (fd/< a b)
  (l/firsto v x)) ;; means (first v) is x
;; => ([[0 1] 0 1 0])


;; e : multiple relations
(l/run* [x]
  (l/conde
   ((l/== 'A x) l/succeed)
   ((l/== 'B x) l/succeed)
   ((l/== 'C x) l/fail)
   ))
;; => (A B)

(l/run* [x]
  (l/conde
   ((l/== 'A x) l/succeed)
   ((l/== 'B x) l/succeed)))
;; => (A B)

(l/run* [x]
  (l/matche [x]
            (['A] l/succeed)
            (['B] l/succeed)))
;; => (A B)

(l/run* [x]
  (l/matche [x]
            (['A])
            ([_] l/fail)))
;; => (A)

;; . (dot) used to delimit the head and tail of the sequence
(l/run* [x]
  (l/fresh [y]
    (l/== y [1 2 3])
    (l/matche [y]
              ([[1 . x]]))))
;; => ((2 3))

;; defne macro
(l/defne membero [x xs]
  ([_ [x . ys]])
  ([_ [y . ys]]
   (membero x ys)))
(l/run* [x]
  (membero x (range 5))
  (membero x (range 3 10)))
;; => (3 4)

(l/run 2 [x y]
  (l/membero x (range 1 10))
  (l/membero y (range 1 10))
  (l/project [x y]
             (l/== (+ x y) 5)))
;; => ([1 4] [2 3])


;; ==== n-queens problem ====
;; backtracking, exhaustive search

(l/defne safeo [q qs]
  ([_ ()])
  ([[x1 y1] [[x2 y2] . t]]
   (l/!= x1 x2)
   (l/!= y1 y2)
   (l/project [x1 x2 y1 y2]
              (l/!= (- x2 x1) (- y2 y1))
              (l/!= (- x1 x2) (- y2 y1)))
   (safeo [x1 y1] t)))

(l/defne nqueenso [n qs]
  ([_ ()])
  ([n [[x y] . t]]
   (nqueenso n t)
   (l/membero x (range n))
   (safeo [x y] t)))

(defn solve-nqueens [n]
  (l/run* [qs]
    (l/== qs (map vector (repeatedly l/lvar) (range n)))
    (nqueenso n qs)))

(defn print-nqueens-solution [solution n]
  (let [solution-set (set solution)
        positions    (for [x (range n)
                           y (range n)]
                       (if (contains? solution-set [x y]) 1 0))]
    (binding [pprint/*print-right-margin* (* n n)]
      (pprint/pprint
       (partition n positions)))))

(defn print-all-nqueens-solutions [solutions n]
  (dorun (for [i    (-> solutions count range)
               :let [s (nth solutions i)]]
           (do
             (println (str "\nSolution " (inc i) ":"))
             (print-nqueens-solution s n)))))

(defn solve-and-print-nqueens [n]
  (-> (solve-nqueens n)
      (print-all-nqueens-solutions n)))


;; ==== sudoku problem =====

(l/defne init-sudoku-board [vars puzzle]
  ([[] []])
  ([[_ . vs] [0 . ps]] (init-sudoku-board vs ps))
  ([[n . vs] [n . ps]] (init-sudoku-board vs ps)))

(defn solve-sudoku [puzzle]
  (let [board      (repeatedly 81 l/lvar)
        rows       (into [] (map vec (partition 9 board)))
        cols       (apply map vector rows)
        val-range  (range 1 10)
        in-range   (fn [x]
                     (fd/in x (apply fd/domain val-range)))
        get-square (fn [x y]
                     (for [x (range x (+ x 3))
                           y (range y (+ y 3))]
                       (get-in rows [x y])))
        squares    (for [x (range 0 9 3)
                         y (range 0 9 3)]
                     (get-square x y))]
    (l/run* [q]
      (l/== q board)
      (l/everyg in-range board)
      (init-sudoku-board board puzzle)
      (l/everyg fd/distinct rows)
      (l/everyg fd/distinct cols)
      (l/everyg fd/distinct squares))))

(defn solve-and-print-sudoku [puzzle]
  (let [solutions (solve-sudoku puzzle)]
    (dorun (for [i    (-> solutions count range)
                 :let [s (nth solutions i)]]
             (do
               (println (str "\nSolution " (inc i) ":"))
               (pprint/pprint
                (partition 9 s)))))))

(def puzzle-1
  [0 9 0 0 0 0 0 5 0
   6 0 0 0 5 0 0 0 2
   1 0 0 8 0 4 0 0 6
   0 7 0 0 8 0 0 3 0
   8 0 3 0 0 0 2 0 9
   0 5 0 0 3 0 0 7 0
   7 0 0 3 0 2 0 0 5
   3 0 0 0 6 0 0 0 7
   0 1 0 0 0 0 0 4 0])

(solve-and-print-sudoku puzzle-1)

(def puzzle-2
  [0 8 0 0 0 9 7 4 3
   0 5 0 0 0 8 0 1 0
   0 1 0 0 0 0 0 0 0
   8 0 0 0 0 5 0 0 0
   0 0 0 8 0 4 0 0 0
   0 0 0 3 0 0 0 0 6
   0 0 0 0 0 0 0 7 0
   0 3 0 5 0 0 0 8 0
   9 7 2 4 0 0 0 5 0])

(solve-and-print-sudoku puzzle-2)


;; ==== core.async ====
;; asynchronous tasks could be parked(suspended) at any time

;; processes and channels in core.async
;; they are sililar to go-routines in Go programming language

;; theory in the paper:
;; Communicating Sequential Processes (CSPs)
;; CSPs 的基本意思(bottom lines)是
;; any systems that prcesses some input and provides an output
;; can be comprised of smaller subsystems, and each subsystem
;; can be defined in terms of processes and queues.
;; A queque simply buffers data, and a process can read from and
;; write to several queues.

;; in core.async, queue are dubbed as `channels`,
;; and can be created using `chan` function

;; channels are buffered or unbuffered,
;; buffered channel can be created by specifying a number
;; to the `chan` function
;; channels can be closed using the `close!` function

;; a buffer object can be created using the `buffer`,
;; `dropping-buffer`, or `sliding-buffer` functions,
;; all requires a number indicating the size of the buffer

;; both
(a/chan (a/buffer n))
(a/chan n)
;; can be used to create a channel that can buffer n values

;; `dropping-buffer` drops newly added values once it is full,
;; whereas `sliding-buffer` drops the oldest values


;; threads are created using `thread` and `go` form


;; ---- read from or write to channels ----

;; read:
;; returns value, or `nil` if the channel is closed

;; write:
;; returns `true`, or `false` if channel is closed

;; read from buffered channel
;; returns value if have data, even channel is closed
;; returns `nil` if exhausted

;; rules for read and write operations
;; 1. the first argument to any operation is a channel
;; 2. a write operation must be passed a value in addition

;; write == put   `put!`
;; read == take   `take!`
;; `put!` and `take!` returns immediately

;; blocking read  `<!!`   blocking if no data
;; blocked write  `>!!`   blocked if no more space in channel
;; they should be used within a `thread` form

;; parking read   `<!`
;; parking write  `>!`
;; they should be used within a `go` form
;; they could park(suspend) the state of the task, and
;; release the underlying thread of execution if
;; an operation cannot be completed immediately


;; ---- creating processes ----
;; `thread` macro, similar to `future` form in the sense that
;; the body of a `thread` form is executed on a new thread
;; and a call to a `thread` form returns immediately
;; it returns a channel (clojure.core.async.impl.channels.ManyToManyChannel)

(doc a/thread)
(a/thread)

;; `go` form can create an asynchronous process that
;; can be parked and scheduled for execution.
;; it returns a channel
;; the underlying thread will not be blocked, rather,
;; they will be reused by another async process
;; and the body of `go` will be parked(when read/write cannot be done immediately)
;; NOTE there can be NO thread-specific operations such as
;; `Thread/sleep` in a `go` from
;; within `go` form, we should use
;; parking read `<!` and parking write `>!` forms


;; `go-loop` macro is a async version of `loop` form
;; the body of `go-loop` will be internally executed within a `go` form

(defn wait-and-print [c]
  (a/go-loop [n 1]
    (let [v (a/<! c)]
      (when v
        (println (str "Got a message: " v))
        (println (str "Got " n " messages so far!"))
        (recur (inc n)))))
  (println "Waiting..."))

(def c (a/chan))
(wait-and-print c)

(a/>!! c :foo)
(a/>!! c :bar)
(a/close! c)
(a/>!! c :foo)

(def c4 (a/chan 4))
(a/onto-chan c4 (range 4))

(repeatedly 3 #(-> c4 a/<!!))

;; waiting for completion of  ONE of channel operations
;; `alts!` and `alts!!` functions
;; the former is intended for use within a `go` form, it parks
;; the latter must be used in a `thread` form, it blocks
;; both of them should be passed a vector of channels, and
;; return a vector of two elements:
;; [ value_for_a_take_operation_OR_boolean_value_for_a_put_operation
;;   the_channel_on_which_the_operation_completed ]
;; use `:default` to specify a default value, which returns when
;; NONE of the operations supplied to `alts!` or `alts!!` have completed

;; `alt!` and `alt!!` macros

(doc a/alt!)

;; each clause form:
;; channel-op[s] result-expr

(defn process-channels [c0 c1 c2 c3 c4 c5]
  (a/go
    (a/alt!
      ;; read from c0, c1, c2, c3
      c0 :r                    ;; returns :r if c0 complete
      c1 ([v] (str v))         ;; process value returned from c1
      [c2 c3] ([v c] (str v))  ;; c is the channel (c2 or c3) which complete first
      ;; and v is the value read from c

      ;; write to c4, c5
      [[c4 :v1] [c5 :v2]] :w))) ;; nested form means write operation

;; `timeout` function
;; a blocking timeout will block the current thread:
(time (a/<!! (a/timeout 1000)))

;; ==== 5 dining philosophers problem ====

(defn init-philosophers [np food forks init-fn]
  (let [p-range (range np)
        p-names (map #(str "Philosopher " (inc %))
                     p-range)
        p-forks (map #(vector (nth forks %)
                              (nth forks (-> % inc (mod np))))
                     p-range)
        p-food  (cycle [food])]
    (map init-fn p-names p-forks p-food)))

(defn make-philosopher [name forks food]
  {:name  name
   :forks forks
   :food  food})

(defn make-forks [nf]
  (let [forks (repeatedly nf #(a/chan 1))]
    (doseq [f forks]
      (a/>!! f :fork))
    forks))

(defn philosopher-process [p-chan max-eat-ms max-think-ms]
  (a/go-loop []
    (let [p             (a/<! p-chan)
          food          (:food p)
          fork-1        ((:forks p) 0)
          fork-2        ((:forks p) 1)
          ;; take forks
          fork-1-result (a/alt!
                          (a/timeout 100) :timeout
                          fork-1 :fork-1)
          fork-2-result (a/alt!
                          (a/timeout 100) :timeout
                          fork-2 :fork-2)]
      (if (and (= fork-1-result :fork-1)
               (= fork-2-result :fork-2))
        (do
          ;; eat
          (a/<! (a/timeout (rand-int max-eat-ms)))
          ;; put down both acquired forks
          (a/>! fork-1 :fork)
          (a/>! fork-2 :fork)
          ;; think
          (a/<! (a/timeout (rand-int max-think-ms)))
          (a/>! p-chan (assoc p :food (dec food))))
        (do
          ;; put down any acquired forks
          (if (= fork-1-result :fork-1)
            (a/>! fork-1 :fork))
          (if (= fork-2-result :fork-2)
            (a/>! fork-2 :fork))
          (a/>! p-chan p)))
      ;; recur
      (when (pos? (dec food)) (recur)))))

(defn start-philosophers [p-chan philosophers]
  (a/onto-chan p-chan philosophers false)
  (dorun (repeatedly (count philosophers)
                     #(philosopher-process p-chan 100 100))))

(defn print-philosophers [p-chan n]
  (let [philosophers (repeatedly n #(a/<!! p-chan))]
    (doseq [p philosophers]
      (println (str (:name p) ":\t food=" (:food p)))
      (a/>!! p-chan p))))

(def all-forks (make-forks 5))

(def all-philosophers
  (init-philosophers 5 1000 all-forks make-philosopher))

(def philosopher-chan (a/chan 5))

(start-philosophers philosopher-chan all-philosophers)
(print-philosophers philosopher-chan 5)
(print-philosophers philosopher-chan 5)

;; actors from Pulsar library
;; they are first popularized in Erlang programming language

;; It is a widely accepted notion in the Clojure community that processes
;; and channels are a much better methodology to model concurrently
;; running processes compared to actors.
;; That aside, actors can be used to provide more resilient error handling and recovery


;; The objectives of core.async are:

;; * To provide facilities for independent threads of activity, communicating via queue-like channels
;; * To support both real threads and shared use of thread pools (in any combination),
;;   as well as ClojureScript on JS engines
;; * To build upon the work done on CSP and its derivatives
