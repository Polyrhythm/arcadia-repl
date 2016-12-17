(ns repl.core
  (:use arcadia.core)
  (:require arcadia.repl)
  (:import
    [UnityEngine Input KeyCode]))

(def prompt  (atom 6))
(def histidx (atom 0))
(def ns-str  (atom "user=>"))
(def history (atom []))

(defn get-keycode [s]
  (try (or (eval (symbol (str "KeyCode/" s))) false) (catch Exception e nil)))

(defn ^:private kcode* [k]
  (cond 
    (keyword? k) (apply str (rest (str k)))
    (symbol? k) (get-keycode k)
    (string? k) k))

(defn key-down? [k]
  (Input/GetKeyDown (kcode* k)))

(defn key? [k]
  (Input/GetKey (kcode* k)))

(defn key-up? [k]
  (Input/GetKeyUp (kcode* k)))

(defn ->input-field [o] (cmpt o UnityEngine.UI.InputField))


(def repl-env (atom (arcadia.repl/env-map)))

(defn clear [input]
  (set! (.text input) @ns-str)
  (reset! prompt (count @ns-str)))

(defn send-input [input]
  (let [form (subs (.text input) @prompt)
        {:keys [result env]} (arcadia.repl/repl-eval-print @repl-env form)]
    (reset! repl-env env)
    (set! (.text input) 
          (str (.text input) result "\n" (ns-name (:*ns* @repl-env)) "=>"))
    (reset! prompt (count (.text input)))
    (set! (.caretPosition input) @prompt)))

(defn check-input [o]
  (let [input (->input-field o)]
    (cond 
      (and (key-down? "c")
           (key? "left alt"))
      (clear input)

      (and (key-down? "return")
           (not (key? "left shift")))
      (send-input input))))

'(hook+ (object-named "InputField") :update #'repl.core/check-input)

