(ns repl.core
  (:use arcadia.core)
  (:require arcadia.repl)
  (:import
    [UnityEngine Input KeyCode]))

(def repl-env (atom (arcadia.repl/env-map)))

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

(defn on-value-changed [go callback]
 (let [input-field (cmpt go UnityEngine.UI.InputField)]
  (.. input-field onValueChanged
   (AddListener (fn [val] (callback val))))))

(defn clear [input]
  (set! (.text input) @ns-str)
  (reset! prompt (count @ns-str)))

(defn remove-enter-newline! [input]
  (let [caret (.caretPosition input)
        s (.text input)]
    (set! (.text input)
      (str (subs s 0 (dec caret))
           (subs s caret)))))

(defn history! [input n]
  (let [idx (+ @histidx (- n))
        histcount (count @history)]
    (log "history!" idx (subs (.text input) 0 @prompt))
  (if (< -1 idx histcount)
    (do 
      (reset! histidx idx)
      (set! (.text input)
        (str (subs (.text input) 0 @prompt)
             (nth @history (- (dec histcount) idx))))
      (set! (.caretPosition input) (count (.text input))))
    (set! (.caretPosition input) (count (.text input))))))

(defn send-input [input]
  (remove-enter-newline! input)
  (let [form (subs (.text input) @prompt)
        {:keys [result env]} (arcadia.repl/repl-eval-print @repl-env form)]
    (when-not (#{"" (last @history)} form)
      (swap! history conj form))
    (reset! histidx -1)
    (reset! repl-env env)
    (set! (.text input) 
          (str (.text input) "\n" result (ns-name (:*ns* @repl-env)) "=>"))
    (reset! prompt (count (.text input)))
    (set! (.caretPosition input) @prompt)))

(defn check-input [o]
  (let [input (->input-field o)]
    (cond 
      (key-down? :up)   (history! input -1)
      (key-down? :down) (history! input  1)

      (and (key-down? "c")
           (key? "left alt"))
      (clear input)

      (and (key-down? "return")
           (not (key? "left shift")))
      (send-input input))))

'(hook+ (object-named "InputField") :update #'repl.core/check-input)

