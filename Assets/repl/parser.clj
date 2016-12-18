(ns repl.parser
  (:use squarepeg.core arcadia.core)
  (:require [clojure.pprint :as pprint]))

(declare list- vector- map- col-)

(def parse (comp pprint/pprint :r))

(defmacro token [k m] `(~'fn ~'[b c] (conj {:token ~k :value ~'(apply str (:ret b))} ~m)))

(defrule w*         {whitespace *})
(defrule w+         {whitespace +})
(defrule -integer   {digit +})
(defrule -po        \()
(defrule -pc        \))
(defrule -vo        \[)
(defrule -vc        \])
(defrule -mo        \{)
(defrule -mc        \})

(def int- (mkret (mkseq w* -integer) (token :int  {:number true})))
(def po-  (mkret (mkseq w* -po)      (token :po   {:open  true})))
(def pc-  (mkret (mkseq w* -pc)      (token :pc   {:close true})))
(def vo-  (mkret (mkseq w* -vo)      (token :vo   {:open  true})))
(def vc-  (mkret (mkseq w* -vc)      (token :vc   {:close true})))
(def mo-  (mkret (mkseq w* -mo)      (token :mo   {:open  true})))
(def mc-  (mkret (mkseq w* -mc)      (token :mc   {:close true})))


(defrule atom-    (mkalt int-))
(defrule form     (mkalt atom- col-))
(defrule list-    [po- {form *} pc-])
(defrule vector-  [vo- {form *} vc-])
(defrule map-     [mo- {form *} mc-])
(defrule col-     (mkalt list- vector- map-))
(defrule code     [{form *} end])


(parse (code " (  {517 [ 1]} 1 )" {} {} {}))


(def rainbows [
  "#ffa500ff"
  "#add8e6ff"
  "#ff00ffff"
  "#008000ff"
  "#00ffffff"
  "#c0c0c0ff"])

(defn highlite [col]
  (let [depth (volatile! 0)]
    (reduce 
      (fn [s t]
        (if (or (:open t) (:close t))
            (do 
              (if (:close t) (vswap! depth dec))
              (let [res (str s "<color=" (nth rainbows @depth) ">" 
                        (:value t) "</color>")] 
                (if (:open t) (vswap! depth inc)) 
                res))
            (str s (:value t))))
      "" col)))

(let [c (cmpt (object-named "visual") UnityEngine.UI.Text)]
  (set! 
    (.text c) 
    (highlite (:r (code "(( ({[ 6 ]} ) 67) \n  (  {517 [ 1]} 1 ))" {} {} {})))))