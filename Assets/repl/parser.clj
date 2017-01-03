(ns repl.parser
  (:use squarepeg.core arcadia.core)
  (:require [clojure.pprint :as pprint]))

(declare list- vector- map- col-)

(def parse (comp pprint/pprint :r))
(defn mkany [s] (apply mkalt (map mklit s)))
(defmacro token [k m] `(~'fn ~'[b c] (conj {:token ~k :value ~'(apply str (:ret b))} ~m)))

(defrule w*           {whitespace *})
(defrule w+           {whitespace +})
(defrule -newline     \newline)
(defrule -non-newline [{! -newline} anything])
(defrule -clj-comment [\; {-non-newline *} (or -newline end)])
(defrule -integer     {digit +})
(defrule -po          \()
(defrule -pc          \))
(defrule -vo          \[)
(defrule -vc          \])
(defrule -mo          \{)
(defrule -mc          \})

(def     -nolead      (mkany "'#%:0123456789" )) 
(def     -no-readchar (mkany "^()[]{};~`'\"@ \n\r\t:/\\#0123456789" ))
(defrule -readchar    [{! -no-readchar} anything])
(defrule -symbol      [-readchar {(mkalt -readchar -nolead) *}])
(defrule -ns          [-symbol \/ -symbol])
(defrule -kw          [\: (mkalt -ns -symbol)])
(defrule -qkw         [\: \: -symbol])
(defrule -illegal [{anything +}])


(def int- (mkret (mkseq w* -integer) (token :int  {:number true})))
(def po-  (mkret (mkseq w* -po)      (token :po   {:open  true})))
(def pc-  (mkret (mkseq w* -pc)      (token :pc   {:close true})))
(def vo-  (mkret (mkseq w* -vo)      (token :vo   {:open  true})))
(def vc-  (mkret (mkseq w* -vc)      (token :vc   {:close true})))
(def mo-  (mkret (mkseq w* -mo)      (token :mo   {:open  true})))
(def mc-  (mkret (mkseq w* -mc)      (token :mc   {:close true})))
(def symbol-  (mkret (mkseq w* -symbol)          (token :symbol    {:symbol true})))
(def ns-      (mkret (mkseq w* -ns)              (token :ns    {:ns true})))
(def comment- (mkret (mkseq w* -clj-comment)     (token :comment   {:comment true})))
(def kw-      (mkret (mkseq w* -kw)              (token :kw    {:kw  true})))
(def qkw-     (mkret (mkseq w* -qkw)             (token :qkw   {:qkw true})))
(def illegal- (mkret (mkseq w* -illegal)         (token :illegal   {})))


(defrule atom-    (mkalt int- ns- symbol- qkw- kw- ))
(defrule form     (mkalt atom- col- comment- ))
(defrule list-    [po- {form *} pc-])
(defrule vector-  [vo- {form *} vc-])
(defrule map-     [mo- {form *} mc-])
(defrule col-     (mkalt list- vector- map-))
(defrule code     [{form *} end])

(def rainbows [
  "#ffa500ff"
  "#add8e6ff"
  "#ff00ffff"
  "#008000ff"
  "#00ffffff"
  "#c0c0c0ff"])

(def scheme {
  :comment "#808080ff"
  :symbol  "#00ff00ff"
  :ns      "#00ffffff"
  :kw      "#ab00ffff"
  :qkw     "#ff0babff"
  :int     "#ffff00ff"
  :illegal "#ff0000ff"})

(defn colorize [t]
  (str "<color=" (scheme (:token t) "#ffa500ff") ">" 
    (:value t) 
    "</color>"))

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
            (str s (colorize t))))
      "" col)))


(require 'repl.core)

(repl.core/on-value-changed (object-named "InputField") 
  (fn [s]
    (let [c (cmpt (object-named "visual") UnityEngine.UI.Text)
          res (highlite (:r (code s {} {} {})))]
      (set! (.text c) (if-not (= res "") res s)))))


'(parse (ns- "foo/bar" {} {} {}))