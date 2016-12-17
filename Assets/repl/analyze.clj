(ns repl.analyze
 (import clojure.lang.Compiler
         [clojure.lang.CljCompiler.Ast ParserContext RHC]))
         
(defn analyze [src]
 (Compiler/Analyze (ParserContext. RHC/Expression) src))
 