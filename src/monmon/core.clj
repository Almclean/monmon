(ns monmon.core
 (:use [clojure.string :only (split join)])
 (:gen-class))

(require '[clojure.core.reducers :as r])

;; Here be dragons, got it from SO
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(defn is-palindrome
 [genome]
    (= genome (join (reverse genome))))

(def enzyme-map {"lac" #"ccc" "gac" #"cac" "banff" #"aaa"})

(def genome "ggcccttttccaaatctcggggcctcatctcgatcgcgcgatacgcatacccccgcgggctttataaacgggcctatagcgcggccctttaaatcgcgctatcgcgatagcgctatagcgctatatttcgg")

(defn split-by-enzyme
    [enzyme genome]
    (split genome (enzyme-map enzyme)))

(defn generate-compliment
    [nucleotide]
    (let 
        [nuc-map {"a" "t", "t" "a", "c" "g", "g" "c", "u" "t"}]
        (cond (string? nucleotide)
            (nuc-map nucleotide)
            :else
            (nuc-map (str nucleotide)))))

(defn generate-compliment-sequence
    [genome]
    (join (into [] (r/map generate-compliment (seq genome)))))

(defn -main
  [& args]
    (cond (empty? args) 
        (println (join "\n" 
            (r/map generate-compliment-sequence (split-by-enzyme "lac" genome))))
        :else 
        (println (join "\n" 
            (r/map generate-compliment-sequence (split-by-enzyme (first args) genome))))))
