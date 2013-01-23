(ns monmon.core
  (:gen-class))

(use '[clojure.string :only (split join)])

;; Here be dragons, got it from SO
(defmacro dbg[x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

(def enzyme-map {"lac" #"ccc" "gac" #"cac" "banff" #"aaa"})

(def genome "ggcccttttccaaatctcggggcctcatctcgatcgcgcgatacgcatacccccgcgggctttataaacgggcctatagcgcggccctttaaatcgcgctatcgcgatagcgctatagcgctatatttcgg")

(defn split-by-enzyme
	[enzyme genome]
	(split genome (enzyme-map enzyme) ))

(defn generate-compliment
	[nucleotide]
	(let 
		[nuc-map {"a" "t" "t"  "a" "c" "g" "g" "c"}]
		(cond (string? nucleotide)
			(nuc-map nucleotide)
			:else
			(nuc-map (str nucleotide)))))

(defn generate-compliment-sequence
	[genome]
	(join (map generate-compliment (seq genome))))

(defn -main
  [& args]
  	(cond (empty? args) 
  		(println (join "\n" 
  			(map generate-compliment-sequence (split-by-enzyme "lac" genome))))
  		:else 
  		(println (join "\n" 
  			(map generate-compliment-sequence (split-by-enzyme (first args) genome))))))