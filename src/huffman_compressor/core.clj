(ns huffman-compressor.core
  (:gen-class))

(defn countChars [fileName]
  (with-open [rdr (clojure.java.io/reader fileName)]
    (loop [result {}]
      (let [x (.read rdr)]
        (if (< x 0) result
          (let [v (get result x 0)]
            (recur (assoc result x (inc v)))))))))

(defn seqFromReader
  "Creates a lazy sequence from an open reader"
  [rdr]
  (let [x (.read rdr)]
    (if (< x 0) nil (cons x (seqFromReader rdr)))))

(defn writeBits
  [bitList stream]
  (loop [list bitList
         wordValue 0
         bitCount 0]
    (if (= bitCount 8)
      (do (.write stream wordValue) (recur list 0 0))
      (let [bit (first list)]
        (if (not= bit nil)
          (recur (rest list) (if (= bit 1) (bit-or wordValue (bit-shift-left 1 bitCount)) wordValue) (inc bitCount))
          (if (not= bitCount 0) (.write stream wordValue) nil))))))

(defn getMin [map]
  "Returns a new map including only the key-value pair from the given map whose value is the minimum."
  (reduce
    #(if (< (first (vals %1)) (map %2)) %1 {%2 (map %2)})
    {(first (keys map)) (first (vals map))}
    (rest (keys map))))

(defn getMax [map]
  "Returns a new map including only the key-value pair from the given map whose value is the maximum."
  (reduce
    #(if (> (first (vals %1)) (map %2)) %1 {%2 (map %2)})
    {(first (keys map)) (first (vals map))}
    (rest (keys map))))

(defn composeTable
  "Creates a new map whose keys are the symbols (the same we had in the given map),
  and its values are list of 0 or 1 values, representing its Huffman codification."
  [frecMap]
  (loop [inMap (reduce #(assoc %1 (conj '() %2) (frecMap %2)) {} (keys frecMap))
               outMap (reduce #(assoc %1 %2 '()) {} (keys frecMap))]
    (if (< (count inMap) 2) outMap
      (let [min1 (getMin inMap)]
        (let [min2 (getMin (dissoc inMap (first (keys min1))))]
          (let [min1Key (first (keys min1))
                min2Key (first (keys min2))]
            (let [symbols (reduce #(conj %1 %2) min1Key min2Key) newFrec (+ (first (vals min1)) (first (vals min2)))]
              (recur
                (assoc (dissoc inMap min1Key min2Key) symbols newFrec)
                (reduce #(assoc %1 %2 (conj (get %1 %2) 1)) (reduce #(assoc %1 %2 (conj (get %1 %2) 0)) outMap min1Key) min2Key)))))))))

(defn getKeyForMinVal
  "Given a map, returns the key within the map whose value is the minimum within the map."
  [map]
  (if (< (count map) 2) (first (keys map))
    (get (reduce #(if (< (get %2 1) (get %1 1)) %2 %1) map) 0)))

(defn composeLengthTable
  "Creates a new map whose key is the encoded symbol length, and values is a list of decoded symbols"
  [frecMap]
  (loop [inMap (reduce #(assoc %1 (conj '() (get %2 0)) (get %2 1)) {} frecMap)
               outMap (reduce #(assoc %1 (get %2 0) 0) {} frecMap)]
    (if (< (count inMap) 2) outMap
      (let [min1Key (getKeyForMinVal inMap)
            min2Key (getKeyForMinVal (dissoc inMap min1Key))
            newSymbols (reduce #(conj %1 %2) min1Key min2Key)]
        (recur
          (assoc (dissoc inMap min1Key min2Key) newSymbols (+ (inMap min1Key) (inMap min2Key)))
          (reduce #(assoc %1 %2 (inc (get %1 %2))) outMap newSymbols))))))

(defn invertSeq
  [seq]
  (reduce #(conj %1 %2) '() seq))

(defn incSeqValue
  [initSeq]
  (loop [seq (invertSeq initSeq)
         carry true
         outSeq '()]
    (let [v (first seq)]
      (if (not= v nil)
        (recur (rest seq) (and carry (= v 1)) (conj outSeq (if carry (if (= v 1) 0 1) v)))
        outSeq))))

(defn getNextSymbolSeq
  "Returns a sequence of 0s and 1s for the following symbol"
  [lastSeqValue bits]
  (if (= (first lastSeqValue) nil)
    (repeat bits 0)
    (reduce #(conj %1 %2) (repeat (- bits (count lastSeqValue)) 0) (invertSeq (incSeqValue lastSeqValue)))))

(defn composeEncodingTable
  "Creates a map keepting the keys from the given map, but providing a list of 0 or 1 in the values"
  [lengthPairsSeq]
  (loop [pairSeq lengthPairsSeq
         lastSeq '()
         result {}]
    (let [pair (first pairSeq)
          symbolBits (get pair 1)]
      (if (= pair nil)
        result
        (let [newSeq (getNextSymbolSeq lastSeq symbolBits)]
          (recur (rest pairSeq) newSeq (assoc result (get pair 0) newSeq)))))))

(defn encode
  "Creates a new lazy sequence of 0s and 1s, resulting on the encoding of the given seq and using the given table"
  [table symbols]
  (let [symbol (first symbols)]
    (if (= symbol nil) nil
      (lazy-seq
        (reduce #(cons %2 %1)
          (encode table (rest symbols))
          (invertSeq (table symbol)))))))

(defn requiredBits
  ([v] (requiredBits v 0))
  ([v n] (if (= v 0) n (requiredBits (bit-shift-right v 1) (inc n)))))

(defn power
  ([base exp] (power base exp 1))
  ([base exp acc]
   (if (= exp 0) acc (power base (dec exp) (* base acc)))))
                   
(defn encodeNumberThreshold
  [max]
  (- (power 2 (requiredBits max)) max 1))

(defn numberToSeq
 ([value bits] (numberToSeq value bits 0 '()))
 ([value bits bitIndex result]
  (if (>= bitIndex bits)
    result
    (if (= (bit-and value (bit-shift-left 1 bitIndex)) 0)
      (numberToSeq value bits (inc bitIndex) (conj result 0))
      (numberToSeq value bits (inc bitIndex) (conj result 1))))))

(defn encodeNumber
 "Creates a sequence of 0s and 1s for the enconded number"
 ([value min max] (encodeNumber (- value min) (- max min)))
 ([value max]
  (let [threshold (encodeNumberThreshold max)
        maxBits (requiredBits max)]
   (if (< value threshold)
    (numberToSeq value (dec maxBits))
    (numberToSeq (+ (* threshold 2) (- value threshold)) maxBits)))))
        
(defn groupByLength
  "Transforms the encoded map to another map where keys are the length of the encoded symbol and value is a sorted seq of symbols"
  [encTable]
  (reduce
    #(assoc %1 (get %2 0) (sort (get %2 1)))
    {}
    (reduce #(let [length (count (get %2 1))] (assoc %1 length (conj (get %1 length '()) (get %2 0)))) {} encTable)))

(defn encodeLengths
 ([lengthTable] (encodeLengths (sort-by #(get %1 0) (reduce #(assoc %1 (get %2 0) (count (get %2 1))) {} lengthTable)) 1 0))
 ([lengthSeq remaining currentLength]
  (let [nextLength (first lengthSeq)]
    (if (= nextLength nil)
      nil
      (if (< currentLength (get nextLength 0))
        (lazy-seq
          (reduce #(cons %2 %1)
            (encodeLengths lengthSeq (* remaining 2) (inc currentLength))
            (invertSeq (encodeNumber 0 remaining))))
        (lazy-seq
          (reduce #(cons %2 %1)
            (encodeLengths (rest lengthSeq) (* (- remaining (get nextLength 1)) 2) (inc currentLength))
            (invertSeq (encodeNumber (get nextLength 1) remaining)))))))))

(defn encodeSymbolsLoop
  [symbols]
  (let [symbol (first symbols)]
    (if (= symbol nil)
      nil
      (lazy-cat (encodeNumber symbol 255) (encodeSymbolsLoop (rest symbols))))))

(defn encodeSymbols
  [lengthTable]
  (encodeSymbolsLoop (reduce #(concat %1 (get %2 1)) '() (sort-by #(get %1 0) lengthTable))))

(defn encodeTable
  "Return a sequence of 0s and 1s for the encoded Huffman table"
  [encTable]
  (let [grouped (groupByLength encTable)]
    (lazy-cat (encodeLengths grouped) (encodeSymbols grouped))))
  
(defn -main []
  (let [fileName "resources/lorem_ipsum.txt"
        outFileName "encoded.bin"
        frecMap (countChars fileName)
        lengthPairsSeq (sort-by #(get %1 1) (sort-by #(get %1 0) (composeLengthTable frecMap)))
        encTable (composeEncodingTable lengthPairsSeq)]
    (println frecMap)
    (println (reduce #(+ %1 %2) 0 (vals frecMap)) "characters read")
    (println (composeTable frecMap))
    (println (getKeyForMinVal frecMap))
    (println lengthPairsSeq)
    (println "InvertSeq:" (invertSeq '(1 0 1 0 0)))
    (println "incSeqValue:" (incSeqValue '(0 0)))
    (println "incSeqValue:" (incSeqValue '(0 1)))
    (println "incSeqValue:" (incSeqValue '(1 0)))
    (println "getNextSymbolSeq:" (getNextSymbolSeq '() 2))
    (println "getNextSymbolSeq:" (getNextSymbolSeq '(0 0) 3))
    (println "getNextSymbolSeq:" (getNextSymbolSeq '(0 1 0) 3))
    (println "getNextSymbolSeq:" (getNextSymbolSeq '(0 1 1) 3))
    (println "Enconding Huffman table:" encTable)
    (with-open [rdr (clojure.java.io/reader fileName)]
      (println "Raw file (first 20 characters):" (take 20 (seqFromReader rdr))))
    (with-open [rdr (clojure.java.io/reader fileName)]
      (println "Encoded file (first 100 bits):" (take 100 (encode encTable (seqFromReader rdr)))))
    (println "Encode number threshold (0..2):" (encodeNumberThreshold 2))
    (println "Encode number threshold (0..3):" (encodeNumberThreshold 3))
    (println "Encode number threshold (0..4):" (encodeNumberThreshold 4))
    (println "Encode number threshold (0..5):" (encodeNumberThreshold 5))
    (println "Encoded number 0:" (encodeNumber 0 0 1))
    (println "Encoded number 1:" (encodeNumber 0 0 2))
    (println "Encoded number 2:" (encodeNumber 0 0 4))
    (println "Encoded number 3:" (encodeNumber 2 0 8))
    (println "Encoded number 4:" (encodeNumber 8 0 12))
    (println "Group by length:" (groupByLength encTable))
    (println "Result of encodeLengths:" (encodeLengths (groupByLength encTable)))
    (println "Result of encodeTable (first 200 bits):" (take 200 (encodeTable encTable)))
    (with-open [rdr (clojure.java.io/reader fileName)
                wtr (clojure.java.io/output-stream outFileName)]
      (writeBits (lazy-cat (encodeTable encTable) (encode encTable (seqFromReader rdr))) wtr))
    (println "Encoded into" outFileName)))
