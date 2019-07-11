(ns huffman-compressor.core-test
  (:require [clojure.test :refer :all]
            [huffman-compressor.core :refer :all]))

(deftest testSymbolMin
  (testing "symbol-min"
    (let [char-< #(< (int %1) (int %2))]
      (is (= \c (symbol-min char-< \c)))
      (is (= \D (symbol-min char-< \c \D)))
      (is (= \a (symbol-min char-< \b \c \a \d))))))

(deftest testComposeEncodingTableFromSymbolSeq
  (testing "ComposeEncodingTableFromSymbolSeq"
    (is (= {} (composeEncodingTableFromSymbolSeq '())))
    (is (= {\a '()} (composeEncodingTableFromSymbolSeq '(\a))))
    (is (= {\a '()} (composeEncodingTableFromSymbolSeq '(\a \a \a))))
    (is (= {\a '(0) \b '(1)} (composeEncodingTableFromSymbolSeq '(\a \a \b \a))))
    (is (= {\a '(0) \b '(1)} (composeEncodingTableFromSymbolSeq '(\b \a \a))))
    (is (= {\a '(0) \b '(1 0) \c '(1 1)} (composeEncodingTableFromSymbolSeq '(\c \b \a \a))))))

(deftest testEncode
  (testing "Encode"
    (let [encTable {0 '(false) 1 '(true false) 2 '(true true)}]
      (is (= '() (encode encTable '())))
      (is (= '(false true true true false) (encode encTable '(0 2 1)))))))
