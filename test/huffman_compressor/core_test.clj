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
    (let [char-< #(< (int %1) (int %2))]
      (is (= {} (composeEncodingTableFromSymbolSeq char-< '())))
      (is (= {\a '()} (composeEncodingTableFromSymbolSeq char-< '(\a))))
      (is (= {\a '()} (composeEncodingTableFromSymbolSeq char-< '(\a \a \a))))
      (is (= {\a '(0) \b '(1)} (composeEncodingTableFromSymbolSeq char-< '(\a \a \b \a))))
      (is (= {\a '(0) \b '(1)} (composeEncodingTableFromSymbolSeq char-< '(\b \a \a))))
      (is (= {\a '(0) \b '(1 0) \c '(1 1)} (composeEncodingTableFromSymbolSeq char-< '(\c \b \a \a))))
      (is (= {\a '(0 0) \b '(0 1) \c '(1 0) \d '(1 1 0) \e '(1 1 1)} (composeEncodingTableFromSymbolSeq char-< '(\a \b \c \d \e)))))))

(deftest testEncode
  (testing "Encode"
    (let [encTable {0 '(false) 1 '(true false) 2 '(true true)}]
      (is (= '() (encode encTable '())))
      (is (= '(false true true true false) (encode encTable '(0 2 1)))))))
