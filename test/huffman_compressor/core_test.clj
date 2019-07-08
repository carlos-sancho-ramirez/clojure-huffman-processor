(ns huffman-compressor.core-test
  (:require [clojure.test :refer :all]
            [huffman-compressor.core :refer :all]))

(deftest testEncode
  (testing "Encode"
    (let [encTable {0 '(false) 1 '(true false) 2 '(true true)}]
      (is (= '() (encode encTable '())))
      (is (= '(false true true true false) (encode encTable '(0 2 1)))))))
