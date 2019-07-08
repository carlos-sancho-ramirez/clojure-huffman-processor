(defproject huffman-compressor "0.0.1-SNAPSHOT"
  :description "A simple clojure application that encodes a plain text file into another file using Huffman compression techniques"
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :aot [huffman-compressor.core]
  :main huffman-compressor.core)
