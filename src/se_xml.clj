(ns se-xml
  (:require [clojure.zip :as zip]
            [hiccup.util]))

(def xml-header "<?xml version=\"1.0\"?>")
(def nl "\n")
(def indent "  ")
(def space " ")

(defn encap
  [st n]
  (str n st n))

(defn html-quote
  [n]
  (encap n "\""))

(defn write-attribute
  [k v]
  (str k "=" (html-quote (hiccup.util/escape-html v))))

(defn write-attributes
  [attr-map]
  (apply
    str
    (interpose
      space
      (map
        (fn [[k v]] (write-attribute k v))
        attr-map))))

(defn write-content
  [n]
  (when (not (nil? n))
    (hiccup.util/escape-html n)))

(defn wrap-up-tag
  [singleton?]
  (if singleton?
    " />" ">"))

(defn write-tag-start
  [tag-name attributes content]
  (str "<" tag-name
       (when (not (empty? attributes))
         (str " " (write-attributes attributes)
              (wrap-up-tag (nil? content))))))

(defn write-tag-close
  [tag-name content]
  (when content
    (str "</" tag-name ">")))

(defn write-tag
  ([tag attributes]
   (write-tag tag attributes nil))
  ([tag attributes content]
   (str
     (write-tag-start tag attributes content)
     (write-content content)
     (write-tag-close tag content))))

(defn write-xml
  [xml-zipper]
  (reduce
    str
    (loop [seq-of-str []
           loc xml-zipper
           depth 0
           entry? true]
      (let [node (zip/node loc)
            tag (:tag node)
            attrs (:attrs node)
            content (:content node)
            singleton? (nil? (zip/down loc))]
        (println tag entry? singleton?)
        (if (and
              (not entry?)
              (nil? (zip/up loc))
              (nil? (zip/right loc)))
          seq-of-str
          (recur
            (conj
              seq-of-str
              (if entry?
                (if singleton?
                  (write-tag tag attrs content)
                  (write-tag-start tag attrs ""))
                (write-tag-close tag content)))
            (if (not singleton?)
              (zip/down loc)
              (if-let [r (zip/right loc)]
                r
                (zip/up loc)))
            (if singleton?
              (if (nil? (zip/right loc))
                (dec depth) depth) (inc depth))
            (or (zip/right loc) (not singleton?))))))
    ""))

