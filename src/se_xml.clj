(ns se-xml
  (:require [clojure.zip :as zip]
            [hiccup.util]))

(def xml-header "<?xml version=\"1.0\"?>")
(def nl "\n")
(def indent "  ")

(defn encap
  [st n]
  (str n st n))

(defn html-quote
  [n]
  (encap n "\""))

(defn write-attribute
  [k v]
  (str (name k) "=" (html-quote (hiccup.util/escape-html v))))

(defn write-attributes
  [attr-map]
  (apply
    str
    (interpose
      " "
      (map
        (fn [[k v]] (write-attribute k v))
        attr-map))))

(defn write-content
  [n]
  (when (not (nil? n))
    (when (not (empty? n))
      (hiccup.util/escape-html (apply str n)))))

(defn wrap-up-tag
  [singleton?]
  (str
    (if singleton?
      " />" ">")
    "\n"))

(defn write-tag-start
  [tag-name attributes content]
  (str "<" (name tag-name)
       (when (not (empty? attributes))
         (str " " (write-attributes attributes)))
       (wrap-up-tag (nil? content))))

(defn write-tag-close
  [tag-name content]
  (when content
    (str "</" (name tag-name) (wrap-up-tag false))))

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
  (apply
    str
    (loop [seq-of-str []
           loc xml-zipper
           depth 0
           entry? true]
      (let [can-right? (not (nil? (zip/right loc)))
            can-up? (not (nil? (zip/up loc)))]
        (if (and
              (not entry?)
              (not can-right?)
              (not can-up?))
          seq-of-str
          (let [singleton? (nil? (zip/down loc)) 
                can-down? (and entry? (not singleton?))
                direction (if can-down?
                            :down
                            (if can-right?
                              :right
                              :up))
                new-entry? (not (= direction :up))
                node (zip/node loc)
                tag (:tag node)
                attrs (:attrs node)
                content (:content node)]
            (recur
              (conj
                seq-of-str
                (if entry?
                  (if singleton?
                    (if (nil? tag)
                      (hiccup.util/escape-html (str content))   
                      (write-tag tag attrs content))
                    (write-tag-start tag attrs true))
                  (write-tag-close tag content)))
              (if can-down?
                (zip/down loc)
                (if can-right?
                  (zip/right loc)
                  (zip/up loc)))
              (if can-down?
                (inc depth)
                (if can-right?
                  depth
                  (dec depth)))
              new-entry?)))))))

