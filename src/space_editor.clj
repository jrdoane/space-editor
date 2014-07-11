(ns space-editor
  (:require
    [clojure.xml :as xml]
    [clojure.zip :as zip]
    [clojure.data.xml :as dx]
    [clojure.data.zip.xml :as zx]
    [hiccup.core :as hic]
    [hiccup.page]
    ))

(def base-faction-tag :Factions)

(def faction-tag :Factions)
(def players-tag :Players)
(def relations-tag :Relations)
(def requests-tag :Requests)
(def dictionary-tag :dictionary)
(def relation-item-tag :MyObjectBuilder_FactionRelation)
(def faction-request-item-tag :MyObjectBuilder_FactionRequests)
(def faction-request-list-tag :FactionRequests)
(def long-tag :long)
(def faction-id-tag :FactionId)
(def relation-faction1-tag :FactionId1)
(def relation-faction2-tag :FactionId2)
(def relation-type-tag :Relation)

(def faction-list [base-faction-tag faction-tag])
(def faction-player-dictionary-list
  [base-faction-tag players-tag dictionary-tag])
(def faction-relation-list
  [base-faction-tag relations-tag])
(def faction-request-list
  [base-faction-tag requests-tag])

(defn node-content [loc]
  (:content (zip/node loc)))

(defn node-content1 [loc]
  (first (node-content loc)))

(defn node-attrs [loc]
  (:attrs (zip/node loc)))

(defn node-tag [loc]
  (:tag (zip/node loc)))

(defn to-top
  [xml-zipper]
  (loop [z xml-zipper]
    (let [zu (zip/up z)]
      (if (nil? zu)
        z
        (recur zu)))))

(defn navigate-to
  [xml-zipper xml-path]
  (apply
    (partial
      zx/xml1->
      xml-zipper)
    xml-path))

(defn return-to
  [zipper tag]
  (loop [z zipper]
    (when (nil? z)
      (throw (Exception. "Hit the top!")))
    (if (= tag (node-tag z))
      z (recur (zip/up z)))))

(defn prune-factions
  "Removes empty factions and returns a vector of two items. The first item is
  a vector of all remaining factions, the second is the updated XML zipper
  sitting at the top of the XML file."
  [xml-zipper]
  (loop [existing-factions #{}
         f (zip/down (navigate-to (to-top xml-zipper) faction-list))
         removed-count 0]
    (if (nil? f)
      f
      (let [faction-name (node-content1 (zx/xml1-> f :Name))
            player-count (count (node-content (zx/xml1-> f :Members)))
            faction-id (node-content1 (zx/xml1-> f faction-id-tag))
            n (zip/right f)]
        (if (nil? n)
          [removed-count existing-factions (to-top f)]
          (if (= player-count 0)
            (do
              (recur
                existing-factions
                (zip/right
                  (let [dtz (zip/remove f)]
                    (if (= :Factions (:tag (zip/node dtz)))
                      (zip/down dtz)
                      (return-to dtz :MyObjectBuilder_Faction))))
                (if (= player-count 0) (inc removed-count) removed-count)))
            (recur
              (conj existing-factions faction-id)
              n removed-count)))))))

(defn ensure-faction-players
  [xml-zipper valid-faction-ids]
  (loop [z (zip/down (navigate-to
                       (to-top xml-zipper)
                       faction-player-dictionary-list))
         drop-counter 0]
    (if (nil? z)
      z
      (let [player-id (node-content1 (zx/xml1-> z :Key))
            faction-id (node-content1 (zx/xml1-> z :Value))
            invalid (not (contains? valid-faction-ids faction-id))
            n (if invalid
                (return-to (zip/remove z) :item)
                z)
            r (zip/right n)]
        (if (nil? r)
          [drop-counter (to-top n)]
          (recur r (if invalid (inc drop-counter) drop-counter)))))))

(defn ensure-faction-relations
  [xml-zipper valid-faction-ids]
  (loop [z (zip/down (navigate-to
                       (to-top xml-zipper)
                       faction-relation-list))
         drop-counter 0]
    (if (nil? z)
      z
      (let [faction-id1 (node-content1 (zx/xml1-> z relation-faction1-tag))
            faction-id2 (node-content1 (zx/xml1-> z relation-faction2-tag))
            invalid (not (or (contains? valid-faction-ids faction-id1)
                             (contains? valid-faction-ids faction-id2)))
            n (if invalid
                (return-to (zip/remove z) relation-item-tag)
                z)
            r (zip/right n)]
        (if (nil? r)
          [drop-counter (to-top n)]
          (recur r (if invalid (inc drop-counter) drop-counter)))))))

(defn ensure-faction-requestees
  [xml-zipper valid-factions]
  (loop [z (zip/down (zx/xml1-> xml-zipper faction-request-list-tag))
         drop-counter 0]
    (if (nil? z)
      z
      (let [is-first? (nil? (zip/left z))
            is-last? (nil? (zip/right z))
            faction-id (node-content1 z)
            invalid (not (contains? valid-factions faction-id))
            n (if invalid
                (let [nl (zip/remove z)]
                  (if (and is-first? (not is-last?))
                    (zip/down nl)
                    z))
                z)
            r (zip/right n)]
        (if is-last?
          [drop-counter n]
          (recur r (if invalid (inc drop-counter) drop-counter)))))))

(defn ensure-faction-requests
  [xml-zipper valid-factions]
  (loop [z (zip/down (navigate-to
                       (to-top xml-zipper)
                       faction-request-list))
         drop-counters [0 0]]
    (if (nil? z)
      z
      (let [is-last? (nil? (zip/right z))
            is-first? (nil? (zip/left z))
            faction-id (node-content1 (zx/xml1-> z faction-id-tag))
            has-reqs? (not (empty? (node-content 
                                   (zx/xml1-> z faction-request-list-tag))))
            invalid (not (contains? valid-factions faction-id))
            rec-proc (when
                       (and has-reqs? (not invalid))
                       (ensure-faction-requestees z valid-factions))
            n (if invalid
                (let [removed (zip/remove z)]
                  (if (and is-first? (not is-last?))
                    (zip/down removed)
                    (return-to
                      removed
                      faction-request-item-tag)))
                (if has-reqs?
                  (let [r (ensure-faction-requestees z valid-factions)]
                    (return-to (second r) faction-request-item-tag))
                  z))
            r (zip/right n)]
        (if is-last?
          (do
            [drop-counters (to-top n)])
          (recur r [(if invalid (inc (first drop-counters))
                      (first drop-counters))
                    (if (not (nil? rec-proc))
                      (+ (second drop-counters) (first rec-proc))
                      (second drop-counters))])))))
  )

(defn session-name
  ([xml-zipper]
   (node-content1 (zx/xml1-> xml-zipper :SessionName)))
  ([xml-zipper new-name]
   nil))

(defn string->xml
  [s]
  (dx/parse-str s))

(defn clean-factions
  [zipper]
  (let [[factions-removed valid-factions new-zipper] (prune-factions zipper)
        [dropped-pf new-zipper] (ensure-faction-players
                                  new-zipper valid-factions)
        [dropped-rels new-zipper] (ensure-faction-relations
                                    new-zipper valid-factions)
        [dropped-frm new-zipper] (ensure-faction-requests
                                   new-zipper valid-factions)]
    [factions-removed dropped-pf dropped-rels dropped-frm new-zipper]))

(defn xml-struct->hiccup
  [xml-struct]
  (if (string? xml-struct)
    xml-struct
    (if (map? xml-struct)
      [(:tag xml-struct) (:attrs xml-struct)
       (xml-struct->hiccup (:content xml-struct))]
      (map (fn [i]
             (xml-struct->hiccup i))
           xml-struct))))

(comment
  (def test-xml
    (string->xml
      (slurp "example-world/Sandbox.sbc")))
  (def zipper (zip/xml-zip test-xml))
  (navigate-to zipper faction-list)
  (map :tag (:content test-xml))
  (:tag (zip/node (zip/down (navigate-to zipper faction-list))))
  (set
    (map :tag
         (:content
           (zip/node
             (zx/xml1-> zipper :Factions :Requests)))))
  (session-name zipper)

  (take 4 (clean-factions zipper))

  (keys (zip/root (last (clean-factions zipper))))

  (spit "test.xml"
        (hic/html
          {:mode :xml}
          (hiccup.page/xml-declaration "UTF-8")
          (xml-struct->hiccup (zip/root (last (clean-factions zipper))))))
  (dx/emit-str test-xml)

  (spit "test.xml" (dx/indent-str (zip/root (last (clean-factions zipper))))))


