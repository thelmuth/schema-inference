(ns erp12.schema-inference.impl.typeclasses)

(def typeclasses
  {:number     #{'int? 'double?}
   :comparable #{'int? 'double? 'char? 'string? 'boolean?}
   :countable  #{:vector :map-of :set 'string?}
   :callable   #{:=> :map-of :set}
   :indexable  #{:vector 'string?}
   }
  
  ;;;;;;; Maybe add in future?
  ;; Seqable (Anything that makes sense to iterate over with map, filter, reduce, etc.)
  ;; Associative (Maps and vectors; stuff that supports both assoc and get)
  )
