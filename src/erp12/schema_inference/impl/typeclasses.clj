(ns erp12.schema-inference.impl.typeclasses)

(def typeclasses
  {:number #{'int? 'double?}
   :comparable #{'int? 'double? 'char? 'string? 'boolean?}
   :counted #{:vector :map-of :set 'string?}
   :callable #{:=> :map-of :set}})