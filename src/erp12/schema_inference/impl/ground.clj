(ns erp12.schema-inference.impl.ground)

(def canonical-ground
  "A map from type keywords (e.g., :boolean, :int) to their corresponding
  canonical schema representations (e.g., 'boolean?, 'int?).
  This is used to standardize ground type schemas within the inference system.
  TODO: Expand this map to include more schemas."
  ;; @todo Expand this map to include more schemas.
  {:boolean 'boolean?
   :int     'int?
   :float   'float?
   :double  'double?
   :string  'string?
   :char    'char?
   :keyword 'keyword?
   :symbol  'symbol?})

(defn cls->schema
  "Takes a Java Class object and returns a schema map representing that class.
  Handles common Java types by mapping them to appropriate schema type predicates
  (e.g., java.lang.String to 'string?, various numeric types like byte, short,
  int, long to 'int?, and double, float to 'double? or 'float?).
  It also handles specific Clojure types like Keyword and Symbol.
  For unrecognized classes, it returns a schema map with the class object
  itself as the :type.
  TODO: Consider adding 'inst? for general Java objects."
  [cls]
  {:type (case (.getName cls)
           "boolean" 'boolean?
           "byte" 'int?
           "short" 'int?
           "int" 'int?
           "long" 'int?
           "double" 'double?
           "float" 'float?
           "char" 'char?
           "[B" 'bytes?
           "java.lang.String" 'string?
           "clojure.lang.Keyword" 'keyword?
           "clojure.lang.Symbol" 'symbol?
           "java.util.UUID" 'uuid?
           ;; @todo 'inst?
           cls)})
