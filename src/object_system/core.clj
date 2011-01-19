(ns object-system.core)

(defn variable [& initial-value]
  (let [value (atom (or nil (first initial-value)))]
    (fn [& keyword-and-new-value]
      (if (empty? keyword-and-new-value)
	(deref value)
	(let [keyword (first keyword-and-new-value)
	      new-value (second keyword-and-new-value)]
	  (cond (= keyword :=) (reset! value new-value)
		(= keyword :value) (deref value)))))))

(def self)

(def super)

(def None 'None)

(defn exaustive-resolve [symbol]
  (loop [namespaces (all-ns)]
    (let [var (ns-resolve (first namespaces) symbol)]
      (if (and (nil? var) (not (empty? namespaces)))
	(recur (rest namespaces))
	var))))

(defn find-class [classname-symbol]
  (if (= classname-symbol None)
    (find-class 'OBJECT)
    (exaustive-resolve classname-symbol)))

(defn find-property [property-keyword classname-symbol]
  (loop [class (find-class classname-symbol)]
    (let [implementation-map (class :object-implementation)]
      (if (contains? implementation-map property-keyword)
	(implementation-map property-keyword)
	(if (= (class :superclass) None)
	  (throw (RuntimeException. (str "does not understand " "':" (name property-keyword) "'")))
	  (recur (find-class (class :superclass))))))))

(defn implementation=>object [implementation-map]
  (fn self [property-keyword & arguments]
    (if (= property-keyword :object-implementation)
      implementation-map
      (let [property (if (contains? implementation-map property-keyword)
		       (implementation-map property-keyword)
		       (find-property property-keyword ((implementation-map :class) :value))) ]
	(binding [self self
		  super (find-class ((((find-class ((implementation-map :class) :value)) :object-implementation) :superclass) :value))]
	  (apply property arguments))))))

(defn attributes=>variable [implementation-map]
  (let [keys (keys implementation-map)
	values (vals implementation-map)]
    (zipmap keys (map (fn [value]
			(if (fn? value)
			  value
			  (variable value)))
		      values))))

(def OBJECT (implementation=>object {:superclass (variable None)
				     :class (variable 'OBJECT)
				     :instance-variable-names (variable [])
				     :has? (fn [property-keyword]
					     (contains? (self :object-implementation) property-keyword))
				     :understand? (fn [property-keyword]
						    (or (self :has? property-keyword) 
							(try (find-property property-keyword (self :class))
							     true
							     (catch RuntimeException exception false))))							
				     :properties (fn []
						   (keys (self :object-implementation)))
				     :set! (fn [attribute-keyword value]
					     (((self :object-implementation) attribute-keyword) := value))
				     :subclass (fn [argument-map]
						 (implementation=>object
							      (attributes=>variable
							       (merge argument-map
								      {:superclass (self :class)}))))				 
				     :new (fn [initial-value-map]
					    (let [instance-variable-names (self :instance-variable-names) ]
					      (implementation=>object
					       (attributes=>variable
						(merge (zipmap instance-variable-names (repeat nil))
						       (select-keys initial-value-map instance-variable-names)
						       {:class (self :class)})))))}))


