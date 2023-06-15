(ns invoice-item)
(require '[clojure.edn])

(defn- discount-factor [{:invoice-item/keys [discount-rate]
                         :or                {discount-rate 0}}]
  (- 1 (/ discount-rate 100.0)))

(defn subtotal
  [{:invoice-item/keys [precise-quantity precise-price discount-rate]
    :as                item
    :or                {discount-rate 0}}]
  (* precise-price precise-quantity (discount-factor item)))

(defn load-invoice
  "Load the invoice file"
  []
  (clojure.edn/read-string (slurp "invoice.edn")))

(defn valid-category?
  [category rate category-type rate-value]
  (cond (= category category-type) (= rate rate-value)))

(defn check-iva?
  [{:tax/keys [category rate]}]
  (valid-category? category rate :iva 19))

(defn check-ret?
  [{:retention/keys [category rate]}]
  (valid-category? category rate :ret_fuente 1))

(defn check-invoice-item
  [{:taxable/keys [taxes] :retentionable/keys [retentions]}]
  (let [tax (check-iva? (first taxes)) ret (check-ret? (first retentions))]
    (if (and tax ret) false
        (or tax ret))))

(defn validate-invoice
  []
  (->>
   (load-invoice)
   (:invoice/items)
   (filter check-invoice-item)))
