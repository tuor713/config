(:uwh.config/merge (:uwh.config/reference "simple.clj")
                   (:uwh.config/reference "nested.clj")
                   {:string "overridden"
                    :nested {:string "overridden2"}})