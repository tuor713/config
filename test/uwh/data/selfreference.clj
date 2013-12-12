{:server {:host "127.0.0.0"
          :port 8080}
 :appserver {:server (:uwh.config/local-ref [:server])}
 :topology {:server1 (:uwh.config/local-ref [:appserver])}}