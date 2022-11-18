(ns xyz.plonk.twitter
  (:require
    [clj-http.client :as http]
    [clojure.string :as str]))

(defn request [{:keys [endpoint bearer-token query-params]}]
  (-> (http/get (str "https://api.twitter.com/2/" endpoint)
        {:headers {"Authorization" (str "Bearer " bearer-token)}
         :as :json
         :cookie-policy :standard
         :query-params query-params
         :throw-exceptions false})
    :body))

(defn paginated-request [{:keys [max-pages] :as params}]
  (loop [page 0
         page-token nil
         results nil]
    (if (and (pos? page)
             (or (and max-pages (= page max-pages))
                 (not page-token)))
      {:results results :pages page}
      (let [{{:keys [next_token]} :meta :keys [data] :as response}
            (request (cond-> params
                       page-token (assoc-in [:query-params :pagination_token] page-token)))]
        (prn response)
        (recur (inc page) next_token (concat results data))))))

(defn get-liked-tweets [{:keys [user-id] :as params}]
  (paginated-request (merge params {:endpoint (str "users/" user-id "/liked_tweets")})))

#_[id,max_results,pagination_token,expansions,tweet.fields,media.fields,poll.fields,place.fields,user.fields]

(def all-tweet-fields "author_id,attachments,conversation_id,created_at,entities,in_reply_to_user_id,referenced_tweets,public_metrics")
(def all-user-fields "name,username")

(defn expand-urls [{:keys [text] {:keys [urls]} :entities :as tweet}]
  (assoc tweet :text-with-urls
    (loop [todo (seq urls)
           result text]
      (if (empty? todo)
        result
        (let [{:keys [url expanded_url]} (first todo)]
          (recur (rest todo)
            (str/replace result url expanded_url)))))))

(defn get-user-ids [{:keys [author_id]}]
  (list author_id))
