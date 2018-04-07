(ns hls-cljs.core
    (:require
    	[rum.core :as rum]
    	[goog.string :as gstring]
    	[goog.string.format]
    	[clojure.string :as cljstr]))
(enable-console-print!)

(def *vidInfo (atom {
	:currentTime 0
	:playerState ""
	:duration 0
	:buffer 0
	:src ""
        :hlsData {
                :levels []
		:bitrate 0
		:maxBitrate 0
		:error ""
		:errorCount 0
	}}))
(defn log [message] (.log js/console message))

;; VIDEO CONTROL
(defn play []
	(.play (.getElementById js/document "player")))

(defn pause []
	(.pause (.getElementById js/document "player")))

(defn playPause [playerState]
	; todo: fix
	(if (= @playerState "paused") play pause))

(defn setPlayerState [vidAtom state]
	(reset! (rum/cursor-in vidAtom [:playerState]) state))

(defn getBufferFromEl [player]
  (let [length (.. player -buffered -length)]
    (or (loop [index 0] (when (< index length)
      (let [end (.end (aget player "buffered") index)
            start (.start (aget player "buffered") index)
            current (aget player "currentTime")]
        (if (and (< start current) (> end current)) (- end current) (recur (inc index)))))) 0)))

;; HLS
(defn onParsed [event data]
  (play)
  (let [levels (:levels (js->clj data :keywordize-keys true))]
    (reset! (rum/cursor-in *vidInfo [:hlsData :levels]) levels)
    (reset! (rum/cursor-in *vidInfo [:hlsData :maxBitrate])
          (:bitrate (last levels)))))

(defn onLevelSwitch [event data]
  (reset! (rum/cursor-in *vidInfo [:hlsData :bitrate])
          (:bitrate (nth (:levels (:hlsData @*vidInfo))
                         (:level (js->clj data :keywordize-keys true))))))

(defn onError [event data]
  (println data)
   (swap! (rum/cursor-in *vidInfo [:hlsData :errorCount]) inc)
   (reset! (rum/cursor-in *vidInfo [:hlsData :error])
		(:details (js->clj data :keywordize-keys true))))
(defn addListeners [hls]
  (let [evts (js->clj (.-Events js/Hls) :keywordize-keys true)]
    (.on hls (:LEVEL_SWITCHED evts) onLevelSwitch)
    (.on hls (:ERROR evts) onError)
    (.on hls (:MANIFEST_PARSED evts) onParsed)))

(defn loadSource [source player]
  (let [hls (new js/Hls)]
    (.loadSource hls source)
    (.attachMedia hls player)
    (addListeners hls)))
;;
;; COMPONENTS
(defn onLoadSource [url]
	(loadSource url (.getElementById js/document "player")))

(defn getUrlParam [urlParams desired]
	(let [fullParam (first (filter (fn [eachPair] (= desired (first (cljstr/split eachPair #"=")))) (cljstr/split urlParams #"&")))]
		(if fullParam (cljstr/join "=" (rest (cljstr/split fullParam #"="))) nil)))

(def checkForWinSrc {:did-mount (fn [localState]
	(let [checkSrc (js/decodeURIComponent (getUrlParam (subs (.. js/window -location -search) 1) "src"))]
		(log (str "loading " checkSrc))
		(if (and checkSrc (re-find #"^http(s)?" checkSrc)) (onLoadSource checkSrc))
		(assoc localState ::src checkSrc)))})

(def clearBuffer
	{:will-unmount (fn [state]
		(log "clearing interval")
        (js/clearInterval (::bufferInterval state))
        (dissoc state ::bufferInterval)) })

(rum/defcs vid <  checkForWinSrc (rum/local nil ::bufferInterval) clearBuffer
		[componentState vidAtom]
	(if (= nil @(::bufferInterval componentState)) (reset! (::bufferInterval componentState)
		(js/setInterval #(reset! (rum/cursor-in vidAtom [:buffer])
			(getBufferFromEl (js/ReactDOM.findDOMNode (:rum/react-component componentState)))) 200)))
	[:video {
		:id "player"
		:style {
			:width "100%"
			:height "100%"
			:position "absolute"
		}
		:controls "true"
		:muted "true"
		:on-click #(playPause (rum/cursor-in vidAtom [:playerState]))
		:on-time-update #(reset! (rum/cursor-in vidAtom [:currentTime])
			(.. % -currentTarget -currentTime))
		:on-duration-change #(reset! (rum/cursor-in vidAtom [:duration])
			(.. % -currentTarget -duration))
		:on-playing #(setPlayerState vidAtom "playing")
		:on-pause #(setPlayerState vidAtom "paused")
		:on-seeking #(setPlayerState vidAtom "seeking")
		:on-ended #(setPlayerState vidAtom "complete")
		:on-waiting #(setPlayerState vidAtom "waiting")
		}])

(rum/defc infoPanel < rum/reactive [vidAtom]
	[:div {
		:id "infoPanel"
		:style {
			:position "absolute"
			:color "white"
			:opacity 0.7
			:padding "10px"
			:backgroundColor "black"
		}}
		[:div {} "buffer: " (gstring/format "%.2f" (rum/react (rum/cursor-in vidAtom [:buffer])))]
		[:div {} "time: " (gstring/format "%.2f" (rum/react (rum/cursor-in vidAtom [:currentTime])))]
		[:div {} "duration: " (rum/react (rum/cursor-in vidAtom [:duration]))]
		[:div {} "state: " (rum/react (rum/cursor-in vidAtom [:playerState]))]
		[:div {} "bitrate: " (rum/react (rum/cursor-in vidAtom [:hlsData :bitrate]))
			"/" (rum/react (rum/cursor-in vidAtom [:hlsData :maxBitrate]))]
		[:div {} "error(" (rum/react (rum/cursor-in vidAtom [:hlsData :errorCount])) "): "
			(rum/react (rum/cursor-in vidAtom [:hlsData :error]))]])

(rum/defc player [vidAtom]
		[:div {
			:id "playerContainer"
			:style {
				:width "100%"
				:height "100%"
				:backgroundColor "black"
				:position "relative"
			}
		}
		(vid vidAtom)
		(if (getUrlParam (subs (.. js/window -location -search) 1) "panel") (infoPanel vidAtom))])

(rum/defc wrapper []
	[:div {
		:style {:width "100%" :height "100%"}}
		(player *vidInfo)])

(defn init []
	(rum/mount (wrapper)
           (.getElementById js/document "container")))
(init)
