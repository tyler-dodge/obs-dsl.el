;;; obs-dsl.el --- A declarative dsl for controlling OBS -*- lexical-binding: t -*-

;; Author: Tyler Dodge
;; Version: 0.5
;; Keywords: convenience, tools
;; Package-Requires: ((emacs "27.1") (deferred "0.5.1") (s "1.12.0") (ht "2.4") (uuid "0.0.3") (dash "2.17.0") (websocket "1.13"))
;; URL: https://github.com/tyler-dodge/obs-dsl
;; Git-Repository: git://github.com/tyler-dodge/obs-dsl.git
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;;
;;;
;;; Commentary:
;; obs-dsl.el provides a dsl macro named obs-dsl intended for use with OBS.
;; The dsl allows for declaratively configuring OBS and updating the configuration
;; via the websocket API.
;;
;; See `obs-dsl' for more details on how to use.
;;; Code:

;; External Dependencies
(require 'ht)
(require 's)
(require 'deferred)

;; Emacs Dependencies
(require 'websocket)
(require 'cl-lib)

(defgroup obs-dsl nil "OBS DSL Options" :group 'external)

(defcustom obs-dsl-url "127.0.0.1:4455"
  "The URL used to open the websocket connection to OBS."
  :type '(string)
  :group 'obs-dsl)

(defmacro obs-dsl (&rest config-lines)
  "Run and evaluate a configuration for DSL for OBS.

See `obs-dsl-url' if you are using a nonstandard port for OBS.

For Example:
`(obs-dsl
  (scene \"Main\"
    (microphone \"mic\")
    (browser \"website\" \"https://tdodge.consulting\" :width 1920 :height 1080)))'

The DSL expects forms of the following format
(scene PROGRAM)
PROGRAM is a lisp form that's evaluated with a few additional functions in the scope.


Inputs:
Inputs represent sources like microphones and screen recorders.
Inputs must be defined before the obs-dsl scope exits, so they cannot be created via async callbacks.

`(defun microphone (NAME DEVICE)
  \"Adds a input microphone named NAME from DEVICE to the current scene.\")'

`(defun browser (NAME URL :x :y :width :height)
  \"Adds a browser window named NAME connected to URL to the current scene.\")'

`(defun video-capture (NAME DEVICE)
  \"Adds a Video Capture Device named NAME connected to DEVICE to the current scene.\")'

`(defun text (NAME STRING :x :y)
  \"Adds text with the contents of STRING named NAME to the current scene.\")'


Actions represent runtime actions that are available after the inputs are setup such as starting recording.
Actions will always be delayed until after the inputs are fully configured, but they may be scheduled during the configuration stage.
The functions that represent actions can leave the scope of obs-dsl. However, if there is another execution of obs-dsl afterwards,
the previous instance of the function will no longer be valid, so they won't impact the new execution.

Actions:

`(defun start-replay-buffer ()
  \"Starts the replay buffer\")'

`(defun stop-replay-buffer ()
  \"Stops the replay buffer\")'

`(defun start-recording ()
  \"Starts recording\")'

`(defun stop-recording ()
  \"Stops recording\")'

`(defun start-streaming ()
  \"Starts streaming\")'

`(defun stop-streaming ()
  \"Stops streaming\")'

Examples:

Defines a scene named Main that has a browser window 1920x1080
named website and a microphone named mic:

`(obs-dsl
  (scene \"Main\"
    (microphone \"mic\")
    (browser \"website\" \"https://tdodge.consulting\" :width 1920 :height 1080)))'


Defines a scene named main-scene that has a browser window 1920x1080
and two microphones A and B:

`(obs-dsl
  (scene \"main-scene\"
    (browser \"main\" \"https://tdodge.consulting\" :width 1920 :height 1080)
    (microphone \"A\")
    (microphone \"B\")))'


Defines a scene named Main that has a browser window 1920x1080 named website and a microphone named mic.
Uses actions to start and stop the recording after 10 seconds

`(obs-dsl
  (scene \"Main\"
    (microphone \"mic\")
    (browser \"website\" \"https://tdodge.consulting\" :width 1920 :height 1080)
    (start-recording)
    (run-at-time 10 nil (lambda () (stop-recording)))))'
"
  (let ((scenes-var (make-symbol "scenes"))
        (done-var (make-symbol "done"))
        (actions-var (make-symbol "actions"))
        (execution-id-var (make-symbol "execution-id"))
        (name-index-var (make-symbol "name-index"))
        (scene-item-index-var (make-symbol "scene-item-index"))
        (inputs-var (make-symbol "output")))
    `(let ((,scenes-var nil)
           (,done-var (ht))
           (,actions-var nil)
           (,name-index-var (ht))
           (,execution-id-var (uuid-string))
           (,inputs-var nil))
       (cl-flet ((done-p () (ht-get ,done-var "DONE")))
         ;; Loops through each line of the dsl. Currently only scenes are supported
         ,@(cl-loop for config-line in config-lines
                    collect
                    (pcase (car-safe config-line)
                      ('scene (-let [(_ name . prog) config-line]
                                `(progn
                                   (let ((,scene-item-index-var 0))
                                     (push ,name ,scenes-var)
                                     ;;
                                     ;;
                                     ;; Define scene specific functions for the scene scope
                                     (cl-flet* ((running-execution-p () (string= obs-dsl--execution-id ,execution-id-var))
                                                (get-input-by-name (input-name)
                                                  (or (ht-get ,name-index-var (concat ,name ":" input-name))
                                                      (user-error "Unable to find an input named %S %s."
                                                                  input-name (ht->alist ,name-index-var))))
                                                (action (callback)
                                                  (let ((fn (lambda ()
                                                              (when (running-execution-p)
                                                                (obs-dsl--with-connection
                                                                 obs
                                                                 (when (running-execution-p)
                                                                   (funcall callback obs)))))))
                                                    (if (done-p)
                                                        (funcall fn)
                                                      (push fn ,actions-var))))
                                                (update-settings (input-name new-settings)
                                                  (action
                                                   (lambda (obs)
                                                     (deferred:$
                                                      (obs-dsl--websocket-send-request
                                                       obs "SetInputSettings"
                                                       (ht ("inputName" input-name)
                                                           ("inputSettings" new-settings)
                                                           ("sceneName" ,name)))))))
                                                ;; Start Of Public Functions
                                                (set-text (input-name value)
                                                  (update-settings input-name (ht ("text" value))))
                                                (set-position (input-name x y)
                                                  (action
                                                   (lambda (obs)
                                                     (deferred:$
                                                      (obs-dsl--websocket-send-request
                                                       obs "SetSceneItemTransform"
                                                       (ht
                                                        ("sceneItemId" (get-input-by-name input-name))
                                                        ("sceneItemTransform" (ht ("positionX" x)
                                                                                  ("positionY" y)))
                                                        ("sceneName" ,name)))))))
                                                (input (input-name type settings transform)
                                                  (when (done-p) (user-error "Cannot update inputs after evaluation is complete."))
                                                  (setq ,scene-item-index-var (1+ ,scene-item-index-var))
                                                  (ht-set ,name-index-var (concat ,name ":" input-name) ,scene-item-index-var)
                                                  (push (ht (:inputSettings settings)
                                                            (:transform transform)
                                                            (:index ,scene-item-index-var)
                                                            (:scene ,name)
                                                            (:name input-name)
                                                            (:type type))
                                                        ,inputs-var))
                                                (browser (&rest arg)
                                                  (-let [(input-name url &plist :x :y :width :height) arg]
                                                    (input input-name "browser_source"
                                                           (ht ("width" width)
                                                               ("height" height)
                                                               ("url" url))
                                                           (ht ("positionX" x)
                                                               ("positionY" y)))))
                                                (microphone (&rest arg)
                                                  (-let [(input-name &key target &allow-other-keys) arg]
                                                    (input input-name "coreaudio_input_capture" (when target (ht ("device_id" target))) nil)))
                                                (text (&rest arg)
                                                  (-let [(input-name value &plist :x :y) arg]
                                                    (input input-name "text_ft2_source_v2" (ht ("text" (or value input-name)))
                                                           (ht ("positionX" x)
                                                               ("positionY" y)))))
                                                (screen-capture (&rest arg)
                                                  (-let [(input-name &plist :x :y :width :height) arg]
                                                    (input input-name "screen_capture"
                                                           (ht)
                                                           (ht ("positionX" x)
                                                               ("positionY" y)
                                                               ("scaleX" width)
                                                               ("scaleY" height)))))
                                                (start-recording ()
                                                  (action
                                                   (lambda (obs)
                                                     (deferred:$
                                                      (obs-dsl--websocket-send-request obs "StartRecord" (ht))
                                                      (deferred:nextc it (lambda (&rest arg) (message "%S" arg)))))))
                                                (stop-recording ()
                                                  (action
                                                   (lambda (obs)
                                                     (deferred:$
                                                      (obs-dsl--websocket-send-request obs "StopRecord" (ht))
                                                      (deferred:nextc it (lambda (&rest arg) (message "%S" arg)))))))
                                                (stop-streaming ()
                                                  (action
                                                   (lambda (obs)
                                                     (deferred:$
                                                      (obs-dsl--websocket-send-request obs "StopStream" (ht))
                                                      (deferred:nextc it (lambda (&rest arg) (message "%S" arg)))))))
                                                (start-streaming ()
                                                  (action
                                                   (lambda (obs)
                                                     (deferred:$
                                                      (obs-dsl--websocket-send-request obs "StartStream" (ht))
                                                      (deferred:nextc it (lambda (&rest arg) (message "%S" arg)))))))
                                                (stop-replay-buffer ()
                                                  (action
                                                   (lambda (obs)
                                                     (deferred:$
                                                      (obs-dsl--websocket-send-request obs "StopReplayBuffer" (ht))
                                                      (deferred:nextc it (lambda (&rest arg) (message "%S" arg)))))))
                                                (start-replay-buffer ()
                                                  (action
                                                   (lambda (obs)
                                                     (deferred:$
                                                      (obs-dsl--websocket-send-request obs "StartReplayBuffer" (ht))
                                                      (deferred:nextc it (lambda (&rest arg) (message "%S" arg))))))))
                                       (setq obs-dsl--execution-id ,execution-id-var)
                                       ,@prog)))))

                      (_ (user-error "Unexpected line in dsl. Only 'scene is supported. Found: %S." config-line)))))
       ;; Send the evaluated dsl to OBS
       (deferred:$
        (obs-dsl--post-evaluation (reverse ,scenes-var) (reverse ,inputs-var))
        (deferred:nextc
         it
         (lambda (&rest _ )
           (--each ,actions-var (funcall it))
           (ht-set ,done-var "DONE" t)))))))

(defun obs-dsl-shutdown ()
  "Terminates all the connections currently active with OBS."
  (interactive)
  (cl-loop for socket in (ht->alist obs-dsl--websockets)
           do (progn
                (ignore-errors (websocket-close (obs-dsl--websocket--socket (cdr socket))))
                (ht-remove obs-dsl--websockets (car socket)))))

(cl-defstruct (obs-dsl--websocket
                (:constructor obs-dsl--websocket--create)
                (:copier obs-dsl--websocket--copy)
                (:conc-name obs-dsl--websocket--))
  "Represents a websocket connection with OBS."
  (rpc-version)
  (state 'initial)
  (request-callbacks (ht))
  (socket)
  (connected-callback))

(defmacro obs-dsl--with-connection (name &rest prog)
  (declare (indent 1))
  `(deferred:$
    (obs-dsl--get-connection)
    (deferred:nextc
     it
     (lambda (obs)
       (let ((,name obs))
         ,@prog)))))

(defun obs-dsl--describe-obs ()
  (interactive)
  (obs-dsl--with-connection obs
    (deferred:$
     (obs-dsl--websocket-send-request obs "GetInputList" (ht))
     (deferred:nextc
      it
      (lambda (response)
        (deferred:parallel
         (append
          (list
            (obs-dsl--websocket-send-request obs "GetMonitorList" (ht))
           (deferred:$
            (obs-dsl--websocket-send-request obs "GetSceneList" (ht))
            (deferred:nextc
             it
             (lambda (response)
               (deferred:parallel
                (cl-loop for scene in  (ht-get (ht-get response "responseData") "scenes")
                         collect 
                         (deferred:$
                          (obs-dsl--websocket-send-request obs "GetSceneItemList" (ht ("sceneName" (ht-get scene "sceneName"))))
                          (deferred:nextc it (lambda (response) (ht-get response "responseData"))))))))))
          (--> response
               (ht-get it "responseData")
               (ht-get it "inputs")
               (--map (cons (ht-get it "inputName") (ht-get it "inputKind")) it)
               (cl-loop for (input-name . input-kind) in it
                        append
                        (list
                         (obs-dsl--websocket-send-request obs "GetInputDefaultSettings" (ht ("inputKind" input-kind)))
                         (obs-dsl--websocket-send-request obs "GetInputSettings" (ht ("inputName" input-name))))))
          nil))))
     (deferred:nextc it (lambda (response)
                          (let ((out (or (get-buffer "*output*") (generate-new-buffer "*output*"))))
                            (display-buffer out)
                            (with-current-buffer out
                              (erase-buffer)
                              (cl-loop for table in response
                                       do
                                       (progn
                                         (insert
                                          (obs-dsl--alist-debug-string (obs-dsl--ht->alist table)))
                                         (insert "\n"))))))))))


(defun obs-dsl--alist-debug-string (alist &optional level)
   (cond
    ((null alist) nil)
    ((eq :false alist) "false")
    ((eq t alist) "true")
    ((stringp alist) alist)
    ((and (consp alist) (consp (car alist)))
     (cl-loop for cons in alist
              concat (concat (obs-dsl--alist-debug-string cons (or level 0)))))
    ((consp alist)
     (concat "\n" (when (> (or level 0) 0)
                    (concat (s-repeat level "  ") " - "))
             (format "%s" (obs-dsl--alist-debug-string (car alist) (1+ (or level 0)))) ": "
             (format "%s" (obs-dsl--alist-debug-string (cdr alist) (1+ (or level 0)))) ))
    (t (format "%s" alist))))

(defun obs-dsl--ht->alist (ht)
  (cond
   ((stringp ht) ht)
   ((sequencep ht)
    (--map (obs-dsl--ht->alist it) ht))
   ((hash-table-p ht)
    (--map (cons (car it) (obs-dsl--ht->alist (cdr it))) (ht->alist ht)))
   (t ht)))

(defvar obs-dsl--websockets (ht)
  "A hash table of the currently active dsl websockets.")

(defvar obs-dsl--execution-id nil
  "The current execution id.
The DSL rejects calls that do not match the current execution id.")

(defun obs-dsl--websocket-send (obs json)
  "Encodes and sends a JSON to the websocket represented by OBS."
  (websocket-send-text
   (obs-dsl--websocket--socket obs)
   (obs-dsl--encode-json json)))

(defun obs-dsl--websocket-send-request-batch (obs requests)
  "Sends a batch of requests to OBS.

REQUESTS is a list of plists which contain the keys :type and :data.

:data corresponds to the payload from
https://github.com/obsproject/obs-websocket/blob/master/docs/generated/protocol.md#general-1-requests
:type corresponds to the name from the corresponding request."
  (let* ((request-id (uuid-string))
         (promise (deferred:new)))
    (ht-set (obs-dsl--websocket--request-callbacks obs)
            request-id
            promise)
    (websocket-send-text
     (obs-dsl--websocket--socket obs)
     (obs-dsl--encode-json (ht ("op" 8)
                               ("executionType" 1)
                               ("d" (ht ("requestId" request-id)
                                        ("requests" (--map (ht ("requestData" (or (plist-get it :data) (error "Expected :data to be set. %S" it)))
                                                               ("requestType" (or (plist-get it :type) (error "Expected :type to be set. %S" it))))
                                                           requests)))))))
    promise))

(defun obs-dsl--websocket-send-request (obs request-type request-data)
  "Sends a single request to OBS with REQUEST-TYPE.
REQUEST-DATA should be a hash-table matching the keys from
https://github.com/obsproject/obs-websocket/blob/master/docs/generated/protocol.md#general-1-requests"
  (let* ((request-id (uuid-string))
         (promise (deferred:new)))
    (ht-set (obs-dsl--websocket--request-callbacks obs)
            request-id
            promise)
    (websocket-send-text
     (obs-dsl--websocket--socket obs)
     (obs-dsl--encode-json (ht ("op" 6)
                               ("d" (ht ("requestId" request-id)
                                        ("requestType" request-type)
                                        ("requestData" request-data))))))
    promise))

(defun obs-dsl--websocket-on-json (obs frame)
  "Callback that handles updates from the OBS websocket.

OBS is a `obs-dsl--websocket', and frame is a `websocket-frame'."
  (let* ((json (obs-dsl--parse-json (websocket-frame-text frame)))
         (json-data (ht-get json "d"))
         (op (ht-get json "op")))
    (pcase (obs-dsl--websocket--state obs)
      ((and 'idle (guard (eq 9 op)))
       (let ((request-id (ht-get json-data "requestId")))
         (-some--> (obs-dsl--websocket--request-callbacks obs)
           (prog1 (ht-get it request-id) (ht-remove it request-id))
           (deferred:callback-post it json-data))))

      ((and 'idle (guard (eq 7 op)))
       (let ((request-id (ht-get json-data "requestId")))
         (-some--> (obs-dsl--websocket--request-callbacks obs)
           (prog1 (ht-get it request-id) (ht-remove it request-id))
           (deferred:callback-post it json-data))))

      ((and 'identifying (guard (eq 2 op)))
       (setf (obs-dsl--websocket--state obs) 'idle)
       (-some--> (obs-dsl--websocket--connected-callback obs)
         (progn (deferred:callback-post it obs)
                (setf (obs-dsl--websocket--connected-callback obs) nil))))

      ((and 'initial (guard (eq 0 op)))
       (setf (obs-dsl--websocket--state obs) 'identifying)
       (obs-dsl--websocket-send
        obs
        (ht ("op" 1)
            ("d" (ht ("rpcVersion" 1))))))

      ((guard (eq 5 op)))
      (_ (json "ERROR: Unexpected json from OBS: %S %S" obs frame)))))

(defun obs-dsl--open-websocket ()
  "Returns a deferred object that will complete once the websocket is connected."
  (let ((promise (deferred:new))
        (session-id (uuid-string)))
    (websocket-open "ws://127.0.0.1:4455"
                    :on-open
                    (lambda (socket)
                      (ht-set obs-dsl--websockets session-id (obs-dsl--websocket--create
                                                           :connected-callback promise
                                                           :socket socket)))
                    :on-close
                    (lambda (socket)
                      (ht-remove obs-dsl--websockets session-id))
                    :on-message
                    (lambda (_ frame)
                      (obs-dsl--websocket-on-json (ht-get obs-dsl--websockets session-id) frame)))
    promise))

(defun obs-dsl--websocket-update (obs scene-names inputs)
  "Handles constructing a batch request to represent SCENE-NAMES and INPUTS.

SCENE-NAMES is a list of strings
INPUTS is a list of plists each containing the keys :scene, :name, :type.

The result is sent via OBS's `obs-dsl--websocket' websocket"
  (deferred:$
   (obs-dsl--websocket-send-request obs "GetSceneList" (ht))
   (deferred:nextc
    it
    (-lambda ((&hash "responseData" (&hash "scenes" scenes)))
      (->> (append (cl-loop for scene in scenes
                            append (let ((id (uuid-string)))
                                     (list (list :type "SetSceneName"
                                                 :data (ht ("newSceneName" id)
                                                           ("sceneName" (ht-get scene "sceneName"))))
                                           (list :type "RemoveScene"
                                                 :data (ht ("sceneName" id))))))
                   (->> scene-names
                        (--map (list :type "CreateScene"
                                     :data (ht ("sceneName" it)))))
                   (cl-loop for input in inputs
                            append
                            (let ((temp-id (uuid-string)))
                              (list (list :type "SetInputName"
                                          :data (ht ("inputName" (ht-get input :name))
                                                    ("newInputName" temp-id)))
                                    (list :type "RemoveInput"
                                          :data (ht ("inputName" temp-id)))
                                    (list :type "CreateInput"
                                          :data (ht ("inputKind" (ht-get input :type))
                                                    ("sceneName" (ht-get input :scene))
                                                    ("inputSettings" (ht-get input :inputSettings))
                                                    ("inputName" (ht-get input :name))))
                                    (list :type "SetSceneItemTransform"
                                          :data (ht ("sceneName" (ht-get input :scene))
                                                    ("sceneItemId" (ht-get input :index))
                                                    ("sceneItemTransform" (ht-get input :transform)))))))
                   (list (list :type "SetCurrentProgramScene"
                               :data (ht ("sceneName" (car scene-names)))))
                   nil)
           (obs-dsl--websocket-send-request-batch obs))))
   (deferred:nextc
    it
    (lambda (&rest response)
      (message "%S" response)))))

(defun obs-dsl--get-connection ()
  "Returns the current or a newly created connection to OBS."
  (or (-some--> (car-safe (ht-values obs-dsl--websockets))
        (deferred:next nil it))
      (obs-dsl--open-websocket)))

(defun obs-dsl--post-evaluation (scenes inputs)
  "Handles sending the generated SCENES and INPUTS from `obs-dsl' to OBS.
Returns a deferred object that completes when the update has been sent to OBS."
  (let* ((scene-name (car scenes)))
    (obs-dsl--with-connection obs
      (obs-dsl--websocket-update obs scenes inputs))))

(defun obs-dsl--encode-json (json)
  "Returns JSON encoded as a string with the configuration expected by obs-dsl."
  (let ((json-key-type 'string)
        (json-object-type 'hash-table)
        (json-array-type 'list))
    (json-encode json)))

(defun obs-dsl--parse-json (json)
  "Parse JSON with the configuration expected by obs-dsl.

Returns a passthrough output from `json-parse-string'."
  (let ((json-key-type 'string)
        (json-object-type 'hash-table)
        (json-array-type 'list))
    (json-parse-string json :object-type 'hash-table :array-type 'list)))

(provide 'obs-dsl)
;;; obs-dsl.el ends here
