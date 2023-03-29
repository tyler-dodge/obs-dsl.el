;;; -*- lexical-binding: t -*-

(require 'websocket)
(require 'deferred)
(require 'ht)

(defcustom obs-dsl-url "127.0.0.1:4455"
  "The URL used to open the websocket connection to OBS.")

(defmacro obs-dsl (&rest config-lines)
  "Run and evaluate a configuration for DSL for OBS.

The DSL expects forms of the following format
(scene PROGRAM)
PROGRAM is a lisp form that's evaluated with a few additional functions in the scope.

(defun microphone (NAME DEVICE)
  \"Adds a input microphone from DEVICE to the current scene named NAME.\")

(defun start-recording-buffer ()
  \"Starts the recording buffer\")

Example:
Defines a scene named main-scene that has two microphones A and B:
(obs-dsl
  (scene \"main-scene\"
    (microphone \"A\")
    (microphone \"B\")))"
  (let ((scenes-var (make-symbol "scenes"))
        (done-var (make-symbol "done"))
        (actions-var (make-symbol "actions"))
        (inputs-var (make-symbol "output")))
    `(let ((,scenes-var nil)
           (,done-var (ht))
           (,actions-var nil)
           (,inputs-var nil))
       ;; Loops through each line of the dsl. Currently only scenes are supported
       ,@(cl-loop for config-line in config-lines
                  collect
                  (pcase (car-safe config-line)
                    ('scene (-let [(_ name . prog) config-line]
                              `(progn
                                 (push ,name ,scenes-var)
                                 ;; Define scene specific functions for the scene scope
                                 (cl-flet ((microphone (input-name)
                                             (push (ht (:scene ,name) (:name input-name) (:type "coreaudio_input_capture"))
                                                   ,inputs-var))
                                           (start-recording-buffer ()
                                             (let ((fn (lambda ()
                                                         (deferred:$
                                                          (obs-dsl--get-connection)
                                                          (deferred:nextc
                                                           it
                                                           (lambda (obs)
                                                             (message "RECORDING")
                                                             (deferred:$
                                                              (obs-dsl--websocket-send-request obs "StartReplayBuffer" (ht))
                                                              (deferred:nextc it (lambda (&rest arg) (-message arg))))))))))
                                               (if (ht-get ,done-var "DONE" nil)
                                                   (funcall fn)
                                                 (push fn ,actions-var)))))
                                   ,@prog))))

                    (_ (user-error "Unexpected line in dsl. Only 'scene is supported. Found: %S." config-line))))
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

(defvar obs-dsl--websockets (ht)
  "A hash table of the currently active dsl websockets.")

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
                                                    ("inputName" (ht-get input :name)))))))
                   (list (list :type "SetCurrentProgramScene"
                               :data (ht ("sceneName" (car scene-names)))))
                   nil)
           (obs-dsl--websocket-send-request-batch obs))))))

(defun obs-dsl--get-connection ()
  "Returns the current or a newly created connection to OBS."
  (or (-some--> (car-safe (ht-values obs-dsl--websockets))
        (deferred:next nil it))
      (obs-dsl--open-websocket)))

(defun obs-dsl--post-evaluation (scenes inputs)
  "Handles sending the generated SCENES and INPUTS from `obs-dsl' to OBS.
Returns a deferred object that completes when the update has been sent to OBS."
  (let* ((scene-name (car scenes)))
    (deferred:$
     (obs-dsl--get-connection)
     (deferred:nextc
      it
      (lambda (obs)
        (obs-dsl--websocket-update obs scenes inputs))))))

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
