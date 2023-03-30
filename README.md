# obs-dsl.el
[![License](https://img.shields.io/badge/license-GPL_3-green.svg)](https://www.gnu.org/licenses/gpl-3.0.txt)
[![Version](https://img.shields.io/github/v/tag/tyler-dodge/obs-dsl)](https://github.com/tyler-dodge/obs-dsl/releases)
[![Build Status](https://app.travis-ci.com/tyler-dodge/obs-dsl.svg?branch=main)](https://travis-ci.com/github/tyler-dodge/obs-dsl) 
[![Coverage Status](https://coveralls.io/repos/github/tyler-dodge/obs-dsl/badge.svg?branch=main)](https://coveralls.io/github/tyler-dodge/obs-dsl?branch=main)

---
`obs-dsl.el` defines a Domain Specific Language (DSL) for configuring OBS at runtime via the websocket API.

Running `eval-defun` (C-M-x) with the cursor focused on the following form will
make it so OBS changes to match the configuration defined within the dsl scope.

```
(obs-dsl
  (scene "Main"
    (start-recording)
    (microphone "mic")
    (browser "website" "https://tdodge.consulting" :width 1920 :height 1080)))
```

## Installation

Coming soon to MELPA

## Usage

The DSL expects forms of the following format
(scene PROGRAM)
PROGRAM is a lisp form that's evaluated with a few additional functions in the scope.

### Inputs
Inputs represent sources like microphones and screen recorders.
Inputs must be defined before the obs-dsl scope exits, so they cannot be created via async callbacks.

```
(defun microphone (NAME DEVICE)
  "Adds a input microphone named NAME from DEVICE to the current scene.")
```

```
(defun browser (NAME URL :x :y :width :height)
  "Adds a browser window named NAME connected to URL to the current scene.")
```

```
(defun video-capture (NAME DEVICE)
  "Adds a Video Capture Device named NAME connected to DEVICE to the current scene.")
```

```
(defun text (NAME STRING :x :y)
  "Adds text with the contents of STRING named NAME to the current scene.")
```


Actions represent runtime actions that are available after the inputs are setup such as starting recording.
Actions will always be delayed until after the inputs are fully configured, but they may be scheduled during the configuration stage.
The functions that represent actions can leave the scope of obs-dsl. However, if there is another execution of obs-dsl afterwards,
the previous instance of the function will no longer be valid, so they won't impact the new execution.

### Actions

```
(defun start-replay-buffer ()
  "Starts the replay buffer")
```

```
(defun stop-replay-buffer ()
  "Stops the replay buffer")
```

```
(defun start-recording ()
  "Starts recording")
```

```
(defun stop-recording ()
  "Stops recording")
```

```
(defun start-streaming ()
  "Starts streaming")
```

```
(defun stop-streaming ()
  "Stops streaming")
```

## Examples

Defines a scene named Main that has a browser window 1920x1080
named website and a microphone named mic:

```
(obs-dsl
  (scene "Main"
    (microphone "mic")
    (browser "website" "https://tdodge.consulting" :width 1920 :height 1080)))
```


Defines a scene named main-scene that has a browser window 1920x1080
and two microphones A and B:

```
(obs-dsl
  (scene "main-scene"
    (browser "main" "https://tdodge.consulting" :width 1920 :height 1080)
    (microphone "A")
    (microphone "B")))
```

Defines a scene named Main that has a browser window 1920x1080 named website and a microphone named mic.
Uses actions to start and stop the recording after 10 seconds

```
(obs-dsl
  (scene "Main"
    (microphone "mic")
    (browser "website" "https://tdodge.consulting" :width 1920 :height 1080)
    (start-recording)
    (run-at-time 10 nil (lambda () (stop-recording)))))
```

## Customization
* [obs-dsl-url](#obs-dsl-url) <a name="obs-dsl-url"></a> The URL used to connect to OBS.

## Contributing

Contributions welcome, but forking preferred. 
I plan to actively maintain this, but I will be prioritizing features that impact me first.

I'll look at most pull requests eventually, but there is no SLA on those being accepted. 
    
Also, I will only respond to pull requests on a case by case basis. 
I have no obligation to comment on, justify not accepting, or accept any given pull request. 
Feel free to start a fork that has more support in that area.

If there's a great pull request that I'm slow on accepting, feel free to fork and rename the project.
