# Apprentice

A Common Lisp coding harness designed around extreme configurability. Designed to be hacked, modified, and expanded.

## Project Philosophy

Most harnesses today aren't that configurable. They determine what agent loops can run, what models are available, the tools agents can use, and what context they receive. The goal of this project is to define abstractions which make harness customization easy, while still giving users ultimate freedom.

Why Common Lisp? Because Lisp macros allow us to make harness abstractions native to the language. Lisp's REPL-driven development perfectly fits a chat interface. And finally, Lisp's image-based runtime makes it easy to modify the harness and see changes instantly.

Apprentice is based on **four** key harness abstractions:
1. Models - The LLM provider and source of intelligence
2. Tools - The capabilities provided to the model
3. Anchors - Any pre-processing happening on a repository or directory
4. Loops - The orchestrating code handling model output and tool requests

See the [Extending The Harness](#extending-the-harness) section on how to define these abstractions on your own.

## Getting Started

First, clone the repository and load and enter the apprentice package:

```sh
git clone https://github.com/skarnati20/apprentice.git ~/quicklisp/local-projects/apprentice
```

```lisp
(ql:quickload :apprentice)
(in-package :apprentice)
```

Look at the available models and choose one:

```lisp
(available-models)             ; => ("llama-cpp" "claude-sonnet-5-5" "gpt-6.2-astra" ...)
(set-model "claude-sonnet-5-5")
```

Make sure you have the proper environment variables set:

```sh
export ANTHROPIC_API_KEY=...
export OPENAI_API_KEY=...
export GEMINI_API_KEY=...
export OPENROUTER_API_KEY=...
```

Or

```lisp
(setf (uiop:getenv "ANTHROPIC_API_KEY") "...")
(setf (uiop:getenv "OPENAI_API_KEY") "...")
(setf (uiop:getenv "GEMINI_API_KEY") "...")
(setf (uiop:getenv "OPENROUTER_API_KEY") "...")
```

Or

```sh
llama-server -m /path/to/model.gguf --port 8080
```

Then add an allowed directory:

```lisp
(add-allowed-dir "~/projects/my-app")
```

And now, chat!

```lisp
(chat "What does the main function in src/main.rs do?")
```

## Commands

| Command | Description | Output |
| --- | --- | --- |
| `(available-models)` | Returns all available models | `("llama-cpp" "claude-sonnet-5-5" "gpt-6.2-astra" ...)` |
| `(set-model "claude-sonnet-5-5")` | Sets the active model | `"claude-sonnet-5-5"` |
| `(model)` | Returns the active model name | `"claude-sonnet-5-5"` |
| `(subagent-model)` | Returns the active subagent model name | `"llama-cpp"` |
| `(set-subagent-model "llama-cpp")` | Sets the subagent model | `"llama-cpp"` |
| `(add-allowed-dir "~/projects/my-app")` | Adds a directory to the allowed directories list | `("/Users/you/projects/my-app/")` |
| `(allowed-dirs)` | Returns all allowed directories | `("/Users/you/projects/my-app/")` |
| `(clear-allowed-dirs)` | Clears all allowed directories | `NIL` |
| `(available-loops)` | Returns all available coding loops | `(:STANDARD :LITTLE-CODER :APPRENTICE)` |
| `(set-loop :apprentice)` | Sets the active coding loop | `:APPRENTICE` |
| `(current-loop)` | Returns the active coding loop | `:APPRENTICE` |
| `(available-anchors)` | Returns all available anchors | `("dense-vector-search" "file-tree")` |
| `(add-anchor "file-tree")` | Enables an anchor by name | `("file-tree")` |
| `(set-anchor-dir "~/projects/my-app")` | Sets anchor directory and processes/indexes files | `"/Users/you/projects/my-app/"` |
| `(anchors)` | Returns currently enabled anchors | `("dense-vector-search" "file-tree")` |
| `(clear-anchors)` | Clears all enabled anchors | `NIL` |
| `(save-anchors)` | Saves currently loaded anchor state to disk | `"/Users/you/projects/my-app/.apprentice"` |
| `(edit-preset :model "claude-sonnet-5-5")` | Updates a single key in the preset file (`:anchor` or `:global`) | Updated plist |
| `(save-preset)` | Saves current settings to local anchor preset `.apprentice/preset.sexp` | Saved plist |
| `(save-preset :global)` | Saves current settings to global preset `~/.apprentice/preset.sexp` | Saved plist |
| `(load-preset)` | Loads preset from anchor directory | Restored plist |
| `(load-preset :global)` | Loads preset from global `~/.apprentice/preset.sexp` | Restored plist |
| `(list-preset)` | Displays detected preset for `:anchor` | Formatted output |
| `(list-preset :global)` | Displays detected preset for `:global` | Formatted output |
| `(add-option :temperature 0.2)` | Adds or sets an option for the coding loop | `((:TEMPERATURE . 0.2))` |
| `(options)` | Returns currently set options | `((:TEMPERATURE . 0.2))` |
| `(clear-options)` | Clears all currently set options | `NIL` |
| `(chat "Summarise this repository" :standard :max-tokens 2000)` | Runs a conversation turn using the specified loop and options | Model response text |
| `(chat #"prompt with unescaped "quotes" and newlines"#)` | Runs chat with raw string syntax (no manual escaping required) | Model response text |
| `(show-turns)` | Prints conversation history turns | Formatted turn output |
| `(show-turns 3)` | Prints turn at specific index | Formatted turn output |
| `(show-turns -1)` | Prints the most recent turn | Formatted turn output |
| `(show-turns 2 :limit 500)` | Prints turn with custom preview character limit | Formatted turn output |
| `(set-preview-limit 300)` | Sets the preview character limit for `show-turns` | `300` |
| `(drop-turns 3)` | Drops turn at specified index from history | Updated chat history |
| `(drop-turns '(2 . 4))` | Drops inclusive range of turns from history | Updated chat history |
| `(clear)` | Clears conversation history | `NIL` |


## Extending The Harness

### Models

Create a new model with `defmodel`:

```lisp
(defmodel openrouter
  :endpoint "https://openrouter.ai/api/v1/chat/completions"
  :headers (("Content-Type"  "application/json")
	    ("Authorization" (format nil "Bearer ~a"
				     (uiop:getenv "OPENROUTER_API_KEY")))
	    ("HTTP-Referer"  (uiop:getenv "OPENROUTER_REFERER"))
	    ("X-Title"       (uiop:getenv "OPENROUTER_TITLE")))
  :params ((model-id "model"       :default "anthropic/claude-sonnet-5.5")
	   (max-tokens  :default 4096)
	   (temperature :default 0.2)
	   (top-p)
	   (stop)
	   (stream      :default nil :as (if value t :false)))
  :format-message (openai-format-message msg)
  :format-tool    (tool->openai tool)
  :parse          (openai-parse raw))
```

| Argument | Description |
| --- | --- |
| `name` | Symbol naming the model. Binds to `*<NAME>-MODEL*`. |
| `:endpoint` | URL endpoint for HTTP POST requests. |
| `:headers` | List of `(NAME VALUE)` pairs sent as request headers. |
| `:params` | Option specifications in the form `(OPTION [JSON-KEY] [:default VALUE] [:as FORM])`. |
| `:messages-key` | JSON key for the message history payload. Defaults to `"messages"`. |
| `:format-message` | Function with `msg` object bound. Outputs a formatted message. |
| `:format-tool` | Function with `tool` bound. Transforms a tool struct into the provider's wire format. |
| `:parse` | Body form with `raw` bound to decoded response JSON. Returns a `turn` struct. |

Then add it to `*models-list*`:

```lisp
(defparameter *models-list*
  (list ...
        *openrouter-model*))
```

### Tools

Create a new tool with `deftool`:

```lisp
(deftool read
  "Read the contents of a file, with line numbers prefixed."
  ((path :string "Absolute path to the file to read")
   &optional
   (offset :integer "1-based line to start from, default 1")
   (limit  :integer "Maximum lines to read, default 2000"))
  :checks (((is-allowed-path *allowed-dirs* path)
	    (format nil "Not allowed to access this path. Allowed dirs: ~a"
		    (format nil "~{~A~^, ~}" *allowed-dirs*))))
  :fn (let ((start (or offset 1)) (n (or limit 2000)))
        (with-open-file (in path :external-format :utf-8)
          (loop for i from 1
                for line = (read-line in nil)
                while (and line (< (- i start) n))
                when (>= i start)
                  collect (format nil "~5d~a~a" i #\Tab line) into out
                finally (return (format nil "~{~a~^~%~}" out))))))
```

| Argument | Description |
| --- | --- |
| `name` | Symbol naming the tool. Binds to `*<NAME>-TOOL*`. |
| `description` | String description sent to the model explaining what the tool does. |
| `params` | Parameter list of `(NAME TYPE DESCRIPTION)` specs (`:string`, `:integer`, `(:array :string)`). Arguments after `&optional` are optional. |
| `:checks` | List of `(TEST MESSAGE)` condition pairs. When a `TEST` fails, `MESSAGE` is returned to the model and `:fn` does not run. |
| `:fn` | Body form executed once all checks pass, with parameter variables bound to arguments. Returns the tool result string. |

Then add it to `*standard-tools*`:

```lisp
(defparameter *standard-tools*
  (list ...
        *read-tool*))
```

### Anchors

Create a new anchor with `defanchor`:

```lisp
(defvar *file-tree-store-name* "file-tree.sexp")

(defanchor file-tree
  "A simple anchor that tracks every file's path, so a tool can
   print the directory as a tree without touching disk again."
  :bindings ((paths nil))
  :process
  (lambda (files)
    (setf paths (mapcar #'file-path files))
    (list :files (length paths)))
  :serialize
  (lambda (folder)
    (with-open-file (out (merge-pathnames *file-tree-store-name* folder)
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (prin1 (list :version 1 :paths paths) out)))
  :deserialize
  (lambda (folder)
    (let ((path (merge-pathnames *file-tree-store-name* folder)))
      (when (probe-file path)
	(let ((data (with-open-file (in path) (read in))))
	  (setf paths (getf data :paths)))))))
```

| Argument | Description |
| --- | --- |
| `name` | Symbol naming the anchor. Binds to `*<NAME>-ANCHOR*`. |
| `description` | String description of the anchor. |
| `:bindings` | List of `(VAR INIT)` state declarations held and modified by the anchor. |
| `:process` | Function of `files` structs that updates bindings before chat requests. |
| `:serialize` | Function of `folder` that writes bindings to disk. |
| `:deserialize` | Function of `folder` that reads data back into bindings. |

Then add it to `*anchors-list*`:

```lisp
(defparameter *anchors-list*
  (list ...
        *file-tree-anchor*))
```

### Loops

Create a new loop by defining a function like so. The only required argument here is `prompt`:

```lisp
(defparameter *escalate-after* 10)

(defun apprentice-loop (prompt &rest options
			&key (model *model*)
			  (system-prompt *apprentice-prompt*)
			  (tools *apprentice-tools*)
			  (max-turns 50)
			  (history nil)
			  (max-parallel-calls 3)
			  (escalate-after *escalate-after*)
			&allow-other-keys)
  (let ((*max-parallel-calls* max-parallel-calls)
	(opts      (model-options options))
	(msgs      (seed-messages system-prompt prompt history))
	(escalated nil))
    (setf *subagent-calls* 0)
    (loop repeat max-turns do
      (let* ((escalate (>= *subagent-calls* escalate-after))
	     (kit      (if escalate *standard-tools* tools)))
	(when (and escalate (not escalated))
	  (setf escalated t)
	  (format t "~&⇧ ~a subagent tasks run: the standard tool kit is now available~%"
		  *subagent-calls*))
	(let ((turn (apply #'run-model model msgs kit opts)))
	  (when (eq (turn-stop turn) :error)
	    (return (list (turn-text turn) msgs)))
	  (setf msgs (append msgs (list turn)))
	  (if (turn-calls turn)
	      (setf msgs (append msgs (list (make-turn
					     :role :tool-results
					     :results (run-calls (turn-calls turn) kit)))))
	      (return (list (turn-text turn) msgs)))))
      finally (return (apply #'force-final-answer
			     model msgs max-turns opts)))))
```

| Argument | Description |
| --- | --- |
| `prompt` | String containing the user prompt. |
| `&rest options` | Additional keyword options forwarded to model calls. |
| `:model` | The model struct used for generation (defaults to `*model*`). |
| `:system-prompt` | System instructions string for the agent loop. |
| `:tools` | List of tool structs initially available to the loop. |
| `:max-turns` | Maximum number of turns before completing (default 50). |
| `:history` | Conversation message history list. |

Then add it to `*loops-list*`:

```lisp
(defparameter *loops-list*
  '(...
    (:apprentice   . apprentice-loop)))
```

## Explanation of Loops

One benefit of Apprentice is that you can create your own loops. While other harnesses allow you to customize their behaviour, by defining a loop you are able to change the literal infrastructure the LLM depends on, which lets you do really creative things.

### Standard Loop (`:standard`)

A vanilla agent loop which takes a user's prompt and keeps looping until the model has no more tool calls. The output of each tool call is fed back into the model for it to think again.

### Little Coder Loop (`:little-coder`)

This is based on the Little Coder project, which tries to optimize the harness to work better with smaller local agents. Use this with `llama-cpp` and look at the original [project repository here](https://github.com/itayinbarr/little-coder).

### Apprentice Loop (`:apprentice`)

This loop restricts the primary model from reading and writing files in order to save its context. Instead, it must delegate investigations and tasks to a subagent, which only provides a brief and limited report to the primary model. Use this loop if you want to save on cost for long-running tasks in the background.

## AI Usage Acknowledgement

This project was developed with LLM-assistance. Most of the core functionality like plumbing, main functions, and the macros (`deftool`, `defmodel`, and `defanchor`) were hand-written and validated with LLMs. Some other code such as the specific model definitions (Gemini, Claude, GPT, etc.) and tool definitions (subagent, exa web-search, etc.) used LLMs extensively.

It is encouraged to improve the existing models, tools, anchors, and loops in this repository and define your own! LLMs are one way to do it, but they won't be perfect. A project like this benefits from using LLMs to extend the capabilities quickly and allow for quicker harness experimentation and iteration.
