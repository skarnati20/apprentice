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
(available-models)             ; => ("llama-cpp" "claude-sonnet-5" "gpt-5.6-terra" ...)
(set-model "claude-sonnet-5")
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
| `(available-models)` | Returns all available models | `("llama-cpp" "claude-sonnet-5" "gpt-5.6-terra" ...)` |
| `(set-model "claude-sonnet-5")` | Sets the active model | `"claude-sonnet-5"` |
| `(model)` | Returns the active model name | `"claude-sonnet-5"` |
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
| `(add-option :temperature 0.2)` | Adds or sets an option for the coding loop | `((:TEMPERATURE . 0.2))` |
| `(options)` | Returns currently set options | `((:TEMPERATURE . 0.2))` |
| `(clear-options)` | Clears all currently set options | `NIL` |
| `(chat "Summarise this repository" :standard :max-tokens 2000)` | Runs a conversation turn using the specified loop and options | Model response text |
| `(show-turns)` | Prints conversation history turns | Formatted turn output |
| `(show-turns 3)` | Prints turn at specific index | Formatted turn output |
| `(show-turns -1)` | Prints the most recent turn | Formatted turn output |
| `(show-turns 2 :limit 500)` | Prints turn with custom preview character limit | Formatted turn output |
| `(set-preview-limit 300)` | Sets the preview character limit for `show-turns` | `300` |
| `(drop-turns 3)` | Drops turn at specified index from history | Updated chat history |
| `(drop-turns '(2 . 4))` | Drops inclusive range of turns from history | Updated chat history |
| `(clear)` | Clears conversation history | `NIL` |

## ChatGPT/Codex Device Authentication

Apprentice can log in interactively with a ChatGPT account (device flow)
and then call Codex through `https://chatgpt.com/backend-api/codex/responses`.
This is opt-in: the existing `OPENAI_API_KEY` providers keep working
unchanged, and nothing logs in unless you explicitly call `openai-login`.

### Quick start

Point a context at your project and pick a profile (credentials are
isolated per project, issuer, client ID, and profile):

```lisp
(defparameter *codex-ctx*
  (make-openai-auth-context "/path/to/my-project"))
(openai-auth-status *codex-ctx*)  ; => :MISSING, and nothing is stored yet
```

Log in explicitly. The verification URL, one-time code, remaining time,
and an initiated-by-you warning print on your terminal only:

```lisp
(openai-login *codex-ctx*)  ; => :AUTHENTICATED, after atomic persistence
```

Check that the browser page is really on the OpenAI account origin before
entering the code. Then register a Codex model with an explicit model slug
and select it. There is no default slug: pick a current one from the
authenticated Codex model catalog or model picker (at research time the
pinned fallback catalog included names such as `gpt-6-astra`):

```lisp
(configure-openai-device-model *codex-ctx* "gpt-6-astra")  ; => "openai-codex"
(set-model "openai-codex")
(chat "What does the main function in src/main.rs do?")
```

Restarting Lisp loses nothing: credentials live under
`<project>/.apprentice/auth/credentials.json`, so status works in a fresh
image once you rebuild the same context. Logging out deletes exactly this
context's three tokens and is idempotent:

```lisp
(openai-auth-status *codex-ctx*)  ; => :PRESENT (or :EXPIRED locally)
(openai-logout *codex-ctx*)       ; => :LOGGED-OUT
```

To go back to the API-key provider at any time, just select it again:

```lisp
(set-model "gpt-5.6-terra")
```

### Storage, permissions, and trust boundary

- Credentials are a plaintext JSON file fallback protected only by OS
  permissions: the auth directory is mode `0700`, credential and temporary
  files are mode `0600` from creation, symlinked or mis-owned stores fail
  closed, and writes are atomic renames. There is no OS keychain support.
- The same-user shell and any Lisp code running as you can read these
  files. Do not treat them as isolated from your own tools or agent loops;
  this boundary keeps other users out, not yourself.
- Status values (`:missing`, `:present`, `:expired`) describe local stored
  state only, never remote validity, and no function returns token values.

### Sessions, expiry, and re-login

There is no automatic refresh or token rotation. When a session expires,
log in again explicitly. A rejected request (`401`) yields re-login
guidance without deleting anything; a forbidden one (`403`, e.g. plan or
entitlement problems) stays a distinct error and also keeps your stored
credentials.

### Runbook

| Situation | What you see | What to do |
| --- | --- | --- |
| Device auth disabled server-side | `device-auth-unavailable` on login | Use an API-key model instead |
| Login takes longer than ~15 minutes, or you cancel | `device-auth-timed-out` / `openai-auth-cancelled`, nothing persisted | Start `openai-login` again if still wanted |
| TLS, proxy, or network failure | Safe transport/inference errors, no secrets in messages | Fix network/proxy/`curl`, then retry |
| Wrong permissions, symlink, corrupt or oversize store | `credential-storage-failure` | Inspect `<project>/.apprentice/auth/`, fix ownership/modes or delete it and re-login |
| Unknown/newer store schema | `reauthentication-required` | Re-login; old files are never migrated |
| Expired credentials | `:expired` status; calls refuse before any request | `openai-login` again, or `openai-logout` to clear |
| Revoked/rejected credentials | Re-login guidance (`401`) or entitlement error (`403`) | Re-login; for `403`, check plan/entitlement instead |
| Rollback | — | `(set-model "gpt-5.6-terra")`, `(openai-logout ctx)`, optionally delete `<project>/.apprentice/auth/` |

### Compatibility caveat

The Codex wire contract was researched against `openai/codex` commit
`7498521d288b9b3b96ffba4eedf089d8d6e06a84` (2026-09-18). That production
API is unversioned and may drift; if Codex calls start failing after an
upstream change, recheck the endpoint, headers, and event shapes against
current upstream before assuming a local bug. The OpenAI-owned public
client ID used for device login does not register Apprentice as an
application.

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
  :params ((model-id "model"       :default "anthropic/claude-sonnet-5")
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
