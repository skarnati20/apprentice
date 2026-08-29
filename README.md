# Apprentice

A Common Lisp coding harness designed around local LLMs and extreme configurability. Designed to be hacked.

## How It Works

The implementation is based on three coding harness abstractions:

1. Models - The LLM that provides responses to your prompt
2. Tools - The functionality you provide the LLM
3. Loops - The coordinating logic around the LLM which *actually* makes it do things

This repo uses macros (e.g. `defmodel` and `deftool`) to make these abstractions easy to create. Therefore creating bespoke functionality not provided by the major harness projects is easy.

Here is an example of creating a model:

```lisp
(defmodel gpt-5.6-terra
  :endpoint "https://api.openai.com/v1/responses"
  :headers (("Content-Type"  "application/json")
	    ("Authorization" (format nil "Bearer ~a" (uiop:getenv "OPENAI_API_KEY"))))
  :messages-key "input"
  :params ((model-id "model"      :default "gpt-5.6-terra")
	   (max-tokens "max_output_tokens" :default 16000)
	   (stream     :default nil :as (if value t :false))
	   (effort     "reasoning" :default "medium" :as (j "effort" value)))
  :format-message (openai-responses-format-message msg)
  :format-tool    (tool->openai-responses tool)
  :parse          (openai-responses-parse raw))
```

Here is an example of creating a tool:

```lisp
(deftool grep
    "Search for a regular expression pattern in files under a directory. Returns matching lines prefixed with file path and line number."
    ((pattern :string "The regular expression to search for")
     (path    :string "Directory or file to search in")
     &optional
     (glob    :string "Optional filename filter, e.g. *.lisp"))
  :checks (((is-allowed-path *allowed-dirs* path)
	    (format nil "Not allowed to access this path. Allowed dirs: ~a"
		    (format nil "~{~A~^, ~}" *allowed-dirs*))))
  :fn (run-argv (append (list "grep" "-rn")
                        (when glob (list "--include" glob))
                        (list "-e" pattern path))))
```

By making customization easier and low-touch, users can explore different harness approaches to find the one suited for their task and workflow.