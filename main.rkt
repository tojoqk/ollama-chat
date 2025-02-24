#lang typed/racket

(provide current-ollama-chat-host
         Ollama-Chat-Role
         ollama-chat-role?
         Ollama-Chat-Message
         ollama-chat-message?
         ollama-chat-message
         ollama-chat-message-role
         ollama-chat-message-content
         Ollama-Chat-Error
         ollama-chat-error?
         ollama-chat-error-content
         ollama-chat)

(require typed/json
         typed/net/http-client
         typed/net/url)

(: current-ollama-chat-host (Parameterof url))
(define current-ollama-chat-host (make-parameter
                                  (string->url "http://127.0.0.1:11434")))

(define-type Ollama-Chat-Role (U 'system 'user 'assistant 'tool))
(define-predicate ollama-chat-role? Ollama-Chat-Role)

(struct ollama-chat-message ([role : Ollama-Chat-Role]
                             [content : String])
  #:type-name Ollama-Chat-Message
  #:transparent)

(: ollama-chat-message->jsexpr (-> Ollama-Chat-Message JSExpr))
(define (ollama-chat-message->jsexpr msg)
  (hash 'role (symbol->string (ollama-chat-message-role msg))
        'content (ollama-chat-message-content msg)))

(: jsexpr->ollama-chat-message (-> JSExpr (Option Ollama-Chat-Message)))
(define (jsexpr->ollama-chat-message jsexpr)
  (and (hash? jsexpr)
       (let ([role/str (hash-ref jsexpr 'role)]
             [content (hash-ref jsexpr 'content)])
         (and (string? role/str)
              (let ([role (string->symbol role/str)])
                (and (ollama-chat-role? role)
                     (string? content)
                     (ollama-chat-message role content)))))))

(: message-list->jsexpr (-> (Listof Ollama-Chat-Message) JSExpr))
(define (message-list->jsexpr msg-list)
  (reverse (map ollama-chat-message->jsexpr msg-list)))

(: send-ollama-api-/chat (-> String
                             (Listof Ollama-Chat-Message)
                             (values Bytes (Listof Bytes) Input-Port)))
(define (send-ollama-api-/chat model message-list)
  (let ([body
         (jsexpr->string (hash 'model model
                               'messages (message-list->jsexpr message-list)
                               'stream #t))]
        [url (current-ollama-chat-host)])
    (http-sendrecv (or (url-host url)
                       (error 'send-ollama-api-/chat
                              "host must be not null"
                              url))
                   "/api/chat"
                   #:data body
                   #:method "POST"
                   #:ssl? (equal? (url-scheme url) "https")
                   #:port (let ([port (url-port url)])
                            (if (exact-positive-integer? port)
                                port
                                (cond [(equal? (url-scheme url) "https") 443]
                                      [else 80]))))))

(struct ollama-chat-response-ok ([message : Ollama-Chat-Message]
                                 [done? : Boolean])
  #:type-name Ollama-Chat-Response-Ok
  #:transparent)

(struct ollama-chat-response-ng ([json : JSExpr])
  #:type-name Ollama-Chat-Response-Ng
  #:transparent)

(define-type Ollama-Chat-Response
  (U Ollama-Chat-Response-Ok Ollama-Chat-Response-Ng))

(: read-ollama-chat-response (-> Input-Port
                                 (U EOF Ollama-Chat-Response)))
(define (read-ollama-chat-response p)
  (let ([json (read-json p)])
    (if (eof-object? json)
        eof
        (let ([json-message (and (hash? json)
                                 (hash-ref json 'message #f))]
              [json-done (and (hash? json)
                              (hash-ref json 'done
                                        (thunk 'error)))])
          (let ([msg (jsexpr->ollama-chat-message json-message)])
            (if (and msg (boolean? json-done))
                (ollama-chat-response-ok msg json-done)
                (ollama-chat-response-ng json)))))))

(struct ollama-chat-error ([content : (U (List 'unexpected-eof)
                                         (List 'invalid-response
                                               JSExpr))])
  #:type-name Ollama-Chat-Error
  #:transparent)

(: ollama-chat (-> String
                   (Listof Ollama-Chat-Message)
                   [#:output-port (Option Output-Port)]
                   (U Ollama-Chat-Message Ollama-Chat-Error)))
(define (ollama-chat model message-list
                     #:output-port [output-port (current-output-port)])
  (define-values (_code _headers port)
    (send-ollama-api-/chat model message-list))
  (define out (open-output-string))
  (let loop ([response (read-ollama-chat-response port)])
    (cond [(eof-object? response) (ollama-chat-error '(unexpected-eof))]
          [(ollama-chat-response-ok? response)
           (let ([msg (ollama-chat-response-ok-message response)]
                 [done? (ollama-chat-response-ok-done? response)])
             (write-string (ollama-chat-message-content msg) out)
             (when output-port
               (write-string (ollama-chat-message-content msg) output-port))
             (cond [done?
                    (when output-port
                      (write-string "\n" output-port))
                    (ollama-chat-message (ollama-chat-message-role msg)
                                        (get-output-string out))]
                   [else (loop (read-ollama-chat-response port))]))]
          [else (ollama-chat-error
                 (list 'invalid-response
                       (ollama-chat-response-ng-json response)))])))
