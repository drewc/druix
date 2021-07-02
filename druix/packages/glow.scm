(define-module (druix packages glow)
  #:use-module (druix packages scheme gerbil gerbil-ethereum)
  #:use-module (druix packages glow-lang)
  #:use-module (druix packages glow-contacts)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (guix packages))

(define-public glow
  (package
    (inherit glow-contacts)
    (name "glow")
    (version "0.0.3")
    (synopsis "A distribution of Glow, a language for DApps")
    (description "Glow is a programming language used to make decentralized applications, otherwise known as DApps")
    (propagated-inputs `(("bash" , bash)
                         ("coreutils", coreutils-minimal)
                         ("glow-lang", glow-lang)
                         ("gerbil-ethereum", gerbil-ethereum)))))
