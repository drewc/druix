(define-module (druix packages databases)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix store))

(define-public sqlite-fPIC
  (package
    (inherit sqlite)
   (name "sqlite-fPIC")
   (arguments
    `(#:configure-flags
      ;; Add -DSQLITE_SECURE_DELETE, -DSQLITE_ENABLE_FTS3,
      ;; -DSQLITE_ENABLE_UNLOCK_NOTIFY and -DSQLITE_ENABLE_DBSTAT_VTAB
      ;; to CFLAGS.  GNU Icecat will refuse to use the system SQLite
      ;; unless these options are enabled.
      (list (string-append "CFLAGS=-O2 -g -fPIC -DSQLITE_SECURE_DELETE "
                           "-DSQLITE_ENABLE_FTS3 "
                           "-DSQLITE_ENABLE_UNLOCK_NOTIFY "
                           "-DSQLITE_ENABLE_DBSTAT_VTAB "
                           ;; Column metadata is required by GNU Jami and Qt, et.al.
                           "-DSQLITE_ENABLE_COLUMN_METADATA"))
      #:phases (modify-phases %standard-phases
                 (add-after 'install 'move-static-library
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let* ((out    (assoc-ref outputs "out"))
                            (static (assoc-ref outputs "static"))
                            (source (string-append out "/lib/libsqlite3.a")))
                       (mkdir-p (string-append static "/lib"))
                       (link source (string-append static "/lib/libsqlite3.a"))
                       (delete-file source)

                       ;; Remove reference to the static library from the .la file
                       ;; so that Libtool looks for it in the usual places.
                       (substitute* (string-append out "/lib/libsqlite3.la")
                         (("^old_library=.*")
                          "old_library=''\n"))
                       #t))))))
   (home-page "https://www.sqlite.org/")
   (synopsis "The SQLite database management system")
   (description
    "SQLite is a software library that implements a self-contained, serverless,
zero-configuration, transactional SQL database engine.  SQLite is the most
widely deployed SQL database engine in the world.  The source code for SQLite
is in the public domain.")
   (license license:public-domain)))

(define-public leveldb-static
  (package
    (inherit leveldb)
    (name "leveldb-static")
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags
           #~(list ;"-DBUILD_SHARED_LIBS=ON"
          "-DLEVELDB_BUILD_TESTS=ON"
                   "-DCMAKE_BUILD_TYPE=Release"
                   ;; Don't install(!) the third_party test frameworks below.
                   "-DINSTALL_GTEST=OFF"
                   "-DBENCHMARK_ENABLE_INSTALL=OFF"
                   )
           #:phases
           #~(modify-phases %standard-phases
               ;; Ceph uses leveldb and depends on RTTI.
               (add-after 'unpack 'allow-RTTI
                 (lambda _
                   (substitute* "CMakeLists.txt"
                     (("set\\(CMAKE_CXX_FLAGS \"\\$\\{CMAKE_CXX_FLAGS\\}  -fPIC\"\\)") ""))))
               (add-after 'unpack 'unpack-third_party-sources
                 ;; These are only for testing, so copying source is fine.
                 (lambda _
                   (copy-recursively #$(package-source googletest)
                                     "third_party/googletest")
                   (copy-recursively #$(package-source googlebenchmark)
                                     "third_party/benchmark"))))))))
