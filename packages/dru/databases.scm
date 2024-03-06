;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2016, 2018, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2012, 2013, 2014, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2017 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014, 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015, 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Leo Famulari <leo@famulari.name>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2016, 2022 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2016 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2015-2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016, 2017, 2018 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2016 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2016-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017, 2018 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2017, 2020 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2017 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2017, 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2017, 2018 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017, 2018, 2019 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2015, 2017, 2018, 2019, 2021, 2022, 2023, 2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Kristofer Buffington <kristoferbuffington@gmail.com>
;;; Copyright © 2018 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2018 Joshua Sierles, Nextjournal <joshua@nextjournal.com>
;;; Copyright © 2018, 2021, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2019 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2019, 2021, 2022 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020, 2021 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020 Lars-Dominik Braun <ldb@leibniz-psychology.org>
;;; Copyright © 2020 Guy Fleury Iteriteka <gfleury@disroot.org>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2021, 2024 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 David Larsson <david.larsson@selfhosted.xyz>
;;; Copyright © 2021 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2021 Bonface Munyoki Kilyungi <me@bonfacemunyoki.com>
;;; Copyright © 2021 Simon Streit <simon@netpanic.org>
;;; Copyright © 2021 Alexandre Hannud Abdo <abdo@member.fsf.org>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2022 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2022 muradm <mail@muradm.net>
;;; Copyright © 2022 Thomas Albers Raviola <thomas@thomaslabs.org>
;;; Copyright © 2021, 2022 jgart <jgart@dismail.de>
;;; Copyright © 2023 Felix Gruber <felgru@posteo.ne
;;; Copyright © 2023 Munyoki Kilyungi <me@bonfacemunyoki.com>
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2024 Drew Crampsie <me@drewc.ca>
;;;
;;; This file is part of Druix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (dru databases)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages language)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages openstack)
  #:use-module (gnu packages pantheon)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages perl-web)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix bzr-download)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system ruby)
  #:use-module (guix build-system cmake)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))


;; Don't forget to update the other postgresql packages when upgrading this one.

(define-public postgresql-16
  (package
    (name "postgresql")
    (version "16.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ftp.postgresql.org/pub/source/v"
                                  version "/postgresql-" version ".tar.bz2"))
              (sha256
               (base32 "0li9kfnybhkilrg1slh3xi5lnq7s8sk62w2bmf2r0b5w9llqhvj4"))
              #;(patches (search-patches "postgresql-disable-resolve_symlinks.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--with-uuid=e2fs" "--with-openssl"
              (string-append "--mandir=" #$output "/share/man")
              ;; PostgreSQL installs its own Makefile (should it?).
              ;; Prevent it from retaining needless references to
              ;; the build tools in order to save size.
              "MKDIR_P=mkdir -p" "INSTALL_BIN=install -c"
              "LD=ld" "TAR=tar")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'patch-/bin/sh
            (lambda _
              ;; Refer to the actual shell.
              (substitute* '("src/bin/pg_ctl/pg_ctl.c"
                             "src/bin/psql/command.c")
                (("/bin/sh") (which "sh")))))
          (add-before 'configure 'set-socket-dir
            (lambda _
              (substitute* '("src/include/pg_config_manual.h")
                (("DEFAULT_PGSOCKET_DIR[^\n]*")
                 "DEFAULT_PGSOCKET_DIR \"/var/run/postgresql\""))))
          (add-after 'build 'build-contrib
            (lambda _
              (invoke "make" "-C" "contrib")))
          (add-after 'install 'install-contrib
            (lambda _
              (invoke "make" "-C" "contrib" "install")))
          (add-after 'install 'install-manuals
            (lambda _
              (with-directory-excursion "doc/src/sgml"
                (invoke "make" "install-man")
                (invoke "make" "postgres.info")
                (install-file "postgres.info"
                              (string-append #$output "/share/info"))))))))
    (native-inputs (list pkg-config icu4c docbook-xml-4.5 docbook2x libxml2 perl texinfo))
    (inputs (list readline `(,util-linux "lib") icu4c openssl zlib))
    (home-page "https://www.postgresql.org/")
    (synopsis "Powerful object-relational database system")
    (description
     "PostgreSQL is a powerful object-relational database system.  It is fully
ACID compliant, has full support for foreign keys, joins, views, triggers, and
stored procedures (in multiple languages).  It includes most SQL:2008 data
types, including INTEGER, NUMERIC, BOOLEAN, CHAR, VARCHAR, DATE, INTERVAL, and
TIMESTAMP.  It also supports storage of binary large objects, including
pictures, sounds, or video.")
    (license (license:x11-style "file://COPYRIGHT"))))

(define-public plsh
  (package
   (name "plsh")
   (version "1.20220917")
   (source (origin
            (method url-fetch)
            (uri (string-append  "https://github.com/petere/plsh/archive/refs/tags/"
                                 version ".tar.gz"))
            (sha256
             (base32 "1nyamw8ci3ifcylncm3p70rkc25ln82l1w9wm50gsml7m742djag"))))
   (build-system gnu-build-system)
   (arguments
    `(#:tests? #f
      #:make-flags
      (list (string-append "datadir=" (assoc-ref %outputs "out") "/share")
            (string-append "docdir="(assoc-ref %outputs "out") "/share/doc")
            (string-append "pkglibdir="(assoc-ref %outputs "out") "/lib")
            (string-append "bindir=" (assoc-ref %outputs "out") "/bin"))
      #:phases
      (modify-phases %standard-phases (delete 'configure))))
    (inputs (list postgresql-16))
    #;(native-inputs (list perl pkg-config))
   (home-page "https://github.com/petere/plsh")
   (synopsis "Procedural language handler for PostgreSQL")
   (description "PL/sh is a procedural language handler for PostgreSQL that allows you
to write stored procedures in a shell of your choice. ")
   (license (list license:expat))))
