(in-package :ri)

(defparameter *current-manifest* *load-truename*)

(flush-repos)

;; forward declare some packages that we need to reference below, but that may not be loaded yet
(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (not (find-package :lift))
      (defpackage lift)))

;; This manifest contains references to the newest versions of the libraries that are available.

(make-instance
 'cliki-repo
 :name :xmls
 :strip-components 1)

(make-instance
 'git-repo
 :name :repo-install
 :url "git://github.com/jpalmucci/repo-install.git")

(make-instance
 'git-repo
 :name :asdf
 :additional-packages '(:uiop)
 :url "http://common-lisp.net/project/asdf/asdf.git")

(make-instance
 'git-repo
 :name :osicat
 :url "http://common-lisp.net/project/osicat/git/osicat.git")

(make-instance
 'git-repo
 :name :trivial-http
 :url "git://github.com/gwkkwg/trivial-http")

;; add all darcs repos from http://common-lisp.net/~loliveira/ediware/
(mapcar #'(lambda (name)
	    (make-instance
	     'darcs-repo
	     :name name
	     :url (concatenate 'string
			       "http://common-lisp.net/~loliveira/ediware/"
			       (string-downcase (string name)))))

	'(:chunga
	  :cl-fad
	  :cl-gd
	  :cl-interpol
	  :cl-ppcre
	  :cl-who
	  :drakma
	  :flexi-streams
	  :html-template
	  :hunchentoot
	  :url-rewrite
	  :cl-unicode
	  :capi-overview
	  :cl-dongle
	  :cl-wbxml
	  :documentation-template
	  :fm-plugin-tools
	  :html-extract
	  :lw-add-ons
	  :lw-doc
	  :lw-win
	  :midgets
	  :odd-streams
	  :rdnzl
	  :regex-plugin
	  :cl-webdav))

(make-instance 
 'git-repo
 :name :stdutils
 :url "https://github.com/eslick/cl-stdutils.git")

(make-instance
 'git-repo
 :name :trivial-shell
 :url "git://github.com/gwkkwg/trivial-shell")

(make-instance
 'svn-repo
 :name :usocket
 :url "svn://common-lisp.net/project/usocket/svn/usocket/trunk")

(make-instance
 'cliki-repo
 :name :split-sequence
 :strip-components 1)

;; forward declare the test package
(defpackage cl-ppcre-test)
(make-instance
 'darcs-repo
 :name :cl-ppcre
 :additional-packages '(:cl-ppcre-test :cl-ppcre-unicode)
 :url "http://common-lisp.net/~loliveira/ediware/cl-ppcre"
 #+ignore
 :tester
 #+ignore
 #'(lambda ()
	     (asdf:load-system :cl-ppcre-test)
	     (cl-ppcre-test::test)))

;; packages ubove this point are required for repo-installer

(make-instance
 'git-repo
 :name :cffi
 :additional-packages '(:cffi-grovel)
 :url "https://github.com/cffi/cffi.git")

(make-instance
 'git-repo
 :name :babel
 :url "git://github.com/cl-babel/babel.git"
 :additional-packages '(:babel-tests)
 :tester (lambda ()
	   (ri:install :babel-tests)
	   (parse-stefil-results (lambda ()
				   (asdf:oos 'asdf:test-op :babel-tests)))))

(make-instance
 'git-repo
 :name :alexandria
 :url "git://common-lisp.net/projects/alexandria/alexandria.git"
 :additional-packages '(:alexandria-tests)
 :tester (lambda ()
	   (ri:install :alexandria-tests)
	   (parse-rt-results (lambda ()
			       (asdf:oos 'asdf:test-op :alexandria-tests)))))

(make-instance
 'git-repo
 :name :gsll
 :url "git://repo.or.cz/gsll.git")

(make-instance
 'cliki-repo
 :name :cl-utilities
 :strip-components 1)

;; obsolete
#+ignore
(make-instance
 'cliki-repo
 :name :cl-base64
 :strip-components 1)

(make-instance
 'git-repo
 :name :cl+ssl
 :url "https://github.com/cl-plus-ssl/cl-plus-ssl.git")

(make-instance
 'git-repo
 :name :trivial-gray-streams
 :url "https://github.com/trivial-gray-streams/trivial-gray-streams.git")

(make-instance
 'cvs-repo
 :name :trivial-https
 :cvsroot ":pserver:anonymous:anonymous@common-lisp.net:/project/cl-plus-ssl/cvsroot"
 :module "trivial-https")

(make-instance
 'git-repo
 :name :puri
 :url "git://github.com/lisp/com.b9.puri.ppcre.git")

(make-instance
 'git-repo
 :name :lispstat
 :url "git://repo.or.cz/CommonLispStat.git")

(make-instance
 'git-repo
 :name :rsm-string
 :url "http://git.debian.org/git/pkg-common-lisp/cl-rsm-string.git")

(make-instance
 'git-repo
 :name :lift
 :url "git://github.com/gwkkwg/lift")

(make-instance
 'git-repo
 :name :lisp-matrix
 :url "git://github.com/blindglobe/lisp-matrix.git")

(make-instance
 'git-repo
 :name :ffa
 :url "git://github.com/tpapp/ffa.git")

(make-instance
 'darcs-repo
 :name :iterate
 :url "http://www.common-lisp.net/project/iterate/darcs/iterate")

(make-instance
 'darcs-repo
 :name :metabang-bind
 :url "http://common-lisp.net/project/metabang-bind")

(make-instance
 'git-repo
 :name :bordeaux-threads
 :url "git://common-lisp.net/projects/bordeaux-threads/bordeaux-threads.git"
 :tester #'(lambda ()
	     (load (make-pathname :directory `(,@(butlast (pathname-directory *current-manifest*)) "bordeaux-threads")
		   :name "bordeaux-threads-test"
		   :type "lisp"))
	     (return-lift-results
	      (lift::run-tests :suite (lift::find-testsuite "TEST-BORDEAUX-THREADS")))))

;; obsolete
#+ignore
(make-instance
 'cvs-repo
 :name :rfc2388
 :cvsroot ":pserver:anonymous:anonymous@common-lisp.net:/project/rfc2388/cvsroot"
 :module "rfc2388")

(make-instance
 'cliki-repo
 :name :md5
 :strip-components 1)

(make-instance
 'darcs-repo
 :name :cl-containers
 :url "http://common-lisp.net/project/cl-containers")

(make-instance
 'darcs-repo
 :name :metatilities-base
 :url "http://common-lisp.net/project/metatilities-base")

(make-instance
 'darcs-repo
 :name :metatilities
 :url "http://common-lisp.net/project/metatilities")

(make-instance
 'darcs-repo
 :name :asdf-binary-locations
 :url "http://common-lisp.net/project/asdf-binary-locations")

(make-instance
 'cvs-repo
 :name :cl-smtp
 :cvsroot ":pserver:anonymous:anonymous@common-lisp.net:/project/cl-smtp/cvsroot"
 :module "cl-smtp")

(make-instance
 'cliki-repo
 :name :cl-qprint)

(make-instance
 'darcs-repo
 :name :trivial-email-utf-8
 :url "http://common-lisp.net/project/bpm/darcs/trivial-email-utf-8")

(make-instance
 'darcs-repo
 :name :cl-store
 :url "http://common-lisp.net/project/cl-store/darcs/cl-store")

(make-instance
 'svn-repo
 :name :cl-lex
 :url "http://cl-lex.googlecode.com/svn/trunk/")

(make-instance
 'git-repo
 :name :fare-csv
 :url "http://common-lisp.net/r/users/frideau/fare-csv.git")

(make-instance
 'git-repo
 :name "https://github.com/sharplispers/parse-number.git")

(make-instance
 'git-repo
 :name :closure-common
 :url "git://repo.or.cz/closure-common.git")

(make-instance
 'git-repo
 :name :cxml
 :url "git://repo.or.cz/cxml.git")

(make-instance
 'git-repo
 :name :cxml-stp
 :url "http://www.lichteblau.com/git/cxml-stp.git")

(make-instance
 'darcs-repo
 :name :xpath
 :url "http://common-lisp.net/project/plexippus-xpath/darcs/plexippus-xpath/")

(make-instance
 'git-repo
 :name :yacc
 :url "git://git.wifi.pps.jussieu.fr/cl-yacc")

(make-instance
 'git-repo
 :name :parse-number
 :url "https://github.com/sharplispers/parse-number")

(make-instance
 'git-repo
 :name :closure-html
 :url "git://repo.or.cz/closure-html.git")

(make-instance
 'git-repo
 :name :closure-common
 :url "git://repo.or.cz/closure-common.git")


(make-instance
 'darcs-repo
 :name :asdf-system-connections
 :url "http://common-lisp.net/project/asdf-system-connections")

(make-instance
 'darcs-repo
 :name :moptilities
 :url "http://common-lisp.net/project/moptilities")

(make-instance
 'darcs-repo
 :name :closer-mop
 :url "http://common-lisp.net/project/closer/repos/closer-mop")

(make-instance
 'darcs-repo
 :name :stefil
 :url "http://common-lisp.net/project/stefil/darcs/stefil")

(make-instance
 'svn-repo
 :name :html-entities
 :url "http://html-entities.googlecode.com/svn/trunk")

(make-instance
 'mercurial-repo
 :name :weblocks
 :url "http://bitbucket.org/skypher/weblocks-stable"
)

(make-instance
 'darcs-repo
 :name :f-underscore
 :url "http://common-lisp.net/project/bpm/darcs/f-underscore")

(make-instance
 'cvs-repo
 :name :anaphora
 :cvsroot ":pserver:anonymous:anonymous@common-lisp.net:/project/anaphora/cvsroot"
 :module "src")

(make-instance
 'git-repo
 :name :parenscript
 :url "http://common-lisp.net/project/parenscript/git/parenscript")

(make-instance
 'darcs-repo
 :name :cl-cont
 :url "http://common-lisp.net/project/cl-cont/darcs/cl-cont")

(make-instance
 'git-repo
 :name :fare-matcher
 :url "git://common-lisp.net/users/frideau/fare-matcher.git")

(make-instance
 'git-repo
 :name :fare-utils
 :url "http://common-lisp.net/r/users/frideau/fare-utils.git")

(make-instance
 'git-repo
 :name :cl-json
 :url "https://github.com/hankhero/cl-json.git")

(make-instance
 'git-repo
 :name :parse-js
 :url "http://marijnhaverbeke.nl/git/parse-js")

(make-instance
 'darcs-repo
 :name :postmodern
 :additional-packages '(:s-sql :cl-postgres)
 :url "http://common-lisp.net/project/postmodern/darcs/postmodern")

(make-instance
 'darcs-repo
 :name :trivial-utf-8
 :url "http://common-lisp.net/project/trivial-utf-8/darcs/trivial-utf-8")

(make-instance
 'darcs-repo
 :name :ieee-floats
 :url "http://common-lisp.net/project/ieee-floats/darcs/ieee-floats")

;; clsql website seems to be dead
#+ignore
(make-instance
 'git-repo
 :name :clsql
 :url "git://git.b9.com/clsql.git"
 :additional-packages '(:clsql-aodbc
			:clsql-db2
			:clsql-mysql
			:clsql-odbc
			:clsql-oracle
			:clsql-postgresql-socket
			:clsql-postgresql
			:clsql-sqlite
			:clsql-sqlite3
			:clsql-tests
			:clsql-uffi
			))

;; superceded by cffi (which has a uffi compatibility layer)
#+ignore
(make-instance
 'git-repo
 :name :uffi
 :url "git://git.b9.com/uffi.git")


(make-instance
 'git-repo
 :name :cl-future
 :url "git://github.com/jpalmucci/cl-future.git")

(make-instance
 'cvs-repo
 :name :cl-soap
 :cvsroot ":pserver:anonymous:anonymous@common-lisp.net:/project/cl-soap/cvsroot"
 :module "cl-soap")

(make-instance
 'cvs-repo
 :name :s-xml
 :cvsroot ":pserver:anonymous:anonymous@common-lisp.net:/project/s-xml/cvsroot"
 :module "s-xml")

(make-instance
 'darcs-repo
 :name :trivial-timeout
 :url "http://common-lisp.net/project/trivial-timeout"
)

(make-instance
 'cliki-repo
 :name :salza2)

(make-instance
 'darcs-repo
 :name :cl-twitter
 :url "http://www.common-lisp.net/project/cl-twitter/darcs/cl-twitter"
 :additional-packages '(:cl-twitter-db))

(make-instance
 'darcs-repo
 :name :elephant
 :url "http://www.common-lisp.net/project/elephant/darcs/elephant-1.0"
 :additional-packages '(:ele-bdb :ele-clp :ele-postmodern))

(make-instance
 'cvs-repo
 :name :portableaserve
 :cvsroot ":pserver:anonymous@portableaserve.cvs.sourceforge.net:/cvsroot/portableaserve"
 :module "portableaserve"
 :additional-packages '(:acl-compat))

(make-instance
 'git-repo
 :name :cl-mysql
 :url "git://github.com/hackinghat/cl-mysql.git")

(make-instance
 'darcs-repo
 :name :cl-perec
 :url "http://common-lisp.net/project/cl-perec/darcs/cl-perec")

(make-instance
 'darcs-repo
 :name :defclass-star
 :url "http://common-lisp.net/project/defclass-star/darcs/defclass-star")

(make-instance
 'darcs-repo
 :name :cl-syntax-sugar
 :url "http://common-lisp.net/project/cl-syntax-sugar/darcs/cl-syntax-sugar")

(make-instance
 'darcs-repo
 :name :cl-walker
 :url "http://common-lisp.net/project/cl-walker/darcs/cl-walker")

(make-instance
 'darcs-repo
 :name :cl-serializer
 :url "http://common-lisp.net/project/cl-serializer/darcs/cl-serializer")

(make-instance
 'git-repo
 :name :local-time
 :url "https://github.com/dlowe-net/local-time.git")

(make-instance
 'git-repo
 :name :ironclad
 :url "git://github.com/froydnj/ironclad.git")

(make-instance
 'darcs-repo
 :name :cl-yalog
 :url "http://www.common-lisp.net/project/cl-dwim/darcs/cl-yalog")

(make-instance
 'darcs-repo
 :name :cl-def
 :url "http://www.common-lisp.net/project/cl-def/darcs/cl-def/")

(make-instance
 'darcs-repo
 :name :metacopy
 :url "http://common-lisp.net/project/metacopy/darcs/metacopy")

(make-instance
 'darcs-repo
 :name :contextl
 :url "http://www.common-lisp.net/project/closer/darcs/contextl")

(make-instance
 'darcs-repo
 :name :lw-compat
 :url "http://www.common-lisp.net/project/closer/darcs/lw-compat")

(make-instance
 'darcs-repo
 :name :computed-class
 :url "http://dwim.hu/live/hu.dwim.computed-class")

(make-instance
 'darcs-repo
 :name :eager-future
 :url "http://common-lisp.net/project/eager-future/repository/eager-future")

(make-instance
 'cvs-repo
 :name :cl-plplot
 :cvsroot ":pserver:anonymous:anonymous@common-lisp.net:/project/cl-plplot/cvsroot"
 :module "cl-plplot")

;; obsolete
#+ignore
(make-instance
 'git-repo
 :name :cl-2d
 :url "git://github.com/tpapp/cl-2d.git")

(make-instance
 'git-repo
 :name :array-operations
 :url "git://github.com/tpapp/array-operations.git")

(make-instance
 'git-repo
 :name :cl-colors
 :url "git://github.com/tpapp/cl-colors.git")

(make-instance
 'git-repo
 :name :cl-cairo2
 :url "git://github.com/rpav/cl-cairo2.git")

(make-instance
 'cvs-repo
 :name :mcclim
 :cvsroot ":pserver:anonymous:anonymous@common-lisp.net:/project/mcclim/cvsroot"
 :module "mcclim"
 :additional-packages '(:clim-examples :clim-listener :clouseau)
 )

(make-instance
 'cvs-repo
 :name :flexichain
 :cvsroot ":pserver:anonymous:anonymous@common-lisp.net:/project/flexichain/cvsroot"
 :module "flexichain")

(make-instance
 'cvs-repo
 :name :climacs
 :cvsroot ":pserver:anonymous:anonymous@common-lisp.net:/project/climacs/cvsroot"
 :module "climacs")

(make-instance
 'git-repo
 :name :spatial-trees
 :url "https://github.com/rpav/spatial-trees")

(make-instance
 'darcs-repo
 :name :clx
 :url "http://common-lisp.net/~crhodes/clx")

(make-instance
 'cvs-repo
 :name :cl-prevalence
 :cvsroot ":pserver:anonymous:anonymous@common-lisp.net:/project/cl-prevalence/cvsroot"
 :module "cl-prevalence")

(make-instance
 'darcs-repo
 :name :s-sysdeps
 :url "http://www.beta9.be/darcs/s-sysdeps")

(make-instance
 'git-repo
 :name :routes
 :additional-packages '(:routes.unify :routes-test)
 :url "git://github.com/archimag/cl-routes.git")

(make-instance
 'cvs-repo
 :name :swank
 :cvsroot ":pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot"
 :module "slime")

(make-instance
 'git-repo
 :name :trivial-backtrace
 :url "http://common-lisp.net/project/trivial-backtrace/trivial-backtrace.git")

(make-instance
 'git-repo
 :name :trivial-features
 :url "http://github.com/trivial-features/trivial-features")

(make-instance
 'git-repo
 :name :trivial-garbage
 :url "https://github.com/trivial-garbage/trivial-garbage.git")

(make-instance
 'git-repo
 :name :cl-uri-templates
 :url "http://github.com/billitch/cl-uri-templates.git")

(make-instance
 'git-repo
 :name :stumpwm
 :url "git://git.savannah.nongnu.org/stumpwm.git")

(make-instance
 'git-repo
 :name :libfixposix
 :url "git://gitorious.org/libfixposix/libfixposix.git")

(make-instance
 'git-repo
 :name :iolib
 :additional-packages '(:iolib-grovel
			:iolib.base
			:iolib.multiplex
			:iolib.os
			:iolib.pathnames
			:iolib.sockets
			:iolib.streams
			:iolib.syscalls
			:iolib.trivial-sockets
			:libfixposix)
 :url "git://gitorious.org/iolib/iolib.git")

(make-instance
 'git-repo
 :name :bencode
 :url "git://github.com/nja/cl-bencode.git")

(make-instance
 'cliki-repo
 :name :cl-log)

(make-instance
 'git-repo
 :name :com.informatimago.common-lisp.llrbtree
 :url "http://github.com/billitch/com.informatimago.common-lisp.llrbtree.git")

(make-instance
 'git-repo
 :name :lessp
 :url "git@github.com:billitch/lessp.git")

(make-instance
 'git-repo
 :name :lowh-facts
 :url "git@github.com:billitch/lowh-facts.git")

(make-instance
 'git-repo
 :name :http-pool
 :url "git://github.com/billitch/http-pool.git")

(make-instance
 'git-repo
 :name :xml-emitter
 :url "git@github.com:billitch/xml-emitter.git")

(make-instance
 'git-repo
 :name :xml-plist-emitter
 :url "git@github.com:billitch/xml-plist-emitter.git")

(make-instance
 'git-repo
 :name :sb-fastcgi
 :url "git@github.com:KDr2/sb-fastcgi.git")

(make-instance
 'git-repo
 :name :idna
 :url "git://github.com/antifuchs/idna.git")

(make-instance
 'git-repo
 :name :cl-ffmpeg
 :url "git://github.com/sykopomp/cl-ffmpeg.git")

(make-instance
 'git-repo
 :name :cl-uglify-js
 :url "git://github.com/mishoo/cl-uglify-js.git")

(make-instance
 'darcs-repo
 :name :cl-uri
 :url "http://common-lisp.net/project/cl-uri/darcs/cl-uri/")

(make-instance
 'git-repo
 :name :cl-mime
 :url "git://github.com/billitch/cl-mime.git")

(make-instance
 'git-repo
 :name :cl-ffmpeg
 :url "git://github.com/sykopomp/cl-ffmpeg.git")

(make-instance
 'git-repo
 :name :external-program
 :url "git://github.com/sellout/external-program.git")

(make-instance
 'git-repo
 :name :exec-js
 :url "git://github.com/billitch/exec-js.git")

(make-instance
 'git-repo
 :name :cl-skip-list
 :url "git://github.com/kraison/cl-skip-list.git")

(make-instance
 'git-repo
 :name :rollback
 :url "https://github.com/billitch/rollback.git")

(make-instance
 'git-repo
 :name :nibbles
 :url "https://github.com/froydnj/nibbles.git")

(make-instance
 'git-repo
 :name :clws
 :url "https://github.com/3b/clws.git")

(make-instance
 'darcs-repo
 :name :chtml-matcher
 :url "http://www.common-lisp.net/project/chtml-matcher/darcs/chtml-matcher")

(make-instance
 'git-repo
 :name :inferior-shell
 :url "http://common-lisp.net/r/projects/qitab/inferior-shell.git")

(make-instance
 'git-repo
 :name :css-selectors
 :url "http://github.com/AccelerationNet/css-selectors.git")

(make-instance
 'git-repo
 :name :buildnode
 :url "http://github.com/AccelerationNet/buildnode.git")

(make-instance
 'git-repo
 :name :symbol-munger
 :url "http://github.com/AccelerationNet/symbol-munger.git")

(make-instance
 'git-repo
 :name :collectors
 :url "http://github.com/AccelerationNet/collectors.git")

(make-instance
 'cliki-repo
 :name :cl-xmlspam)

(make-instance
 'git-repo
 :name :optima
 :url "http://github.com/m2ym/optima.git")

(make-instance
 'git-repo
 :name :cl-pdf
 :additional-packages '(:cl-pdf-parser)
 :url "http://github.com/archimag/cl-pdf.git")

(make-instance
 'git-repo
 :name :zpb-ttf
 :url "https://github.com/xach/zpb-ttf.git")

(make-instance
 'cliki-repo
 :name :chipz)

(make-instance
 'git-repo
 :name :spatial-trees
 :url "https://github.com/rpav/spatial-trees.git")

(make-instance
 'git-repo
 :name :chronicity
 :url "https://github.com/chaitanyagupta/chronicity.git")

(make-instance
 'git-repo
 :name :cl-markup
 :url "https://github.com/arielnetworks/cl-markup.git")

(make-instance
 'git-repo
 :name :rw-ut
 :url "https://github.com/nallen05/rw-ut.git")

(make-instance
 'git-repo
 :name :cl-bcrypt
 :url "https://github.com/billitch/cl-bcrypt.git")

(make-instance
 'git-repo
 :name :oauth2
 :url "https://github.com/Neronus/oauth2.git")

(make-instance
 'git-repo
 :name :gravatar
 :url "https://github.com/sellout/cl-gravatar.git")

(make-instance
 'git-repo
 :name :fiveam
 :url "https://github.com/sionescu/fiveam.git")

(make-instance
 'git-repo
 :name :can
 :additional-packages '(:L>can)
 :url "https://github.com/LowH/cl-can.git")

(make-instance
 'git-repo
 :name :do-urlencode
 :url "https://github.com/drdo/do-urlencode.git")

(make-instance
 'git-repo
 :name :log4cl
 :url "https://github.com/7max/log4cl")

(make-instance
 'git-repo
 :name :cl-libevent2
 :additional-packages '(:cl-libevent2-ssl)
 :url "https://github.com/orthecreedence/cl-libevent2.git")

(make-instance
 'git-repo
 :name :cl-async
 :additional-packages '(:cl-async-ssl)
 :url "https://github.com/orthecreedence/cl-async.git")

(make-instance
 'git-repo
 :name :cl-async-future
 :url "https://github.com/orthecreedence/cl-async-future.git")

(make-instance
 'git-repo
 :name :http-parse
 :url "https://github.com/orthecreedence/http-parse.git")

(make-instance
 'git-repo
 :name :wookie
 :url "https://github.com/orthecreedence/wookie.git")

(make-instance
 'git-repo
 :name :cl-inflector
 :url "https://github.com/AccelerationNet/cl-inflector.git")

(make-instance
 'git-repo
 :name :cl-containers
 :url "https://github.com/gwkkwg/cl-containers.git")

(make-instance
 'git-repo
 :name :cl-markdown
 :url "https://github.com/gwkkwg/cl-markdown.git")

(make-instance
 'git-repo
 :name :dynamic-classes
 :url "https://github.com/gwkkwg/dynamic-classes.git")

(make-instance
 'git-repo
 :name :metabang-bind
 :url "https://github.com/gwkkwg/metabang-bind.git")

(make-instance
 'git-repo
 :name :metalities
 :url "https://github.com/gwkkwg/metalities.git")

(make-instance
 'git-repo
 :name :metalities-base
 :url "https://github.com/gwkkwg/metalities-base.git")
