# Generated automatically from Makefile.in by configure.

## cvs -d:pserver:anoncvs@subversions.gnu.org:/cvsroot/mldonkey login

include ../../config/Makefile.config

all: rpm

cvs-checkout:
	mkdir -p mldonkey-cvs
	cd mldonkey-cvs;cvs -z3 -d:pserver:anoncvs@subversions.gnu.org:/cvsroot/mldonkey co mldonkey

sourcedist:
	rm -rf mldonkey
	cp -dpR mldonkey-cvs/mldonkey mldonkey
	cd mldonkey; find . -name CVS -exec rm -rf {} \; || echo OK
	cp mldonkey.spec mldonkey/mldonkey.spec
	tar jcf mldonkey.sources.tar.bz2 mldonkey

rpm: sourcedist
	rpm -ta mldonkey.sources.tar.bz2
