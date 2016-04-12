SHELL = /bin/sh


SUBS = lib fwd coast collocation scenedump bufr/bufrlib bufr/amv_bufr

all:
	- PWD=`pwd`;\
	for i in ${SUBS}; do \
		echo '<<<<' $$i '>>>>'; \
		cd ${PWD}/$$i; \
		make $@; \
	done


clean:
	- PWD=`pwd`;\
	for i in ${SUBS}; do \
		echo '<<<<' $$i '>>>>'; \
		cd ${PWD}/$$i; \
		make $@; \
	done
