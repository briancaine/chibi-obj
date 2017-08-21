all : package

bcaine-misc-util-%.tgz : src/bcaine/misc-util.sld src/bcaine/misc-util.scm
	snow-chibi package --version=`cat src/bcaine/misc-util.version` --description="`cat src/bcaine/misc-util.desc`" src/bcaine/misc-util.sld

bcaine-format-0.0.1.tgz : src/bcaine/format.sld src/bcaine/format.scm
	snow-chibi package --version=0.0.1 src/bcaine/format.sld

bcaine-sld-stub-expand-0.0.1.tgz : src/bcaine/sld-stub-expand.sld src/bcaine/sld-stub-expand.scm
	snow-chibi package --version=0.0.1 src/bcaine/sld-stub-expand.sld

bcaine-obj-0.0.1.tgz : src/bcaine/obj.sld src/bcaine/obj-classes.scm src/bcaine/obj-default-classes.scm src/bcaine/obj-methods-internal.scm src/bcaine/obj-methods-syntax.scm
	snow-chibi package --version=0.0.1 src/bcaine/obj.sld

bcaine-defstruct-0.0.1.tgz : src/bcaine/defstruct.sld src/bcaine/defstruct.scm
	snow-chibi package --version=0.0.1 src/bcaine/defstruct.sld

package : bcaine-misc-util-%.tgz bcaine-format-0.0.1.tgz bcaine-sld-stub-expand-0.0.1.tgz bcaine-obj-0.0.1.tgz bcaine-defstruct-0.0.1.tgz

clean :
	rm -f *.tgz
