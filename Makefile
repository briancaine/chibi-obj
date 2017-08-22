all : package

bcaine-misc-util-%.tgz : src/bcaine/misc-util.sld src/bcaine/misc-util.scm src/bcaine/misc-util.version src/bcaine/misc-util.desc
	snow-chibi package --version=`cat src/bcaine/misc-util.version` --description="`cat src/bcaine/misc-util.desc`" src/bcaine/misc-util.sld

bcaine-format-%.tgz : src/bcaine/format.sld src/bcaine/format.scm src/bcaine/format.desc src/bcaine/format.version
	snow-chibi package --version=`cat src/bcaine/format.version` --description="`cat src/bcaine/format.desc`" src/bcaine/format.sld

bcaine-sld-stub-expand-%.tgz : src/bcaine/sld-stub-expand.sld src/bcaine/sld-stub-expand.scm src/bcaine/sld-stub-expand.version src/bcaine/sld-stub-expand.desc
	snow-chibi package --version=`cat src/bcaine/sld-stub-expand.version` --description="`cat src/bcaine/sld-stub-expand.desc`" src/bcaine/sld-stub-expand.sld

bcaine-obj-%.tgz : src/bcaine/obj.sld src/bcaine/obj-classes.scm src/bcaine/obj-default-classes.scm src/bcaine/obj-methods-internal.scm src/bcaine/obj-methods-syntax.scm src/bcaine/obj.version src/bcaine/obj.desc
	snow-chibi package --version=`cat src/bcaine/obj.version` --description="`cat src/bcaine/obj.desc`" src/bcaine/obj.sld

bcaine-defstruct-%.tgz : src/bcaine/defstruct.sld src/bcaine/defstruct.scm src/bcaine/defstruct.version src/bcaine/defstruct.desc
	snow-chibi package --version=`cat src/bcaine/defstruct.version` --description="`cat src/bcaine/defstruct.desc`" src/bcaine/defstruct.sld

package : bcaine-misc-util-%.tgz bcaine-format-%.tgz bcaine-sld-stub-expand-%.tgz bcaine-obj-%.tgz bcaine-defstruct-%.tgz

clean :
	rm -f *.tgz
