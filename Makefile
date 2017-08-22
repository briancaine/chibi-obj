all : package

bcaine-misc-util.placeholder : src/bcaine/misc-util.sld src/bcaine/misc-util.scm src/bcaine/misc-util.version src/bcaine/misc-util.desc
	snow-chibi package --version=`cat src/bcaine/misc-util.version` --description="`cat src/bcaine/misc-util.desc`" src/bcaine/misc-util.sld
	touch bcaine-misc-util.placeholder

bcaine-format.placeholder : src/bcaine/format.sld src/bcaine/format.scm src/bcaine/format.desc src/bcaine/format.version
	snow-chibi package --version=`cat src/bcaine/format.version` --description="`cat src/bcaine/format.desc`" src/bcaine/format.sld
	touch bcaine-format.placeholder

bcaine-sld-stub-expand.placeholder : src/bcaine/sld-stub-expand.sld src/bcaine/sld-stub-expand.scm src/bcaine/sld-stub-expand.version src/bcaine/sld-stub-expand.desc
	snow-chibi package --version=`cat src/bcaine/sld-stub-expand.version` --description="`cat src/bcaine/sld-stub-expand.desc`" src/bcaine/sld-stub-expand.sld
	touch bcaine-sld-stub-expand.placeholder

bcaine-obj.placeholder : src/bcaine/obj.sld src/bcaine/obj-classes.scm src/bcaine/obj-default-classes.scm src/bcaine/obj-methods-internal.scm src/bcaine/obj-methods-syntax.scm src/bcaine/obj.version src/bcaine/obj.desc
	snow-chibi package --version=`cat src/bcaine/obj.version` --description="`cat src/bcaine/obj.desc`" src/bcaine/obj.sld
	touch bcaine-obj.placeholder

bcaine-defstruct.placeholder : src/bcaine/defstruct.sld src/bcaine/defstruct.scm src/bcaine/defstruct.version src/bcaine/defstruct.desc
	snow-chibi package --version=`cat src/bcaine/defstruct.version` --description="`cat src/bcaine/defstruct.desc`" src/bcaine/defstruct.sld
	touch bcaine-defstruct.placeholder

package : bcaine-misc-util.placeholder bcaine-format.placeholder bcaine-sld-stub-expand.placeholder bcaine-obj.placeholder bcaine-defstruct.placeholder

clean :
	rm -f *.tgz *.placeholder
