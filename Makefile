all : package

package : src/bcaine/defstruct.sld src/bcaine/chicken-misc.sld src/bcaine/format.sld src/bcaine/obj.sld src/bcaine/format.scm src/bcaine/chicken-misc.scm src/bcaine/defstruct.scm src/bcaine/obj-methods-internal.scm src/bcaine/obj-methods-syntax.scm src/bcaine/obj-classes.scm src/bcaine/obj-default-classes.scm
	snow-chibi package --version=0.0.1 src/bcaine/defstruct.sld
	snow-chibi package --version=0.0.1 src/bcaine/chicken-misc.sld
	snow-chibi package --version=0.0.1 src/bcaine/format.sld
	snow-chibi package --version=0.0.1 src/bcaine/obj.sld

clean :
	rm -f *.tgz
