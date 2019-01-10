NAME = ft_graphics

SOURCES = ft_graphics.ml

CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep

LIBS = $(WITHGRAHICS)
WITHGRAPHICS = graphics.cma -cclib -lGraphics

all: depend $(NAME)

$(NAME): opt byt
	ln -s $(NAME).byt $(NAME)

opt: $(NAME).opt
byt: $(NAME).byt

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)

$(NAME).by: $(OBJS)
	$(CAMLC) -o $(NAME).byt $(LIBS) $(OBJS)

$(NAME).opt: $(OPTOBJS)
	$(CAMLOPT) -o $(NAME).opt $(LIBS:.cma=.cmxa) $(OPTOBJS)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

# CLEAN
#

depend: .depend
	$(CAMLDEP) $(SOURCES) > .depend
