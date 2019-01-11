NAME=cipher
SOURCES=cipher.ml uncipher.ml


all: $(NAME)

$(NAME) :
	ocamlopt $(SOURCES) -o $(NAME)

clean :
	/bin/rm -f cipher.cmx
	/bin/rm -f cipher.cmi
	/bin/rm -f cipher.o
	/bin/rm -f uncipher.cmx
	/bin/rm -f uncipher.cmi
	/bin/rm -f uncipher.o

fclean : clean
	/bin/rm -f $(NAME)

re : fclean $(NAME)
