SRC	= main.hs HaskttpdConfig.hs HaskttpdHandler.hs HaskttpdParser.hs

HFLAGS	=		
HLDFLAGS=	
NAME	= haskttpd
RUNOPTS	= 

HC	= ghc
HLD	= ghc

HOBJ	= $(SRC:.hs=.o)

$(NAME): $(SRC)
	$(HLD) $(HLDFLAGS) -o $(NAME) --make $(SRC)

clean:
	rm -f $(HOBJ)
	rm -f *.hi

distclean: clean
	rm -f $(NAME)

re: distclean $(NAME)

.PHONY: clean distclean re
