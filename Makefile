# Commands:
#	make			= builds program statically
#	make static		= builds program statically
#	make dynamic	= builds program dynamically
#	make all		= builds everything (builds program statically)
#	make clean		= cleans up the build but does not remove the sandbox
#	make clean-tmp	= same as clean but does not remove the executable
#	make clean-all	= same as clean but also removes the sandbox


### Configuration
################################################################################

# Program Name
PROGRAM=lorenz

# Sources
SOURCES=src/Main.hs \
        src/Lorenz/App.hs \
        src/Lorenz/SDL.hs \
        src/Lorenz/OpenGL.hs \
        src/Lorenz/Loop.hs \
        src/Lorenz/Events.hs \
        src/Lorenz/Logic.hs \
        src/Lorenz/Data.hs

################################################################################


### Internal Variables
################################################################################
static: GHCFLAGS=-O2
dynamic: GHCFLAGS=-O2 -dynamic

SDL_VERSION="SDL-0.6.5"

export CABAL_SANDBOX_CONFIG=$(PWD)/cabal.sandbox.config
################################################################################


### Rules
################################################################################

.PHONY: all
all: static

# Make a static binary.
.PHONY: static
static: sandbox $(PROGRAM)

# Make a dynamic binary.
.PHONY: dynamic
dynamic: sandbox $(PROGRAM)

# Install a cabal sandbox.
.PHONY: sandbox
sandbox: .cabal-sandbox

# Clean up the build.
.PHONY: clean 
clean: clean-tmp
	rm -f $(PROGRAM)

# Clean up the intermediate build files.
.PHONY: clean-tmp
clean-tmp:
	cabal clean

# Clean up all files including the binary and sandbox.
.PHONY: clean-all
clean-all: clean clean-sandbox

################################################################################


### Internal Rules
################################################################################

# The manual build of SDL is necessary as it needs to be patched.
.cabal-sandbox:
	cabal sandbox init
	cabal unpack $(SDL_VERSION)
	patch $(SDL_VERSION)/Graphics/UI/SDL/Events.hsc < src/Events.patch
	cd $(SDL_VERSION) && cabal --require-sandbox install --jobs
	rm -Rf $(SDL_VERSION)
	cabal --require-sandbox install --jobs --only-dependencies

.PHONY: clean-sandbox
clean-sandbox:
	cabal sandbox delete

dist:
	cabal configure --ghc-options="$(GHCFLAGS)"

$(PROGRAM): dist/build/$(PROGRAM)/$(PROGRAM)
	cp dist/build/$(PROGRAM)/$(PROGRAM) $(PROGRAM)

dist/build/$(PROGRAM)/$(PROGRAM): $(SOURCES) dist
	cabal --require-sandbox install --jobs --only-dependencies
	cabal build

################################################################################
