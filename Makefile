all: gilo

SRC := $(wildcard *.go) cmd/editor/main.go

gilo: $(SRC) go.mod
	cd cmd/editor && go build -mod=vendor -o ../../gilo

clean:
	rm gilo
