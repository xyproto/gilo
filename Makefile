all: kilo

SRC := $(wildcard *.go) cmd/editor/main.go

kilo: $(SRC) go.mod
	cd cmd/editor && go build -mod=vendor -o ../../kilo

clean:
	rm kilo
