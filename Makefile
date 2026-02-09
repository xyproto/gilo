all: kilo

kilo: ioctl_darwin.go ioctl_linux.go kilo.go cmd/editor/main.go go.mod
	cd cmd/editor && go build -mod=vendor -o ../../kilo

clean:
	rm kilo
