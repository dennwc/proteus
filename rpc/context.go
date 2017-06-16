package rpc

import (
	"fmt"
	"go/types"
	"strings"

	"gopkg.in/src-d/proteus.v1/protobuf"
)

type context struct {
	proto   *protobuf.Package
	pkg     *types.Package
	imports []string
}

func (c *context) isNameDefined(name string) bool {
	for _, n := range c.pkg.Scope().Names() {
		if n == name {
			return true
		}
	}
	return false
}

func (c *context) findMessage(name string) *protobuf.Message {
	for _, m := range c.proto.Messages {
		if m.Name == name {
			return m
		}
	}
	return nil
}

func (c *context) findSignature(rpc *protobuf.RPC) *types.Signature {
	var fn types.Object
	if rpc.Recv != "" {
		recv := c.pkg.Scope().Lookup(rpc.Recv)
		fn, _, _ = types.LookupFieldOrMethod(recv.Type(), true, c.pkg, rpc.Method)
	} else {
		fn = c.pkg.Scope().Lookup(rpc.Method)
	}

	return fn.Type().(*types.Signature)
}

func (c *context) argumentType(rpc *protobuf.RPC) string {
	signature := c.findSignature(rpc)
	skip := 0
	if rpc.HasCtx {
		skip++
	}
	obj := firstTypeName(skip, signature.Params())
	c.addImport(obj.Pkg().Path())

	return c.objectNameInContext(obj)
}

func (c *context) returnType(rpc *protobuf.RPC) string {
	signature := c.findSignature(rpc)
	obj := firstTypeName(0, signature.Results())
	c.addImport(obj.Pkg().Path())

	return c.objectNameInContext(obj)
}

// objectNameInContext returns the name of the object prefixed by its package name
// if needed
func (c *context) objectNameInContext(obj types.Object) string {
	if c.pkg.Path() == obj.Pkg().Path() {
		return obj.Name()
	} else {
		return fmt.Sprintf("%s.%s", obj.Pkg().Name(), obj.Name())
	}
}

func firstTypeName(skip int, tuple *types.Tuple) types.Object {
	t := tuple.At(skip).Type()
	if inner, ok := t.(*types.Pointer); ok {
		t = inner.Elem()
	}
	return t.(*types.Named).Obj()
}

func (c *context) pkgPath() string {
	if c.pkg != nil {
		return c.pkg.Path()
	}

	return ""
}

func (c *context) addImport(path string) {
	if path == c.pkgPath() {
		return
	}

	for _, i := range c.imports {
		if i == path {
			return
		}
	}
	c.imports = append(c.imports, path)
}

func firstUpper(n string) string {
	return strings.ToUpper(string(n[0])) + n[1:]
}

func firstLower(n string) string {
	return strings.ToLower(string(n[0])) + n[1:]
}

func serverImplName(n string) string {
	return firstLower(n) + "Server"
}

func serverInfName(n string) string {
	return firstUpper(n) + "Server"
}

func clientInfName(n string) string {
	return firstUpper(n) + "Client"
}

func clientImplName(n string) string {
	// name xxxClient is already taken by protobuf codegen
	return firstLower(n) + "Remote"
}

func serverConstructorName(n string) string {
	return fmt.Sprintf("New%sServer", firstUpper(n))
}

func clientConstructorName(n string) string {
	// name NewXxxClient is already taken by protobuf codegen
	return fmt.Sprintf("New%sRemote", firstUpper(n))
}
