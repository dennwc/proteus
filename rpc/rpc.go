package rpc // import "gopkg.in/src-d/proteus.v1/rpc"

import (
	"fmt"
	"go/ast"
	"go/printer"
	"go/token"
	"go/types"
	"os"
	"path/filepath"
	"strings"

	"gopkg.in/src-d/proteus.v1/protobuf"
	"gopkg.in/src-d/proteus.v1/report"

	"gopkg.in/src-d/go-parse-utils.v1"
)

// Generator generates implementations of an RPC server for a package.
// It will declare a new type with the following name: {serviceName}Server
// with the first letter in lower case. In case the type already exists in the
// package, it will not be generated again. The purpose of that is that you can
// customize the type, even though the methods are automatically generated.
// Same happens with the constructor. If it does not exist, a function named
// new{ServiceName}Server with no parameters and a single result of the type
// {serviceName}Server will be generated. It can be defined, to avoid a default
// implementation, but the same signature is required, as it is used for
// registering the server implementation.
//
// So, if you have a service named FooService, you can implement
// `fooServiceServer` and `func newFooServiceServer() *fooServiceServer`.
//
// All generated methods will use as receiver a field in the server
// implementation with the same name as the type of the receiver.
// For example, the method generated for `func (*Foo) Bar()` will be require
// that our `fooServiceServer` had a field with that name.
//
// 	type fooServiceServer struct {
//		Foo *Foo
//	}
//
// For every method you want to generate an RPC method for, you have to
// implement its receiver by yourself in the server implementation type and the
// constructor.
//
// A single file per package will be generated containing all the RPC methods.
// The file will be written to the package path and it will be named
// "server.proteus.go"
type Generator struct {
	importer *parseutil.Importer
}

// NewGenerator creates a new Generator.
func NewGenerator() *Generator {
	return &Generator{parseutil.NewImporter()}
}

// Generate creates a new file in the package at the given path and implements
// the server according to the given proto package.
func (g *Generator) Generate(proto *protobuf.Package, path string) error {
	if len(proto.Services) == 0 {
		report.Warn("no Services in the given proto file, not generating anything")
		return nil
	}

	pkg, err := g.importer.ImportWithFilters(
		path,
		parseutil.FileFilters{
			func(pkg, file string, typ parseutil.FileType) bool {
				return !strings.HasSuffix(file, ".pb.go")
			},
			func(pkg, file string, typ parseutil.FileType) bool {
				return !strings.HasSuffix(file, ".proteus.go")
			},
		},
	)
	if err != nil {
		return err
	}

	ctx := &context{
		proto: proto,
		pkg:   pkg,
	}

	var decls []ast.Decl
	for _, svc := range proto.Services {
		decls = append(decls, g.declServer(ctx, svc)...)
		if !svc.Global { // no client for global services
			decls = append(decls, g.declClient(ctx, svc)...)
		}
	}

	return g.writeFile(g.buildFile(ctx, decls), path)
}
func (g *Generator) declServer(ctx *context, svc *protobuf.Service) []ast.Decl {
	var decls []ast.Decl

	iname := serverImplName(svc.Name)

	if !svc.Global {
		decls = append(decls, staticInfCheck(serverInfName(svc.Name), iname))
	}
	if !ctx.isNameDefined(iname) {
		decls = append(decls, g.declServerImplType(iname, svc))
	}
	if name := serverConstructorName(svc.Name); !ctx.isNameDefined(name) {
		report.Warn("constructor %s for service %s is not implemented", iname, name)
		decls = append(decls, g.declServerConstructor(iname, name, svc))
	}

	for _, rpc := range svc.RPCs {
		decls = append(decls, g.declServerMethod(ctx, iname, rpc))
	}
	return decls
}
func (g *Generator) declClient(ctx *context, svc *protobuf.Service) []ast.Decl {
	var decls []ast.Decl

	iname := clientImplName(svc.Name)

	decls = append(decls, staticInfCheck(svc.Name, iname))

	if !ctx.isNameDefined(iname) {
		decls = append(decls, g.declClientImplType(ctx, iname, svc))
	}
	if name := clientConstructorName(svc.Name); !ctx.isNameDefined(name) {
		report.Warn("constructor %s for service %s is not implemented", iname, name)
		decls = append(decls, g.declClientConstructor(ctx, iname, name, svc))
	}

	for _, rpc := range svc.RPCs {
		decls = append(decls, g.declClientMethod(ctx, iname, rpc))
	}
	return decls
}

func (g *Generator) declServerTypeAssert(implName string, svc *protobuf.Service) ast.Decl {
	var flds *ast.FieldList
	if svc.Global {
		flds = fields()
	} else {
		flds = fields(&ast.Field{
			Names: idents(svc.Name),
			Type:  ast.NewIdent(svc.Name),
		})
	}
	return typeDecl(implName, &ast.StructType{
		Fields: flds,
	})
}

func (g *Generator) declServerImplType(implName string, svc *protobuf.Service) ast.Decl {
	var flds *ast.FieldList
	if svc.Global {
		flds = fields()
	} else {
		flds = fields(&ast.Field{
			Names: idents(svc.Name),
			Type:  ast.NewIdent(svc.Name),
		})
	}
	return typeDecl(implName, &ast.StructType{
		Fields: flds,
	})
}

const fieldNameOpts = "opts"

func (g *Generator) declClientImplType(ctx *context, implName string, svc *protobuf.Service) ast.Decl {
	return typeDecl(implName, &ast.StructType{
		Fields: fields(&ast.Field{
			Names: idents(svc.Name),
			Type:  ast.NewIdent(clientInfName(svc.Name)),
		}, &ast.Field{
			Names: idents(fieldNameOpts),
			Type:  &ast.ArrayType{Elt: g.packageSelector(ctx, grpcImport, "grpc", "CallOption")},
		}),
	})
}

func (g *Generator) declServerConstructor(implName, constructorName string, svc *protobuf.Service) ast.Decl {
	var (
		params  *ast.FieldList
		results *ast.FieldList
		elems   []ast.Expr
	)
	if svc.Global {
		params = fields()
		results = fields(&ast.Field{
			Type: ptr(ast.NewIdent(implName)),
		})
	} else {
		params = fields(&ast.Field{
			Names: idents("s"),
			Type:  ast.NewIdent(svc.Name),
		})
		results = fields(&ast.Field{
			Type: ast.NewIdent(serverInfName(svc.Name)),
		})
		elems = []ast.Expr{
			&ast.KeyValueExpr{
				Key:   ast.NewIdent(svc.Name),
				Value: ast.NewIdent("s"),
			},
		}
	}
	return simpleConstructor(constructorName, params, results, addr(&ast.CompositeLit{
		Type: ast.NewIdent(implName),
		Elts: elems,
	}))
}

func (g *Generator) packageSelector(ctx *context, imp, pkg, name string) ast.Expr {
	ctx.addImport(imp)
	return packageSelector(pkg, name)
}

const grpcImport = "google.golang.org/grpc"

func (g *Generator) declClientConstructor(ctx *context, implName, constructorName string, svc *protobuf.Service) ast.Decl {
	return simpleConstructor(constructorName, fields(&ast.Field{
		Names: idents("s"),
		Type:  ast.NewIdent(clientInfName(svc.Name)),
	}, &ast.Field{
		Names: idents("opts"),
		Type: &ast.Ellipsis{
			Elt: g.packageSelector(ctx, grpcImport, "grpc", "CallOption"),
		},
	}), fields(&ast.Field{ // return type
		Type: ast.NewIdent(svc.Name),
	}), addr(&ast.CompositeLit{ // results
		Type: ast.NewIdent(implName),
		Elts: []ast.Expr{
			kv(svc.Name, "s"),
			kv(fieldNameOpts, "opts"),
		},
	}))
}

func (g *Generator) genServerMethodType(ctx *context, rpc *protobuf.RPC) *ast.FuncType {
	var in, out string

	if isGenerated(rpc.Input) {
		in = typeName(rpc.Input)
	} else {
		in = ctx.argumentType(rpc)
	}

	if isGenerated(rpc.Output) {
		out = typeName(rpc.Output)
	} else {
		out = ctx.returnType(rpc)
	}

	return &ast.FuncType{
		Params: fields(
			field("ctx", ast.NewIdent("xcontext.Context")),
			field("in", ptr(ast.NewIdent(in))),
		),
		Results: fields(
			field("result", ptr(ast.NewIdent(out))),
			field("err", ast.NewIdent("error")),
		),
	}
}

func typeToExpr(ctx *context, t types.Type) ast.Expr {
	switch vt := t.(type) {
	case *types.Named:
		o := vt.Obj()
		name := ast.NewIdent(o.Name())
		if p := o.Pkg(); p != nil && p != ctx.pkg {
			return &ast.SelectorExpr{
				X:   ast.NewIdent(p.Name()),
				Sel: name,
			}
		}
		return name
	case *types.Basic:
		return ast.NewIdent(vt.Name())
	case *types.Pointer:
		return &ast.StarExpr{X: typeToExpr(ctx, vt.Elem())}
	case *types.Slice:
		return &ast.ArrayType{Elt: typeToExpr(ctx, vt.Elem())}
	default:
		panic(fmt.Errorf("unsupported type: %T", vt))
	}
}

func (g *Generator) tupleToFields(ctx *context, tu *types.Tuple, names bool) []*ast.Field {
	var out []*ast.Field
	for i := 0; i < tu.Len(); i++ {
		v := tu.At(i)
		name := ""
		if names {
			name = v.Name()
		}
		out = append(out, field(name, typeToExpr(ctx, v.Type())))
	}
	return out
}

func (g *Generator) genClientMethodType(ctx *context, rpc *protobuf.RPC) *ast.FuncType {
	signature := ctx.findSignature(rpc)
	typ := &ast.FuncType{
		Params: fields(g.tupleToFields(ctx, signature.Params(), true)...),
		// do not save return parameter names to prevent name collisions
		Results: fields(g.tupleToFields(ctx, signature.Results(), false)...),
	}
	if rpc.IsVariadic {
		i := len(typ.Params.List) - 1
		typ.Params.List[i].Type = &ast.Ellipsis{Elt: typ.Params.List[i].Type.(*ast.ArrayType).Elt}
	}
	return typ
}

func rpcFuncName(recv string, rpc *protobuf.RPC) ast.Expr {
	if rpc.Recv != "" {
		return ast.NewIdent(fmt.Sprintf("%s.%s.%s", recv, rpc.Recv, rpc.Method))
	}
	return ast.NewIdent(rpc.Method)
}

func genMsgReqFieldName(i int) string {
	return fmt.Sprintf("Arg%d", i+1)
}

func (g *Generator) genServerMethodCall(ctx *context, rpc *protobuf.RPC) ast.Expr {
	call := &ast.CallExpr{Fun: rpcFuncName(methRecvName, rpc)}

	if rpc.HasCtx {
		call.Args = append(call.Args, ast.NewIdent("ctx"))
	}
	if rpc.IsVariadic {
		call.Ellipsis = token.Pos(1)
	}

	if !isGenerated(rpc.Input) {
		var in ast.Expr = ast.NewIdent("in")
		if !rpc.Input.IsNullable() {
			in = &ast.StarExpr{
				X: in,
			}
		}
		call.Args = append(call.Args, in)
	} else {
		msg := ctx.findMessage(typeName(rpc.Input))
		for i := range msg.Fields {
			call.Args = append(call.Args, ast.NewIdent("in."+genMsgReqFieldName(i)))
		}
	}

	return call
}

func (g *Generator) genBaseMethodBody(methodType *ast.FuncType) *ast.BlockStmt {
	return &ast.BlockStmt{
		List: []ast.Stmt{
			&ast.AssignStmt{
				Tok: token.ASSIGN,
				Lhs: []ast.Expr{ast.NewIdent("result")},
				Rhs: []ast.Expr{
					call(
						ast.NewIdent("new"),
						methodType.Results.List[0].Type.(*ast.StarExpr).X,
					),
				},
			},
		},
	}
}

func (g *Generator) genServerMethodBody(ctx *context, rpc *protobuf.RPC, typ *ast.FuncType) *ast.BlockStmt {
	if !isGenerated(rpc.Output) {
		return g.genMethodBodyForNotGeneratedOutput(ctx, rpc, typ)
	} else {
		return g.genMethodBodyForGeneratedOutput(ctx, rpc, typ)
	}
}

func flatFieldNames(args []*ast.Field) []*ast.Ident {
	var arr []*ast.Ident
	for _, arg := range args {
		for _, name := range arg.Names {
			arr = append(arr, name)
		}
	}
	return arr
}

func newMessage(ctx *context, in protobuf.Type, args []*ast.Field) ast.Expr {
	if !isGenerated(in) && len(args) == 1 {
		return args[0].Names[0]
	}
	typ := typeName(in)
	msg := ctx.findMessage(typ)
	var elts []ast.Expr
	names := flatFieldNames(args)
	for i := range msg.Fields {
		elts = append(elts, &ast.KeyValueExpr{
			Key:   ast.NewIdent(genMsgReqFieldName(i)),
			Value: names[i],
		})
	}
	return addr(&ast.CompositeLit{
		Type: ast.NewIdent(typ),
		Elts: elts,
	})
}

func fromMessage(ctx *context, in protobuf.Type, out []*ast.Field) []ast.Expr {
	if !isGenerated(in) && len(out) == 1 {
		name := ast.NewIdent("resp")
		if !in.IsNullable() {
			return []ast.Expr{&ast.StarExpr{X: name}}
		}
		return []ast.Expr{name}
	}
	typ := typeName(in)
	msg := ctx.findMessage(typ)
	var elts []ast.Expr
	for i := range msg.Fields {
		elts = append(elts, &ast.SelectorExpr{
			X:   ast.NewIdent("resp"),
			Sel: ast.NewIdent(genMsgRespFieldName(i)),
		})
	}
	return elts
}

func (g *Generator) genClientMethodBody(ctx *context, recv string, rpc *protobuf.RPC, typ *ast.FuncType) *ast.BlockStmt {
	in := typ.Params.List
	var (
		lhs [2]ast.Expr
		rhs [3]ast.Expr
	)
	// first argument is context
	if rpc.HasCtx {
		rhs[0] = ast.NewIdent(in[0].Names[0].Name)
		in = in[1:]
	} else {
		ctx.addImport("context")
		rhs[0] = call(packageSelector("context", "TODO"))
	}
	// second is the message
	rhs[1] = newMessage(ctx, rpc.Input, in)
	// third one is RPC options
	rhs[2] = &ast.SelectorExpr{X: ast.NewIdent(recv), Sel: ast.NewIdent(fieldNameOpts)}

	out := typ.Results.List
	lhs[0] = ast.NewIdent("resp")
	if rpc.HasError {
		lhs[1] = ast.NewIdent("err")
		out = out[:len(out)-1]
	} else {
		lhs[1] = ast.NewIdent("_")
	}
	results := fromMessage(ctx, rpc.Output, out)
	if len(results) == 0 {
		lhs[0] = ast.NewIdent("_")
	}
	if rpc.HasError {
		results = append(results, ast.NewIdent("err"))
	}

	tok := token.DEFINE
	if len(results) == 0 {
		tok = token.ASSIGN
	}
	var body []ast.Stmt
	// RPC call
	body = append(body, &ast.AssignStmt{
		Tok: tok,
		Lhs: lhs[:],
		Rhs: []ast.Expr{
			callVar(rpcFuncName(recv, rpc), rhs[:]...),
		},
	})
	if len(results) > 0 {
		body = append(body, &ast.ReturnStmt{Results: results})
	}
	return &ast.BlockStmt{List: body}
}

func genMsgRespFieldName(i int) string {
	return fmt.Sprintf("Result%d", i+1)
}

func (g *Generator) genMethodBodyAssignmentsForGeneratedOutput(ctx *context, rpc *protobuf.RPC, msg *protobuf.Message) (lhs []ast.Expr) {
	for i, f := range msg.Fields {
		name := "_"
		if f != nil {
			name = "result." + genMsgRespFieldName(i)
		}
		lhs = append(lhs, ast.NewIdent(name))
	}
	return
}

func emptyBodyForMethodCall(body *ast.BlockStmt, methodCall ast.Expr) *ast.BlockStmt {
	body.List = []ast.Stmt{
		&ast.ExprStmt{
			X: methodCall,
		},
		new(ast.ReturnStmt),
	}
	return body
}

func (g *Generator) genMethodBodyForGeneratedOutput(ctx *context, rpc *protobuf.RPC, typ *ast.FuncType) *ast.BlockStmt {
	body := g.genBaseMethodBody(typ)
	methodCall := g.genServerMethodCall(ctx, rpc)
	call := &ast.AssignStmt{
		Tok: token.ASSIGN,
		Rhs: []ast.Expr{methodCall},
	}

	msg := ctx.findMessage(typeName(rpc.Output))

	if len(msg.Fields) == 0 && !rpc.HasError {
		return emptyBodyForMethodCall(body, methodCall)
	} else if len(msg.Fields) == 0 {
		body.List = nil
	}

	body.List = append(body.List, call)
	lhs := g.genMethodBodyAssignmentsForGeneratedOutput(ctx, rpc, msg)
	call.Lhs = append(call.Lhs, lhs...)

	if rpc.HasError {
		call.Lhs = append(call.Lhs, ast.NewIdent("err"))
	}

	body.List = append(body.List, new(ast.ReturnStmt))
	return body
}

func (g *Generator) genMethodBodyForNotGeneratedOutput(ctx *context, rpc *protobuf.RPC, typ *ast.FuncType) *ast.BlockStmt {
	body := g.genBaseMethodBody(typ)
	methodCall := g.genServerMethodCall(ctx, rpc)
	call := &ast.AssignStmt{Tok: token.ASSIGN}

	needToAddressOutput := !isGenerated(rpc.Output) && !rpc.Output.IsNullable()

	// Specific code
	if needToAddressOutput {
		call.Lhs = append(call.Lhs, ast.NewIdent("aux"))
		call.Tok = token.DEFINE
	} else {
		call.Lhs = append(call.Lhs, ast.NewIdent("result"))
	}

	if rpc.HasError {
		call.Lhs = append(call.Lhs, ast.NewIdent("err"))
	}

	call.Rhs = append(call.Rhs, methodCall)
	body.List = append(body.List, call)

	if needToAddressOutput {
		body.List = append(body.List, &ast.AssignStmt{
			Tok: token.ASSIGN,
			Lhs: []ast.Expr{ast.NewIdent("result")},
			Rhs: []ast.Expr{
				&ast.UnaryExpr{
					Op: token.AND,
					X:  ast.NewIdent("aux"),
				},
			},
		})
	}
	body.List = append(body.List, new(ast.ReturnStmt))
	return body
}

const methRecvName = "s"

func (g *Generator) declServerMethod(ctx *context, iname string, rpc *protobuf.RPC) ast.Decl {
	typ := g.genServerMethodType(ctx, rpc)
	return &ast.FuncDecl{
		Recv: fields(field(methRecvName, ptr(ast.NewIdent(iname)))),
		Name: ast.NewIdent(rpc.Name),
		Type: typ,
		Body: g.genServerMethodBody(ctx, rpc, typ),
	}
}

func hasFieldWithName(name string, fld []*ast.Field) bool {
	for _, f := range fld {
		for _, n := range f.Names {
			if name == n.Name {
				return true
			}
		}
	}
	return false
}

func (g *Generator) declClientMethod(ctx *context, iname string, rpc *protobuf.RPC) ast.Decl {
	typ := g.genClientMethodType(ctx, rpc)
	recv := methRecvName
	if hasFieldWithName(recv, typ.Params.List) {
		recv += "_"
	}
	return &ast.FuncDecl{
		Recv: fields(field(recv, ptr(ast.NewIdent(iname)))),
		Name: ast.NewIdent(rpc.Name),
		Type: typ,
		Body: g.genClientMethodBody(ctx, recv, rpc, typ),
	}
}

func (g *Generator) buildFile(ctx *context, decls []ast.Decl) *ast.File {
	f := &ast.File{
		Name: ast.NewIdent(ctx.pkg.Name()),
	}

	var specs = []ast.Spec{newNamedImport("xcontext", "golang.org/x/net/context")}
	for _, i := range ctx.imports {
		specs = append(specs, newImport(i))
	}

	f.Decls = append(f.Decls, &ast.GenDecl{
		Tok:    token.IMPORT,
		Lparen: token.Pos(1),
		Specs:  specs,
	})
	f.Decls = append(f.Decls, decls...)

	return f
}

func (g *Generator) writeFile(file *ast.File, path string) error {
	fileName := filepath.Join(goSrc, path, "server.proteus.go")
	f, err := os.Create(fileName)
	if err != nil {
		return err
	}
	defer f.Close()

	return printer.Fprint(f, token.NewFileSet(), file)
}

func typeName(t protobuf.Type) string {
	if typ, ok := t.(*protobuf.Named); ok {
		return typ.Name
	}
	return ""
}

func isGenerated(t protobuf.Type) bool {
	if typ, ok := t.(*protobuf.Named); ok {
		return typ.Generated
	}
	return false
}

// shorthands for some AST structures

func newImport(path string) *ast.ImportSpec {
	return &ast.ImportSpec{
		Path: &ast.BasicLit{
			Kind:  token.STRING,
			Value: fmt.Sprintf(`"%s"`, removeGoPath(path)),
		},
	}
}

func newNamedImport(name, path string) *ast.ImportSpec {
	return &ast.ImportSpec{
		Name: &ast.Ident{Name: name},
		Path: &ast.BasicLit{
			Kind:  token.STRING,
			Value: fmt.Sprintf(`"%s"`, removeGoPath(path)),
		},
	}
}

// staticInfCheck declares a static check for type to implement an interface.
func staticInfCheck(inf, typ string) ast.Decl {
	return &ast.GenDecl{
		Tok: token.VAR,
		Specs: []ast.Spec{
			&ast.ValueSpec{
				Names: idents("_"),
				Type:  ast.NewIdent(inf),
				Values: []ast.Expr{
					&ast.CallExpr{
						Fun: &ast.ParenExpr{
							X: &ast.StarExpr{X: ast.NewIdent(typ)},
						},
						Args: []ast.Expr{ast.NewIdent("nil")},
					},
				},
			},
		},
	}
}

func simpleConstructor(name string, in, out *ast.FieldList, results ...ast.Expr) ast.Decl {
	return &ast.FuncDecl{
		Name: ast.NewIdent(name),
		Type: &ast.FuncType{
			Params:  in,
			Results: out,
		},
		Body: &ast.BlockStmt{
			List: []ast.Stmt{
				&ast.ReturnStmt{
					Results: results,
				},
			},
		},
	}
}

func typeDecl(name string, t ast.Expr) ast.Decl {
	return &ast.GenDecl{
		Tok: token.TYPE,
		Specs: []ast.Spec{
			&ast.TypeSpec{
				Name: ast.NewIdent(name),
				Type: t,
			},
		},
	}
}

func field(name string, typ ast.Expr) *ast.Field {
	var names []*ast.Ident
	if name != "" {
		names = idents(name)
	}
	return &ast.Field{
		Names: names,
		Type:  typ,
	}
}

func fields(fields ...*ast.Field) *ast.FieldList {
	return &ast.FieldList{List: fields}
}

func call(fun ast.Expr, args ...ast.Expr) ast.Expr {
	return &ast.CallExpr{
		Fun:  fun,
		Args: args,
	}
}

func callVar(fun ast.Expr, args ...ast.Expr) ast.Expr {
	return &ast.CallExpr{
		Fun:      fun,
		Args:     args,
		Ellipsis: token.Pos(1),
	}
}
func packageSelector(pkg, name string) ast.Expr {
	return &ast.SelectorExpr{
		X:   ast.NewIdent(pkg),
		Sel: ast.NewIdent(name),
	}
}

func ptr(expr ast.Expr) ast.Expr {
	return &ast.StarExpr{X: expr}
}

func addr(expr ast.Expr) ast.Expr {
	return &ast.UnaryExpr{
		Op: token.AND,
		X:  expr,
	}
}

func idents(names ...string) []*ast.Ident {
	arr := make([]*ast.Ident, 0, len(names))
	for _, name := range names {
		arr = append(arr, ast.NewIdent(name))
	}
	return arr
}

func kv(k, v string) ast.Expr {
	return &ast.KeyValueExpr{
		Key:   ast.NewIdent(k),
		Value: ast.NewIdent(v),
	}
}

var goSrc = filepath.Join(os.Getenv("GOPATH"), "src")

func removeGoPath(path string) string {
	return strings.TrimLeft(strings.Replace(path, goSrc, "", -1), "/")
}
