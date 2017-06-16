package rpc

import (
	"bytes"
	"go/ast"
	"go/importer"
	"go/parser"
	"go/printer"
	"go/token"
	"go/types"
	"io/ioutil"
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/require"
	"github.com/stretchr/testify/suite"
	"gopkg.in/src-d/proteus.v1/protobuf"
	"gopkg.in/src-d/proteus.v1/resolver"
	"gopkg.in/src-d/proteus.v1/scanner"
)

type RPCSuite struct {
	suite.Suite
	g *Generator
}

func (s *RPCSuite) SetupTest() {
	s.g = NewGenerator()
}

const expectedImplType = "type Foo struct {\n}"

func (s *RPCSuite) TestDeclImplType() {
	output, err := render(s.g.declServerImplType("Foo", &protobuf.Service{Global: true}))
	s.Nil(err)
	s.Equal(expectedImplType, output)
}

const expectedImplInfType = "type FooSrv struct {\n\tFoo Foo\n}"

func (s *RPCSuite) TestDeclImplInfType() {
	output, err := render(s.g.declServerImplType("FooSrv", &protobuf.Service{Name: "Foo"}))
	s.Nil(err)
	s.Equal(expectedImplInfType, output)
}

const expectedConstructor = `func NewFoo() *Foo {
	return &Foo{}
}`

func (s *RPCSuite) TestDeclConstructor() {
	output, err := render(s.g.declServerConstructor("Foo", "NewFoo", &protobuf.Service{Global: true}))
	s.Nil(err)
	s.Equal(expectedConstructor, output)
}

const expectedConstructorInf = `func NewFooSrv(s Foo) FooServer {
	return &FooSrv{Foo: s}
}`

func (s *RPCSuite) TestDeclConstructorInf() {
	output, err := render(s.g.declServerConstructor("FooSrv", "NewFooSrv", &protobuf.Service{Name: "Foo"}))
	s.Nil(err)
	s.Equal(expectedConstructorInf, output)
}

const expectedFuncNotGenerated = `func (s *FooServer) DoFoo(ctx xcontext.Context, in *Foo) (result *Bar, err error) {
	result = new(Bar)
	result = DoFoo(in)
	return
}`

const expectedFuncNotGeneratedCtx = `func (s *FooServer) DoFooCtx(ctx xcontext.Context, in *Foo) (result *Bar, err error) {
	result = new(Bar)
	result = DoFooCtx(ctx, in)
	return
}`

const expectedFuncNotGeneratedAndNotNullable = `func (s *FooServer) DoFoo(ctx xcontext.Context, in *Foo) (result *Bar, err error) {
	result = new(Bar)
	aux := DoFoo(in)
	result = &aux
	return
}`

const expectedFuncNotGeneratedAndNotNullableIn = `func (s *FooServer) DoFoo(ctx xcontext.Context, in *Foo) (result *Bar, err error) {
	result = new(Bar)
	result = DoFoo(*in)
	return
}`

const expectedFuncGenerated = `func (s *FooServer) DoFoo(ctx xcontext.Context, in *FooRequest) (result *FooResponse, err error) {
	result = new(FooResponse)
	result.Result1, result.Result2, result.Result3 = DoFoo(in.Arg1, in.Arg2, in.Arg3)
	return
}`

const expectedFuncGeneratedVariadic = `func (s *FooServer) DoFoo(ctx xcontext.Context, in *FooRequest) (result *FooResponse, err error) {
	result = new(FooResponse)
	result.Result1, result.Result2, result.Result3 = DoFoo(in.Arg1, in.Arg2, in.Arg3...)
	return
}`

const expectedFuncGeneratedWithError = `func (s *FooServer) DoFoo(ctx xcontext.Context, in *FooRequest) (result *FooResponse, err error) {
	result = new(FooResponse)
	result.Result1, result.Result2, result.Result3, err = DoFoo(in.Arg1, in.Arg2, in.Arg3)
	return
}`

const expectedMethod = `func (s *FooServer) Fooer_DoFoo(ctx xcontext.Context, in *FooRequest) (result *FooResponse, err error) {
	result = new(FooResponse)
	result.Result1, result.Result2, result.Result3, err = s.Fooer.DoFoo(in.Arg1, in.Arg2, in.Arg3)
	return
}`

const expectedMethodExternalInput = `func (s *FooServer) T_Foo(ctx xcontext.Context, in *ast.BlockStmt) (result *T_FooResponse, err error) {
	result = new(T_FooResponse)
	_ = s.T.Foo(in)
	return
}`

const expectedFuncEmptyInAndOut = `func (s *FooServer) Empty(ctx xcontext.Context, in *Empty) (result *Empty, err error) {
	Empty()
	return
}`

const expectedFuncEmptyInAndOutWithError = `func (s *FooServer) Empty(ctx xcontext.Context, in *Empty) (result *Empty, err error) {
	err = Empty()
	return
}`

func (s *RPCSuite) TestDeclMethod() {
	cases := []struct {
		name   string
		rpc    *protobuf.RPC
		output string
	}{
		{
			"func not generated",
			&protobuf.RPC{
				Name:   "DoFoo",
				Method: "DoFoo",
				Input:  nullable(protobuf.NewNamed("", "Foo")),
				Output: nullable(protobuf.NewNamed("", "Bar")),
			},
			expectedFuncNotGenerated,
		},
		{
			"func not generated with ctx",
			&protobuf.RPC{
				Name:   "DoFooCtx",
				Method: "DoFooCtx",
				HasCtx: true,
				Input:  nullable(protobuf.NewNamed("", "Foo")),
				Output: nullable(protobuf.NewNamed("", "Bar")),
			},
			expectedFuncNotGeneratedCtx,
		},
		{
			"func output not generated and not nullable",
			&protobuf.RPC{
				Name:   "DoFoo",
				Method: "DoFoo",
				Input:  nullable(protobuf.NewNamed("", "Foo")),
				Output: notNullable(protobuf.NewNamed("", "Bar")),
			},
			expectedFuncNotGeneratedAndNotNullable,
		},
		{
			"func output not generated and not nullable input",
			&protobuf.RPC{
				Name:   "DoFoo",
				Method: "DoFoo",
				Input:  notNullable(protobuf.NewNamed("", "Foo")),
				Output: nullable(protobuf.NewNamed("", "Bar")),
			},
			expectedFuncNotGeneratedAndNotNullableIn,
		},
		{
			"func generated",
			&protobuf.RPC{
				Name:   "DoFoo",
				Method: "DoFoo",
				Input:  nullable(protobuf.NewGeneratedNamed("", "FooRequest")),
				Output: nullable(protobuf.NewGeneratedNamed("", "FooResponse")),
			},
			expectedFuncGenerated,
		},
		{
			"func generated with variadic arg",
			&protobuf.RPC{
				Name:       "DoFoo",
				Method:     "DoFoo",
				Input:      nullable(protobuf.NewGeneratedNamed("", "FooRequest")),
				Output:     nullable(protobuf.NewGeneratedNamed("", "FooResponse")),
				IsVariadic: true,
			},
			expectedFuncGeneratedVariadic,
		},
		{
			"func generated with error",
			&protobuf.RPC{
				Name:     "DoFoo",
				Method:   "DoFoo",
				HasError: true,
				Input:    nullable(protobuf.NewGeneratedNamed("", "FooRequest")),
				Output:   nullable(protobuf.NewGeneratedNamed("", "FooResponse")),
			},
			expectedFuncGeneratedWithError,
		},
		{
			"method call",
			&protobuf.RPC{
				Name:     "Fooer_DoFoo",
				Method:   "DoFoo",
				Recv:     "Fooer",
				HasError: true,
				Input:    nullable(protobuf.NewGeneratedNamed("", "FooRequest")),
				Output:   nullable(protobuf.NewGeneratedNamed("", "FooResponse")),
			},
			expectedMethod,
		},
		{
			"method with external type input",
			&protobuf.RPC{
				Name:     "T_Foo",
				Method:   "Foo",
				Recv:     "T",
				HasError: false,
				Input:    nullable(protobuf.NewNamed("go.ast", "BlockStmt")),
				Output:   nullable(protobuf.NewGeneratedNamed("", "T_FooResponse")),
			},
			expectedMethodExternalInput,
		},
		{
			"func with empty input and output",
			&protobuf.RPC{
				Name:   "Empty",
				Method: "Empty",
				Input:  nullable(protobuf.NewGeneratedNamed("", "Empty")),
				Output: nullable(protobuf.NewGeneratedNamed("", "Empty")),
			},
			expectedFuncEmptyInAndOut,
		},
		{
			"func with empty input and output with error",
			&protobuf.RPC{
				Name:     "Empty",
				Method:   "Empty",
				HasError: true,
				Input:    nullable(protobuf.NewGeneratedNamed("", "Empty")),
				Output:   nullable(protobuf.NewGeneratedNamed("", "Empty")),
			},
			expectedFuncEmptyInAndOutWithError,
		},
	}

	proto := &protobuf.Package{
		Messages: []*protobuf.Message{
			&protobuf.Message{
				Name: "FooRequest",
				Fields: []*protobuf.Field{
					&protobuf.Field{
						Name:     "FirstField",
						Pos:      1,
						Repeated: false,
						Type:     protobuf.NewBasic("int64"),
					},
					&protobuf.Field{
						Name:     "SecondField",
						Pos:      2,
						Repeated: false,
						Type:     protobuf.NewBasic("string"),
					},
					&protobuf.Field{
						Name:     "ThirdField",
						Pos:      3,
						Repeated: false,
						Type:     protobuf.NewBasic("string"),
					},
				},
			},
			&protobuf.Message{
				Name: "FooResponse",
				Fields: []*protobuf.Field{
					&protobuf.Field{
						Name:     "PrimerField",
						Pos:      1,
						Repeated: false,
						Type:     protobuf.NewBasic("int64"),
					},
					&protobuf.Field{
						Name:     "SegundoField",
						Pos:      2,
						Repeated: false,
						Type:     protobuf.NewBasic("string"),
					},
					&protobuf.Field{
						Name:     "TercerField",
						Pos:      3,
						Repeated: false,
						Type:     protobuf.NewBasic("string"),
					},
				},
			},
			&protobuf.Message{
				Name:   "T_FooResponse",
				Fields: make([]*protobuf.Field, 1),
			},
			&protobuf.Message{
				Name: "Empty",
			},
		},
	}

	ctx := &context{
		proto: proto,
		pkg:   s.fakePkg(),
	}

	for _, c := range cases {
		output, err := render(s.g.declServerMethod(ctx, "FooServer", c.rpc))
		s.Nil(err, c.name, c.name)
		s.Equal(c.output, output, c.name)
	}
}

const expectedGeneratedFile = `package subpkg

import (
	xcontext "golang.org/x/net/context"
	"google.golang.org/grpc"
)

type subpkgServiceServer struct {
}

func NewSubpkgServiceServer() *subpkgServiceServer {
	return &subpkgServiceServer{}
}
func (s *subpkgServiceServer) Generated(ctx xcontext.Context, in *GeneratedRequest) (result *GeneratedResponse, err error) {
	result = new(GeneratedResponse)
	result.Result1, err = Generated(in.Arg1)
	return
}
func (s *subpkgServiceServer) MyContainer_Name(ctx xcontext.Context, in *MyContainer_NameRequest) (result *MyContainer_NameResponse, err error) {
	result = new(MyContainer_NameResponse)
	result.Result1 = s.MyContainer.Name()
	return
}
func (s *subpkgServiceServer) Point_GeneratedMethod(ctx xcontext.Context, in *Point_GeneratedMethodRequest) (result *Point, err error) {
	result = new(Point)
	result = s.Point.GeneratedMethod(in.Arg1)
	return
}
func (s *subpkgServiceServer) Point_GeneratedMethodOnPointer(ctx xcontext.Context, in *Point_GeneratedMethodOnPointerRequest) (result *Point, err error) {
	result = new(Point)
	result = s.Point.GeneratedMethodOnPointer(in.Arg1)
	return
}

var _ SvcServer = (*svcServer)(nil)

type svcServer struct {
	Svc Svc
}

func NewSvcServer(s Svc) SvcServer {
	return &svcServer{Svc: s}
}
func (s *svcServer) GenArgs(ctx context.Context, in *GenArgsRequest) (result *Point, err error) {
	result = new(Point)
	result = s.Svc.GenArgs(in.Arg1, in.Arg2)
	return
}
func (s *svcServer) GenArgsErr(ctx context.Context, in *GenArgsErrRequest) (result *Point, err error) {
	result = new(Point)
	result, err = s.Svc.GenArgsErr(in.Arg1, in.Arg2)
	return
}
func (s *svcServer) GenRet(ctx context.Context, in *Point) (result *GenRetResponse, err error) {
	result = new(GenRetResponse)
	result.Result1, result.Result2 = s.Svc.GenRet(in)
	return
}
func (s *svcServer) GenRetErr(ctx context.Context, in *Point) (result *GenRetErrResponse, err error) {
	result = new(GenRetErrResponse)
	result.Result1, result.Result2, err = s.Svc.GenRetErr(in)
	return
}
func (s *svcServer) GenVar(ctx context.Context, in *GenVarRequest) (result *GenVarResponse, err error) {
	err = s.Svc.GenVar(in.Arg1, in.Arg2...)
	return
}
func (s *svcServer) Name(ctx context.Context, in *NameRequest) (result *NameResponse, err error) {
	result = new(NameResponse)
	result.Result1 = s.Svc.Name()
	return
}

var _ Svc = (*svcRemote)(nil)

type svcRemote struct {
	Svc	SvcClient
	opts	[]grpc.CallOption
}

func NewSvcRemote(s SvcClient, opts ...grpc.CallOption) Svc {
	return &svcRemote{Svc: s, opts: opts}
}
func (s *svcRemote) GenArgs(x int, y int) *Point {
	resp, _ := s.Svc.GenArgs(context.TODO(), &GenArgsRequest{Arg1: x, Arg2: y}, s.opts...)
	return resp
}
func (s *svcRemote) GenArgsErr(x int, y int) (*Point, error) {
	resp, err := s.Svc.GenArgsErr(context.TODO(), &GenArgsErrRequest{Arg1: x, Arg2: y}, s.opts...)
	return resp, err
}
func (s_ *svcRemote) GenRet(s *Point) (int, int) {
	resp, _ := s_.Svc.GenRet(context.TODO(), s, s_.opts...)
	return resp.Result1, resp.Result2
}
func (s_ *svcRemote) GenRetErr(s *Point) (int, int, error) {
	resp, err := s_.Svc.GenRetErr(context.TODO(), s, s_.opts...)
	return resp.Result1, resp.Result2, err
}
func (s_ *svcRemote) GenVar(s string, p ...*Point) error {
	_, err := s_.Svc.GenVar(context.TODO(), &GenVarRequest{Arg1: s, Arg2: p}, s_.opts...)
	return err
}
func (s *svcRemote) Name() string {
	resp, _ := s.Svc.Name(context.TODO(), &NameRequest{}, s.opts...)
	return resp.Result1
}
`

func (s *RPCSuite) TestGenerate() {
	pkg := "gopkg.in/src-d/proteus.v1/fixtures/subpkg"
	scanner, err := scanner.New(pkg)
	s.Nil(err)

	pkgs, err := scanner.Scan()
	s.Nil(err)

	r := resolver.New()
	r.Resolve(pkgs)

	t := protobuf.NewTransformer()
	s.Nil(s.g.Generate(t.Transform(pkgs[0]), pkg))

	data, err := ioutil.ReadFile(projectPath("fixtures/subpkg/server.proteus.go"))
	s.Nil(err)
	s.Equal(expectedGeneratedFile, string(data), "%s", data)

	s.Nil(os.Remove(projectPath("fixtures/subpkg/server.proteus.go")))
}

func TestServiceImplName(t *testing.T) {
	require.Equal(t, "fooServer", serverImplName("foo"))
}

func TestConstructorName(t *testing.T) {
	require.Equal(t, "NewFooServer", serverConstructorName("foo"))
}

const testPkg = `package fake

import "go/ast"
import "context"

type Foo struct{}
type Bar struct {}

func DoFoo(in *Foo) *Bar {
	return nil
}

func DoFooCtx(ctx context.Context, in *Foo) *Bar {
	return nil
}

func MoreFoo(a int) *ast.BlockStmt {
	return nil
}

type T struct{}

func (*T) Foo(s *ast.BlockStmt) int {
	return 0
}
`

func (s *RPCSuite) fakePkg() *types.Package {
	fs := token.NewFileSet()

	f, err := parser.ParseFile(fs, "src.go", testPkg, 0)
	if err != nil {
		panic(err)
	}

	config := types.Config{
		FakeImportC: true,
		Importer:    importer.Default(),
	}

	pkg, err := config.Check("", fs, []*ast.File{f}, nil)
	s.Nil(err)
	return pkg
}

func nullable(t protobuf.Type) protobuf.Type {
	src := scanner.NewNamed("", "X")
	src.SetNullable(true)
	t.SetSource(src)
	return t
}

func notNullable(t protobuf.Type) protobuf.Type {
	src := scanner.NewNamed("", "X")
	src.SetNullable(false)
	t.SetSource(src)
	return t
}

func render(decl ...ast.Decl) (string, error) {
	fs := token.NewFileSet()
	var buf bytes.Buffer

	for _, d := range decl {
		if err := printer.Fprint(&buf, fs, d); err != nil {
			return "", err
		}
	}

	return buf.String(), nil
}

func TestRPCSuite(t *testing.T) {
	suite.Run(t, new(RPCSuite))
}

func projectPath(path string) string {
	return filepath.Join(os.Getenv("GOPATH"), "src", "gopkg.in/src-d/proteus.v1", path)
}
