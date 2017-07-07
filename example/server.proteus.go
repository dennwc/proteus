package example

import (
	"context"
	xcontext "golang.org/x/net/context"
	"google.golang.org/grpc"
	"gopkg.in/src-d/proteus.v1/example/categories"
)

type exampleServiceServer struct {
}

func NewExampleServiceServer() *exampleServiceServer {
	return &exampleServiceServer{}
}
func (s *exampleServiceServer) GetAlphaTime(ctx xcontext.Context, in *GetAlphaTimeRequest) (result *MyTime, err error) {
	result = new(MyTime)
	aux := GetAlphaTime()
	result = &aux
	return
}
func (s *exampleServiceServer) GetDurationForLength(ctx xcontext.Context, in *GetDurationForLengthRequest) (result *MyDuration, err error) {
	result = new(MyDuration)
	result = GetDurationForLength(in.Arg1)
	return
}
func (s *exampleServiceServer) GetDurationForLengthCtx(ctx xcontext.Context, in *GetDurationForLengthCtxRequest) (result *MyDuration, err error) {
	result = new(MyDuration)
	result, err = GetDurationForLengthCtx(ctx, in.Arg1)
	return
}
func (s *exampleServiceServer) GetOmegaTime(ctx xcontext.Context, in *GetOmegaTimeRequest) (result *MyTime, err error) {
	result = new(MyTime)
	result, err = GetOmegaTime()
	return
}
func (s *exampleServiceServer) GetPhone(ctx xcontext.Context, in *GetPhoneRequest) (result *Product, err error) {
	result = new(Product)
	result = GetPhone()
	return
}
func (s *exampleServiceServer) RandomCategory(ctx xcontext.Context, in *RandomCategoryRequest) (result *categories.CategoryOptions, err error) {
	result = new(categories.CategoryOptions)
	aux := RandomCategory()
	result = &aux
	return
}
func (s *exampleServiceServer) RandomNumber(ctx xcontext.Context, in *RandomNumberRequest) (result *RandomNumberResponse, err error) {
	result = new(RandomNumberResponse)
	result.Result1 = RandomNumber(in.Arg1, in.Arg2)
	return
}

var _ RandomServer = (*randomServer)(nil)

type randomServer struct {
	Random Random
}

func NewRandomServer(s Random) RandomServer {
	return &randomServer{Random: s}
}
func (s *randomServer) GetAlphaTimeI(ctx xcontext.Context, in *GetAlphaTimeIRequest) (result *MyTime, err error) {
	result = new(MyTime)
	aux := s.Random.GetAlphaTimeI()
	result = &aux
	return
}
func (s *randomServer) GetDurationForLengthCtxI(ctx xcontext.Context, in *GetDurationForLengthCtxIRequest) (result *MyDuration, err error) {
	result = new(MyDuration)
	result, err = s.Random.GetDurationForLengthCtxI(ctx, in.Arg1)
	return
}
func (s *randomServer) GetDurationForLengthI(ctx xcontext.Context, in *GetDurationForLengthIRequest) (result *MyDuration, err error) {
	result = new(MyDuration)
	result = s.Random.GetDurationForLengthI(in.Arg1)
	return
}
func (s *randomServer) GetOmegaTimeI(ctx xcontext.Context, in *GetOmegaTimeIRequest) (result *MyTime, err error) {
	result = new(MyTime)
	result, err = s.Random.GetOmegaTimeI()
	return
}
func (s *randomServer) GetPhoneI(ctx xcontext.Context, in *GetPhoneIRequest) (result *Product, err error) {
	result = new(Product)
	result = s.Random.GetPhoneI()
	return
}
func (s *randomServer) RandomBoolI(ctx xcontext.Context, in *RandomBoolIRequest) (result *RandomBoolIResponse, err error) {
	result = new(RandomBoolIResponse)
	result.Result1 = s.Random.RandomBoolI()
	return
}
func (s *randomServer) RandomCategoryI(ctx xcontext.Context, in *RandomCategoryIRequest) (result *categories.CategoryOptions, err error) {
	result = new(categories.CategoryOptions)
	aux := s.Random.RandomCategoryI()
	result = &aux
	return
}
func (s *randomServer) RandomNumberI(ctx xcontext.Context, in *RandomNumberIRequest) (result *RandomNumberIResponse, err error) {
	result = new(RandomNumberIResponse)
	result.Result1 = s.Random.RandomNumberI(in.Arg1, in.Arg2)
	return
}

var _ Random = (*randomRemote)(nil)

type randomRemote struct {
	Random RandomClient
	opts   []grpc.CallOption
}

func NewRandomRemote(s RandomClient, opts ...grpc.CallOption) Random {
	return &randomRemote{Random: s, opts: opts}
}
func (s *randomRemote) GetAlphaTimeI() MyTime {
	resp, _ := s.Random.GetAlphaTimeI(context.TODO(), &GetAlphaTimeIRequest{}, s.opts...)
	return *resp
}
func (s *randomRemote) GetDurationForLengthCtxI(ctx context.Context, meters int64) (*MyDuration, error) {
	resp, err := s.Random.GetDurationForLengthCtxI(ctx, &GetDurationForLengthCtxIRequest{Arg1: meters}, s.opts...)
	return resp, err
}
func (s *randomRemote) GetDurationForLengthI(meters int64) *MyDuration {
	resp, _ := s.Random.GetDurationForLengthI(context.TODO(), &GetDurationForLengthIRequest{Arg1: meters}, s.opts...)
	return resp
}
func (s *randomRemote) GetOmegaTimeI() (*MyTime, error) {
	resp, err := s.Random.GetOmegaTimeI(context.TODO(), &GetOmegaTimeIRequest{}, s.opts...)
	return resp, err
}
func (s *randomRemote) GetPhoneI() *Product {
	resp, _ := s.Random.GetPhoneI(context.TODO(), &GetPhoneIRequest{}, s.opts...)
	return resp
}
func (s *randomRemote) RandomBoolI() bool {
	resp, _ := s.Random.RandomBoolI(context.TODO(), &RandomBoolIRequest{}, s.opts...)
	return resp.Result1
}
func (s *randomRemote) RandomCategoryI() categories.CategoryOptions {
	resp, _ := s.Random.RandomCategoryI(context.TODO(), &RandomCategoryIRequest{}, s.opts...)
	return *resp
}
func (s *randomRemote) RandomNumberI(mean float64, std float64) float64 {
	resp, _ := s.Random.RandomNumberI(context.TODO(), &RandomNumberIRequest{Arg1: mean, Arg2: std}, s.opts...)
	return resp.Result1
}
