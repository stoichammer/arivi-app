namespace hs arivi
namespace py arivi


// AriviNetworkService contract

exception Failure {
  1: i32 code,
  2: string reason
}

typedef i32 int // We can use typedef to get pretty names for the types we are using

service AriviNetworkService
{
    bool ping(),

    string sendRequest(1: int logid, 2:string jsonReq) throws (1:Failure fail),
}
