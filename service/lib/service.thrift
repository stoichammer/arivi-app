namespace hs arivi
namespace py arivi


// AriviNetworkService contract

struct Message {
  1: i32 count = 0,
  2: Priority priority,
  3: Operation opcode,
  4: string payload,
}

enum Priority {
    HIGH = 1,
    MEDIUM = 2,
    LOW = 3
}

enum Operation {
    SET_NODE_CAPABILITY = 1,
    GET_BLOCK_HEADERS = 2,
    GET_ESTIMATE_FEE = 3,
    SUBSCRIBE_NEW_HEADERS = 4,
    GET_UNCONFIRMED_TX = 5,
    GET_UTXOS = 6,
    SUBSCRIBE_SCRIPT_HASH =7,
    UNSUBSCRIBE_SCRIPT_HASH =8,
    BROADCAST_TX =9,
    GET_RAW_TX_FROM_HASH=10,
    GET_TX_MERKLE_PATH=11
}

exception InvalidOperation {
  1: i32 failedOpcode,
  2: string reason
}

typedef i32 int // We can use typedef to get pretty names for the types we are using

service AriviNetworkService
{
    bool ping(),

    string sendRequest(1:i32 logid, 2:Message msg) throws (1:InvalidOperation ouch),
}
