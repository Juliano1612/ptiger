#include "ptiger-token.h"

namespace Ptiger{

const char * get_token_description(TokenId tid){
  switch (tid) {
    #define PTIGER_TOKEN(name, descr)\
    case name:\
      return descr;
    #define PTIGER_TOKEN_KEYWORD(x, y) PTIGER_TOKEN(x, y)
      PTIGER_TOKEN_LIST
    #undef PTIGER_TOKEN_KEYWORD
    #undef PTIGER_TOKEN
    default:
      gcc_unreachable();
  }
}

const char *token_id_to_str(TokenId tid){
  switch (tid) {
    #define PTIGER_TOKEN(name, _)\
    case name:\
      return #name;
    #define PTIGER_TOKEN_KEYWORD(x,y) PTIGER_TOKEN(x,y)
      PTIGER_TOKEN_LIST
    #undef PTIGER_TOKEN_KEYWORD
    #undef PTIGER_TOKEN
    default:
      gcc_unreachable();
  }
}


}
