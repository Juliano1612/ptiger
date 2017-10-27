#include <iostream>
#include <memory>

#include "ptiger/ptiger-parser.h"
#include "ptiger/ptiger-lexer.h"
#include "ptiger/ptiger-tree.h"
#include "ptiger/ptiger-symbol.h"
#include "ptiger/ptiger-symbol-mapping.h"
#include "ptiger/ptiger-scope.h"

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "tree.h"
#include "tree-iterator.h"
#include "input.h"
#include "diagnostic.h"
#include "stringpool.h"
#include "cgraph.h"
#include "gimplify.h"
#include "gimple-expr.h"
#include "convert.h"
#include "print-tree.h"
#include "stor-layout.h"
#include "fold-const.h"

namespace Ptiger{

  struct Parser{
    public:
      Parser (Lexer &lexer_) : lexer (lexer_) {}
      void parse_program();

    private:
      Lexer &lexer;

  };

  bool Parser::done_end_of_file(){
    const_TokenPtr t = lexer.peek_token ();
    return (t->get_id () == Ptiger::END_OF_FILE);
  }

  void Parser::parse_program(){
    parse_statement_seq(&Parser::done_end_of_file);
  }

  void Parser::parse_statement_seq(){
    while(lexer.peek_input()->get_id() != Ptiger::END_OF_FILE){
      parse_statement();
    }
  }

  void Parser::parse_statement_seq (bool (Parser::*done) ()){
    while (!(this->*done) ()){
        parse_statement ();
    }
  }

  void Parser::parse_statement(){
    const_TokenPtr t = lexer.peek_input();
    switch (t->get_id()) {
      case Ptiger::TYPE:
        parse_type_declaration();
        break;
      case Ptiger::VAR:
        parse_variable_declaration();
        break;
      case Ptiger::FUNCTION:
        parse_function_declaration();
        break;
    }
  }

}
