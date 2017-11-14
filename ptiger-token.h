#ifndef PTIGER_TOKEN_H
#define PTIGER_TOKEN_H

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "input.h"

#include <string>
#include <tr1/memory>

namespace Ptiger
{

#define PTIGER_TOKEN_LIST \
	PTIGER_TOKEN(FIRST_TOKEN, "<first-token-marker>")\
	PTIGER_TOKEN(END_OF_FILE, "end of file")\
	PTIGER_TOKEN(AND, "&")\
	PTIGER_TOKEN(ASSIGN, ":=")\
	PTIGER_TOKEN(COLON, ":")\
	PTIGER_TOKEN(COMMA, ",")\
	PTIGER_TOKEN(DIVIDE, "/")\
	PTIGER_TOKEN(DOT, ".")\
	PTIGER_TOKEN(EQUAL, "=")\
	PTIGER_TOKEN(GREATEREQUAL, ">=")\
	PTIGER_TOKEN(GREATERTHAN, ">")\
	PTIGER_TOKEN(ID, "identifier")\
	PTIGER_TOKEN(INTEGER_LITERAL, "int literal")\
	PTIGER_TOKEN(LBRACKETS, "{")\
	PTIGER_TOKEN(LBRACE, "[")\
	PTIGER_TOKEN(LOWEREQUAL, "<=")\
	PTIGER_TOKEN(LOWERTHAN, "<")\
	PTIGER_TOKEN(LPARENTESIS, "(")\
	PTIGER_TOKEN(MINUS, "-")\
	PTIGER_TOKEN(NEQUAL, "<>")\
	PTIGER_TOKEN(OR, "|")\
	PTIGER_TOKEN(PLUS, "+")\
	PTIGER_TOKEN(RBRACKETS, "}")\
	PTIGER_TOKEN(RBRACE, "]")\
	PTIGER_TOKEN(REAL_LITERAL, "real literal")\
	PTIGER_TOKEN(RPARENTESIS, ")")\
	PTIGER_TOKEN(SEMICOLON, ";")\
	PTIGER_TOKEN(STRING_LITERAL, "string literal")\
	PTIGER_TOKEN(TIMES, "*")\
	PTIGER_TOKEN_KEYWORD(ARRAY, "array")\
	PTIGER_TOKEN_KEYWORD(BREAK, "break")\
	PTIGER_TOKEN_KEYWORD(DO, "do")\
	PTIGER_TOKEN_KEYWORD(ELSE, "else")\
	PTIGER_TOKEN_KEYWORD(END, "end")\
	PTIGER_TOKEN_KEYWORD(ENDIF, "endif")\
	PTIGER_TOKEN_KEYWORD(FOR, "for")\
	PTIGER_TOKEN_KEYWORD(FUNCTION, "function")\
	PTIGER_TOKEN_KEYWORD(IF, "if")\
	PTIGER_TOKEN_KEYWORD(IN, "in")\
	PTIGER_TOKEN_KEYWORD(INT, "int")\
	PTIGER_TOKEN_KEYWORD(LET, "let")\
	PTIGER_TOKEN_KEYWORD(NIL, "nil")\
	PTIGER_TOKEN_KEYWORD(OF, "of")\
	PTIGER_TOKEN_KEYWORD(REAL, "real")\
	PTIGER_TOKEN_KEYWORD(STRING, "string")\
	PTIGER_TOKEN_KEYWORD(TATOMIC, "tatomic")\
	PTIGER_TOKEN_KEYWORD(THEN, "then")\
	PTIGER_TOKEN_KEYWORD(TO, "to")\
	PTIGER_TOKEN_KEYWORD(TYPE, "type")\
	PTIGER_TOKEN_KEYWORD(VAR, "var")\
	PTIGER_TOKEN_KEYWORD(WHILE, "while")\
	PTIGER_TOKEN_KEYWORD(WRITE, "write")\
	PTIGER_TOKEN(LAST_TOKEN, "<last-token-marker>")



enum TokenId
{
#define PTIGER_TOKEN(name, _) name,
#define PTIGER_TOKEN_KEYWORD(x, y) PTIGER_TOKEN (x, y)
  PTIGER_TOKEN_LIST
#undef PTIGER_TOKEN_KEYWORD
#undef PTIGER_TOKEN
};

const char *get_token_description(TokenId tid);
const char *token_id_to_str(TokenId tid);

/*const char *get_token_description (TokenId tid){
		switch (tid) {
			#define PTIGER_TOKEN(name, descr)
				case name:
					return descr;
			#define PTIGER_TOKEN_KEYWORD(x,y) PTIGER_TOKEN(x,y)
				PTIGER_TOKEN_LIST
			#undef PTIGER_TOKEN_KEYWORD
			#undef PTIGER_TOKEN
				default:
					gcc_unreachable();
		}
}

const char *token_id_to_str (TokenId tid){
	switch (tid) {
		#define PTIGER_TOKEN(name, _)
		case name:
			return #name;
		#define PTIGER_TOKEN_KEYWORD (x,y) PTIGER_TOKEN (x,y)
			PTIGER_TOKEN_LIST
		#undef PTIGER_TOKEN_KEYWORD
		#undef PTIGER_TOKEN
			default:
			gcc_unreachable();
	}
}*/

struct Token;
typedef std::tr1::shared_ptr<Token> TokenPtr;
typedef std::tr1::shared_ptr<const Token> const_TokenPtr;

struct Token{
	private:

		TokenId token_id;
		location_t locus;
		std::string *str;

		Token (TokenId token_id_, location_t locus_) : token_id(token_id_), locus (locus_), str(0){

			}

		Token (TokenId token_id_, location_t locus_, const std::string& str_) : token_id (token_id_), locus (locus_), str (new std::string (str_)){

			}

			//No default initializer
			Token ();
			//Do not copy/assign tokens
			Token (const Token &);
			Token &operator = (const Token &);

	public:

		~Token() {delete str;}

		static TokenPtr	make (TokenId token_id, location_t locus){
			return TokenPtr(new Token (token_id, locus));
		}

		static TokenPtr make_identifier (location_t locus, const std::string& str){
			return TokenPtr(new Token(ID, locus, str));
		}

		static TokenPtr make_integer (location_t locus, const std::string& str){
			return TokenPtr(new Token(INTEGER_LITERAL, locus, str));
		}

		static TokenPtr make_real (location_t locus, const std::string& str){
			return TokenPtr(new Token(REAL_LITERAL, locus, str));
		}

		static TokenPtr make_string (location_t locus, const std::string& str){
			return TokenPtr(new Token (STRING_LITERAL, locus, str));
		}

		TokenId get_id () const{
			return token_id;
		}

		location_t get_locus () const{
			return locus;
		}

		const std::string & get_str () const{
			gcc_assert (str != NULL);
			return *str;
		}

	  // diagnostics
	  const char *
	  get_token_description () const
	  {
	    return Ptiger::get_token_description (token_id);
	  }

	  // debugging
	  const char *
	  token_id_to_str () const
	  {
	    return Ptiger::token_id_to_str (token_id);
	  }


};


}

#endif // TINY_TOKEN_H
