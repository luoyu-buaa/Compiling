package tokenizer;

import error.TokenizeError;
import util.Pos;
import error.ErrorCode;

public class Tokenizer {

	private StringIter it;

	public Tokenizer(StringIter it) {
		this.it = it;
	}

	// 这里本来是想实现 Iterator<Token> 的，但是 Iterator 不允许抛异常，于是就这样了

	/**
	 * 获取下一个 Token
	 *
	 * @return
	 * @throws TokenizeError 如果解析有异常则抛出
	 */
	public Token nextToken() throws TokenizeError { //over!!!
		it.readAll();

		// 跳过之前的所有空白字符
		skipSpaceCharacters();
		//读入结束
		if (it.isEOF()) {
			return new Token(TokenType.Eof, "", it.currentPos(), it.currentPos());
		}

		char peek = it.peekChar();
		if (Character.isDigit(peek)) {
			//判断是否为数字常量
			return lexUIntOrDouble();
		} else if (Character.isAlphabetic(peek) || peek == '_') {
			//判断是否为标识符或者关键字
			return lexIdentOrKeyword();
		} else if (peek == '"') {
			//判断是否为字符串常量
			return lexString();
		}else if(peek == '\''){
			//判断是否为字符字面量
			return lexChar();
		}
		else {
			//判断是否为运算符或者未知错误 //新加：判断是否为注释
			return lexOperatorOrUnknownOrComment();
		}

	}

	private boolean in_line(char a) {//over
		boolean result = false;
		if (a == '\\' || a == '\'' || a == '"' || a == 'n' || a == 'r' || a == 't')
			result = true;
		return result;
	}

	private Token lexString() throws TokenizeError {//over
		//读入一个"，直到下一个"为止。
		Pos start = it.currentPos();
		Pos end = start;
		String value = "";
		it.nextChar();
		char peek;
		while(!it.isEOF() && (peek = it.peekChar()) != '"') {
			if (peek == '\\'){
				it.nextChar();
				value += judge1(it.nextChar());
			}
			else value += it.nextChar();
		}
		if (it.isEOF())
			throw new TokenizeError(ErrorCode.InvalidInput,it.previousPos());
		it.nextChar();
		end = it.currentPos();
		Token result = new Token(TokenType.String_Literal, value, start, end);
		return result;
	}

	private char judge1(char a) throws TokenizeError{//over
		char result;
		switch (a){
			case '\\':
				result = '\\';
				break;
			case '"':
				result = '"';
				break;
			case '\'':
				result = '\'';
				break;
			case 'n':
				result = '\n';
				break;
			case 'r':
				result = '\r';
				break;
			case 't':
				result = '\t';
				break;
			default:
				throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
		}
		return result;
	}

	private String methods3() throws TokenizeError{
		String res = "";
		do {
			res += (it.nextChar());
		} while (Character.isDigit(it.peekChar()));
		return res;
	}
	private Token lexUIntOrDouble() throws TokenizeError {//over
		Pos start = it.currentPos();
		Pos end;
		String strvalue = "";
		strvalue = methods3();
		if(it.peekChar() == '.'){
			strvalue += it.nextChar();
			if (Character.isDigit(it.peekChar()) ){
				strvalue += it.nextChar();
				while (Character.isDigit(it.peekChar()))
					strvalue += it.nextChar();
				if (it.peekChar() == 'E' || it.peekChar() == 'e'){
					strvalue += it.nextChar();
					if (it.peekChar() == '+' || it.peekChar() == '-')
						strvalue += it.nextChar();
					if(Character.isDigit(it.peekChar())){
						do {
							strvalue += it.nextChar();
						}while (Character.isDigit(it.peekChar()));
						return new Token(TokenType.Double_Literal,Double.valueOf(strvalue),start,it.currentPos());
					}else throw new TokenizeError(ErrorCode.InvalidInput,it.previousPos());
				}else
					return new Token(TokenType.Double_Literal,Double.valueOf(strvalue),start,it.currentPos());
			}else throw new TokenizeError(ErrorCode.InvalidInput,it.previousPos());
		}
		end = it.currentPos();
		Token result = new Token(TokenType.Uint_Literal, Long.valueOf(strvalue), start, end);
		return result;
	}

	private String removeZero(String str) {
		int len = str.length(), i = 0;
		while (i < len && str.charAt(i) == '0') {
			i++;
		}
		if (i == len)//
			return "0";
		return str.substring(i);
	}
	private String fix1() throws TokenizeError{
		String result ="";
		do {
			result += it.nextChar();
		}while (Character.isLetterOrDigit(it.peekChar()) || it.peekChar() == '_');
		return result;
	}
	private TokenType fix2(String a) throws TokenizeError{
		TokenType res;
		switch (a) {
			case "fn":
				res = TokenType.Fn;
				break;
			case "let":
				res = TokenType.Let;
				break;
			case "const":
				res = TokenType.Const;
				break;
			case "as":
				res = TokenType.As;
				break;
			case "while":
				res = TokenType.While;
				break;
			case "if":
				res = TokenType.If;
				break;
			case "else":
				res = TokenType.Else;
				break;
			case "return":
				res = TokenType.Return;
				break;
			case "break":
				res = TokenType.Break;
				break;
			case "continue":
				res = TokenType.Continue;
				break;
			case "int":
				res=TokenType.Int;
				break;
			case "void":
				res=TokenType.Void;
				break;
			case "double":
				res=TokenType.Double_Literal;
				break;
			default:
				res = TokenType.Ident;
		}
		return res;
	}
	private Token lexIdentOrKeyword() throws TokenizeError { //over
		Pos start = it.currentPos();
		String strvalue = fix1();
		TokenType ty = fix2(strvalue);
		Token result = new Token(ty,strvalue,start,it.currentPos());
		return result;
	}


	private Token lexOperatorOrUnknownOrComment() throws TokenizeError { //over
		Pos start_pos;
		switch (it.nextChar()) {
			case '+':
				return new Token(TokenType.Plus, "+", it.previousPos(), it.currentPos());
			case '-':
				if(it.peekChar() != '>')
				return new Token(TokenType.Minus, "-", it.previousPos(), it.currentPos());
				else {
					start_pos = it.previousPos();
					it.nextChar();
					return new Token(TokenType.Arrow,"->", start_pos, it.currentPos());
				}
			case '*':
				return new Token(TokenType.Mul, "*", it.previousPos(), it.currentPos());
			case '/':
				start_pos = it.previousPos();
				//注释
				if(it.peekChar() == '/'){
					it.nextChar();
					while (it.nextChar() != '\n');
					return nextToken();
				}
				return new Token(TokenType.Div, "/", it.previousPos(), it.currentPos());
			case '=':
				if(it.peekChar() != '=')
				return new Token(TokenType.Assign, "=", it.previousPos(), it.currentPos());
				else{
					// ==
					start_pos = it.previousPos();
					it.nextChar();
					return new Token(TokenType.Eq, "==", start_pos, it.currentPos());
				}
			case '!':
				if(it.peekChar() != '=')
					throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
				else {
					//!=
					start_pos = it.previousPos();
					it.nextChar();
					return new Token(TokenType.Neq, "!=", start_pos, it.currentPos());
				}
			case '<':
				if(it.peekChar() != '=')
				return new Token(TokenType.Lt, "<", it.previousPos(), it.currentPos());
				else {
					//<=
					start_pos = it.previousPos();
					it.nextChar();
					return new Token(TokenType.Le, "<=", start_pos, it.currentPos());
				}
			case '>':
				if(it.peekChar() != '=')
				return new Token(TokenType.Gt, ">", it.previousPos(), it.currentPos());
				else {
					//>=
					start_pos = it.previousPos();
					it.nextChar();
					return new Token(TokenType.Ge, '>', start_pos, it.currentPos());
				}
			case '(':
				return new Token(TokenType.L_paren, '(', it.previousPos(), it.currentPos());
			case ')':
				return new Token(TokenType.R_paren, ')', it.previousPos(), it.currentPos());
			case '{':
				return new Token(TokenType.L_brace,'{',  it.previousPos(),it.currentPos());
			case '}':
				return new Token(TokenType.R_brace, '}', it.previousPos(), it.currentPos());
			case ',':
				return new Token(TokenType.Comma, ',', it.previousPos(), it.currentPos());
			case ':':
				return new Token(TokenType.Colon, ':', it.previousPos(), it.currentPos());
			case ';':
				return new Token(TokenType.Semicolon, ';', it.previousPos(), it.currentPos());
			default:
				// 不认识这个输入，摸了
				throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
		}
	}

	private void skipSpaceCharacters() {
		while (!it.isEOF() && Character.isWhitespace(it.peekChar())) {
			it.nextChar();
		}
	}

	private char judge_char(char a) throws TokenizeError{
		char result;
		switch (a){
			case '\\':
				result  = '\\';
				break;
			case '"':
				result = '"';
				break;
			case '\'':
				result = '\'';
				break;
			case 'r':
				result = '\r';
				break;
			case 'n':
				result = '\n';
				break;
			case 't':
				result = '\t';
				break;
			default:
				throw new TokenizeError(ErrorCode.InvalidInput,it.previousPos());
		}
		return result;
	}
	private Token lexChar() throws TokenizeError{ //over
		Pos start_pos = it.currentPos();
		char result;
		it.nextChar();
		char peek = it.peekChar();
		if (peek != '\''){
			if (peek == '\\'){
				it.nextChar();
				result = judge_char(it.nextChar());
			}else result = it.nextChar();
			if(it.peekChar() == '\''){
				it.nextChar();
				return new Token(TokenType.Char_Literal,result,start_pos,it.currentPos());
			}else  throw new TokenizeError(ErrorCode.InvalidInput,it.previousPos());
		}
		throw new TokenizeError(ErrorCode.InvalidInput,it.previousPos());
	}

}
