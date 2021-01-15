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
	public Token nextToken() throws TokenizeError {
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

	private boolean in_line(char a) {
		boolean result = false;
		if (a == '\\' || a == '\'' || a == '\"' || a == 'n' || a == 'r' || a == 't')
			result = true;
		return result;
	}

	private Token lexString() throws TokenizeError {//暂定over
		//读入一个"，直到下一个"为止。
		Pos start = it.currentPos();
		Pos end = start;
		String value = "\"";
		it.nextChar();
		char peek = it.peekChar();
		while(peek != '"') {
			if(it.isEOF())//没有找到另一个双引号
				throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
			value += it.nextChar();
			if (peek == '\\'){
				peek = it.nextChar();
				if(!in_line(peek))//反斜线后面没跟正确的转义符
				throw new TokenizeError(ErrorCode.InvalidInput,it.currentPos());
				else
					continue;
			}
			peek = it.nextChar();
		}
		value += it.nextChar();
		end = it.currentPos();
		Token result = new Token(TokenType.String_Literal, value, start, end);
		return result;
	}

	private Token lexUIntOrDouble() throws TokenizeError {//over
		Pos start = it.currentPos();
		Pos end;
		char peek = it.peekChar();
		String strvalue = "";
		while (Character.isDigit(peek)) {
			strvalue += it.nextChar();
			peek = it.peekChar();
		}
		if(peek == '.'){
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

	private Token lexIdentOrKeyword() throws TokenizeError { //over
		// 请填空：
		// 直到查看下一个字符不是数字或字母或下划线为止:
		// -- 前进一个字符，并存储这个字符
		//
		// 尝试将存储的字符串解释为关键字
		// -- 如果是关键字，则返回关键字类型的 token
		// -- 否则，返回标识符
		//
		// Token 的 Value 应填写标识符或关键字的字符串
		Pos start = it.currentPos();
		Pos end = start;
		char peek = it.peekChar();
		//这是字符串的首字符
		char first_ch = peek;

		String strvalue = "";
		while (Character.isDigit(peek) || Character.isAlphabetic(peek) || peek == '_') {
			strvalue += it.nextChar();
			peek = it.peekChar();
		}
		end = it.currentPos();

		//如果第一个字符为下划线，则必定为标识符
		if (first_ch == '_')
			return new Token(TokenType.Ident, "ident", start, end);

		Token result;
		switch (strvalue) {
			case "fn":
				result = new Token(TokenType.Fn, "Fn", start, end);
				break;
			case "let":
				result = new Token(TokenType.Let, "Let", start, end);
				break;
			case "const":
				result = new Token(TokenType.Const, "Const", start, end);
				break;
			case "as":
				result = new Token(TokenType.As, "As", start, end);
				break;
			case "while":
				result = new Token(TokenType.While, "While", start, end);
				break;
			case "if":
				result = new Token(TokenType.If, "If", start, end);
				break;
			case "else":
				result = new Token(TokenType.Else, "Else", start, end);
				break;
			case "return":
				result = new Token(TokenType.Return, "Return", start, end);
				break;
			case "break":
				result = new Token(TokenType.Break, "Break", start, end);
				break;
			case "continue":
				result = new Token(TokenType.Continue, "Continue", start, end);
				break;
			case "int":
				result = new Token(TokenType.Int,"Int",start,end);
				break;
			case "void":
				result = new Token(TokenType.Void,"Void",start,end);
				break;
			case "double":
				result = new Token(TokenType.Double_Literal,"Double_Literal",start,end);
				break;
				//标识符
			default:
				result = new Token(TokenType.Ident, strvalue, start, end);
		}
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

	private Token lexChar() throws TokenizeError{ //over
		Pos start_pos = it.currentPos();
		char result;
		it.nextChar();
		char peek = it.peekChar();
		if (peek != '\''){
			if (peek == '\\'){
				it.nextChar();
				switch (it.nextChar()){
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
			}else result = it.nextChar();
			if(it.peekChar() == '\''){
				it.nextChar();
				return new Token(TokenType.Char_Literal,result,start_pos,it.currentPos());
			}else  throw new TokenizeError(ErrorCode.InvalidInput,it.previousPos());
		}
		throw new TokenizeError(ErrorCode.InvalidInput,it.previousPos());
	}

}
