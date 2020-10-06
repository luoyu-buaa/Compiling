package day1;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class MyTest {
	public static void main(String[] args) {
//		File file = new File(args[0]); //评测时用
		File file = new File("C:\\Users\\Luoyu\\Desktop\\b.txt");
		BufferedReader br = null;
		String text = "";
		try {
			br = new BufferedReader(new FileReader(file));
			String tail = "";
			while ((tail = br.readLine()) != null) {
				text = text + tail + "\n";
			}
			;
			br.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		Tool test1 = new Tool();
		test1.go_test(text);
	}

}

class Tool {
	public String token = "";
	public String the_text = "";
	public int len;
	public int index;
	public char a;

	public void go_test(String text) {// 主逻辑
		len = text.length();
//		System.out.println(len);
		the_text = text;
		token = "";
		index = 0;
		if (len == 0)
			return;
		a = the_text.charAt(index);
		while (index < len) {
			if (Character.isWhitespace(a)) {
				index++;
				if (index < len) {
					a = the_text.charAt(index);
				}
			}

			else if (Character.isDigit(a)) {
				judge_Int();
			} else if (Character.isLetter(a)) {
				judge_Ident();
			} else if (isSeparator(a)) {
				//
			} else
				break;
			if (!token.equals(""))
				System.out.println(token);
			clean_token();
		}
	}

	public static String removeZero(String str) {
		int len = str.length(), i = 0;
		while (i < len && str.charAt(i) == '0') {
			i++;
		}
		if (i == len)// 全是0
			return "0";
		return str.substring(i);
	}

	public void clean_token() {
		token = "";
	}

	public void judge_Int() {
		do {
			token += a;
			index++;
			if (index < len)
				a = the_text.charAt(index);
			else
				break;
		} while (Character.isDigit(a));
		token = removeZero(token);
		token = "Int(" + token + ")";
	}

	public void judge_Ident() {
		do {
			token += a;
			index++;
			if (index < len)
				a = the_text.charAt(index);
			else
				break;
		} while (Character.isLetter(a) || Character.isDigit(a));
		if (token.equals("BEGIN"))
			token = "Begin";
		else if (token.equals("END"))
			token = "End";
		else if (token.equals("FOR"))
			token = "For";
		else if (token.equals("IF"))
			token = "If";
		else if (token.equals("THEN"))
			token = "Then";
		else if (token.equals("ELSE"))
			token = "Else";
		else
			token = "Ident(" + token + ")";
		// System.out.println();
	}

	public boolean isSeparator(char b) {
		boolean result = true;
		if (b == ':') {
			if (index + 1 < len && the_text.charAt(index + 1) == '=') {
				index += 2;
				token = "Assign";
				if (index < len)
					this.a = the_text.charAt(index);
				return true;
			} else
				token = "Colon";
		} else if (b == '+')
			token = "Plus";
		else if (b == '*')
			token = "Star";
		else if (b == ',')
			token = "Comma";
		else if (b == '(')
			token = "LParenthesis";
		else if (b == ')')
			token = "RParenthesis";
		else
			result = false;
		index++;
		if (index < len)
			this.a = the_text.charAt(index);
		return result;
	}
}
