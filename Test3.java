//package 实验3;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

public class Test3 {
		public String text;
		public int len;
		public char stack[] = new char[10000];
		public int top;
		public int now_sign;
	public Test3() {
		text = "";
		stack[0] = '#';
		top = 0;
		now_sign = 0;
	}
	

	@SuppressWarnings("resource")
	public static void main(String[] args) throws FileNotFoundException {
		Test3 t = new Test3();
		
		File file = new File(args[0]); //评测时用
//		File file = new File("C:\\Users\\Luoyu\\Desktop\\b.txt");
		BufferedReader br = null;			
		br = new BufferedReader(new FileReader(file));

		try {
			t.text = br.readLine();
		} catch (IOException e) {
			// TODO 自动生成的 catch 块
			e.printStackTrace();
		}
		t.text += "#";
		t.len = t.text.length();
		char out,in;
		boolean result;
		
		while(true) {
			out = t.text.charAt(t.now_sign);//当前要读入的字符
			
			if(t.stack[t.top] == 'E')//栈顶或次栈顶的终结符
				in = t.stack[t.top-1];
			else 
				in = t.stack[t.top];
			
			switch (in) {
			case '+':
				result = t.judge1(out);
				break;	
			case '*':
				result = t.judge2(out);
				break;
			case 'i':
				result = t.judge3(out);
				break;
			case '(':
				result = t.judge4(out);
				break;
			case ')':
				result = t.judge5(out);
				break;
			case '#':
				result = t.judge6(out);
				break;
			default:
				System.out.println("E");
				result = false;
			}
			if(!result)
				break;
			if(t.stack[t.top] == 'E' && t.top == 1 && t.now_sign == t.len-1)
				break;
		}

	}
	
	public boolean judge1(char a) {//+
		boolean result = true;
		if(a =='+' || a == ')' || a == '#')
			result = statute(a);
		else {
			stack[top+1] = a;
			top++;
			now_sign ++;
			System.out.println("I"+a);
		}
		return result;
	}
	
	public boolean judge2(char a) {//*
		boolean result = true;
		if(a =='+' || a == '*' || a == ')' || a == '#')
			result = statute(a);
		else {
			stack[top+1] = a;
			top++;
			now_sign ++;
			System.out.println("I"+a);
		}
		return result;
	}
	
	public boolean judge3(char a) {//i
		boolean result = true;
		if(a =='+' || a == '*' || a == ')' || a == '#')
			result = statute(a);
		else {
			System.out.println("E");
			result = false;
		}
		return result;
	}	
	
	public boolean judge4(char a) {//(
		boolean result = true;
		if(a=='+' || a == '*' || a == 'i' || a== '(' || a == ')') {
			stack[top+1] = a;
			top++;
			now_sign ++;
			System.out.println("I"+a);
		}
		else {
			System.out.println("E");
			result = false;
		}
		return result;
	}	
	
	public boolean judge5(char a) {//)
		boolean result = true;
		if(a =='+' || a == '*' || a == ')' || a == '#')
			result = statute(a);
		else {
			System.out.println("E");
			result = false;
		}
		return result;
	}	
	
	public boolean judge6(char a) {//#
		boolean result = true;
		if(a=='+' || a == '*' || a == 'i' || a== '(') {
			stack[top+1] = a;
			top++;
			now_sign ++;
			System.out.println("I"+a);
		}
		else {
			System.out.println("E");
			result = false;
		}
		return result;
	}
	
	public boolean statute(char a) {
		boolean result = true;
		if(stack[top]=='i') {//i->E
			stack[top] = 'E';
			System.out.println("R");
		}
		else if(top>=3 && stack[top]==')' && stack[top-1]=='E' && stack[top-2]=='(') {//(E)->E
			top-=2;
			stack[top] = 'E';
			System.out.println("R");
		}
		else if(top>=3 && stack[top]=='E' && stack[top-1]=='+' && stack[top-2]=='E') { //E+E->E
			top-=2;
			stack[top] = 'E';
			System.out.println("R");
		}
		else if(top>=3 && stack[top]=='E' && stack[top-1]=='*' && stack[top-2]=='E') { //E*E->E
			top-=2;
			stack[top] = 'E';
			System.out.println("R");
		}
		else {
			result = false;
			System.out.println("RE");
		}
		return result;
	}
}
