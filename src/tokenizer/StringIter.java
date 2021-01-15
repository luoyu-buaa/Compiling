package tokenizer;

import java.util.ArrayList;
import java.util.Optional;
import java.util.Scanner;

import util.Pos;

/**
 * 这是一个从 C++ 版本抄过来的字符迭代器
 */
public class StringIter {
    // 以行为基础的缓冲区
    ArrayList<String> linesBuffer = new ArrayList<>();

    Scanner scanner;
    // 指向下一个要读取的字符
    Pos ptrNext = new Pos(0, 0);

    Pos ptr = new Pos(0, 0);

    boolean initialized = false;

    Optional<Character> peeked = Optional.empty();

    public StringIter(Scanner scanner) {
        this.scanner = scanner;
    }

    // 从这里开始其实是一个基于行号的缓冲区的实现
    // 为了简单起见，我们没有单独拿出一个类实现
    // 核心思想和 C 的文件输入输出类似，就是一个 buffer 加一个指针，有三个细节
    // 1.缓冲区包括 \n
    // 2.指针始终指向下一个要读取的 char
    // 3.行号和列号从 0 开始

    // 一次读入全部内容，并且替换所有换行为 \n
    // 这样其实是不合理的，这里只是简单起见这么实现
    public void readAll() {
        if (initialized) {
            return;
        }
        new_use();
        // todo:check read \n?
        initialized = true;
    }
    public void new_use(){
        while (scanner.hasNext()) {
            String the_next=scanner.nextLine();
            System.out.println(the_next);
            linesBuffer.add(the_next + '\n');
        }
    }

    /**
     * 获取下一个字符的位置
     */
    public Pos nextPos() {
        if (ptr.row >= linesBuffer.size()) {//读完了
            throw new Error("advance after EOF");
        }
        if (ptr.col == linesBuffer.get(ptr.row).length() - 1) {//到达当前行的末尾
            return new Pos(ptr.row + 1, 0);
        }
        return new Pos(ptr.row, ptr.col + 1);
    }

    /**
     * 获取当前字符的位置
     */
    public Pos currentPos() {
        return ptr;
    }

    /**
     * 获取上一个字符的位置
     */
    public Pos previousPos() {
        if (ptr.row == 0 && ptr.col == 0) {
            throw new Error("previous position from beginning");
        }
        if (ptr.col == 0) {
            return new Pos(ptr.row - 1, linesBuffer.get(ptr.row - 1).length() - 1);
        }
        return new Pos(ptr.row, ptr.col - 1);
    }

    /**
     * 将指针指向下一个字符，并返回当前字符
     */
    public char nextChar() {
        if (this.peeked.isPresent()) {
            char ch = this.peeked.get();
            this.peeked = Optional.empty();
            this.ptr = ptrNext;
            return ch;
        } else {
            char ch = this.getNextChar();
            this.ptr = ptrNext;
            return ch;
        }
    }

    private char getNextChar() {//注意最初读入时的初始化操作
        if (isEOF()) {
            return 0;
        }
        char result = linesBuffer.get(ptrNext.row).charAt(ptrNext.col);
        ptrNext = nextPos();
        return result;
    }

    /**
     * 查看下一个字符，但不移动指针
     */
    public char peekChar() {
        if (peeked.isPresent()) {
            return peeked.get();
        } else {
            char ch = getNextChar();
            this.peeked = Optional.of(ch);
            return ch;
        }
    }

    public Boolean isEOF() {//判断当前字符是否为EOF(文件末尾)
        return ptr.row >= linesBuffer.size();
    }

    // Note: Is it evil to unread a buffer?
    public void unreadLast() {
        ptr = previousPos();
    }

}
