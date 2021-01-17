package tokenizer;

public enum TokenType {
    //统一首字母大写，与实际表达无关
    /** 空 */
    None,

    /** 标识符 */
    Ident,
    Int,
    Void,

    /**关键字 注意实际中全是小写*/
    Fn,
    Let,
    Const,
    As,
    While,
    If,
    Else,
    Return,
    Break,
    Continue,

    /** 字面量 */
    //无符号整数常量
    Uint_Literal,
    //字符串常量
    String_Literal,
    //字符字面量
    Char_Literal,
    //浮点数
    Double_Literal,

    /** 运算符 */
    /** + */
    Plus,
    /** - */
    Minus,
    /** * */
    Mul,
    /** / */
    Div,
    /** = */
    Assign,
    /** == */
    Eq,
    /** != */
    Neq,
    /** < */
    Lt,
    /** > */
    Gt,
    /** <= */
    Le,
    /** >= */
    Ge,
    /** ( */
    L_paren,
    /** ) */
    R_paren,
    /** { */
    L_brace,
    /** } */
    R_brace,
    /** -> */
    Arrow,
    /** , */
    Comma,
    /** : */
    Colon,
    /** ; */
    Semicolon,
    /** 读取结束 */
    Eof;
    
}
