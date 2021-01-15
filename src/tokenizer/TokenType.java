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

    @Override
    public String toString() {
        switch (this) {
            case None:
                return "None";
            case Ident:
                return "Ident ";
            case Fn:
                return "Fn";
            case Let:
                return "Let";
            case Const:
                return "Const";
            case As:
                return "As";
            case While:
                return "While";
            case If:
                return "If";
            case Else:
                return "Else";
            case Return:
                return "Return";
            case Break:
                return "Break";
            case Continue:
                return "Continue";
            case Uint_Literal:
                return "Uint_Literal";
            case String_Literal:
                return "String_Literal";
            case Char_Literal:
                return "Char_Literal";
            case Double_Literal:
                return "Double_Literal";
            case Plus:
                return "Plus";
            case Minus:
                return "Minus";
            case Mul:
                return "Mul";
            case Div:
                return "Div";
            case Assign:
                return "Assign";
            case Eq:
                return "Eq";
            case Neq:
                return "Neq";
            case Lt:
                return "Lt";
            case Gt:
                return "Gt";
            case Le:
                return "Le";
            case Ge:
                return "Ge";
            case L_paren:
                return "L_paren";
            case R_paren:
                return "R_paren";
            case L_brace:
                return "L_brace";
            case R_brace:
                return "R_brace";
            case Arrow:
                return "Arrow";
            case Comma:
                return "Comma";
            case Colon:
                return "Colon";
            case Semicolon:
                return "Semicolon";
            case Eof:
                return "Eof";
            case Int:
                return "Int";
            case Void:
                return "Void";
            default:
                return "InvalidToken";
        }
    }
}
