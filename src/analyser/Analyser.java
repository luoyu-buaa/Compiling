package analyser;

import error.*;

import instruction.Instruction;
import instruction.FunctionInstruction;
import instruction.Operation;

import symbol.StorageType;
import symbol.Symbol;
import symbol.SymbolType;

import tokenizer.Token;
import tokenizer.TokenType;
import tokenizer.Tokenizer;

import util.Pos;
import util.WriteFile;

import java.sql.Array;
import java.util.*;
import java.io.IOException;

public final class Analyser {
    //初始化词法分析器
    Tokenizer tokenizer;


    /** 当前偷看的 token */
    Token peekedToken = null;

    /** 栈式符号表 */
    Stack<Symbol> symbolTable = new Stack<>();
    /** 哈希*/
    HashMap<String,Integer> hashMap = new HashMap<>();
    /** 分程序索引*/
    Stack<Integer> index = new Stack<>();

    /** 指令集*/
    ArrayList<Instruction> instructions;

    /** start函数的指令集*/
    ArrayList<Instruction> startInstructions;

    /** 全局变量*/
    ArrayList<String> Globals;

    //偏移量
    int global_offset = 0;
    int argument_offset = 0;
    int local_offset = 0;
    int func_offset = 1;

    /** OPG算法矩阵 */
    ArrayList<TokenType> platform = new ArrayList<>(Arrays.asList(TokenType.Gt,
            TokenType.Lt, TokenType.Ge, TokenType.Le, TokenType.Eq, TokenType.Neq,
            TokenType.Plus, TokenType.Minus, TokenType.Mul, TokenType.Div, TokenType.As));
    public int[][] jz = {
            {2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1},
            {2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1},
            {2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1},
            {2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1},
            {2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1},
            {2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1},
            {2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1},
            {2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1},
            {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1},
            {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1},
            {2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2}};

    public Analyser(Tokenizer tokenizer) {//根据写好的词法分析器完成语法分析器的初始化
        this.tokenizer = tokenizer;
        this.instructions = new ArrayList<>();
        this.startInstructions = new ArrayList<>();
        this.Globals = new ArrayList<>();
        this.index.push(0);
    }

    //主分析程序
    public void analyse(String FileName) throws CompileError, IOException { //over
        analyseProgram();
        for(String global : Globals)
            System.out.println(global);
        System.out.println();//换行
        for (Instruction instruction : instructions)
            System.out.println(instruction.toString());
        System.out.println();//换行
        for (Instruction startInstruction: startInstructions)
            System.out.println(startInstruction.toString());
        WriteFile.writeFile(FileName,Globals,instructions,startInstructions);
    }

    /**
     * 查看下一个 Token
     *
     * @return
     * @throws TokenizeError
     */
    private Token peek() throws TokenizeError {
        if (peekedToken == null) {
            peekedToken = tokenizer.nextToken();
        }
        return peekedToken;
    }

    /**
     * 获取下一个 Token,指针前进
     * 
     * @return
     * @throws TokenizeError
     */
    private Token next() throws TokenizeError {
        if (peekedToken != null) {
            var token = peekedToken;
            peekedToken = null;
            return token;
        } else {
            return tokenizer.nextToken();
        }
    }

    /**
     * 如果下一个 token 的类型是 tt，则返回 true
     * 
     * @param tt
     * @return
     * @throws TokenizeError
     */
    private boolean check(TokenType tt) throws TokenizeError {
        var token = peek();
        return token.getTokenType() == tt;
    }

    /**
     * 如果下一个 token 的类型是 tt，则前进一个 token 并返回这个 token
     * 
     * @param tt 类型
     * @return 如果匹配则返回这个 token，否则返回 null
     * @throws TokenizeError
     */
    private Token nextIf(TokenType tt) throws TokenizeError {
        var token = peek();
        if (token.getTokenType() == tt) {
            return next();
        } else {
            return null;
        }
    }

    /**
     * 如果下一个 token 的类型是 tt，则前进一个 token 并返回，否则抛出异常
     * 
     * @param tt 类型
     * @return 这个 token
     * @throws CompileError 如果类型不匹配
     */
    private Token expect(TokenType tt) throws CompileError {
        var token = peek();
        if (token.getTokenType() == tt) {
            return next();
        } else {
            throw new ExpectedTokenError(tt, token);
        }
    }

    /**
     * 添加一个符号
     * 
     * @param name          名字
     * @param isInitialized 是否已赋值
     * @param isConstant    是否是常量
     * @param curPos        当前 token 的位置（报错用）
     * @throws AnalyzeError 如果重复定义了则抛异常
     */
    private Symbol add_Symbol(String name, boolean isConstant, boolean isInitialized, SymbolType symbolType, StorageType storageType, Pos curPos) throws AnalyzeError {
        Integer address = this.hashMap.get(name);
        if (address != null && address >= this.index.peek()) {
            throw new AnalyzeError(ErrorCode.DuplicateDeclaration, curPos);
        } else {
            if (address != null) {
                switch (storageType) {
                    case global:
                        this.symbolTable.push(new Symbol(name, isConstant, isInitialized, symbolType, address, storageType, global_offset++));
                        if (isConstant)
                            Globals.add("1");
                        else
                            Globals.add("0");
                        break;
                    case argument:
                        this.symbolTable.push(new Symbol(name, isConstant, isInitialized, symbolType, address, storageType, argument_offset++));
                        break;
                    case local:
                        this.symbolTable.push(new Symbol(name, isConstant, isInitialized, symbolType, address, storageType, local_offset++));
                        break;
                }
                System.out.println("add/dup:" + symbolTable.peek().getName());
            } else {
                switch (storageType) {
                    case global:
                        this.symbolTable.push(new Symbol(name, isConstant, isInitialized, symbolType, storageType, global_offset++));
                        if (isConstant)
                            Globals.add("1");
                        else
                            Globals.add("0");
                        break;
                    case argument:
                        this.symbolTable.push(new Symbol(name, isConstant, isInitialized, symbolType, storageType, argument_offset++));
                        break;
                    case local:
                        this.symbolTable.push(new Symbol(name, isConstant, isInitialized, symbolType, storageType, local_offset++));
                        break;
                }
                System.out.println("add:" + symbolTable.peek().getName());
            }
            this.hashMap.put(name, symbolTable.size() - 1);
        }
        return this.symbolTable.peek();
    }

    private Symbol add_FuncSymbol(String name, Pos curPos) throws AnalyzeError {
        Integer loc = this.hashMap.get(name);
        if (loc != null && loc >= this.index.peek()) {
            throw new AnalyzeError(ErrorCode.DuplicateDeclaration, curPos);
        } else {
            Symbol symbol = new Symbol(name, true, StorageType.global, global_offset++, func_offset++);
            this.symbolTable.push(symbol);
            this.hashMap.put(name, symbolTable.size() - 1);
            this.index.push(symbolTable.size());
            Globals.add(name);
            System.out.println("add:" + symbolTable.peek().getName());
            return symbol;
        }
    }

    private void add_Block() {
        this.index.push(this.symbolTable.size());
        System.out.println("add block");
    }

    private void change_Initialized(String name, Pos curPos) throws AnalyzeError {
        Symbol sym = symbolTable.get(hashMap.get(name));
        if (sym.isConstant())
            throw new AnalyzeError(ErrorCode.AssignToConstant, curPos);
        else {
            if (!sym.isInitialized())
                sym.setInitialized(true);
        }
    }
    private void remove_BlockSymbols(boolean isFunction) {
        int end = index.pop();
        for (int i = symbolTable.size() - 1; i >= end; i--) {
            Symbol tmpSymbol = symbolTable.pop();
            if (tmpSymbol.getChain() == -1) {
                hashMap.remove(tmpSymbol.getName());
                System.out.println();
            } else {
                hashMap.put(tmpSymbol.getName(), tmpSymbol.getChain());
            }
        }
        if (isFunction)
            argument_offset = 0;
        System.out.println("remove block");
    }

    private void analyseProgram() throws CompileError {//over的主框架
        // program -> decl_stmt* function*
        //函数以fn开头
        //decl以let或const开头
        Globals.add("_start");
        startInstructions.add(new FunctionInstruction(Operation.func, 0, 0, 0, global_offset++));
        while (check(TokenType.Fn) || check(TokenType.Let) || check(TokenType.Const)){
            if (check(TokenType.Fn))
                analyse_function();
            else
                analyseconst_decl_stmt(StorageType.global);
        }
        Token end = expect(TokenType.Eof);
        if (this.hashMap.get("main") == null)
            throw new AnalyzeError(ErrorCode.InvalidInput,end.getStartPos());
        startInstructions.add(new Instruction(Operation.stackalloc, 0));
        startInstructions.add(new Instruction(Operation.call, this.symbolTable.get(this.hashMap.get("main")).getFuncOffset()));
    }

    //over
    private boolean[] analyse_stmt(boolean in_while,SymbolType re_ty, int lp_loc,ArrayList<Integer> bk_list) throws CompileError{
        /* stmt ->
            expr_stmt
            | decl_stmt  //let或const开头
            | if_stmt  //if开头
            | while_stmt  //while开头
            | return_stmt  //return开头
            | block_stmt   //'{'开头
            | empty_stmt //  仅一个';'
*/
        var peeked = peek();
        switch (peeked.getTokenType()){
            case Let:
            case Const:
                analyselet_decl_stmt(StorageType.local);
                return new boolean[]{false, false};

            case If:
                return  analyse_if_stmt(in_while,re_ty,lp_loc,bk_list);

            case While:
                analyse_while_stmt(re_ty);
                return new boolean[]{false, false};

            case Break:
                if (in_while)
                    analyse_break_stmt(bk_list);
                else
                    throw new AnalyzeError(ErrorCode.InvalidInput,peek().getStartPos());
                return new boolean[]{false, true};

            case Continue:
                if (in_while)
                    analyse_continue_stmt(lp_loc);
                else
                    throw new AnalyzeError(ErrorCode.InvalidInput,peek().getStartPos());
                return new boolean[]{false,true};

            case Return:
                analyse_return_stmt(re_ty);
                return new boolean[]{true, false};

            case L_brace:
                return analyse_block_stmt(false,in_while,re_ty,lp_loc,bk_list);

            case Semicolon:
                expect(TokenType.Semicolon);
                return new boolean[]{false, false};

            default:
                analyse_expr_stmt();
                return new boolean[]{false, false};
        }
    }

    private void analyse_continue_stmt(int loc) throws CompileError{//over
        expect(TokenType.Continue);
        expect(TokenType.Semicolon);
        instructions.add(new Instruction(Operation.br,loc - instructions.size()));
    }

    private void analyse_break_stmt(ArrayList<Integer> bk_list) throws  CompileError{//over
        expect(TokenType.Break);
        expect(TokenType.Semicolon);
        bk_list.add(instructions.size());
        instructions.add(new Instruction(Operation.br));
    }

    private boolean[] analyse_if_stmt(boolean in_while,SymbolType re,int loc,ArrayList<Integer> br_list) throws CompileError{ //over
        /*
        if_stmt -> 'if' expr block_stmt ('else' (block_stmt | if_stmt))?
                        ^~~~ ^~~~~~~~~~         ^~~~~~~~~~~~~~~~~~~~~~
                        |     if_block           else_block
                        condition
        * */
        boolean h_return;
        boolean h_BreakorContinue;
        boolean h_Else = false;
        ArrayList<Integer> bToe = new ArrayList<>();
        expect(TokenType.If);
        OPGSymbol em = analyse_opg_expr(false);
        if (em.getType() == SymbolType.VOID)
            throw new AnalyzeError(ErrorCode.InvalidInput,em.getStartPos());
        instructions.add(new Instruction(Operation.brtrue,1));
        instructions.add(new Instruction(Operation.br));
        int br_loc = instructions.size();
        br_loc = br_loc - 1;
        boolean[] c = analyse_block_stmt(false,in_while,re,loc,br_list);
        h_return = c[0];
        h_BreakorContinue = c[1];
        bToe.add(instructions.size());
        instructions.add(new Instruction(Operation.br));
        instructions.get(br_loc).setParam1(instructions.size() - br_loc - 1);
        if(peek().getTokenType() == TokenType.Else){
            while (nextIf(TokenType.Else) != null){
                if(peek().getTokenType() == TokenType.If){
                    next();
                    em = analyse_opg_expr(false);
                    if (em.getType() == SymbolType.VOID)
                        throw new AnalyzeError(ErrorCode.InvalidInput,em.getStartPos());
                    instructions.add(new Instruction(Operation.brtrue,1));
                    instructions.add(new Instruction(Operation.br));
                    br_loc = instructions.size();
                    br_loc = br_loc - 1;
                    c = analyse_block_stmt(false,in_while,re,loc,br_list);
                    h_return = h_return & c[0];
                    h_BreakorContinue &= c[1];
                    bToe.add(instructions.size());
                    instructions.add(new Instruction(Operation.br));
                    instructions.get(br_loc).setParam1(instructions.size() - br_loc - 1);
                } else {
                    c = analyse_block_stmt(false,in_while,re,loc,br_list);
                    h_return = h_return & c[0];
                    h_BreakorContinue = h_BreakorContinue & c[1];
                    h_Else = true;
                    break;
                }
            }
        }
        if (!h_Else){
            h_return = false;
            h_BreakorContinue = false;
        }
        for (Integer b1 : bToe){
            instructions.get(b1).setParam1(instructions.size() - b1 - 1);
        }
        return  new boolean[]{h_return,h_BreakorContinue};
    }

    private void analyse_while_stmt(SymbolType re) throws CompileError{ //over
        //while语句
        /*
        while_stmt -> 'while' expr block_stmt
                              ^~~~ ^~~~~~~~~~while_block
                       condition
        * */
        expect(TokenType.While);
        ArrayList<Integer> br_list = new ArrayList<>();
        int loc = instructions.size();
        loc -= 1;
        analyse_opg_expr(false);
        instructions.add(new Instruction(Operation.brtrue,1));
        int br_loc = instructions.size();
        instructions.add(new Instruction(Operation.br));
        boolean h_BreakorContinue = analyse_block_stmt(false,true,re,loc,br_list)[1];
        if (!h_BreakorContinue)
            instructions.add(new Instruction(Operation.br,loc-instructions.size()));
        instructions.get(br_loc).setParam1(instructions.size() - br_loc -1);
        for (Integer bN : br_list){
            instructions.get(bN).setParam1(instructions.size() - bN - 1);
        }
//        analyse_expr();
//        analyse_block_stmt();
    }

    private void analyse_return_stmt(SymbolType re) throws CompileError{//over
        //return 语句
        //return_stmt -> 'return' expr? ';'
        Token wish = expect(TokenType.Return);
        if (re != SymbolType.VOID)
            instructions.add(new Instruction(Operation.arga,0));
        SymbolType ty = SymbolType.VOID;
        if (check(TokenType.Minus) || check(TokenType.Ident) || check(TokenType.Uint_Literal) || check(TokenType.Double_Literal) ||
                check(TokenType.String_Literal) || check(TokenType.Char_Literal) || check(TokenType.L_paren)) {
            OPGSymbol em = analyse_opg_expr(false);
            ty = em.getType();
        }
        expect(TokenType.Semicolon);
        if (ty != re)
            throw new AnalyzeError(ErrorCode.InvalidInput,wish.getStartPos());
        if (ty != SymbolType.VOID)
            instructions.add(new Instruction(Operation.store64));
        instructions.add(new Instruction(Operation.ret));
    }

    //over
    private boolean[] analyse_block_stmt(boolean is_Fun, boolean inside_while, SymbolType return_ty, int loopLoc, ArrayList<Integer> break_list) throws CompileError{
        //block_stmt -> '{' stmt* '}'

        boolean now_return = false;
        boolean now_BrearkOrContinue = false;
        int return_s = 0;
        int Break_Or_Continue_s = 0;
        expect(TokenType.L_brace);
        if(!is_Fun)
            add_Block();
        while (peek().getTokenType() != TokenType.R_brace && peek().getTokenType() != TokenType.Eof){
            if (return_s == 0 && now_return)
                return_s = instructions.size();
            else if (Break_Or_Continue_s == 0 && now_BrearkOrContinue)
                Break_Or_Continue_s = instructions.size();
            if (now_return && now_BrearkOrContinue)
                analyse_stmt(inside_while,return_ty,loopLoc,break_list);
            else if (now_return)
                now_BrearkOrContinue = analyse_stmt(inside_while,return_ty,loopLoc,break_list)[1];
            else if(now_BrearkOrContinue)
                now_return = analyse_stmt(inside_while,return_ty,loopLoc,break_list)[0];
            else {
                boolean[] c = analyse_stmt(inside_while,return_ty,loopLoc,break_list);
                now_return = c[0];
                now_BrearkOrContinue = c[1];
            }
        }
        expect(TokenType.R_brace);
        if(return_s > 0)
            instructions.subList(return_s,instructions.size()).clear();
        if(Break_Or_Continue_s > 0)
            instructions.subList(Break_Or_Continue_s,instructions.size()).clear();
        remove_BlockSymbols(is_Fun);
        return new boolean[]{now_return,now_BrearkOrContinue};
    }

    private void analyse_empty_stmt() throws CompileError{
        //empty_stmt -> ';'
        expect(TokenType.Semicolon);
    }



    private void analyselet_decl_stmt(StorageType storageType) throws CompileError{ //over
        //let_decl_stmt -> 'let' IDENT ':' ty ('=' expr)? ';'

        boolean is_g;
        if(storageType == StorageType.global)
            is_g = true;
        else
            is_g = false;
        expect(TokenType.Let);
        Token name = expect(TokenType.Ident);
        expect(TokenType.Colon);
        SymbolType ty = analysety();
        if(ty == SymbolType.VOID)
            throw new AnalyzeError(ErrorCode.InvalidInput,name.getStartPos());
        Symbol sym = add_Symbol(name.getValueString(),false,false,ty,storageType,name.getStartPos());

        // 下个 token 是等于号吗？如果是的话分析expr
        if (nextIf(TokenType.Assign)!=null){
            if (is_g)
                startInstructions.add(new Instruction(Operation.globa,sym.getOffset()));
            else
                instructions.add(new Instruction(Operation.loca,sym.getOffset()));
            OPGSymbol em = analyse_opg_expr(is_g);
            j1(ty,em);
            change_Initialized(name.getValueString(),name.getStartPos());
            if (is_g)
                startInstructions.add(new Instruction(Operation.store64));
            else
                instructions.add(new Instruction(Operation.store64));
        }
        expect(TokenType.Semicolon);
    }

    private void analyseconst_decl_stmt(StorageType storageType) throws CompileError{//over
        //const_decl_stmt -> 'const' IDENT ':' ty '=' expr ';'
        boolean is_g;
        if (storageType == StorageType.global)
            is_g = true;
        else
            is_g = false;
        expect(TokenType.Const);
        Token name = expect(TokenType.Ident);
        expect(TokenType.Colon);
        SymbolType ty = analysety();
        if (ty == SymbolType.VOID)
            throw new AnalyzeError(ErrorCode.InvalidInput,name.getStartPos());
        Symbol sym = add_Symbol(name.getValueString(),true,true,ty,storageType,name.getStartPos());
        expect(TokenType.Assign);
        if (is_g)
            startInstructions.add(new Instruction(Operation.globa,sym.getOffset()));
        else
            instructions.add(new Instruction(Operation.loca,sym.getOffset()));

        OPGSymbol sym_opg = analyse_opg_expr(is_g);
        j1(ty,sym_opg);
        expect(TokenType.Semicolon);
        if (is_g)
            startInstructions.add(new Instruction((Operation.store64)));
        else
            instructions.add(new Instruction(Operation.store64));
    }

    private void j1(SymbolType ty,OPGSymbol em) throws CompileError{
        if(ty != em.getType())
            throw new AnalyzeError(ErrorCode.InvalidInput,em.getStartPos());
    }

    private OPGSymbol analyse_opg_expr(boolean is_g) throws CompileError{ //over
        Stack<TokenType> symStack = new Stack<>();
        Stack<OPGSymbol> expr_Stack = new Stack<>();
        if(symStack.empty()){
            symStack.push(TokenType.Eof);
            expr_Stack.push(analyse_other_expr(is_g));
        }
        while (!symStack.empty()){
            TokenType next = peek().getTokenType();
            int x = platform.indexOf(symStack.peek());
            int y = platform.indexOf(next);
            if(x == -1 && y == -1) break;
            else if (x == -1 || y != -1 && jz[x][y] == 1){
                symStack.push(next);
                next();
                if (next == TokenType.As){
                    SymbolType ty = analysety();
                    expr_Stack.push(new OPGSymbol(ty,null));
                }
                else
                    expr_Stack.push(analyse_other_expr(is_g));
            }else if (y == -1 || jz[x][y] == 2){
                my_reduction(symStack,expr_Stack,is_g);
            }
        }
        return expr_Stack.peek();
    }

    private void my_reduction(Stack<TokenType>sym,Stack<OPGSymbol> exprs,boolean is_g) throws CompileError{ //over
        if (exprs.size() <= 0 )
            throw new EmptyStackException();
        else {
            SymbolType NO2_Type = exprs.pop().getType();
            OPGSymbol NO1 = exprs.peek();
            TokenType ty = sym.pop();
            SymbolType NO1_ty = NO1.getType();
            if (TokenType.As == ty){
                if (NO1_ty == SymbolType.VOID || NO2_Type == SymbolType.VOID)
                    throw new AnalyzeError(ErrorCode.InvalidInput,NO1.getStartPos());
                else {
                    if (NO1_ty == SymbolType.INT && NO2_Type == SymbolType.DOUBLE){
                        NO1.setType(NO2_Type);
                        if (is_g)
                            startInstructions.add(new Instruction(Operation.itof));
                        else
                            instructions.add(new Instruction(Operation.itof));
                    }else if (NO1_ty == SymbolType.DOUBLE && NO2_Type == SymbolType.INT) {
                        NO1.setType(NO2_Type);
                        if (is_g)
                            startInstructions.add(new Instruction(Operation.ftoi));
                        else
                            instructions.add(new Instruction(Operation.ftoi));
                    }
                }
            }   else {
                if (NO1_ty == NO2_Type){
                    switch (ty){
                        case Gt:
                            if (NO1_ty == SymbolType.INT)
                                instructions.add(new Instruction(Operation.cmpi));
                            else
                                instructions.add(new Instruction(Operation.cmpf));
                            instructions.add(new Instruction(Operation.setgt));
                            NO1.setType(SymbolType.BOOL);
                            break;
                        case Lt:
                            if (NO1_ty == SymbolType.INT)
                                instructions.add(new Instruction(Operation.cmpi));
                            else
                                instructions.add(new Instruction(Operation.cmpf));
                            instructions.add(new Instruction(Operation.setlt));
                            NO1.setType(SymbolType.BOOL);
                            break;
                        case Ge:
                            if (NO1_ty == SymbolType.INT)
                                instructions.add(new Instruction(Operation.cmpi));
                            else
                                instructions.add(new Instruction(Operation.cmpf));
                            instructions.add(new Instruction(Operation.setlt));
                            instructions.add(new Instruction(Operation.not));
                            NO1.setType(SymbolType.BOOL);
                            break;
                        case Le:
                            if (NO1_ty == SymbolType.INT)
                                instructions.add(new Instruction(Operation.cmpi));
                            else
                                instructions.add(new Instruction(Operation.cmpf));
                            instructions.add(new Instruction(Operation.setgt));
                            instructions.add(new Instruction(Operation.not));
                            NO1.setType(SymbolType.BOOL);
                            break;
                        case Eq:
                            if (NO1_ty == SymbolType.INT)
                                instructions.add(new Instruction(Operation.cmpi));
                            else
                                instructions.add(new Instruction(Operation.cmpf));
                            instructions.add(new Instruction(Operation.not));
                            NO1.setType(SymbolType.BOOL);
                            break;
                        case Neq:
                            if (NO1_ty == SymbolType.INT)
                                instructions.add(new Instruction(Operation.cmpi));
                            else
                                instructions.add(new Instruction(Operation.cmpf));
                            NO1.setType(SymbolType.BOOL);
                            break;
                        case Plus:
                            if (NO1_ty == SymbolType.INT){
                                if (is_g)
                                    startInstructions.add(new Instruction(Operation.addi));
                                else
                                    instructions.add(new Instruction(Operation.addi));
                            }else {
                                if (is_g)
                                    startInstructions.add(new Instruction(Operation.addf));
                                else
                                    instructions.add(new Instruction(Operation.addf));
                            }
                            break;
                        case Minus:
                            if (NO1_ty == SymbolType.INT){
                                if (is_g)
                                    startInstructions.add(new Instruction(Operation.subi));
                                else
                                    instructions.add(new Instruction(Operation.subi));
                            }else {
                                if (is_g)
                                    startInstructions.add(new Instruction(Operation.subf));
                                else
                                    instructions.add(new Instruction(Operation.subf));
                            }
                            break;
                        case Mul:
                            if (NO1_ty == SymbolType.INT) {
                                if (is_g)
                                    startInstructions.add(new Instruction(Operation.muli));
                                else
                                    instructions.add(new Instruction(Operation.muli));
                            }else {
                                if (is_g)
                                    startInstructions.add(new Instruction(Operation.mulf));
                                else
                                    instructions.add(new Instruction(Operation.mulf));
                            }
                            break;
                        case Div:
                            if (NO1_ty == SymbolType.INT){
                                if (is_g)
                                    startInstructions.add(new Instruction(Operation.divi));
                                else
                                    instructions.add(new Instruction(Operation.divi));
                            }else {
                                if (is_g)
                                    startInstructions.add(new Instruction(Operation.divf));
                                else
                                    instructions.add(new Instruction(Operation.divf));
                            }
                            break;
                    }
                }else throw new AnalyzeError(ErrorCode.InvalidInput,NO1.getStartPos());
            }
        }
    }

    private OPGSymbol analyse_other_expr(boolean is_g) throws CompileError{//over
        ArrayList<Instruction> chosed_Instruction;
        Token token;
        if (is_g)
            chosed_Instruction = startInstructions;
        else
            chosed_Instruction = instructions;
        if (peek().getTokenType() == TokenType.Uint_Literal){
            token = next();
            chosed_Instruction.add(new Instruction(Operation.push, token.getValue()));
            return new OPGSymbol(SymbolType.INT, token.getStartPos());
        }else if (check(TokenType.Double_Literal)){
            token = expect(TokenType.Double_Literal);
            chosed_Instruction.add(new Instruction(Operation.push, Double.doubleToRawLongBits((double) token.getValue())));
            return new OPGSymbol(SymbolType.DOUBLE,token.getStartPos());
        }else if (check(TokenType.String_Literal)){
            token = expect(TokenType.String_Literal);
            chosed_Instruction.add(new Instruction(Operation.push, (long) global_offset++));
            Globals.add(token.getValueString());
            return new OPGSymbol(SymbolType.INT,token.getStartPos());
        }else if(check(TokenType.Char_Literal)){
            token = expect(TokenType.Char_Literal);
            chosed_Instruction.add(new Instruction(Operation.push,(long)(char) token.getValue()));
            return new OPGSymbol(SymbolType.INT,token.getStartPos());
        }
        else if (check(TokenType.Ident)){
            token = expect(TokenType.Ident);
            Integer now_index = this.hashMap.get(token.getValueString());
            Symbol sym = null;
            if (now_index != null)
                sym = this.symbolTable.get(now_index);
            if (check(TokenType.Assign)){
                if (sym == null)
                    throw new AnalyzeError(ErrorCode.InvalidInput, token.getStartPos());
                switch (sym.getStorageType()){
                    case global:
                        instructions.add(new Instruction(Operation.globa, sym.getOffset()));
                        break;
                    case argument:
                        instructions.add(new Instruction(Operation.arga, sym.getOffset()));
                        break;
                    case local:
                        instructions.add(new Instruction(Operation.loca, sym.getOffset()));
                        break;
                }
                Token Assign = next();
                OPGSymbol em = analyse_opg_expr(false);
                change_Initialized(token.getValueString(),token.getStartPos());
                if (em.getType() != sym.getSymbolType())
                    throw new AnalyzeError(ErrorCode.InvalidInput, em.getStartPos());
                instructions.add(new Instruction(Operation.store64));
                return new OPGSymbol(SymbolType.VOID, Assign.getStartPos());
            }else if (peek().getTokenType() == TokenType.L_paren){
                next();
                SymbolType fun_re_ty;
                ArrayList<SymbolType> params;
                int call_name_offset = -1;
                if (sym == null){
                    switch (token.getValueString()){
                        case "getint":
                        case "getchar":
                            fun_re_ty = SymbolType.INT;
                            params = new ArrayList<>();
                            break;
                        case "getdouble":
                            fun_re_ty = SymbolType.DOUBLE;
                            params = new ArrayList<>();
                            break;
                        case "putint":
                        case "putchar":
                        case "putstr":
                            fun_re_ty = SymbolType.VOID;
                            params = new ArrayList<SymbolType>(){{
                                add(SymbolType.INT);
                            }};
                            break;
                        case "putdouble":
                            fun_re_ty = SymbolType.VOID;
                            params = new ArrayList<SymbolType>(){{
                                add(SymbolType.DOUBLE);
                            }};
                            break;
                        case "putln":
                            fun_re_ty = SymbolType.VOID;
                            params = new ArrayList<>();
                            break;
                        default:
                            throw new AnalyzeError(ErrorCode.InvalidInput, token.getStartPos());
                    }
                    Globals.add(token.getValueString());
                    call_name_offset = global_offset++;
                }else {
                    fun_re_ty = sym.getSymbolType();
                    params = sym.getParams();
                }

                int sizeof_stack;
                if (fun_re_ty ==  SymbolType.VOID)
                    sizeof_stack = 0;
                else
                    sizeof_stack = 1;
                chosed_Instruction.add(new Instruction(Operation.stackalloc,sizeof_stack));
                int sizeof_params = params.size();
                int i = 0;
                if (check(TokenType.Minus) || check(TokenType.Ident) || check(TokenType.Uint_Literal)
                        || check(TokenType.Double_Literal) || check(TokenType.String_Literal) || check(TokenType.Char_Literal)
                        || check(TokenType.L_paren)){
                    OPGSymbol em = analyse_opg_expr(is_g);
                    if (i+1 > sizeof_params || em.getType() != params.get(i++))
                        throw new AnalyzeError(ErrorCode.InvalidInput,em.getStartPos());
                    while (nextIf(TokenType.Comma) != null){
                        em = analyse_opg_expr(is_g);
                        if (i + 1 > sizeof_params || em.getType() != params.get(i++))
                            throw new AnalyzeError(ErrorCode.InvalidInput, em.getStartPos());
                    }
                }
                expect(TokenType.R_paren);
                if (sym == null)
                    chosed_Instruction.add(new Instruction(Operation.callname,call_name_offset));
                else
                    chosed_Instruction.add(new Instruction(Operation.call,sym.getFuncOffset()));
                return new OPGSymbol(fun_re_ty, token.getStartPos());
            }else {
                if (sym == null)
                    throw new AnalyzeError(ErrorCode.InvalidInput, token.getStartPos());
                switch (sym.getStorageType()){
                    case global:
                        chosed_Instruction.add(new Instruction(Operation.globa, sym.getOffset()));
                        break;
                    case argument:
                        instructions.add(new Instruction(Operation.arga,sym.getOffset()));
                        break;
                    case local:
                        instructions.add(new Instruction(Operation.loca,sym.getOffset()));
                        break;
                }
                chosed_Instruction.add(new Instruction(Operation.load64));
                return new OPGSymbol(sym.getSymbolType(),token.getStartPos());
            }
        }else if (peek().getTokenType() == TokenType.Minus){
            return analyse_negate_expr(is_g);
        }else if (peek().getTokenType() == TokenType.L_paren){
            next();
            OPGSymbol em = analyse_opg_expr(is_g);
            expect(TokenType.R_paren);
            return em;
        }else
            throw new ExpectedTokenError(Arrays.asList(TokenType.Uint_Literal,TokenType.Double_Literal,TokenType.String_Literal,
                    TokenType.Char_Literal,TokenType.Ident,TokenType.Minus),peek());
    }
    private SymbolType analysety() throws  CompileError{ //
        //ty -> int|void
        if (peek().getTokenType() == TokenType.Int){
            next();
            return SymbolType.INT;
        }
        else if (nextIf(TokenType.Double_Literal) != null)
            return SymbolType.DOUBLE;
        else if (nextIf(TokenType.Void) != null)
            return SymbolType.VOID;
        else {
            List<TokenType> list = Arrays.asList(TokenType.Int,TokenType.Double_Literal,TokenType.Void);
            throw  new ExpectedTokenError(list,peek());
        }
    }

    private void analyse_expr_stmt() throws CompileError{ //over
        /*expr_stmt -> expr ';'
         * */
        OPGSymbol em = analyse_opg_expr(false);
        expect(TokenType.Semicolon);
        if (em.getType() != SymbolType.VOID)
            instructions.add(new Instruction(Operation.pop));
    }
    private void analyse_expr() throws CompileError{
        //表达式
        /*expr ->
                operator_expr
                        | negate_expr
                        | assign_expr
                        | as_expr
                        | call_expr
                        | literal_expr
                        | ident_expr
                        | group_expr */

    }

    private void analyse_operator_expr() throws CompileError{ //over
        //operator_expr -> expr binary_operator expr
        analyse_expr();
        analyse_binary_operator();
        analyse_expr();
    }

    private void analyse_binary_operator() throws CompileError{//not over
        //运算符
        //binary_operator -> '+' | '-' | '*' | '/' | '==' | '!=' | '<' | '>' | '<=' | '>='

    }


    private OPGSymbol analyse_negate_expr(boolean is_g) throws CompileError{//over
        //取反表达式
        //negate_expr -> '-' expr
        expect(TokenType.Minus);
        OPGSymbol em = analyse_other_expr(is_g);
        if (em.getType() == SymbolType.INT){
            if (is_g)
                startInstructions.add(new Instruction(Operation.negi));
            else
                instructions.add(new Instruction(Operation.negi));
        }else {
            if (is_g)
                startInstructions.add(new Instruction(Operation.negf));
            else
                instructions.add(new Instruction(Operation.negf));
        }
        return em;
    }

    private void analyse_assign_expr() throws CompileError{//over
        //赋值表达式
        //l_expr -> IDENT
        //assign_expr -> l_expr '=' expr
        expect(TokenType.Ident);
        expect(TokenType.Assign);
        analyse_expr();
    }

    private void analyse_as_expr() throws CompileError{
        //类型转换表达式
        //as_expr -> expr 'as' ty
        analyse_expr();
        expect(TokenType.As);
        analysety();
    }

    private void analyse_call_expr() throws CompileError{//over
        //call_param_list -> expr (',' expr)*
        //call_expr -> IDENT '(' call_param_list? ')'
        expect(TokenType.Ident);
        expect(TokenType.L_paren);
        var peek = peek();
        if(peek.getTokenType() != TokenType.R_paren){

        }
        else next();
    }

    private void analyse_call_param_list() throws CompileError{//over
        //call_param_list -> expr (',' expr)*
        analyse_expr();
        while (peek().getTokenType() == TokenType.Comma){
            next();
            analyse_expr();
        }
    }

    private void analyse_literal_expr() throws CompileError{//not over
        //字面量表达式
        //literal_expr -> UINT_LITERAL | DOUBLE_LITERAL | STRING_LITERAL
        //暂时不考虑浮点类型

    }

    private void analyse_ident_expr() throws CompileError{
        //标识符表达式
        //ident_expr -> IDENT
        expect(TokenType.Ident);

    }

    private void analyse_group_expr() throws CompileError{
        //括号表达式
        //group_expr -> '(' expr ')'
        expect(TokenType.L_paren);
        analyse_expr();
        expect(TokenType.R_paren);
    }

    private void analyse_function() throws CompileError{ //over
        //函数声明
        /*
        function -> 'fn' IDENT '(' function_param_list? ')' '->' ty block_stmt
                         ^~~~      ^~~~~~~~~~~~~~~~~~~~          ^~ ^~~~~~~~~~
                         |              |                        |  |
                         function_name  param_list     return_type  function_body
        * */
        expect(TokenType.Fn);
        Token name = expect(TokenType.Ident);
        Symbol func_Symbol = add_FuncSymbol(name.getValueString(), name.getStartPos());
        local_offset = 0;
        FunctionInstruction functionInstruction = new FunctionInstruction(Operation.func);
        instructions.add(functionInstruction);
        expect(TokenType.L_paren);
        if(peek().getTokenType() != TokenType.R_paren){
            analyse_function_param_list(func_Symbol.getParams());
        }
        expect(TokenType.R_paren);
        expect(TokenType.Arrow);
        SymbolType ty = analysety();
        func_Symbol.setSymbolType(ty);
        functionInstruction.setParamCount(argument_offset);
        if (ty == SymbolType.VOID)
            functionInstruction.setReturnCount(0);
        else {
            functionInstruction.setReturnCount(1);
            int end = symbolTable.size();
            end = end - 1;
            for (int i = 0;i < argument_offset;i++){
                Symbol symbol = this.symbolTable.get(end - 1);
                symbol.setOffset(symbol.getOffset() + 1);
            }
        }
        functionInstruction.setOffset(func_Symbol.getOffset());
        boolean[] c = analyse_block_stmt(true,false,ty,0,null);
        if(ty != SymbolType.VOID && !c[0])
            throw new AnalyzeError(ErrorCode.InvalidInput,name.getStartPos());
        if(ty == SymbolType.VOID && !c[0])
            instructions.add(new Instruction((Operation.ret)));
        functionInstruction.setLocalCount(local_offset);
    }

    private void analyse_function_param_list(ArrayList<SymbolType> params) throws CompileError{
        //参数列表
        //function_param_list -> function_param (',' function_param)*
        analyse_function_param(params);
        while (peek().getTokenType() == TokenType.Comma){
            next();
            analyse_function_param(params);
        }
    }

    private void analyse_function_param(ArrayList<SymbolType> params) throws CompileError{
        //参数
        //function_param -> 'const'? IDENT ':' ty

        //judge判断是否为const类型
        boolean judge = false;
        if(peek().getTokenType() == TokenType.Const){
            next();
            judge = true;
        }
        Token name = expect(TokenType.Ident);
        expect(TokenType.Colon);
        SymbolType ty = analysety();
        add_Symbol(name.getValueString(),judge,true,ty,StorageType.argument,name.getStartPos());
        params.add(ty);
    }
}
