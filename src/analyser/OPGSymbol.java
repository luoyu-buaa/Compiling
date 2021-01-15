package analyser;

import symbol.SymbolType;
import util.Pos;

public class OPGSymbol{
    private SymbolType type;
    private Pos startPos;

    public OPGSymbol(SymbolType type, Pos startPos) {
        this.type = type;
        this.startPos = startPos;
    }

    public SymbolType getType() {
        return type;
    }

    public Pos getStartPos() {
        return startPos;
    }

    public void setType(SymbolType type) {
        this.type = type;
    }
}
