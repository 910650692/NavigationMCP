package com.sgm.navi.mcp.core;

/**
 * MCP工具参数和返回值的数据类型定义
 */
public enum MCPDataType {
    STRING("string"),
    NUMBER("number"),
    INTEGER("integer"),
    BOOLEAN("boolean"),
    OBJECT("object"),
    ARRAY("array");
    
    private final String type;
    
    MCPDataType(String type) {
        this.type = type;
    }
    
    public String getType() {
        return type;
    }
    
    @Override
    public String toString() {
        return type;
    }
}