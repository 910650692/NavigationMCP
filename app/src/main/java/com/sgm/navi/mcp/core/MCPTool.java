package com.sgm.navi.mcp.core;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * MCP工具方法注解
 * 用于标记可以被MCP调用的工具方法
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface MCPTool {
    /**
     * 工具名称，用于MCP调用识别
     */
    String name();
    
    /**
     * 工具描述
     */
    String description() default "";
    
    /**
     * 返回值类型
     */
    MCPDataType returnType() default MCPDataType.STRING;
    
    /**
     * 工具版本
     */
    String version() default "1.0";
}