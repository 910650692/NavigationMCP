package com.sgm.navi.mcp.core;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * MCP工具方法参数注解
 * 用于标记MCP工具方法的参数信息
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.PARAMETER)
public @interface MCPParam {
    /**
     * 参数名称
     */
    String name();
    
    /**
     * 参数类型
     */
    MCPDataType type() default MCPDataType.STRING;
    
    /**
     * 参数描述
     */
    String description() default "";
    
    /**
     * 是否必需参数
     */
    boolean required() default true;
    
    /**
     * 默认值（如果有）
     */
    String defaultValue() default "";
}