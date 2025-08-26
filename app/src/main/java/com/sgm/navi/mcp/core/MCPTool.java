package com.sgm.navi.mcp.core;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * MCP工具方法注解
 * 用于标记可以被MCP调用的工具方法
 * 兼容新SDK字段格式
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface MCPTool {
    /**
     * 工具名称，用于MCP调用识别
     */
    String name();
    
    /**
     * 工具标题（可选，用于UI显示）
     */
    String title() default "";
    
    /**
     * 工具描述
     */
    String description() default "";
    
    /**
     * 返回值描述（可选，描述返回值的格式和内容）
     */
    String returnDescription() default "";
    
    /**
     * 返回值类型
     */
    MCPDataType returnType() default MCPDataType.STRING;
    
    /**
     * 是否为只读操作的提示（可选）
     */
    boolean readOnlyHint() default false;
    
    /**
     * 是否为开放世界操作的提示（可选）
     */
    boolean openWorldHint() default false;
    
    /**
     * 工具版本
     */
    String version() default "1.0";
}