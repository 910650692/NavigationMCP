package com.sgm.navi.mcp;

import com.sgm.navi.mcp.ToolResponse;

interface IToolCallback {
    void onToolResult(in ToolResponse response);
    void onToolError(in String error);
}