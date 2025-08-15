package com.sgm.navi.mcp;

import com.sgm.navi.mcp.ToolRequest;
import com.sgm.navi.mcp.IToolCallback;

interface IMCPService {
    void registerTool(in String name, in String description, in String schema, in String packageName);
    void callTool(in ToolRequest request, in IToolCallback callback);
    String[] getRegisteredTools();
}