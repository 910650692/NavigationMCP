package com.sgm.navi.mcp;

import com.sgm.navi.mcp.ToolRequest;
import com.sgm.navi.mcp.ToolResponse;

interface ISGMNavigationService {
    ToolResponse callTool(in ToolRequest request);
}