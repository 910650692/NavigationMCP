# MCP HTTP服务配置说明

## 概述

SGM导航应用现已支持完整的MCP HTTP服务，外部客户端（如CherryStudio）可以通过标准JSON-RPC 2.0协议与车端导航工具进行通信。

## 完整链路架构

```
CherryStudio → HTTP POST(JSON-RPC) → MCPCoordinatorService → AIDL → SGMNavigationService → 导航工具
```

## 服务启动顺序

1. **NaviApplication** 启动时自动启动 **MCPCoordinatorService**
2. 延迟1秒后启动 **SGMNavigationService** 
3. SGMNavigationService 连接到 MCPCoordinatorService 并注册工具
4. MCPCoordinatorService 启动HTTP服务器监听8080端口

## CherryStudio配置

### 配置文件格式
```json
{
  "name": "sgm-navigation",
  "url": "http://你的设备IP:8080/mcp"
}
```

### 配置步骤
1. 打开CherryStudio
2. 进入MCP服务器配置
3. 添加新的MCP服务器
4. 输入配置信息：
   - **名称**: SGM Navigation
   - **URL**: http://192.168.x.x:8080/mcp （替换为实际设备IP）

## 支持的MCP方法

| 方法 | 描述 | 状态 |
|------|------|------|
| `initialize` | 初始化MCP连接 | ✅ 支持 |
| `tools/list` | 获取可用工具列表 | ✅ 支持 |
| `tools/call` | 调用指定工具 | ✅ 支持 |

## 可用的导航工具

1. **get_current_location** - 获取车辆当前GPS位置信息
2. **navigate_to** - 启动导航到指定目的地
3. **search_nearby** - 搜索附近的兴趣点(POI)

## 测试示例

### 1. 初始化连接
```json
{
  "jsonrpc": "2.0",
  "method": "initialize",
  "params": {
    "protocolVersion": "2024-11-05",
    "capabilities": {}
  },
  "id": 1
}
```

### 2. 获取工具列表
```json
{
  "jsonrpc": "2.0",
  "method": "tools/list",
  "params": {},
  "id": 2
}
```

### 3. 获取当前位置
```json
{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "get_current_location",
    "arguments": {}
  },
  "id": 3
}
```

## 日志监控

### 关键日志标签
- `MCPCoordinatorService` - 协调中心服务日志
- `HttpServerManager` - HTTP服务器日志
- `JsonRpcHandler` - JSON-RPC处理日志
- `SGMNavigationService` - 导航服务日志

### 成功启动的日志示例
```
D/MCPCoordinatorService: MCP协调中心服务启动，支持AIDL和HTTP JSON-RPC接口
D/HttpServerManager: ✅ MCP HTTP服务器启动成功，端口: 8080
D/SGMNavigationService: ✅ 已连接到MCP协调中心
D/SGMNavigationService: ✅ 导航工具注册完成，共注册 3 个工具
```

## 故障排除

### 常见问题

1. **HTTP服务器启动失败**
   - 检查端口8080是否被占用
   - 确认网络权限已授予

2. **CherryStudio连接失败**
   - 确认设备IP地址正确
   - 检查防火墙设置
   - 验证HTTP服务器是否正在运行

3. **工具调用失败**
   - 检查SGMNavigationService是否正常连接
   - 验证工具是否已正确注册

### 网络测试

使用curl测试HTTP服务器：
```bash
curl -X POST http://192.168.x.x:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "method": "tools/list",
    "params": {},
    "id": 1
  }'
```

## 技术实现细节

### HTTP服务器
- 基于NanoHTTPD轻量级HTTP服务器
- 支持CORS跨域请求
- 自动处理JSON-RPC 2.0协议

### 协议转换
- JSON-RPC请求 → AIDL调用
- 异步工具调用 → 同步HTTP响应
- 统一错误处理和日志记录

### 安全特性
- 仅响应POST /mcp路径
- 完整的请求验证
- 超时保护机制（10秒）

---

**开发时间**: 2025年8月14日  
**技术栈**: Android + AIDL + NanoHTTPD + JSON-RPC 2.0
**测试状态**: 准备就绪，等待实际测试验证