# SGM导航MCP工具开发计划 - 导航功能专项

## 一、现状分析

### 已完成的导航相关MCP工具（4个）
- `get_current_location` - 获取车辆当前位置
- `search_poi` - POI搜索
- `search_nearby_poi` - 附近POI搜索  
- `start_navigation` - 启动导航

## 二、用户高频导航场景分析

基于用户使用导航的实际场景，以下是高频需求：

1. **回家/去公司** - 每天通勤场景，最高频
2. **查看路况/避堵** - 出行前查看路况，选择最优路线
3. **加油站/充电站** - 车辆能源补给需求
4. **停车场** - 到达目的地前的停车需求
5. **途经点设置** - 多目的地串联导航
6. **路线偏好设置** - 避开收费/高速/拥堵
7. **导航中操作** - 静音、结束导航、查看全程
8. **历史记录/收藏** - 常去地点快速导航

## 三、按优先级规划的MCP工具开发

### P0级 - 核心高频功能（1周内完成）

#### 1. FavoriteTools（收藏/常用地址工具）

**工具列表：**
- `set_home_address` - 设置家地址
- `set_company_address` - 设置公司地址  
- `go_home` - 一键回家导航
- `go_company` - 一键去公司导航
- `add_favorite` - 添加收藏地点
- `get_favorites` - 获取收藏列表
- `delete_favorite` - 删除收藏

**应用场景**：通勤是最高频场景，一键导航极大提升体验

**实现要点**：
- 基于BehaviorPackage的收藏功能
- 缓存常用地址，提高响应速度
- 支持自定义名称和图标

#### 2. RouteTools（路线管理工具）

**工具列表：**
- `get_route_info` - 获取当前路线信息（距离、时间、路况）
- `calculate_route` - 路线规划（支持多种偏好）
- `switch_route` - 切换备选路线
- `add_waypoint` - 添加途经点
- `remove_waypoint` - 删除途经点
- `get_route_traffic` - 获取路线实时路况

**应用场景**：用户需要查看路况、比较路线、设置途经点

**实现要点**：
- 基于RoutePackage的路线规划能力
- 支持多路线对比
- 实时路况更新

### P1级 - 常用便捷功能（2周内完成）

#### 3. NavigationControlTools（导航控制工具）

**工具列表：**
- `pause_navigation` - 暂停导航
- `resume_navigation` - 继续导航
- `stop_navigation` - 结束导航
- `mute_voice` - 静音播报
- `unmute_voice` - 开启播报
- `zoom_to_overview` - 查看全程
- `zoom_to_current` - 回到当前位置
- `switch_day_night_mode` - 切换日夜模式

**应用场景**：导航过程中的常用操作

**实现要点**：
- 基于NaviPackage的导航控制
- 状态同步和反馈
- 支持语音控制集成

#### 4. PoiSearchTools（特定POI搜索工具）

**工具列表：**
- `search_gas_station` - 搜索加油站
- `search_charging_station` - 搜索充电站
- `search_parking_lot` - 搜索停车场
- `search_restaurant` - 搜索餐厅
- `search_hotel` - 搜索酒店
- `search_along_route` - 沿途搜索POI

**应用场景**：特定类型POI的快速搜索，尤其是加油/充电/停车

**实现要点**：
- 基于SearchPackage的搜索能力
- 预设搜索类别和关键词
- 智能排序（距离、评分、价格）

### P2级 - 增强体验功能（3-4周内完成）

#### 5. HistoryTools（历史记录工具）

**工具列表：**
- `get_navigation_history` - 获取导航历史
- `get_search_history` - 获取搜索历史
- `clear_history` - 清除历史记录
- `navigate_to_history` - 导航到历史地点

**应用场景**：快速访问最近去过的地方

**实现要点**：
- 基于UserTrackPackage的历史记录
- 智能排序和去重
- 支持按时间、频率筛选

#### 6. RoutePreferenceTools（路线偏好工具）

**工具列表：**
- `set_avoid_highway` - 设置避开高速
- `set_avoid_toll` - 设置避开收费
- `set_avoid_congestion` - 设置避开拥堵
- `set_prefer_highway` - 设置高速优先
- `get_route_preference` - 获取当前偏好设置
- `set_custom_avoid_road` - 设置自定义避让路段

**应用场景**：个性化路线规划需求

**实现要点**：
- 基于SettingPackage的偏好设置
- 持久化存储用户偏好
- 支持临时偏好和永久偏好

#### 7. TrafficTools（路况信息工具）

**工具列表：**
- `get_traffic_info` - 获取实时路况
- `get_traffic_events` - 获取交通事件
- `report_traffic_event` - 上报交通事件
- `get_commute_traffic` - 获取通勤路况预测

**应用场景**：出行前查看路况，避开拥堵

**实现要点**：
- 实时路况数据获取
- 交通事件推送
- 历史路况分析

## 四、技术实施规范

### 1. 代码结构
```
com.sgm.navi.mcp.tools/
├── FavoriteTools.java       # 收藏/常用地址
├── RouteTools.java          # 路线管理
├── NavigationControlTools.java  # 导航控制
├── PoiSearchTools.java      # POI搜索
├── HistoryTools.java        # 历史记录
├── RoutePreferenceTools.java  # 路线偏好
└── TrafficTools.java        # 路况信息
```

### 2. 开发规范

#### 参数设计原则
- 使用扁平化JSON结构，避免嵌套
- 明确标注必填/选填参数
- 提供参数默认值
- 包含调用示例

#### 错误处理
```java
// 统一使用BaseToolHelper的错误响应格式
JsonObject error = baseHelper.createErrorResponse(
    "错误类型",
    "详细错误信息"
);
```

#### 异步处理
```java
// 使用CompletableFuture处理异步调用
CompletableFuture<String> future = new CompletableFuture<>();
future = future.completeOnTimeout(null, 6000, TimeUnit.MILLISECONDS);
```

### 3. 测试要求

- **单元测试**：每个工具方法必须有对应的单元测试
- **集成测试**：验证MCP注册和调用流程
- **场景测试**：覆盖主要使用场景
- **性能测试**：响应时间不超过3秒

## 五、开发计划

### 第1周（P0级）
- Day 1-3: 完成FavoriteTools开发和测试
- Day 4-5: 完成RouteTools开发和测试

### 第2周（P1级）
- Day 1-2: 完成NavigationControlTools
- Day 3-5: 完成PoiSearchTools

### 第3周（P2级前半部分）
- Day 1-3: 完成HistoryTools
- Day 4-5: 完成RoutePreferenceTools基础功能

### 第4周（P2级后半部分）
- Day 1-3: 完成RoutePreferenceTools高级功能
- Day 4-5: 完成TrafficTools

## 六、预期成果

### 功能覆盖
- **MCP工具数量**：从4个增加到40+个
- **场景覆盖率**：覆盖90%以上的高频导航场景
- **操作效率**：减少50%的操作步骤

### 用户体验提升
1. **一键操作**：如"带我回家"、"去公司"
2. **智能理解**：支持自然语言交互
3. **个性化**：记住用户偏好和习惯
4. **场景化**：支持复杂的串联场景

### 技术指标
- **响应时间**：95%的请求在3秒内响应
- **成功率**：API调用成功率>99%
- **可用性**：支持离线/在线混合模式

## 七、风险与对策

### 风险点
1. **性能风险**：大量MCP工具可能影响系统性能
2. **兼容性风险**：需要兼容不同版本的导航引擎
3. **数据一致性**：多个工具操作同一数据源

### 应对措施
1. **性能优化**：使用缓存、批处理、异步调用
2. **版本管理**：建立版本兼容性矩阵
3. **事务管理**：确保数据操作的原子性

## 八、后续扩展建议

1. **智能推荐**：基于用户习惯推荐目的地
2. **多模态交互**：支持语音、手势等交互方式
3. **跨设备同步**：手机、车机、智能手表数据同步
4. **生态集成**：与第三方服务（如美团、大众点评）集成

---

*文档版本：v1.0*  
*创建日期：2025-08-21*  
*最后更新：2025-08-21*