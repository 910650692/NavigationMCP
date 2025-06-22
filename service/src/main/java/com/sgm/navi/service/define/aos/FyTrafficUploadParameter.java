package com.sgm.navi.service.define.aos;

/**
 * Author: QiuYaWei
 * Date: 2025/2/28
 * Description: [在这里描述文件功能]
 */
public class FyTrafficUploadParameter {
    public int action;// 1 - 创建 4 - 更新 5 - 恢复 是 默认 1
    public double lat;
    public double lon;
    public int type; // 0 踩 1 赞
    public String eventId; // 交通事件ID
}
