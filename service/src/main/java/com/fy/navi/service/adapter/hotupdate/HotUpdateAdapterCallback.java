package com.fy.navi.service.adapter.hotupdate;

import com.fy.navi.service.define.hotupdate.MapNumInfo;

public interface HotUpdateAdapterCallback {

    /**
     * 网络请求aos审图号回调
     * @param errorCode 回调操作状态码, 返回错误码 0:成功, 其他:失败
     * @param mapNumInfo 审图号信息
     */
    void onRequestMapNum(int errorCode, MapNumInfo mapNumInfo);
}
