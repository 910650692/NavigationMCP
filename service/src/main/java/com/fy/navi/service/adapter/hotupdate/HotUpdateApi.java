package com.fy.navi.service.adapter.hotupdate;

import com.fy.navi.service.define.hotupdate.MapNumInfo;

public interface HotUpdateApi {

    /**
     * 网络请求aos审图号信息
     * @param mapNumInfo 本地审图号信息
     * @return 是否成功发起获取aos审图号的网络请求
     *  1 发起网络请求成功，并通过pObserver回调结果
     *  0 发起网络请求失败
     * -1 pObserver 观察者为空
     * -2 参数错误，localMapNum.strKey为空
     */
    int requestMapNum(MapNumInfo mapNumInfo);

    /**
     * 初始化服务
     */
    void initService();

    /**
     * 注册回调
     * @param key 回调key
     * @param resultCallback 回调
     */
    void registerCallback(String key, HotUpdateAdapterCallback resultCallback);
}
