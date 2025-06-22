package com.sgm.navi.service.adapter.hotupdate;

import com.sgm.navi.service.AdapterConfig;
import com.sgm.navi.service.define.hotupdate.MapNumInfo;

import java.util.Objects;

public final class HotUpdateAdapter {

    private static final String HOT_UPDATE_API_PKG = Objects.requireNonNull(HotUpdateAdapter.class.getPackage()).getName();
    private static final String HOT_UPDATE_API_CLS = "HotUpdateAdapterImpl";
    private final HotUpdateApi mHotUpdateApi;

    private HotUpdateAdapter() {
        mHotUpdateApi = (HotUpdateApi) AdapterConfig.getObject(HOT_UPDATE_API_PKG, HOT_UPDATE_API_CLS);
    }

    /**
     * 网络请求aos审图号信息
     * @param mapNumInfo 本地审图号信息
     * @return 是否成功发起获取aos审图号的网络请求
     *  1 发起网络请求成功，并通过pObserver回调结果
     *  0 发起网络请求失败
     * -1 pObserver 观察者为空
     * -2 参数错误，localMapNum.strKey为空
     */
    public int requestMapNum(final MapNumInfo mapNumInfo) {
        return mHotUpdateApi.requestMapNum(mapNumInfo);
    }

    /**
     * 初始化服务
     */
    public void initService() {
        mHotUpdateApi.initService();
    }

    /**
     * 注册回调
     * @param key 回调key
     * @param resultCallback 回调
     */
    public void registerCallback(final String key, final HotUpdateAdapterCallback resultCallback) {
        mHotUpdateApi.registerCallback(key, resultCallback);
    }

    public static HotUpdateAdapter getInstance() {
        return HotUpdateAdapterHolder.INSTANCE;
    }

    private static final class HotUpdateAdapterHolder {
        private static final HotUpdateAdapter INSTANCE = new HotUpdateAdapter();
    }
}
