package com.sgm.navi.service.logicpaket.hotupdate;

import com.sgm.navi.service.adapter.hotupdate.HotUpdateAdapter;
import com.sgm.navi.service.adapter.hotupdate.HotUpdateAdapterCallback;
import com.sgm.navi.service.define.hotupdate.MapNumInfo;

import java.util.Hashtable;

public class HotUpdatePackage implements HotUpdateAdapterCallback {

    private final HotUpdateAdapter mHotUpdateAdapter;
    private final Hashtable<String, HotUpdateCallback> mCallbackList;


    public HotUpdatePackage() {
        mCallbackList = new Hashtable<>();
        mHotUpdateAdapter = HotUpdateAdapter.getInstance();
    }

    /**
     * 初始化服务
     */
    public void initService() {
        mHotUpdateAdapter.initService();
        mHotUpdateAdapter.registerCallback("HotUpdatePackage", this);
    }

    /**
     * 注册回调
     * @param key 回调key
     * @param callback 回调
     */
    public synchronized void registerCallBack(final String key, final HotUpdateCallback callback) {
        if (callback != null && !mCallbackList.contains(callback)) {
            mCallbackList.put(key, callback);
        }
    }

    /**
     * 取消注册回调
     * @param key 回调key
     */
    public synchronized void unRegisterCallBack(final String key) {
        mCallbackList.remove(key);
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
        return mHotUpdateAdapter.requestMapNum(mapNumInfo);
    }

    @Override
    public void onRequestMapNum(final int errorCode, final MapNumInfo mapNumInfo) {
        for (HotUpdateCallback hotUpdateCallback : mCallbackList.values()) {
            hotUpdateCallback.onRequestMapNum(errorCode, mapNumInfo);
        }
    }

    public static HotUpdatePackage getInstance() {
        return HotUpdatePackageHolder.INSTANCE;
    }

    private static final class HotUpdatePackageHolder {
        private static final HotUpdatePackage INSTANCE = new HotUpdatePackage();
    }
}
