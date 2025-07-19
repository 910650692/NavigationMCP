package com.sgm.navi.service.logicpaket.activate;

import com.sgm.navi.patacnetlib.NetQueryManager;
import com.sgm.navi.patacnetlib.response.activate.AppKeyResponse;
import com.sgm.navi.patacnetlib.response.activate.UuidResponse;
import com.sgm.navi.service.adapter.activate.ActivateAdapter;
import com.sgm.navi.service.adapter.activate.ActivateObserver;

import java.util.ArrayList;
import java.util.List;

public final class ActivatePackage implements ActivateObserver {
    private final ActivateAdapter mActivateAdapter;
    private final List<IActivateObserver> mActObserverList;

    private ActivatePackage() {
        mActivateAdapter = ActivateAdapter.getInstance();
        mActObserverList = new ArrayList<>();
        mActivateAdapter.addActivateObserver(this);
    }

    private static final class Helper {
        private static final ActivatePackage INSTANCE = new ActivatePackage();
    }

    public static ActivatePackage getInstance() {
        return ActivatePackage.Helper.INSTANCE;
    }

    public boolean checkActivation() {
        return mActivateAdapter.checkActivation();
    }

    /**
     * 开始激活流程
     */
    public void startActivate() {
        mActivateAdapter.initActivate();
    }

    /**
     * 添加激活观察者
     * @param actObserver actObserver
     */
    public void addActObserver(final IActivateObserver actObserver) {
        if (actObserver == null || mActObserverList.contains(actObserver)) {
            return;
        }
        mActObserverList.add(actObserver);
    }

    /**
     * 反初始化
     */
    public void unInit() {
        mActivateAdapter.unInit();
    }

    /**
     * 移除激活观察者
     * @param actObserver actObserver
     */
    public void removeActObserver(final IActivateObserver actObserver) {
        if (actObserver == null || !mActObserverList.contains(actObserver)) {
            return;
        }
        mActObserverList.remove(actObserver);
    }

    @Override
    public void onActivating() {
        for (IActivateObserver actObserver : mActObserverList) {
            if (actObserver != null) {
                actObserver.onActivating();
            }
        }
    }

    @Override
    public void onActivated() {
        for (IActivateObserver actObserver : mActObserverList) {
            if (actObserver != null) {
                actObserver.onActivated();
            }
        }
    }

    @Override
    public void onActivatedError(final int errCode, final String msg) {
        for (IActivateObserver actObserver : mActObserverList) {
            if (actObserver != null) {
                actObserver.onActivatedError(errCode, msg);
            }
        }
    }

    @Override
    public String getAppKeyFromDB() {
        return mActivateAdapter.getAppKeyFromDB();
    }

    @Override
    public void getAppKeyFromNet(NetQueryManager.INetResultCallBack<AppKeyResponse> callBack) {
        mActivateAdapter.getAppKeyFromNet(callBack);
    }

    @Override
    public String getUuidFromDB() {
        return mActivateAdapter.getUuidFromDB();
    }

    @Override
    public void getUuidFromNet(NetQueryManager.INetResultCallBack<UuidResponse> callBack) {
        mActivateAdapter.getUuidFromNet(callBack);
    }

    /**
     * 手动激活（无需调用）
     *
     * @param loginCode 激活码
     * @param userCode 序列号
     */
    public void manualActivate(final String userCode, final String loginCode) {
        mActivateAdapter.manualActivate(userCode, loginCode);
    }
}
