package com.fy.navi.service.logicpaket.activate;

import com.fy.navi.service.adapter.activate.ActivateAdapter;
import com.fy.navi.service.adapter.activate.ActivateObserver;

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
        mActObserverList.add(actObserver);
    }

    /**
     * 移除激活观察者
     * @param actObserver actObserver
     */
    public void removeActObserver(final IActivateObserver actObserver) {
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
    public void onNetActivateFailed(final int failedCount) {
        for (IActivateObserver actObserver : mActObserverList) {
            if (actObserver != null) {
                actObserver.onNetActivateFailed(failedCount);
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
    public void onActivatedError() {
        for (IActivateObserver actObserver : mActObserverList) {
            if (actObserver != null) {
                actObserver.onActivatedError();
            }
        }
    }

    /**
     * 重试网络激活（无需调用）
     */
    public void netActivateRetry() {
        mActivateAdapter.netActivateRetry();
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
