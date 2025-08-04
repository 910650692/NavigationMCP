package com.sgm.navi.service.logicpaket.navi;


import android.car.Car;
import android.car.CarAppFocusManager;
import android.content.Context;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.logicpaket.cruise.CruisePackage;

public class AppFocusHelper implements CarAppFocusManager.OnAppFocusChangedListener, CarAppFocusManager.OnAppFocusOwnershipCallback {

    private static final String TAG = "AppFocusHelper";
    private CarAppFocusManager mCarAppFocusManager;
    private Car mCar;
    private boolean mIsCarMapNavigating = false;

    private AppFocusHelper() {
    }

    public static AppFocusHelper getInstance() {
        return InstanceHolder.INSTANCE;
    }

    private static class InstanceHolder {
        private static final AppFocusHelper INSTANCE = new AppFocusHelper();
    }

    public void init(Context context) {
        connectToCar();
    }

    private void connectToCar() {
        if (AppCache.getInstance().getMContext() == null) {
            return;
        }
        mCar = Car.createCar(
                AppCache.getInstance().getMContext(),
                null,
                Car.CAR_WAIT_TIMEOUT_WAIT_FOREVER,
                (car, ready) -> {
                    if (ready) {
                        Logger.d(TAG, "Car connected successfully");
                        mCarAppFocusManager = (CarAppFocusManager) car.getCarManager(Car.APP_FOCUS_SERVICE);
                        if (mCarAppFocusManager != null) {
                            mCarAppFocusManager.addFocusListener(this, CarAppFocusManager.APP_FOCUS_TYPE_NAVIGATION);
                        }
                    }
                }
        );
    }

    // 车机地图开始导航
    public void startCarMapNavigation() {
        if (mCarAppFocusManager != null) {
            int result = mCarAppFocusManager.requestAppFocus(CarAppFocusManager.APP_FOCUS_TYPE_NAVIGATION, this);
            if (ConvertUtils.equals(result,CarAppFocusManager.APP_FOCUS_REQUEST_SUCCEEDED)) {
                mIsCarMapNavigating = true;
                Logger.d(TAG, "车机地图获得导航焦点，开始导航/巡航");
            } else {
                Logger.e(TAG, "其余焦点信息 " + result);
            }
        }
    }

    // 车机地图停止导航
    public void stopCarMapNavigation() {
        if (mCarAppFocusManager != null && mIsCarMapNavigating) {
            mCarAppFocusManager.abandonAppFocus(this, CarAppFocusManager.APP_FOCUS_TYPE_NAVIGATION);
            mIsCarMapNavigating = false;
            Logger.d(TAG, "车机地图放弃导航焦点，停止导航/巡航");
        }
    }

    // 焦点变化监听
    @Override
    public void onAppFocusChanged(int appType, boolean active) {
        Logger.d(TAG, "onAppFocusChanged appType = " + appType + " active = " + active);
//        if (ConvertUtils.equals(appType, CarAppFocusManager.APP_FOCUS_TYPE_NAVIGATION)) {
//            if (active) {
//                if (mIsCarMapNavigating) {
//                    // Carplay获得焦点，车机地图退出导航
//                    Logger.d(TAG, "Carplay开始导航，车机地图退出导航");
//                    NaviPackage.getInstance().stopNavigation(false);
//                }
//            }
//        }
    }

    // 焦点被系统收回时回调
    @Override
    public void onAppFocusOwnershipLost(int appType) {
        if (ConvertUtils.equals(appType, CarAppFocusManager.APP_FOCUS_TYPE_NAVIGATION) && mIsCarMapNavigating) {
            Logger.d(TAG, "车机地图导航焦点被收回，退出导航/巡航");
            mIsCarMapNavigating = false;
            NaviPackage.getInstance().stopNavigation(false);

            CruisePackage cruise = CruisePackage.getInstance();
            if (cruise != null) {
                cruise.stopCruiseWithFocusLost();
            }
        }
    }

    @Override
    public void onAppFocusOwnershipGranted(int appType) {

    }

    public void release() {
        if (mCarAppFocusManager != null) {
            mCarAppFocusManager.removeFocusListener(this);
        }
        if (mCar != null) {
            mCar.disconnect();
            mCar = null;
        }
    }
}
