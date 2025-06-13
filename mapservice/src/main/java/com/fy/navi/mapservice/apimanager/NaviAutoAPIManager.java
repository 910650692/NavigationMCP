package com.fy.navi.mapservice.apimanager;

import android.content.Context;
import android.os.IBinder;
import android.os.RemoteException;

import com.fy.navi.mapservice.base.BaseManager;
import com.fy.navi.mapservice.base.BinderManager;
import com.fy.navi.mapservice.base.MapSdk;
import com.fy.navi.mapservice.bean.INaviConstant;
import com.fy.navi.mapservice.bean.common.BaseSearchPoi;
import com.fy.navi.mapservice.bean.common.BaseGeoPoint;
import com.fy.navi.mapservice.callback.OnDistrictInfoChangeListener;
import com.fy.navi.mapservice.callback.OnGuidePanelDataListener;
import com.fy.navi.mapservice.callback.OnInitStateChangeListener;
import com.fy.navi.mapservice.callback.OnLocationChangeListener;
import com.fy.navi.mapservice.callback.OnNaviBroadcastStateListener;
import com.fy.navi.mapservice.callback.OnNaviStatusChangeListener;
import com.fy.navi.mapservice.callback.OnRoutePlanResultListener;
import com.fy.navi.mapservice.callback.OnSearchResultListener;
import com.fy.navi.mapservice.callback.OnSpeedLimitChangeListener;
import com.fy.navi.mapservice.callback.OnSrNaviInfoChangeListener;
import com.fy.navi.mapservice.callback.OnTurnInfoChangeListener;
import com.fy.navi.mapservice.common.INaviAutoApiBinder;
import com.fy.navi.mapservice.common.INaviAutoApiCallback;
import com.fy.navi.mapservice.common.INaviAutoCountDownLightCallback;
import com.fy.navi.mapservice.common.INaviAutoLocationCallback;
import com.fy.navi.mapservice.common.INaviAutoRouteCallback;
import com.fy.navi.mapservice.common.INaviAutoSearchCallback;
import com.fy.navi.mapservice.common.INaviAutoSpeedCallBack;
import com.fy.navi.mapservice.common.INaviAutoStatusCallback;
import com.fy.navi.mapservice.util.Logger;

import java.util.ArrayList;
import java.util.List;

public final class NaviAutoAPIManager extends BaseManager<INaviAutoApiBinder> {

    private static final String TAG = NaviAutoAPIManager.class.getSimpleName();
    private INaviAutoApiBinder mBinder;
    private String mPkgName = "default";
    private boolean mInitStatus = false;

    private final List<OnInitStateChangeListener> mInitStateListenerList = new ArrayList<>();
    private final List<OnLocationChangeListener> mLocationListenerList = new ArrayList<>();
    private final List<OnDistrictInfoChangeListener> mDistrictInfoList = new ArrayList<>();
    private final List<OnNaviStatusChangeListener> mNaviStatusListenerList = new ArrayList<>();
    private final List<OnSearchResultListener> mSearchResultListenerList = new ArrayList<>();
    private final List<OnRoutePlanResultListener> mRoutePlanListenerList = new ArrayList<>();
    private final List<OnGuidePanelDataListener> mGuidePanelDataListenerList = new ArrayList<>();
    private final List<OnSpeedLimitChangeListener> mSpeedLimitListenerList = new ArrayList<>();
    private final List<OnTurnInfoChangeListener> mTurnInfoListenerList = new ArrayList<>();
    private final List<OnSrNaviInfoChangeListener> mSrNaviInfoListenerList = new ArrayList<>();
    private final List<OnNaviBroadcastStateListener> mNaviBroadcastListenerList = new ArrayList<>();

    public static NaviAutoAPIManager getInstance() {
        return SingleHolder.INSTANCE;
    }

    private static final class SingleHolder {
        private static final NaviAutoAPIManager INSTANCE = new NaviAutoAPIManager();
    }

    private NaviAutoAPIManager() {

    }

    private final IEngineStatusCallback mEngineStatusCallback = new IEngineStatusCallback() {
        @Override
        public void onInitSuccess() {
            try {
                mBinder = updateBinder();
                mBinder.asBinder().linkToDeath(mDeathRecipient, 0);
                unRegisterBinderResultCallback();
                registerBinderResultCallback();
                onEngineInit(true);
            } catch (RemoteException e) {
                Logger.e(TAG, "onUpdateBinderCallback remoteException: " + e.getMessage());
            }
        }

        @Override
        public void onInitFailed() {
            if (null != mBinder) {
                mBinder.asBinder().unlinkToDeath(mDeathRecipient, 0);
                mBinder = null;
            }
            onEngineInit(false);
        }
    };

    private final IBinder.DeathRecipient mDeathRecipient = new IBinder.DeathRecipient() {
        @Override
        public void binderDied() {
            if (mBinder == null) {
                return;
            }
            mBinder.asBinder().unlinkToDeath(this, 0);
            mBinder = null;
        }
    };

    @Override
    protected INaviAutoApiBinder updateBinder() {
        return BinderManager.getInstance().getNaviAutoBinder();
    }

    @Override
    protected boolean checkBinder() {
        return null != mBinder && mBinder.asBinder().pingBinder();
    }

    @Override
    protected void onEngineInit(final boolean success) {
        Logger.d(TAG, mPkgName + "-->  onEngineInit: " + success);
        for (OnInitStateChangeListener initStateChangeListener : mInitStateListenerList) {
            if (null != initStateChangeListener) {
                try {
                    if (success) {
                        mInitStatus = true;
                        initStateChangeListener.onInitSuccess();
                    } else {
                        mInitStatus = false;
                        initStateChangeListener.onFailure();
                    }
                } catch (NullPointerException | IllegalArgumentException | ClassCastException
                         | IllegalStateException exception) {
                    Logger.d(TAG, "dispatch initSuccess: " + exception.getMessage());
                }
            }
        }
    }

    @Override
    protected void registerBinderResultCallback() {
        if (checkBinder()) {
            try {
                mBinder.addNaviAutoApiCallback(mPkgName, mNaviAutoCallback);
                mBinder.addNaviAutoLocationCallback(mPkgName, mNaviAutoLocationCallback);
                mBinder.addNaviAutoRouteCallback(mPkgName, mNaviAutoRouteCallback);
                mBinder.addNaviAutoSearchCallback(mPkgName, mNaviAutoSearchCallback);
                mBinder.addNaviAutoStatusCallback(mPkgName, mNaviApiStatusCallback);
                mBinder.addNaviAutoSpeedCallBack(mPkgName, mNaviAutoSpeedCallBack);
                mBinder.addNaviAutoCountDownLightCallback(mPkgName, mCountDownLightCallback);
            } catch (RemoteException exception) {
                Logger.e(TAG, "registerNaviAutoApiCallback error: " + exception.getMessage());
            }
        }
    }

    @Override
    protected void unRegisterBinderResultCallback() {
        if (checkBinder()) {
            try {
                mBinder.removeNaviAutoApiCallback(mPkgName, mNaviAutoCallback);
                mBinder.removeNaviAutoLocationCallback(mPkgName, mNaviAutoLocationCallback);
                mBinder.removeNaviAutoRouteCallback(mPkgName, mNaviAutoRouteCallback);
                mBinder.removeNaviAutoSearchCallback(mPkgName, mNaviAutoSearchCallback);
                mBinder.removeNaviAutoStatusCallback(mPkgName, mNaviApiStatusCallback);
                mBinder.removeNaviAutoSpeedCallBack(mPkgName, mNaviAutoSpeedCallBack);
                mBinder.removeNaviAutoCountDownLightCallback(mPkgName, mCountDownLightCallback);
            } catch (RemoteException exception) {
                Logger.e(TAG, "unRegisterNaviAutoApiCallback error: " + exception.getMessage());
            }
        }
    }

    //Map状态
    private final INaviAutoStatusCallback mNaviApiStatusCallback = new INaviAutoStatusCallback.Stub() {
        @Override
        public void onNaviStatusChange(final String status) {
            Logger.d(TAG, mPkgName + "-->  onNaviStatusChange: " + status);
            for (OnNaviStatusChangeListener listener : mNaviStatusListenerList) {
                if (null != listener) {
                    try {
                        listener.onNaviStatusChange(status);
                    } catch (NullPointerException | IllegalArgumentException | ClassCastException
                             | IllegalStateException exception) {
                        Logger.e(TAG, "dispatch naviStatus error: " + exception.getMessage());
                    }
                }
            }
        }

        @Override
        public void onPanelData(final int panelDataStatus) {
            Logger.d(TAG, mPkgName + "-->  onPanelData: " + panelDataStatus);
            for (OnGuidePanelDataListener listener : mGuidePanelDataListenerList) {
                if (null != listener) {
                    try {
                        listener.onPanelData(panelDataStatus);
                    } catch (NullPointerException | IllegalArgumentException | ClassCastException
                             | IllegalStateException exception) {
                        Logger.e(TAG, "dispatch onPanelData error: " + exception.getMessage());
                    }
                }
            }
        }

        @Override
        public void onNaviStartAfterFiveMinutes() {
            Logger.d(TAG, mPkgName + "--> onNaviStartAfterFiveMinutes");
            for (OnNaviStatusChangeListener listener : mNaviStatusListenerList) {
                if (null != listener) {
                    try {
                        listener.onNaviStartAfterFiveMinutes();
                    } catch (NullPointerException | IllegalArgumentException | ClassCastException
                             | IllegalStateException exception) {
                        Logger.e(TAG, "dispatch naviStartDelay error: " + exception.getMessage());
                    }
                }
            }
        }

        @Override
        public void onNaviManualStop() {
            Logger.d(TAG, mPkgName + "--> onNaviManualStop");
            for (OnNaviStatusChangeListener listener : mNaviStatusListenerList) {
                if (null != listener) {
                    try {
                        listener.onNaviManualStop();
                    } catch (NullPointerException | IllegalArgumentException | ClassCastException
                             | IllegalStateException exception) {
                        Logger.e(TAG, "dispatch naviStop error: " + exception.getMessage());
                    }
                }
            }
        }

        @Override
        public void onNaviBroadcastStatus(final boolean open) {
            for (OnNaviBroadcastStateListener naviBroadcastStateListener : mNaviBroadcastListenerList) {
                if (null != naviBroadcastStateListener) {
                    try {
                        naviBroadcastStateListener.onNaviBroadcastStateChanged(open);
                    } catch (NullPointerException | IllegalArgumentException | IllegalStateException exception) {
                        Logger.e(TAG, "dispatch naviBroadcastState: " + exception.getMessage());
                    }
                }
            }
        }
    };

    //定位信息
    private final INaviAutoLocationCallback mNaviAutoLocationCallback = new INaviAutoLocationCallback.Stub() {
        @Override
        public void onLocationInfoChange(final String locationInfo) {
            Logger.d(TAG, mPkgName + "-->  onLocationInfoChange: " + locationInfo);
            for (OnLocationChangeListener listener : mLocationListenerList) {
                if (null != listener) {
                    try {
                        listener.onLocationChange(locationInfo);
                    } catch (NullPointerException | IllegalArgumentException | ClassCastException
                             | IllegalStateException exception) {
                        Logger.e(TAG, "dispatch locationInfo error: " + exception.getMessage());
                    }
                }
            }
        }

        @Override
        public void onDistrictInfoChange(final String districtInfo) {
            Logger.d(TAG, mPkgName + "-->  onDistrictInfoChange: " + districtInfo);
            for (OnDistrictInfoChangeListener listener : mDistrictInfoList) {
                if (null != listener) {
                    try {
                        listener.onDistrictInfoChange(districtInfo);
                    } catch (NullPointerException | IllegalArgumentException | ClassCastException
                             | IllegalStateException exception) {
                        Logger.e(TAG, "dispatch districtInfo error: " + exception.getMessage());
                    }
                }
            }
        }
    };

    //搜索结果
    private final INaviAutoSearchCallback mNaviAutoSearchCallback = new INaviAutoSearchCallback.Stub() {
        @Override
        public void onSearchFailed(final boolean silent, final int errorCode) {
            Logger.d(TAG, mPkgName + "-->  onSearchFailed: silent = " + silent + "; errorCode = " + errorCode);
            for (OnSearchResultListener listener : mSearchResultListenerList) {
                if (null != listener) {
                    try {
                        listener.onSearchError(silent, errorCode);
                    } catch (NullPointerException | IllegalArgumentException | ClassCastException
                             | IllegalStateException exception) {
                        Logger.e(TAG, "dispatch searchFailed error: " + exception.getMessage());
                    }
                }
            }
        }

        @Override
        public void onSearchResult(final boolean silent, final String searchResult) {
            Logger.d(TAG, mPkgName + "-->  onSearchResult: silent = " + silent);
            for (OnSearchResultListener listener : mSearchResultListenerList) {
                if (null != listener) {
                    try {
                        listener.onSearchResult(silent, searchResult);
                    } catch (NullPointerException | IllegalArgumentException | ClassCastException
                             | IllegalStateException exception) {
                        Logger.e(TAG, "dispatch searchResult error: " + exception.getMessage());
                    }
                }
            }
        }

        @Override
        public void onReverseGeoSearchResult(final int taskId, final String reverseResult) {
            Logger.d(TAG, mPkgName + "-->  onReverseGeoSearchResult: taskId = " + taskId + "; reverseResult = " + reverseResult);
            for (OnSearchResultListener listener : mSearchResultListenerList) {
                if (null != listener) {
                    try {
                        listener.onReverseGeoSearchResult(taskId, reverseResult);
                    } catch (NullPointerException | IllegalArgumentException | ClassCastException
                             | IllegalStateException exception) {
                        Logger.e(TAG, "dispatch searchResult error: " + exception.getMessage());
                    }
                }
            }
        }
    };

    //路线规划结果
    private final INaviAutoRouteCallback mNaviAutoRouteCallback = new INaviAutoRouteCallback.Stub() {
        @Override
        public void onRoutePlanFailed(final int code, final String errorMsg) {
            Logger.d(TAG, mPkgName + "-->  onRoutePlanFailed: code = " + code);
            for (OnRoutePlanResultListener listener : mRoutePlanListenerList) {
                if (null != listener) {
                    try {
                        listener.onRoutePlanError(code, errorMsg);
                    } catch (NullPointerException | IllegalArgumentException | ClassCastException
                             | IllegalStateException exception) {
                        Logger.e(TAG, "dispatch routePlanFailed error: " + exception.getMessage());
                    }
                }
            }
        }

        @Override
        public void onRoutePlanResult(final String routeResult) {
            Logger.d(TAG, mPkgName + "-->  onRoutePlanResult: " + routeResult);
            for (OnRoutePlanResultListener listener : mRoutePlanListenerList) {
                if (null != listener) {
                    try {
                        listener.onRoutePlanResult(routeResult);
                    } catch (NullPointerException | IllegalArgumentException | ClassCastException
                             | IllegalStateException exception) {
                        Logger.e(TAG, "dispatch routePlanResult error: " + exception.getMessage());
                    }
                }
            }
        }
    };

    private final INaviAutoSpeedCallBack mNaviAutoSpeedCallBack = new INaviAutoSpeedCallBack.Stub() {
        @Override
        public void onSpeedLimitChange(final int curSpeed, final int limitSpeed) {
            Logger.d(TAG, mPkgName + "-->  onSpeedLimitChange");
            for (OnSpeedLimitChangeListener listener : mSpeedLimitListenerList) {
                if (null != listener) {
                    try {
                        listener.onSpeedLimitChange(curSpeed, limitSpeed);
                    } catch (NullPointerException | IllegalArgumentException | ClassCastException
                             | IllegalStateException exception) {
                        Logger.e(TAG, "dispatch speedLimitChange error: " + exception.getMessage());
                    }
                }
            }
        }
    };

    private final INaviAutoCountDownLightCallback mCountDownLightCallback = new INaviAutoCountDownLightCallback.Stub() {
        @Override
        public void onCountDownLightInfo(final String lightInfo) {
            Logger.d(TAG, mPkgName + "--> onCountDownLightInfo");
            for (OnSrNaviInfoChangeListener srNaviInfoChangeListener : mSrNaviInfoListenerList) {
                if (null != srNaviInfoChangeListener) {
                    try {
                        srNaviInfoChangeListener.onCountDownLightInfo(lightInfo);
                    } catch (NullPointerException | IllegalStateException | IllegalArgumentException
                             | ClassCastException exception) {
                        Logger.e(TAG, "dispatch onCountDownLightInfo error: " + exception.getMessage());
                    }
                }
            }
        }
    };

    //引导信息
    private final INaviAutoApiCallback mNaviAutoCallback = new INaviAutoApiCallback.Stub() {

        @Override
        public void onTurnInfoChange(final String turnInfo) {
            Logger.d(TAG, mPkgName + "-->  onTurnInfoChange: ");
            for (OnTurnInfoChangeListener listener : mTurnInfoListenerList) {
                if (null != listener) {
                    try {
                        listener.onTurnInfoUpdated(turnInfo);
                    } catch (NullPointerException | IllegalArgumentException | ClassCastException
                             | IllegalStateException exception) {
                        Logger.e(TAG, "dispatch turnInfo error: " + exception.getMessage());
                    }
                }
            }
        }

        @Override
        public void onNaviArrival() {
            Logger.d(TAG, mPkgName + "--> onNaviArrival");
            for (OnSrNaviInfoChangeListener srNaviInfoChangeListener : mSrNaviInfoListenerList) {
                if (null != srNaviInfoChangeListener) {
                    try {
                        srNaviInfoChangeListener.onNaviArrival();
                    } catch (NullPointerException | IllegalArgumentException exception) {
                        Logger.e(TAG, "dispatch onNaviArrival error: " + exception.getMessage());
                    }
                }
            }
        }

        @Override
        public void onNaviStop() {
            Logger.d(TAG, mPkgName + "--> onNaviStop");
            for (OnSrNaviInfoChangeListener srNaviInfoChangeListener : mSrNaviInfoListenerList) {
                if (null != srNaviInfoChangeListener) {
                    try {
                        srNaviInfoChangeListener.onNaviStop();
                    } catch (NullPointerException | IllegalStateException exception) {
                        Logger.e(TAG, "dispatch onNaviStop error: " + exception.getMessage());
                    }
                }
            }
        }
    };

    /**
     * 初始化.
     *
     * @param context Context.
     */
    public void init(final Context context) {
        init(context, context.getPackageName());
    }

    /**
     * 初始化.
     *
     * @param context Context.
     * @param pkgName String，client package name.
     */
    public void init(final Context context, final String pkgName) {
        init(context, pkgName, "");
    }

    /**
     * 接口初始化.
     *
     * @param context Context.
     * @param pkgName String，客户端包名.
     * @param params  String，参考高德地图SDK开发应用接口依赖中Onstar调用示例添加,具体作用未知.
     */
    public void init(final Context context, final String pkgName, final String params) {
        synchronized (NaviAutoAPIManager.class) {
            Logger.d(TAG, "initClient: " + pkgName + ", params: " + params);
            mPkgName = pkgName;
            BinderManager.getInstance().addEngineInitCallback(mEngineStatusCallback);
            MapSdk.getInstance().startConnect(context);
        }
    }

    /**
     * 反初始化.
     */
    public void unInit() {
        mInitStatus = false;
        mInitStateListenerList.clear();
        mLocationListenerList.clear();
        mDistrictInfoList.clear();
        mNaviStatusListenerList.clear();
        mSearchResultListenerList.clear();
        mRoutePlanListenerList.clear();
        mTurnInfoListenerList.clear();
        mGuidePanelDataListenerList.clear();
        mSpeedLimitListenerList.clear();
        unRegisterBinderResultCallback();
    }

    /**
     * 设置初始化状态监听接口.
     *
     * @param initStateChangeListener OnInitStateChangeListener.
     */
    public void setOnInitStateChangeListener(final OnInitStateChangeListener initStateChangeListener) {
        if (!(null == initStateChangeListener || mInitStateListenerList.contains(initStateChangeListener))) {
            mInitStateListenerList.add(initStateChangeListener);
        }
    }

    /**
     * 设置位置信息改变监听接口.
     *
     * @param locationChangeListener OnLocationChangeListener.
     */
    public void setOnLocationChangeListener(final OnLocationChangeListener locationChangeListener) {
        if (mInitStatus && null != locationChangeListener && !mLocationListenerList.contains(locationChangeListener)) {
            mLocationListenerList.add(locationChangeListener);
        }
    }

    /**
     * 移除位置信息改变监听接口.
     *
     * @param locationChangeListener OnLocationChangeListener.
     */
    public void removeOnLocationChangeListener(final OnLocationChangeListener locationChangeListener) {
        if (mInitStatus && null != locationChangeListener) {
            mLocationListenerList.remove(locationChangeListener);
        }
    }

    /**
     * 添加行政区划信息信息改变监听.
     *
     * @param districtInfoChangeListener OnDistrictInfoChangeListener.
     */
    public void setOnDistrictInfoChangeListener(final OnDistrictInfoChangeListener districtInfoChangeListener) {
        if (mInitStatus && null != districtInfoChangeListener && !mDistrictInfoList.contains(districtInfoChangeListener)) {
            mDistrictInfoList.add(districtInfoChangeListener);
        }
    }

    /**
     * 移除行政区划信息信息改变监听.
     *
     * @param districtInfoChangeListener OnDistrictInfoChangeListener.
     */
    public void removeOnDistrictInfoChangeListener(final OnDistrictInfoChangeListener districtInfoChangeListener) {
        if (mInitStatus && null != districtInfoChangeListener) {
            mDistrictInfoList.remove(districtInfoChangeListener);
        }
    }

    /**
     * 添加Navi状态改变监听.
     *
     * @param naviStatusChangeListener OnNaviStatusChangeListener.
     */
    public void setOnNaviStatusChangeListener(final OnNaviStatusChangeListener naviStatusChangeListener) {
        if (mInitStatus && null != naviStatusChangeListener && !mNaviStatusListenerList.contains(naviStatusChangeListener)) {
            mNaviStatusListenerList.add(naviStatusChangeListener);
        }
    }

    /**
     * 移除Navi状态改变监听.
     *
     * @param naviStatusChangeListener OnNaviStatusChangeListener.
     */
    public void removeOnNaviStatusChangeListener(final OnNaviStatusChangeListener naviStatusChangeListener) {
        if (mInitStatus && null != naviStatusChangeListener) {
            mNaviStatusListenerList.remove(naviStatusChangeListener);
        }
    }

    /**
     * 设置搜索结果回调监听.
     *
     * @param searchResultListener OnSearchResultListener.
     */
    public void setOnSearchResultListener(final OnSearchResultListener searchResultListener) {
        if (mInitStatus && null != searchResultListener && !mSearchResultListenerList.contains(searchResultListener)) {
            mSearchResultListenerList.add(searchResultListener);
        }
    }

    /**
     * 移除搜索结果回调监听.
     *
     * @param searchResultListener OnSearchResultListener.
     */
    public void removeOnSearchResultListener(final OnSearchResultListener searchResultListener) {
        if (mInitStatus && null != searchResultListener) {
            mSearchResultListenerList.remove(searchResultListener);
        }
    }

    /**
     * 设置算路结果监听.
     *
     * @param routePlanResultListener OnRoutePlanResultListener.
     */
    public void addRoutePlanResultListener(final OnRoutePlanResultListener routePlanResultListener) {
        if (mInitStatus && null != routePlanResultListener && !mRoutePlanListenerList.contains(routePlanResultListener)) {
            mRoutePlanListenerList.add(routePlanResultListener);
        }
    }

    /**
     * 移除算路结果监听.
     *
     * @param routePlanResultListener OnRoutePlanResultListener.
     */
    public void removeRoutePlanResultListener(final OnRoutePlanResultListener routePlanResultListener) {
        if (mInitStatus && null != routePlanResultListener) {
            mRoutePlanListenerList.remove(routePlanResultListener);
        }
    }

    /**
     * 添加导航中诱导面板状态监听.
     *
     * @param guidePanelDataListener OnGuidePanelDataListener.
     */
    public void setGuidePanelDataChangeListener(final OnGuidePanelDataListener guidePanelDataListener) {
        if (mInitStatus && null != guidePanelDataListener && !mGuidePanelDataListenerList.contains(guidePanelDataListener)) {
            mGuidePanelDataListenerList.add(guidePanelDataListener);
        }
    }

    /**
     * 移除导航中诱导面板状态监听.
     *
     * @param guidePanelDataListener OnGuidePanelDataListener.
     */
    public void removeGuidePanelDataChangeListener(final OnGuidePanelDataListener guidePanelDataListener) {
        if (mInitStatus && null != guidePanelDataListener) {
            mGuidePanelDataListenerList.remove(guidePanelDataListener);
        }
    }

    /**
     * 设置限速回调.
     *
     * @param speedLimitChangeListener OnSpeedLimitChangeListener.
     */
    public void setOnSpeedLimitChangeListener(final OnSpeedLimitChangeListener speedLimitChangeListener) {
        if (mInitStatus && null != speedLimitChangeListener && !mSpeedLimitListenerList.contains(speedLimitChangeListener)) {
            mSpeedLimitListenerList.add(speedLimitChangeListener);
        }
    }

    /**
     * 移除当前限速监听.
     *
     * @param speedLimitChangeListener OnSpeedLimitChangeListener.
     */
    public void removeOnSpeedLimitChangeListener(final OnSpeedLimitChangeListener speedLimitChangeListener) {
        if (mInitStatus && null != speedLimitChangeListener) {
            mSpeedLimitListenerList.remove(speedLimitChangeListener);
        }
    }

    /**
     * 设置TBT信息监听.
     *
     * @param turnInfoChangeListener OnTurnInfoChangeListener.
     */
    public void addOnTurnInfoChangeListener(final OnTurnInfoChangeListener turnInfoChangeListener) {
        if (mInitStatus && null != turnInfoChangeListener && !mTurnInfoListenerList.contains(turnInfoChangeListener)) {
            mTurnInfoListenerList.add(turnInfoChangeListener);
        }
    }

    /**
     * 移除TBT信息监听.
     *
     * @param turnInfoChangeListener OnTurnInfoChangeListener.
     */
    public void removeOnTurnInfoChangeListener(final OnTurnInfoChangeListener turnInfoChangeListener) {
        if (mInitStatus && null != turnInfoChangeListener) {
            mTurnInfoListenerList.remove(turnInfoChangeListener);
        }
    }

    /**
     * 添加SR引导信息变更监听.
     *
     * @param srNaviInfoChangeListener OnSrNaviInfoChangeListener.
     */
    public void addOnSrNaviInfoChangeListener(final OnSrNaviInfoChangeListener srNaviInfoChangeListener) {
        if (mInitStatus && null != srNaviInfoChangeListener && !mSrNaviInfoListenerList.contains(srNaviInfoChangeListener)) {
            mSrNaviInfoListenerList.add(srNaviInfoChangeListener);
        }
    }

    /**
     * 移除SR引导信息监听.
     *
     * @param srNaviInfoChangeListener OnSrNaviInfoChangeListener.
     */
    public void removeOnSrNaviInfoChangeListener(final OnSrNaviInfoChangeListener srNaviInfoChangeListener) {
        if (mInitStatus && null != srNaviInfoChangeListener) {
            mSrNaviInfoListenerList.remove(srNaviInfoChangeListener);
        }
    }

    /**
     * 添加引导播报状态改变监听.
     *
     * @param naviBroadcastStateListener OnNaviBroadcastStateListener.
     */
    public void addNaviBroadcastChangeListener(final OnNaviBroadcastStateListener naviBroadcastStateListener) {
        if (mInitStatus && null != naviBroadcastStateListener && !mNaviBroadcastListenerList.contains(naviBroadcastStateListener)) {
            mNaviBroadcastListenerList.add(naviBroadcastStateListener);
        }
    }

    /**
     * 移除引导播报状态改变监听.
     *
     * @param naviBroadcastStateListener OnNaviBroadcastStateListener.
     */
    public void removeNaviBroadcastChangeListener(final OnNaviBroadcastStateListener naviBroadcastStateListener) {
        if (mInitStatus && null != naviBroadcastStateListener) {
            mNaviBroadcastListenerList.remove(naviBroadcastStateListener);
        }
    }

    /**
     * 打开地图HMI.
     */
    public void openMap() {
        if (mInitStatus && checkBinder()) {
            try {
                Logger.d(TAG, mPkgName + "-->  openMap: ");
                mBinder.openMap(mPkgName);
            } catch (RemoteException exception) {
                Logger.e(TAG, "openMap error: " + exception.getMessage());
            }
        }
    }

    /**
     * 获取当前位置信息.
     *
     * @return String，BaseLocationInfo对应json类型字符串.
     */
    public String getCurrentLocation() {
        String locationInfo = "";
        if (mInitStatus && checkBinder()) {
            try {
                Logger.d(TAG, mPkgName + "-->  getCurrentLocation: ");
                locationInfo = mBinder.getCurrentLocation(mPkgName);
            } catch (RemoteException exception) {
                Logger.e(TAG, "getCurrentLocation error: " + exception.getMessage());
            }
        }

        return locationInfo;
    }

    /**
     * 获取当前行政区划信息.
     *
     * @return String，BaseDistrictInfo对应的json字符串.
     */
    public String getDistrictDetailInfo() {
        String districtInfo = "";
        if (mInitStatus && checkBinder()) {
            try {
                Logger.d(TAG, mPkgName + "-->  getDistrictDetailInfo: ");
                districtInfo = mBinder.getDistrictDetailInfo(mPkgName);
            } catch (RemoteException exception) {
                Logger.e(TAG, "getDistrictDetailInfo error: " + exception.getMessage());
            }
        }

        return districtInfo;
    }

    /**
     * 打开HMI搜索界面并触发关键字搜索.
     *
     * @param keyword String 搜索关键字.
     */
    public void jumpToSearchPage(final String keyword) {
        if (null == keyword || keyword.isEmpty()) {
            Logger.w(TAG, "empty keyword");
            return;
        }

        if (mInitStatus && checkBinder()) {
            try {
                Logger.d(TAG, mPkgName + "-->  jumpToSearchPage: " + keyword);
                mBinder.jumpToSearchPage(mPkgName, keyword);
            } catch (RemoteException exception) {
                Logger.e(TAG, "jumpToSearchPage error: " + exception.getMessage());
            }
        }
    }

    /**
     * 通过输入的基础地址执行逆地理搜索.
     *
     * @param geoPoint BaseGeoPoint, 地址经纬度.
     * @return int，搜索taskId.
     */
    public int requestReverseGeoSearch(final BaseGeoPoint geoPoint) {
        if (null == geoPoint) {
            Logger.w(TAG, "empty GeoPoint");
            return -1;
        }

        if (mInitStatus && checkBinder()) {
            try {
                Logger.d(TAG, mPkgName + "-->  requestReverseGeoSearch: " + geoPoint);
                return mBinder.requestReverseGeoSearch(mPkgName, geoPoint);
            } catch (RemoteException exception) {
                Logger.e(TAG, "requestReverseGeoSearch error: " + exception.getMessage());
            }
        }

        return -1;
    }

    /**
     * 自车位置周边搜索.
     *
     * @param keyword   String，搜索关键字.
     * @param pageIndex int,搜索页面下标，从1开始，最大值见上一轮搜索结果BaseSearchResult里的maxPage.
     */
    public void nearbySearch(final String keyword, final int pageIndex) {
        if (null == keyword || keyword.isEmpty()) {
            Logger.w(TAG, "nearbySearch keyword is empty");
            return;
        }

        if (mInitStatus && checkBinder()) {
            try {
                Logger.d(TAG, mPkgName + "-->  nearbySearch keyword: " + keyword + ", pageIndex: " + pageIndex);
                mBinder.nearbySearch(mPkgName, keyword, pageIndex);
            } catch (RemoteException exception) {
                Logger.w(TAG, "nearbySearch error: " + exception.getMessage());
            }
        }
    }

    /**
     * 搜索地址并发起路线规划.
     *
     * @param address String，地址信息，越精准，搜索结果越准确.
     */
    public void searchAndNavi(final String address) {
        if (null == address || address.isEmpty()) {
            Logger.w(TAG, "empty address");
            return;
        }

        if (mInitStatus && checkBinder()) {
            try {
                Logger.d(TAG, mPkgName + "-->  searchAndNavi: " + address);
                mBinder.searchAndNavi(mPkgName, address);
            } catch (RemoteException exception) {
                Logger.e(TAG, "searchAndNavi error: " + exception.getMessage());
            }
        }
    }

    /**
     * 取消所有搜索请求.
     */
    public void cancelAllSearchRequest() {
        if (mInitStatus && checkBinder()) {
            try {
                Logger.d(TAG, mPkgName + "-->  cancelAllSearchRequest: ");
                mBinder.cancelAllSearchRequest(mPkgName);
            } catch (RemoteException exception) {
                Logger.e(TAG, "cancelAllSearchRequest error: " + exception.getMessage());
            }
        }
    }

    /**
     * 根据选定的搜索结果发起路线规划.
     *
     * @param destination BaseSearchPoi，目的地，单个Poi信息.
     */
    public void routePlan(final BaseSearchPoi destination) {
        if (mInitStatus && checkBinder()) {
            try {
                Logger.d(TAG, mPkgName + "-->  routePlan: " + destination);
                mBinder.routePlan(mPkgName, destination);
            } catch (RemoteException exception) {
                Logger.e(TAG, "routePlan error: " + exception.getMessage());
            }
        }
    }

    /**
     * 开始导航.
     */
    public void startNavi() {
        if (mInitStatus && checkBinder()) {
            try {
                Logger.d(TAG, mPkgName + "-->  startNavi: ");
                mBinder.startNavi(mPkgName);
            } catch (RemoteException exception) {
                Logger.e(TAG, "routePlan error: " + exception.getMessage());
            }
        }
    }

    /**
     * 当前是否处于引导态.
     *
     * @return boolean  true-处于引导态  false-相反.
     */
    public boolean isNaviStatus() {
        boolean result = false;
        if (mInitStatus && checkBinder()) {
            try {
                Logger.d(TAG, mPkgName + "-->  isNaviStatus: ");
                result = mBinder.isNaviStatus(mPkgName);
            } catch (RemoteException exception) {
                Logger.e(TAG, "startNavi error: " + exception.getMessage());
            }
        }

        return result;
    }

    /**
     * 获取当前诱导面板状态.
     *
     * @return int，见INaviConstant.GuidePanelStatus.
     */
    public int getGuidePanelStatus() {
        int result = -1;

        if (mInitStatus && checkBinder()) {
            try {
                Logger.d(TAG, mPkgName + "-->  getGuidePanelStatus: ");
                result = mBinder.getGuidePanelStatus(mPkgName);
            } catch (RemoteException exception) {
                Logger.e(TAG, "startNavi error: " + exception.getMessage());
            }
        }

        return result;
    }

    /**
     * 获取导航类型.
     *
     * @return String，当前Map状态值，见见INaviConstant.NaviStatusType.
     */
    public String getNaviType() {
        String type = INaviConstant.NaviStatusType.NO_STATUS;
        if (mInitStatus && checkBinder()) {
            try {
                Logger.d(TAG, mPkgName + "-->  getNaviType: ");
                type = mBinder.getNaviType(mPkgName);
            } catch (RemoteException exception) {
                Logger.e(TAG, "getNaviType error: " + exception.getMessage());
            }
        }

        return type;
    }

    /**
     * 获取诱导信息.
     *
     * @return String，BaseTbtInfo对应的json字符串.
     */
    public String getTBTInfo() {
        String tbtInfo = "";
        if (mInitStatus && checkBinder()) {
            try {
                Logger.d(TAG, mPkgName + "-->  getTBTInfo: ");
                tbtInfo = mBinder.getTBTInfo(mPkgName);
            } catch (RemoteException exception) {
                Logger.e(TAG, "getTBTInfo error: " + exception.getMessage());
            }
        }

        return tbtInfo;
    }

    /**
     * 对SR TBT面板执行显示/隐藏.
     *
     * @param open 执行工作 true:显示  false:隐藏.
     */
    public void openSrTbt(final boolean open) {
        if (mInitStatus && checkBinder()) {
            try {
                mBinder.openSrTbt(mPkgName, open);
            } catch (RemoteException remoteException) {
                Logger.w(TAG, mPkgName + "openSrTbt error: " + remoteException.getMessage());
            }
        }
    }

    /**
     * 停止导航.
     *
     * @return 停止导航执行结果  true:执行成功  false:执行失败.
     */
    public boolean stopNavigation() {
        boolean result = false;
        if (mInitStatus && checkBinder()) {
            try {
                result = mBinder.stopNavi(mPkgName);
            } catch (RemoteException exception) {
                Logger.w(TAG, mPkgName + "stopNavigation error:" + exception.getMessage());
            }
        }
        return result;
    }

    /**
     * Launcher widget接口，回家.
     */
    public void backHome() {
        if (mInitStatus && checkBinder()) {
            try {
                mBinder.backHome(mPkgName);
            } catch (RemoteException exception) {
                Logger.w(TAG, mPkgName + "backHome error:" + exception.getMessage());
            }
        }
    }

    /**
     * Launcher widget接口，去公司.
     */
    public void goCompany() {
        if (mInitStatus && checkBinder()) {
            try {
                mBinder.goCompany(mPkgName);
            } catch (RemoteException exception) {
                Logger.w(TAG, mPkgName + "goCompany error:" + exception.getMessage());
            }
        }
    }

    /**
     * Launcher widget接口，点击搜索按钮.
     */
    public void openBasicSearch() {
        if (mInitStatus && checkBinder()) {
            try {
                mBinder.openBasicSearch(mPkgName);
            } catch (RemoteException exception) {
                Logger.w(TAG, mPkgName + "openBasicSearch error:" + exception.getMessage());
            }
        }
    }

    /**
     * 获取当前引导播报状态.
     *
     * @return true-打开  false-关闭.
     */
    public boolean getNaviBroadcastStatus() {
        boolean open = false;
        if (mInitStatus && checkBinder()) {
            try {
                open = mBinder.getNaviBroadcastStatus(mPkgName);
            } catch (RemoteException exception) {
                Logger.w(TAG, mPkgName + "getBroadcast status error:" + exception.getMessage());
            }
        }

        return open;
    }

    /**
     * Launcher widget接口，打开/关闭引导播报.
     *
     * @param open  true-打开  false-关闭.
     */
    public void toggleNaviBroadcast(final boolean open) {
        if (mInitStatus && checkBinder()) {
            try {
                mBinder.toggleNaviBroadcast(mPkgName, open);
            } catch (RemoteException exception) {
                Logger.w(TAG, mPkgName + "toggleNaviBroadcast error:" + exception.getMessage());
            }
        }
    }

    /**
     * Launcher widget接口，引导时点击添加途径点.
     */
    public void clickPassBySearch() {
        if (mInitStatus && checkBinder()) {
            try {
                mBinder.clickPassBySearch(mPkgName);
            } catch (RemoteException exception) {
                Logger.w(TAG, mPkgName + "clickPassBySearch error:" + exception.getMessage());
            }
        }
    }


}
