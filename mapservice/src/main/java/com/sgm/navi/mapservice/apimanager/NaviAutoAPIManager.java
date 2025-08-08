package com.sgm.navi.mapservice.apimanager;

import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.Intent;
import android.os.IBinder;
import android.os.RemoteException;

import com.sgm.navi.mapservice.BuildConfig;
import com.sgm.navi.mapservice.base.BaseManager;
import com.sgm.navi.mapservice.base.BinderManager;
import com.sgm.navi.mapservice.base.MapSdk;
import com.sgm.navi.mapservice.bean.INaviConstant;
import com.sgm.navi.mapservice.bean.common.BaseSearchPoi;
import com.sgm.navi.mapservice.bean.common.BaseGeoPoint;
import com.sgm.navi.mapservice.callback.OnDestChangeListener;
import com.sgm.navi.mapservice.callback.OnDistrictInfoChangeListener;
import com.sgm.navi.mapservice.callback.OnGuidePanelDataListener;
import com.sgm.navi.mapservice.callback.OnInitStateChangeListener;
import com.sgm.navi.mapservice.callback.OnLocationChangeListener;
import com.sgm.navi.mapservice.callback.OnNaviBroadcastStateListener;
import com.sgm.navi.mapservice.callback.OnNaviStatusChangeListener;
import com.sgm.navi.mapservice.callback.OnPoiInformListener;
import com.sgm.navi.mapservice.callback.OnRoutePlanResultListener;
import com.sgm.navi.mapservice.callback.OnSearchResultListener;
import com.sgm.navi.mapservice.callback.OnSpeedLimitChangeListener;
import com.sgm.navi.mapservice.callback.OnSrNaviInfoChangeListener;
import com.sgm.navi.mapservice.callback.OnTurnInfoChangeListener;
import com.sgm.navi.mapservice.common.INaviAutoApiBinder;
import com.sgm.navi.mapservice.common.INaviAutoApiCallback;
import com.sgm.navi.mapservice.common.INaviAutoCountDownLightCallback;
import com.sgm.navi.mapservice.common.INaviAutoLocationCallback;
import com.sgm.navi.mapservice.common.INaviAutoPoiCallBack;
import com.sgm.navi.mapservice.common.INaviAutoRouteCallback;
import com.sgm.navi.mapservice.common.INaviAutoSearchCallback;
import com.sgm.navi.mapservice.common.INaviAutoSpeedCallBack;
import com.sgm.navi.mapservice.common.INaviAutoStatusCallback;
import com.sgm.navi.mapservice.util.Logger;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class NaviAutoAPIManager extends BaseManager<INaviAutoApiBinder> {

    private static final String TAG = NaviAutoAPIManager.class.getSimpleName();
    private static final String CLIENT_SR = "com.sgm.hmi.sr";
    private INaviAutoApiBinder mBinder;
    private String mPkgName = "default";
    private String mVersionInfo = "";
    private boolean mInitStatus = false;
    private Context mContext;
    private boolean mShowSrTbt = false;

    private final List<OnInitStateChangeListener> mInitStateListenerList = new ArrayList<>();

    private final List<OnLocationChangeListener> mLocationListenerList = new ArrayList<>();
    private final List<OnDistrictInfoChangeListener> mDistrictInfoList = new ArrayList<>();

    private final List<OnNaviStatusChangeListener> mNaviStatusListenerList = new ArrayList<>();
    private final List<OnGuidePanelDataListener> mGuidePanelDataListenerList = new ArrayList<>();
    private final List<OnNaviBroadcastStateListener> mNaviBroadcastListenerList = new ArrayList<>();

    private final List<OnSearchResultListener> mSearchResultListenerList = new ArrayList<>();

    private final List<OnRoutePlanResultListener> mRoutePlanListenerList = new ArrayList<>();
    private final List<OnDestChangeListener> mDestChangeListenerList = new ArrayList<>();

    private final List<OnSpeedLimitChangeListener> mSpeedLimitListenerList = new ArrayList<>();
    private final List<OnTurnInfoChangeListener> mTurnInfoListenerList = new ArrayList<>();
    private final List<OnSrNaviInfoChangeListener> mSrNaviInfoListenerList = new ArrayList<>();
    private final List<OnPoiInformListener> mPoiInfoListenerList = new ArrayList<>();

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
                if (null != mBinder) {
                    mBinder.asBinder().linkToDeath(mDeathRecipient, 0);
                    onEngineInit(true);
                } else {
                    onEngineInit(false);
                }
            } catch (RemoteException exception) {
                Logger.e(TAG, "onUpdateBinderCallback remoteException",
                        exception.getMessage(), Arrays.toString(exception.getStackTrace()));
            }
        }

        @Override
        public void onInitFailed() {
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
        final INaviAutoApiBinder naviAutoApiBinder = BinderManager.getInstance().getNaviAutoBinder();
        Logger.i(TAG, "getNaviAutoApiBinder", null != naviAutoApiBinder);
        return naviAutoApiBinder;
    }

    @Override
    protected boolean checkBinder() {
        return null != mBinder && mBinder.asBinder().pingBinder();
    }

    @Override
    protected void onEngineInit(final boolean success) {
        Logger.d(TAG, mVersionInfo, "-->  onEngineInit: ", success);
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
                    Logger.d(TAG, "dispatch initSuccess: ", exception.getMessage());
                }
            }
        }
    }

    @Override
    protected void registerBinderResultCallback() {

    }

    @Override
    protected void unRegisterBinderResultCallback() {

    }

    //Map状态
    private final INaviAutoStatusCallback mNaviApiStatusCallback = new INaviAutoStatusCallback.Stub() {
        @Override
        public void onNaviStatusChange(final String status) {
            Logger.d(TAG, mVersionInfo, "-->  onNaviStatusChange: ", status);
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
            Logger.d(TAG, mVersionInfo, "-->  onPanelData: ", panelDataStatus);
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
            Logger.d(TAG, mVersionInfo, "--> onNaviStartAfterFiveMinutes");
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
            Logger.d(TAG, mVersionInfo, "--> onNaviManualStop");
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
                    } catch (NullPointerException | IllegalArgumentException |
                             IllegalStateException exception) {
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
            Logger.d(TAG, mVersionInfo, "-->  onLocationInfoChange: ", locationInfo);
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
            Logger.d(TAG, mVersionInfo, "-->  onDistrictInfoChange: ", districtInfo);
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
            Logger.d(TAG, mVersionInfo, "-->  onSearchFailed: silent = ", silent, "; errorCode = ", errorCode);
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
            Logger.d(TAG, mVersionInfo, "-->  onSearchResult: silent = ", silent);
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
            Logger.d(TAG, mVersionInfo, "-->  onReverseGeoSearchResult: taskId = ", taskId, "; reverseResult = ", reverseResult);
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
            Logger.d(TAG, mVersionInfo, "-->  onRoutePlanFailed: code = ", code);
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
            Logger.d(TAG, mVersionInfo, "-->  onRoutePlanResult: ", routeResult);
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

        @Override
        public void onDestChanged(final String destInfo) {
            Logger.d(TAG, mVersionInfo, "-->  onDestChanged:", destInfo);
            for (OnDestChangeListener listener : mDestChangeListenerList) {
                if (null != listener) {
                    try {
                        listener.onDestChangeListener(destInfo);
                    } catch (NullPointerException | IllegalArgumentException | ClassCastException
                             | IllegalStateException exception) {
                        Logger.e(TAG, "dispatch destInfo error: " + exception.getMessage());
                    }
                }
            }
        }
    };

    private final INaviAutoSpeedCallBack mNaviAutoSpeedCallBack = new INaviAutoSpeedCallBack.Stub() {
        @Override
        public void onSpeedLimitChange(final int curSpeed, final int limitSpeed) {
            Logger.d(TAG, mVersionInfo, "-->onSpeedLimitChange");
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

    private final INaviAutoPoiCallBack mNaviAutoPoiCallBack = new INaviAutoPoiCallBack.Stub() {
        @Override
        public void onChargingStationInform(final String chargingStationInfo) {
            Logger.d(TAG, mVersionInfo, "-->onChargingStationInform");
            for (OnPoiInformListener listener : mPoiInfoListenerList) {
                if (null != listener) {
                    try {
                        listener.onChargingStationInform(chargingStationInfo);
                    } catch (NullPointerException | IllegalArgumentException | ClassCastException
                             | IllegalStateException exception) {
                        Logger.e(TAG, "dispatch chargingStationInfo error: " + exception.getMessage());
                    }
                }
            }
        }

        @Override
        public void onParkingLotInform(final String parkingLotInfo) {
            Logger.d(TAG, mVersionInfo, "-->onParkingLotInform");
            for (OnPoiInformListener listener : mPoiInfoListenerList) {
                if (null != listener) {
                    try {
                        listener.onParkingLotInform(parkingLotInfo);
                    } catch (NullPointerException | IllegalArgumentException | ClassCastException
                             | IllegalStateException exception) {
                        Logger.e(TAG, "dispatch parkingLotInfo error: " + exception.getMessage());
                    }
                }
            }
        }

        @Override
        public void onSAPAInform(final String serviceInfo) {
            Logger.d(TAG, mVersionInfo, "-->  onSAPAInform");
            for (OnPoiInformListener listener : mPoiInfoListenerList) {
                if (null != listener) {
                    try {
                        listener.onSAPAInform(serviceInfo);
                    } catch (NullPointerException | IllegalArgumentException | ClassCastException
                             | IllegalStateException exception) {
                        Logger.e(TAG, "dispatch serviceInfo error: " + exception.getMessage());
                    }
                }
            }
        }

        @Override
        public void onGasStationInform(final String gasStationInfo) {
            Logger.d(TAG, mVersionInfo, "--> onGasStationInform");
            for (OnPoiInformListener listener : mPoiInfoListenerList) {
                if (null != listener) {
                    try {
                        listener.onGasStationInform(gasStationInfo);
                    } catch (NullPointerException | IllegalArgumentException | ClassCastException
                             | IllegalStateException exception) {
                        Logger.e(TAG, "dispatch gasStationInfo error: " + exception.getMessage());
                    }
                }
            }
        }
    };

    private final INaviAutoCountDownLightCallback mCountDownLightCallback = new INaviAutoCountDownLightCallback.Stub() {
        @Override
        public void onCountDownLightInfo(final String lightInfo) {
            Logger.d(TAG, mVersionInfo, "--> onCountDownLightInfo");
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

        @Override
        public void onNaviArrival() {
            Logger.d(TAG, mVersionInfo, "--> onNaviArrival");
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
            Logger.d(TAG, mVersionInfo, "--> onNaviStop");
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

    //引导信息
    private final INaviAutoApiCallback mNaviAutoCallback = new INaviAutoApiCallback.Stub() {

        @Override
        public void onTurnInfoChange(final String turnInfo) {
            Logger.d(TAG, mVersionInfo, "-->  onTurnInfoChange: ");
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
            mPkgName = pkgName;
            mContext = context;
            mVersionInfo = "MapService Version[" + BuildConfig.LIB_VERSION + "] " + pkgName;
            Logger.d(TAG, "initClient: ", mVersionInfo, ", params: ", params);
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
            checkRemoteLocationCallback(true);
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
            checkRemoteLocationCallback(false);
        }
    }

    /**
     * 添加行政区划信息信息改变监听.
     *
     * @param districtInfoChangeListener OnDistrictInfoChangeListener.
     */
    public void setOnDistrictInfoChangeListener(final OnDistrictInfoChangeListener districtInfoChangeListener) {
        if (mInitStatus && null != districtInfoChangeListener && !mDistrictInfoList.contains(districtInfoChangeListener)) {
            checkRemoteLocationCallback(true);
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
            checkRemoteLocationCallback(false);
        }
    }

    /**
     * 检查定位相关两个接口集合是否都为空.
     * LocationInfo和DistrictInfo使用INaviAutoLocationCallback
     *
     * @param add  true:添加RemoteCallback  false:移除RemoteCallback.
     */
    private void checkRemoteLocationCallback(final boolean add) {
        final boolean empty = mLocationListenerList.isEmpty() && mDistrictInfoList.isEmpty();
        if (checkBinder() && empty) {
            try {
                if (add) {
                    mBinder.addNaviAutoLocationCallback(mPkgName, mNaviAutoLocationCallback);
                } else {
                    mBinder.removeNaviAutoLocationCallback(mPkgName, mNaviAutoLocationCallback);
                }
            } catch (RemoteException exception) {
                Logger.e(TAG, "checkRemoteLocationCallback error: " + exception.getMessage());
            }
        }
    }

    /**
     * 添加Navi状态改变监听.
     *
     * @param naviStatusChangeListener OnNaviStatusChangeListener.
     */
    public void setOnNaviStatusChangeListener(final OnNaviStatusChangeListener naviStatusChangeListener) {
        if (mInitStatus && null != naviStatusChangeListener && !mNaviStatusListenerList.contains(naviStatusChangeListener)) {
            checkNaviStatusEmpty(true);
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
            checkNaviStatusEmpty(false);
        }
    }

    /**
     * 添加导航中诱导面板状态监听.
     *
     * @param guidePanelDataListener OnGuidePanelDataListener.
     */
    public void setGuidePanelDataChangeListener(final OnGuidePanelDataListener guidePanelDataListener) {
        if (mInitStatus && null != guidePanelDataListener && !mGuidePanelDataListenerList.contains(guidePanelDataListener)) {
            checkNaviStatusEmpty(true);
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
            checkNaviStatusEmpty(false);
        }
    }

    /**
     * 添加引导播报状态改变监听.
     *
     * @param naviBroadcastStateListener OnNaviBroadcastStateListener.
     */
    public void addNaviBroadcastChangeListener(final OnNaviBroadcastStateListener naviBroadcastStateListener) {
        if (mInitStatus && null != naviBroadcastStateListener && !mNaviBroadcastListenerList.contains(naviBroadcastStateListener)) {
            checkNaviStatusEmpty(true);
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
            checkNaviStatusEmpty(false);
        }
    }

    /**
     * 检查Map状态相关接口集合是否为空
     * NaviStatusListener、GuidePanelDataListener和NaviBroadcastListener同时使用INaviAutoStatusCallback
     *
     * @param add  true:添加Remote接口  false:移除Remote接口.
     */
    private void checkNaviStatusEmpty(final boolean add) {
        final boolean empty = mNaviStatusListenerList.isEmpty() && mGuidePanelDataListenerList.isEmpty()
                && mNaviBroadcastListenerList.isEmpty();
        if (checkBinder() && empty) {
            try {
                if (add) {
                    mBinder.addNaviAutoStatusCallback(mPkgName, mNaviApiStatusCallback);
                } else {
                    mBinder.removeNaviAutoStatusCallback(mPkgName, mNaviApiStatusCallback);
                }
            } catch (RemoteException exception) {
                Logger.e(TAG, "checkNaviStatusEmpty error: " + exception.getMessage());
            }
        }
    }


    /**
     * 设置搜索结果回调监听.
     *
     * @param searchResultListener OnSearchResultListener.
     */
    public void setOnSearchResultListener(final OnSearchResultListener searchResultListener) {
        if (mInitStatus && null != searchResultListener && !mSearchResultListenerList.contains(searchResultListener)) {
            checkSearchAboutEmpty(true);
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
            checkSearchAboutEmpty(false);
        }
    }

    /**
     * 检查搜索相关Listener集合是否为空
     * 当前只有SearchResultListener使用INaviAutoSearchCallback.
     *
     * @param add  true:添加Remote接口  false:移除Remote接口.
     */
    private void checkSearchAboutEmpty(final boolean add) {
        final boolean empty = mSearchResultListenerList.isEmpty();
        if (checkBinder() && empty) {
            try {
                if (add) {
                    mBinder.addNaviAutoSearchCallback(mPkgName, mNaviAutoSearchCallback);
                } else {
                    mBinder.removeNaviAutoSearchCallback(mPkgName, mNaviAutoSearchCallback);
                }
            } catch (RemoteException exception) {
                Logger.e(TAG, "checkSearchAboutEmpty error: " + exception.getMessage());
            }
        }
    }


    /**
     * 设置算路结果监听.
     *
     * @param routePlanResultListener OnRoutePlanResultListener.
     */
    public void addRoutePlanResultListener(final OnRoutePlanResultListener routePlanResultListener) {
        if (mInitStatus && null != routePlanResultListener && !mRoutePlanListenerList.contains(routePlanResultListener)) {
            checkRouteAboutEmpty(true);
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
            checkRouteAboutEmpty(false);
        }
    }

    /**
     * 添加目的地变更监听
     *
     * @param listener OnDestChangeListener.
     */
    public void addDestChangeListener(final OnDestChangeListener listener) {
        if (mInitStatus && null != listener && !mDestChangeListenerList.contains(listener)) {
            checkRouteAboutEmpty(true);
            mDestChangeListenerList.add(listener);
        }
    }

    /**
     * 移除目的地变更监听
     *
     * @param listener OnDestChangeListener.
     */
    public void removeDestChangeListener(final OnDestChangeListener listener) {
        if (mInitStatus && null != listener) {
            mDestChangeListenerList.remove(listener);
            checkRouteAboutEmpty(false);
        }
    }

    /**
     * 检查路线规划相关接口集合是否为空.
     * RoutePlanListener和DestChangeListener都通过INaviAutoRouteCallback
     *
     * @param add  true:添加RemoteCallback  false:移除RemoteCallback.
     */
    private void checkRouteAboutEmpty(final boolean add) {
        final boolean empty = mRoutePlanListenerList.isEmpty() && mDestChangeListenerList.isEmpty();
        if (checkBinder() && empty) {
            try {
                if (add) {
                    mBinder.addNaviAutoRouteCallback(mPkgName, mNaviAutoRouteCallback);
                } else {
                    mBinder.removeNaviAutoRouteCallback(mPkgName, mNaviAutoRouteCallback);
                }
            } catch (RemoteException exception) {
                Logger.e(TAG, "checkNaviAutoRouteCallback error: " + exception.getMessage());
            }
        }
    }


    /**
     * 设置限速回调.
     *
     * @param speedLimitChangeListener OnSpeedLimitChangeListener.
     */
    public void setOnSpeedLimitChangeListener(final OnSpeedLimitChangeListener speedLimitChangeListener) {
        if (mInitStatus && null != speedLimitChangeListener && !mSpeedLimitListenerList.contains(speedLimitChangeListener)) {
            checkSpeedLimitEmpty(true);
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
            checkSpeedLimitEmpty(false);
        }
    }

    /**
     * 检查SpeedLimitChangeListener集合是否为空.
     *
     * @param add  true:添加RemoteCallback  false:移除RemoteCallback.
     */
    private void checkSpeedLimitEmpty(final boolean add) {
        final boolean empty = mSpeedLimitListenerList.isEmpty();
        if (checkBinder() && empty) {
            try {
                if (add) {
                    mBinder.addNaviAutoSpeedCallBack(mPkgName, mNaviAutoSpeedCallBack);
                } else {
                    mBinder.removeNaviAutoSpeedCallBack(mPkgName, mNaviAutoSpeedCallBack);
                }
            } catch (RemoteException exception) {
                Logger.e(TAG, "checkSpeedLimitEmpty error: " + exception.getMessage());
            }
        }
    }

    /**
     * 设置TBT信息监听.
     *
     * @param turnInfoChangeListener OnTurnInfoChangeListener.
     */
    public void addOnTurnInfoChangeListener(final OnTurnInfoChangeListener turnInfoChangeListener) {
        if (mInitStatus && null != turnInfoChangeListener && !mTurnInfoListenerList.contains(turnInfoChangeListener)) {
            checkTurnInfoListenerEmpty(true);
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
            checkTurnInfoListenerEmpty(false);
        }
    }

    /**
     * 检查TurnInfoListener集合是否为空.
     *
     * @param add true:添加RemoteCallback  false:移除RemoteCallback
     */
    private void checkTurnInfoListenerEmpty(final boolean add) {
        final boolean empty = mTurnInfoListenerList.isEmpty();
        if (checkBinder() && empty) {
            try {
                if (add) {
                    mBinder.addNaviAutoApiCallback(mPkgName, mNaviAutoCallback);
                } else {
                    mBinder.removeNaviAutoApiCallback(mPkgName, mNaviAutoCallback);
                }
            } catch (RemoteException exception) {
                Logger.e(TAG, "checkTurnInfoListenerEmpty error: " + exception.getMessage());
            }
        }
    }


    /**
     * 添加SR引导信息变更监听.
     *
     * @param srNaviInfoChangeListener OnSrNaviInfoChangeListener.
     */
    public void addOnSrNaviInfoChangeListener(final OnSrNaviInfoChangeListener srNaviInfoChangeListener) {
        if (mInitStatus && null != srNaviInfoChangeListener && !mSrNaviInfoListenerList.contains(srNaviInfoChangeListener)) {
            checkSrInfoEmpty(true);
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
            checkSrInfoEmpty(false);
        }
    }

    /**
     * 检查SrInfoListener集合是否为空.
     *
     * @param add  true:添加RemoteCallback  false:移除RemoteCallback
     */
    private void checkSrInfoEmpty(final boolean add) {
        final boolean empty = mSrNaviInfoListenerList.isEmpty();
        if (checkBinder() && empty) {
            try {
                if (add) {
                    mBinder.addNaviAutoCountDownLightCallback(mPkgName, mCountDownLightCallback);
                } else {
                    mBinder.removeNaviAutoCountDownLightCallback(mPkgName, mCountDownLightCallback);
                }
            } catch (RemoteException exception) {
                Logger.e(TAG, "checkSrInfoEmpty error: " + exception.getMessage());
            }
        }
    }

    /**
     * 添加POI通知监听
     *
     * @param listener OnPoiInformListener.
     */
    public void addPoiInformListener(final OnPoiInformListener listener) {
        if (mInitStatus && null != listener && !mPoiInfoListenerList.contains(listener)) {
            checkPoiInformEmpty(true);
            mPoiInfoListenerList.add(listener);
        }
    }

    /**
     * 移除POI通知监听
     *
     * @param listener OnPoiInformListener.
     */
    public void removePoiInformListener(final OnPoiInformListener listener) {
        if (mInitStatus && null != listener) {
            mPoiInfoListenerList.remove(listener);
            checkPoiInformEmpty(false);
        }
    }

    /**
     * 检查PoiInformListener集合是否为空.
     *
     * @param add true:添加RemoteCallback  false:移除RemoteCallback
     */
    private void checkPoiInformEmpty(final boolean add) {
        final boolean empty = mPoiInfoListenerList.isEmpty();
        if (checkBinder() && empty) {
            try {
                if (add) {
                    mBinder.addNaviAutoPoiCallBack(mPkgName, mNaviAutoPoiCallBack);
                } else {
                    mBinder.removeNaviAutoPoiCallBack(mPkgName, mNaviAutoPoiCallBack);
                }
            } catch (RemoteException exception) {
                Logger.e(TAG, "checkPoiInformEmpty error: " + exception.getMessage());
            }
        }
    }

    /**
     * 打开地图HMI.
     */
    public void openMap() {
        if (mInitStatus && checkBinder()) {
            try {
                Logger.d(TAG, mVersionInfo, "--> openMap: ");
                mBinder.openMap(mPkgName);
            } catch (RemoteException exception) {
                Logger.e(TAG, "openMap error: " + exception.getMessage());
            }
        } else {
            startMapActivityFallback();
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
                Logger.d(TAG, mVersionInfo, "--> getCurrentLocation: ");
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
                Logger.d(TAG, mVersionInfo, "-->  getDistrictDetailInfo: ");
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
                Logger.d(TAG, mVersionInfo, "-->  jumpToSearchPage: ", keyword);
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
                Logger.d(TAG, mVersionInfo, "-->  requestReverseGeoSearch: ", geoPoint);
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
                Logger.d(TAG, mVersionInfo, "-->  nearbySearch keyword: ", keyword, ", pageIndex: ", pageIndex);
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
                Logger.d(TAG, mVersionInfo, "-->  searchAndNavi: ", address);
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
                Logger.d(TAG, mVersionInfo, "-->  cancelAllSearchRequest: ");
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
                Logger.d(TAG, mVersionInfo, "-->  routePlan: ", destination);
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
                Logger.d(TAG, mVersionInfo, "-->  startNavi: ");
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
                Logger.d(TAG, mVersionInfo, "--> isNaviStatus: ");
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
                Logger.d(TAG, mVersionInfo, "-->  getGuidePanelStatus: ");
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
                Logger.d(TAG, mVersionInfo, "-->  getNaviType: ");
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
                Logger.d(TAG, mVersionInfo, "-->  getTBTInfo:");
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
        if (!CLIENT_SR.equals(mPkgName)) {
            return;
        }
        if (open == mShowSrTbt) {
            return;
        }

        if (mInitStatus && checkBinder()) {
            mShowSrTbt = open;
            Logger.d(TAG, mVersionInfo, "-->  openSrTbt:");
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
            Logger.d(TAG, mVersionInfo, "-->  stopNavigation: ");
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
            Logger.d(TAG, mVersionInfo, "-->  backHome: ");
            try {
                mBinder.backHome(mPkgName);
            } catch (RemoteException exception) {
                Logger.w(TAG, mPkgName + "backHome error:" + exception.getMessage());
            }
        } else {
            startMapActivityFallback();
        }
    }

    /**
     * Launcher widget接口，去公司.
     */
    public void goCompany() {
        if (mInitStatus && checkBinder()) {
            Logger.d(TAG, mVersionInfo, "-->  goCompany: ");
            try {
                mBinder.goCompany(mPkgName);
            } catch (RemoteException exception) {
                Logger.w(TAG, mPkgName + "goCompany error:" + exception.getMessage());
            }
        } else {
            startMapActivityFallback();
        }
    }

    /**
     * Launcher widget接口，点击搜索按钮.
     */
    public void openBasicSearch() {
        if (mInitStatus && checkBinder()) {
            Logger.d(TAG, mVersionInfo, "-->  openBasicSearch: ");
            try {
                mBinder.openBasicSearch(mPkgName);
            } catch (RemoteException exception) {
                Logger.w(TAG, mPkgName + "openBasicSearch error:" + exception.getMessage());
            }
        } else {
            startMapActivityFallback();
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
            Logger.d(TAG, mVersionInfo, "-->  getNaviBroadcastStatus: ");
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
     * @param open true-打开  false-关闭.
     */
    public void toggleNaviBroadcast(final boolean open) {
        if (mInitStatus && checkBinder()) {
            Logger.d(TAG, mVersionInfo, "-->  toggleNaviBroadcast: ");
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
            Logger.d(TAG, mVersionInfo, "-->  clickPassBySearch: ");
            try {
                mBinder.clickPassBySearch(mPkgName);
            } catch (RemoteException exception) {
                Logger.w(TAG, mPkgName + "clickPassBySearch error:" + exception.getMessage());
            }
        }
    }

    /**
     * 备用方案：直接启动MapActivity
     * 用于导航服务未准备好时的降级处理
     */
    private void startMapActivityFallback() {
        try {
            if (mContext == null) {
                Logger.e(TAG, "Context is null, cannot start MapActivity");
                return;
            }
            Intent intent = new Intent();
            intent.setClassName("com.sgm.navi.hmi", "com.sgm.navi.hmi.map.MapActivity");
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            mContext.startActivity(intent);
            Logger.d(TAG, mVersionInfo, "-->  Fallback: MapActivity started successfully");
        } catch (ActivityNotFoundException e) {
            Logger.e(TAG, "MapActivity not found: " + e.getMessage());
        } catch (Exception e) {
            Logger.e(TAG, "Failed to start MapActivity: " + e.getMessage());
        }
    }


}
