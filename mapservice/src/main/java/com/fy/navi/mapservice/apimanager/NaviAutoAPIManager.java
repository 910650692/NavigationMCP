package com.fy.navi.mapservice.apimanager;

import android.content.Context;
import android.os.RemoteException;
import android.util.Log;

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
import com.fy.navi.mapservice.callback.OnNaviStatusChangeListener;
import com.fy.navi.mapservice.callback.OnRoutePlanResultListener;
import com.fy.navi.mapservice.callback.OnSearchResultListener;
import com.fy.navi.mapservice.callback.OnSpeedLimitChangeListener;
import com.fy.navi.mapservice.callback.OnTurnInfoChangeListener;
import com.fy.navi.mapservice.common.INaviAutoApiBinder;
import com.fy.navi.mapservice.common.INaviAutoApiCallback;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;

public class NaviAutoAPIManager extends BaseManager<INaviAutoApiBinder> {

    private static final String TAG = NaviAutoAPIManager.class.getSimpleName();
    private WeakReference<Context> mContextReference;
    private String mPkgName = "default";
    private boolean mInitStatus = false;

    private List<OnInitStateChangeListener> mInitStateListenerList = new ArrayList<>();
    private List<OnLocationChangeListener> mLocationListenerList = new ArrayList<>();
    private List<OnDistrictInfoChangeListener> mDistrictInfoList = new ArrayList<>();
    private List<OnNaviStatusChangeListener> mNaviStatusListenerList = new ArrayList<>();
    private List<OnSearchResultListener> mSearchResultListenerList = new ArrayList<>();
    private List<OnRoutePlanResultListener> mRoutePlanListenerList = new ArrayList<>();
    private List<OnGuidePanelDataListener> mGuidePanelDataListenerList = new ArrayList<>();
    private List<OnSpeedLimitChangeListener> mSpeedLimitListenerList = new ArrayList<>();
    private List<OnTurnInfoChangeListener> mTurnInfoListenerList = new ArrayList<>();

    public static NaviAutoAPIManager getInstance() {
        return SingleHolder.mInstance;
    }

    private static class SingleHolder {
        private static final NaviAutoAPIManager mInstance = new NaviAutoAPIManager();
    }

    private NaviAutoAPIManager() {}

    @Override
    protected INaviAutoApiBinder updateBinder() {
        return BinderManager.getInstance().getNaviAutoBinder();
    }

    @Override
    protected void onEngineInit(boolean success) {
        Log.d(TAG, "onEngineInit: " + success);
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
                } catch (Exception exception) {
                    Log.d(TAG, "dispatch initSuccess: " + exception.getMessage());
                }
            }
        }
    }

    @Override
    protected void registerBinderResultCallback() {
        if (checkBinder()) {
            try {
                mBinder.addNaviAutoApiCallback(mPkgName, mNaviAutoCallback);
            } catch (Exception exception) {
                Log.e(TAG, "registerNaviAutoApiCallback error: " + exception.getMessage());
            }
        }
    }

    @Override
    protected void unRegisterBinderResultCallback() {
        if (checkBinder()) {
            try {
                mBinder.removeNaviAutoApiCallback(mPkgName, mNaviAutoCallback);
            } catch (Exception exception) {
                Log.e(TAG, "unRegisterNaviAutoApiCallback error: " + exception.getMessage());
            }
        }
    }

    private final INaviAutoApiCallback.Stub mNaviAutoCallback = new INaviAutoApiCallback.Stub() {

        @Override
        public void onNaviStatusChange(String naviStatus) {
            for (OnNaviStatusChangeListener listener : mNaviStatusListenerList) {
                if (null != listener) {
                    try {
                        listener.onNaviStatusChange(naviStatus);
                    } catch (Exception exception) {
                        Log.e(TAG, "dispatch naviStatus error: " + exception.getMessage());
                    }
                }
            }
        }

        @Override
        public void onLocationInfoChange(String locationInfo) {
            for (OnLocationChangeListener listener : mLocationListenerList) {
                if (null != listener) {
                    try {
                        listener.onLocationChange(locationInfo);
                    } catch (Exception exception) {
                        Log.e(TAG, "dispatch locationInfo error: " + exception.getMessage());
                    }
                }
            }
        }

        @Override
        public void onDistrictInfoChange(String districtInfo) {
            for (OnDistrictInfoChangeListener listener : mDistrictInfoList) {
                if (null != listener) {
                    try {
                        listener.onDistrictInfoChange(districtInfo);
                    } catch (Exception exception) {
                        Log.e(TAG, "dispatch districtInfo error: " + exception.getMessage());
                    }
                }
            }
        }

        @Override
        public void onSearchFailed(boolean silent, int errorCode) {
            for (OnSearchResultListener listener : mSearchResultListenerList) {
                if (null != listener) {
                    try {
                        listener.onSearchError(silent, errorCode);
                    } catch (Exception exception) {
                        Log.e(TAG, "dispatch searchFailed error: " + exception.getMessage());
                    }
                }
            }
        }

        @Override
        public void onSearchResult(boolean silent, String searchResult) {
            for (OnSearchResultListener listener : mSearchResultListenerList) {
                if (null != listener) {
                    try {
                        listener.onSearchResult(silent, searchResult);
                    } catch (Exception exception) {
                        Log.e(TAG, "dispatch searchResult error: " + exception.getMessage());
                    }
                }
            }
        }

        @Override
        public void onReverseGeoSearchResult(int taskId, String reverseResult) {
            for (OnSearchResultListener listener : mSearchResultListenerList) {
                if (null != listener) {
                    try {
                        listener.onReverseGeoSearchResult(taskId, reverseResult);
                    } catch (Exception exception) {
                        Log.e(TAG, "dispatch searchResult error: " + exception.getMessage());
                    }
                }
            }
        }

        @Override
        public void onRoutePlanFailed(int code, String errorMsg) {
            for (OnRoutePlanResultListener listener : mRoutePlanListenerList) {
                if (null != listener) {
                    try {
                        listener.onRoutePlanError(code, errorMsg);
                    } catch (Exception exception) {
                        Log.e(TAG, "dispatch routePlanFailed error: " + exception.getMessage());
                    }
                }
            }
        }

        @Override
        public void onRoutePlanResult(String routeResult) {
            for (OnRoutePlanResultListener listener : mRoutePlanListenerList) {
                if (null != listener) {
                    try {
                        listener.onRoutePlanResult(routeResult);
                    } catch (Exception exception) {
                        Log.e(TAG, "dispatch routePlanResult error: " + exception.getMessage());
                    }
                }
            }
        }

        @Override
        public void onPanelData(int panelDataStatus) {
            for (OnGuidePanelDataListener listener : mGuidePanelDataListenerList) {
                if (null != listener) {
                    try {
                        listener.onPanelData(panelDataStatus);
                    } catch (Exception exception) {
                        Log.e(TAG, "dispatch onPanelData error: " + exception.getMessage());
                    }
                }
            }
        }

        @Override
        public void onSpeedLimitChange(int curSpeed, int limitSpeed) {
            for (OnSpeedLimitChangeListener listener : mSpeedLimitListenerList) {
                if (null != listener) {
                    try {
                        listener.onSpeedLimitChange(curSpeed, limitSpeed);
                    } catch (Exception exception) {
                        Log.e(TAG, "dispatch speedLimitChange error: " + exception.getMessage());
                    }
                }
            }
        }

        @Override
        public void onTurnInfoChange(String turnInfo) {
            for (OnTurnInfoChangeListener listener : mTurnInfoListenerList) {
                if (null != listener) {
                    try {
                        listener.onTurnInfoUpdated(turnInfo);
                    } catch (Exception exception) {
                        Log.e(TAG, "dispatch turnInfo error: " + exception.getMessage());
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
    public void init(Context context) {
        init(context, context.getPackageName());
    }

    /**
     * 初始化.
     *
     * @param context Context.
     * @param pkgName String，client package name.
     */
    public void init(Context context, String pkgName) {
        init(context, pkgName, "");
    }

    /**
     * 接口初始化.
     *
     * @param context Context.
     * @param pkgName String，客户端包名.
     * @param params String，参考高德地图SDK开发应用接口依赖中Onstar调用示例添加
     *               具体作用未知.
     */
    public void init(Context context, String pkgName, String params) {
        synchronized (NaviAutoAPIManager.class) {
            mContextReference = new WeakReference<>(context);
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
    public void setOnInitStateChangeListener(OnInitStateChangeListener initStateChangeListener) {
        if (!mInitStateListenerList.contains(initStateChangeListener)) {
            mInitStateListenerList.add(initStateChangeListener);
        }
    }

    /**
     * 设置位置信息改变监听接口.
     *
     * @param locationChangeListener OnLocationChangeListener.
     */
    public void setOnLocationChangeListener(OnLocationChangeListener locationChangeListener) {
        if (mInitStatus &&  !mLocationListenerList.contains(locationChangeListener)) {
            mLocationListenerList.add(locationChangeListener);
        }
    }

    /**
     * 移除位置信息改变监听接口.
     *
     * @param locationChangeListener OnLocationChangeListener.
     */
    public void removeOnLocationChangeListener(OnLocationChangeListener locationChangeListener) {
        if (mInitStatus) {
            mLocationListenerList.remove(locationChangeListener);
        }
    }

    /**
     * 添加行政区划信息信息改变监听.
     *
     * @param districtInfoChangeListener OnDistrictInfoChangeListener.
     */
    public void setOnDistrictInfoChangeListener(OnDistrictInfoChangeListener districtInfoChangeListener) {
        if (mInitStatus && !mDistrictInfoList.contains(districtInfoChangeListener)) {
            mDistrictInfoList.add(districtInfoChangeListener);
        }
    }

    /**
     * 移除行政区划信息信息改变监听.
     *
     * @param districtInfoChangeListener OnDistrictInfoChangeListener.
     */
    public void removeOnDistrictInfoChangeListener(OnDistrictInfoChangeListener districtInfoChangeListener) {
        if (mInitStatus) {
            mDistrictInfoList.remove(districtInfoChangeListener);
        }
    }

    /**
     * 添加Navi状态改变监听.
     *
     * @param naviStatusChangeListener OnNaviStatusChangeListener.
     */
    public void setOnNaviStatusChangeListener(OnNaviStatusChangeListener naviStatusChangeListener) {
        if (mInitStatus && !mNaviStatusListenerList.contains(naviStatusChangeListener)) {
            mNaviStatusListenerList.add(naviStatusChangeListener);
        }
    }

    /**
     * 移除Navi状态改变监听.
     *
     * @param naviStatusChangeListener OnNaviStatusChangeListener.
     */
    public void removeOnNaviStatusChangeListener(OnNaviStatusChangeListener naviStatusChangeListener) {
        if (mInitStatus) {
            mNaviStatusListenerList.remove(naviStatusChangeListener);
        }
    }

    /**
     * 设置搜索结果回调监听.
     *
     * @param searchResultListener OnSearchResultListener.
     */
    public void setOnSearchResultListener(OnSearchResultListener searchResultListener) {
        if (mInitStatus && !mSearchResultListenerList.contains(searchResultListener)) {
            mSearchResultListenerList.add(searchResultListener);;
        }
    }

    /**
     * 移除搜索结果回调监听.
     *
     * @param searchResultListener OnSearchResultListener.
     */
    public void removeOnSearchResultListener(OnSearchResultListener searchResultListener) {
        if (mInitStatus) {
            mSearchResultListenerList.remove(searchResultListener);
        }
    }

    /**
     * 设置算路结果监听.
     *
     * @param routePlanResultListener OnRoutePlanResultListener.
     */
    public void addRoutePlanResultListener(OnRoutePlanResultListener routePlanResultListener) {
        if (mInitStatus && !mRoutePlanListenerList.contains(routePlanResultListener)) {
            mRoutePlanListenerList.add(routePlanResultListener);
        }
    }

    /**
     * 移除算路结果监听.
     *
     * @param routePlanResultListener OnRoutePlanResultListener.
     */
    public void removeRoutePlanResultListener(OnRoutePlanResultListener routePlanResultListener) {
        if (mInitStatus) {
            mRoutePlanListenerList.remove(routePlanResultListener);
        }
    }

    /**
     * 添加导航中诱导面板状态监听.
     *
     * @param guidePanelDataListener OnGuidePanelDataListener.
     */
    public void setGuidePanelDataChangeListener(OnGuidePanelDataListener guidePanelDataListener) {
        if (mInitStatus && !mGuidePanelDataListenerList.contains(guidePanelDataListener)) {
            mGuidePanelDataListenerList.add(guidePanelDataListener);;
        }
    }

    /**
     * 移除导航中诱导面板状态监听.
     *
     * @param guidePanelDataListener OnGuidePanelDataListener.
     */
    public void removeGuidePanelDataChangeListener(OnGuidePanelDataListener guidePanelDataListener) {
        if (mInitStatus) {
            mGuidePanelDataListenerList.remove(guidePanelDataListener);
        }
    }

    /**
     * 设置限速回调.
     *
     * @param speedLimitChangeListener OnSpeedLimitChangeListener.
     */
    public void setOnSpeedLimitChangeListener(OnSpeedLimitChangeListener speedLimitChangeListener) {
        if (mInitStatus && !mSpeedLimitListenerList.contains(speedLimitChangeListener)) {
            mSpeedLimitListenerList.add(speedLimitChangeListener);
        }
    }

    /**
     * 移除当前限速监听.
     *
     * @param speedLimitChangeListener OnSpeedLimitChangeListener.
     */
    public void removeOnSpeedLimitChangeListener(OnSpeedLimitChangeListener speedLimitChangeListener) {
        if (mInitStatus) {
            mSpeedLimitListenerList.remove(speedLimitChangeListener);
        }
    }

    /**
     * 设置TBT信息监听.
     *
     * @param turnInfoChangeListener OnTurnInfoChangeListener.
     */
    public void addOnTurnInfoChangeListener(OnTurnInfoChangeListener turnInfoChangeListener) {
        if (mInitStatus && !mTurnInfoListenerList.contains(turnInfoChangeListener)) {
            mTurnInfoListenerList.add(turnInfoChangeListener);
        }
    }

    /**
     * 移除TBT信息监听.
     *
     * @param turnInfoChangeListener OnTurnInfoChangeListener.
     */
    public void removeOnTurnInfoChangeListener(OnTurnInfoChangeListener turnInfoChangeListener) {
        if (mInitStatus) {
            mTurnInfoListenerList.remove(turnInfoChangeListener);
        }
    }

    /**
     * 打开地图HMI.
     */
    public void openMap() {
        if (mInitStatus && checkBinder()) {
            try {
                mBinder.openMap(mPkgName);
            } catch (Exception exception) {
                Log.e(TAG, "openMap error: " + exception.getMessage());
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
                locationInfo = mBinder.getCurrentLocation(mPkgName);
            } catch (Exception exception) {
                Log.e(TAG, "getCurrentLocation error: " + exception.getMessage());
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
                districtInfo = mBinder.getDistrictDetailInfo(mPkgName);
            } catch (Exception exception) {
                Log.e(TAG, "getDistrictDetailInfo error: " + exception.getMessage());
            }
        }

        return districtInfo;
    }

    /**
     * 打开HMI搜索界面并触发关键字搜索.
     *
     * @param keyword String 搜索关键字.
     */
    public void jumpToSearchPage(String keyword) {
        if (!mInitStatus) {
            Log.w(TAG, "not initStatus");
            return;
        }
        if (null == keyword || keyword.isEmpty()) {
            Log.w(TAG, "empty keyword");
            return;
        }

        if (checkBinder()) {
            try {
                mBinder.jumpToSearchPage(mPkgName, keyword);
            } catch (Exception exception) {
                Log.e(TAG, "jumpToSearchPage error: " + exception.getMessage());
            }
        }
    }

    /**
     * 通过输入的基础地址执行逆地理搜索.
     * @param geoPoint
     */
    public int requestReverseGeoSearch(BaseGeoPoint geoPoint) {
        if (!mInitStatus) {
            Log.w(TAG, "not initStatus");
            return -1;
        }
        if (null == geoPoint) {
            Log.w(TAG, "empty GeoPoint");
            return -1;
        }

        if (checkBinder()) {
            try {
                return mBinder.requestReverseGeoSearch(mPkgName, geoPoint);
            } catch (Exception exception) {
                Log.e(TAG, "requestReverseGeoSearch error: " + exception.getMessage());
            }
        }

        return -1;
    }

    /**
     * 搜索地址并发起路线规划.
     *
     * @param address String，地址信息，越精准，搜索结果越准确.
     */
    public void searchAndNavi(String address) {
        if (!mInitStatus) {
            Log.w(TAG, "not initStatus");
            return;
        }
        if (null == address || address.isEmpty()) {
            Log.w(TAG, "empty address");
            return;
        }

        if (checkBinder()) {
            try {
                mBinder.searchAndNavi(mPkgName, address);
            } catch (Exception exception) {
                Log.e(TAG, "searchAndNavi error: " + exception.getMessage());
            }
        }
    }

    /**
     * 取消所有搜索请求.
     */
    public void cancelAllSearchRequest() {
        if (!mInitStatus) {
            Log.w(TAG, "not initStatus");
            return;
        }
        if (checkBinder()) {
            try {
                mBinder.cancelAllSearchRequest(mPkgName);
            } catch (Exception exception) {
                Log.e(TAG, "cancelAllSearchRequest error: " + exception.getMessage());
            }
        }
    }

    /**
     * 根据选定的搜索结果发起路线规划.
     *
     * @param destination BaseSearchPoi，目的地，单个Poi信息.
     */
    public void routePlan(BaseSearchPoi destination) {
        if (!mInitStatus) {
            Log.w(TAG, "not initStatus");
            return;
        }
        if (checkBinder()) {
            try {
                mBinder.routePlan(mPkgName, destination);
            } catch (Exception exception) {
                Log.e(TAG, "routePlan error: " + exception.getMessage());
            }
        }
    }

    /**
     * 开始导航.
     */
    public void startNavi() {
        if (!mInitStatus) {
            Log.w(TAG, "not initStatus");
            return;
        }
        if (checkBinder()) {
            try {
                mBinder.startNavi(mPkgName);
            } catch (Exception exception) {
                Log.e(TAG, "routePlan error: " + exception.getMessage());
            }
        }
    }

    /**
     * 当前是否处于引导态.
     * @return boolean  true-处于引导态  false-相反.
     */
    public boolean isNaviStatus() {
        boolean result = false;
        if (mInitStatus && checkBinder()) {
            try {
                result = mBinder.isNaviStatus(mPkgName);
            } catch (Exception exception) {
                Log.e(TAG, "startNavi error: " + exception.getMessage());
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
                result = mBinder.getGuidePanelStatus(mPkgName);
            } catch (Exception exception) {
                Log.e(TAG, "startNavi error: " + exception.getMessage());
            }
        }

        return result;
    }

    /**
     * 获取导航类型.
     */
    public String getNaviType() {
        String type = INaviConstant.NaviStatusType.NO_STATUS;
        if (mInitStatus && checkBinder()) {
            try {
                type = mBinder.getNaviType(mPkgName);
            } catch (RemoteException exception) {
                Log.e(TAG, "getNaviType error: " + exception.getMessage());
            }
        }

        return type;
    }

    /**
     * 获取诱导信息.
     * @return String，BaseTbtInfo对应的json字符串.
     */
    public String getTBTInfo() {
        String tbtInfo = "";
        if (mInitStatus && checkBinder()) {
            try {
                tbtInfo = mBinder.getTBTInfo(mPkgName);
            } catch (RemoteException exception) {
                Log.e(TAG, "getTBTInfo error: " + exception.getMessage());
            }
        }

        return tbtInfo;
    }


}
