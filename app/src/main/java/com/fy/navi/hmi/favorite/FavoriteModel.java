package com.fy.navi.hmi.favorite;

import android.app.Activity;
import android.content.Context;
import android.content.IntentFilter;

import androidx.core.content.ContextCompat;

import com.alibaba.android.arouter.utils.TextUtils;
import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.define.user.account.AccessTokenParam;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorCallBack;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.ui.base.BaseModel;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;

public class FavoriteModel extends BaseModel<FavoriteViewModel> implements BehaviorCallBack, SettingUpdateObservable.SettingUpdateObserver, SearchResultCallback {
    private static final String TAG = FavoriteModel.class.getName();
    private final BehaviorPackage mBehaviorPackage;
    private final SettingManager mSettingManager;
    private final AccountPackage mAccountPackage;
    private final SearchPackage mSearchPackage;

    public FavoriteModel() {
        mBehaviorPackage = BehaviorPackage.getInstance();
        mAccountPackage = AccountPackage.getInstance();
        mSearchPackage = SearchPackage.getInstance();
        mSettingManager = new SettingManager();
        mSettingManager.init();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mBehaviorPackage.registerCallBack(this);
        mSearchPackage.registerCallBack(TAG, this);
        SettingUpdateObservable.getInstance().addObserver("FavoriteModel", this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mSearchPackage.unRegisterCallBack(TAG);
        mBehaviorPackage.unRegisterCallBack(this);
        SettingUpdateObservable.getInstance().removeObserver("FavoriteModel", this);
    }

    /**
     * 手动同步
     */
    public void startSync() {
        mBehaviorPackage.startSync();
    }

    /**
     * 获取精简收藏点列表（普通poi）
     */
    public void getSimpleFavoriteList() {
        if (mViewModel == null) {
            return;
        }
        mViewModel.updateFavoriteView(mBehaviorPackage.getFavoritePoiData(), 0);
    }

    /**
     * getHomeInfo
     */
    public void getHomeFavoriteInfo() {
        if (mViewModel == null) {
            return;
        }
        mViewModel.updateHomeView(mBehaviorPackage.getHomeFavoriteInfo());
    }

    /**
     * getCompanyInfo
     */
    public void getCompanyFavoriteInfo() {
        if (mViewModel == null) {
            return;
        }
        mViewModel.updateCompanyView(mBehaviorPackage.getCompanyFavoriteInfo());
    }

    /**
     * getCompanyInfo
     */
    public ArrayList<PoiInfoEntity> getFavoriteAddressInfo() {
       return mBehaviorPackage.getFavoriteAddressInfo();
    }


    @Override
    public void onUpdateSyncTime(final String syncTime) {
//        mViewModel.updateSyncTime(syncTime);
        mSettingManager.insertOrReplace(SettingController.KEY_SETTING_SYNC_TIME, syncTime);
    }

    @Override
    public void onUpdateName(String name) {
        if (mViewModel != null) {
            mViewModel.updateFavoriteName(name);
        }
    }

    /**
     * setHomeCompanyDisplay
     * @param isHomeCompanyDisplay
     */
    public void setHomeCompanyDisplay(final boolean isHomeCompanyDisplay) {
        mSettingManager.insertOrReplace(SettingController.KEY_SETTING_HOME_COMPANY_DISPLAYED, String.valueOf(isHomeCompanyDisplay));
    }

    /**
     * initView
     */
    public void initView() {
        getHomeCompanyDisplay();
        getCarType();
    }

    /**
     * getHomeCompanyDisplay
     */
    public void getHomeCompanyDisplay() {
        String value = mSettingManager.getValueByKey(SettingController.KEY_SETTING_HOME_COMPANY_DISPLAYED);
        if (TextUtils.isEmpty(value)) {
            value = SettingController.VALUE_GENERIC_TRUE;
        }
        final boolean isHomeCompanyDisplay = Boolean.parseBoolean(value);
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_HOME_COMPANY_DISPLAYED, isHomeCompanyDisplay);
    }

    /**
     * getCarType
     */
    public void getCarType() {
        final int carMode = CalibrationPackage.getInstance().powerType();
        final boolean isEVCar = carMode == 1;
        mViewModel.dualChoiceControl(SettingController.KEY_SETTING_IS_EV_CAR, isEVCar);
    }

    /**
     * 删除POI收藏点
     * @param poiInfo
     * @return string
     */
    public String removeFavorite(final PoiInfoEntity poiInfo) {
        return mBehaviorPackage.removeFavorite(poiInfo);
    }

    /**
     * 收藏点置顶/取消置顶
     * @param info
     * @param isSetTop
     * @return string
     */
    public String topFavorite(final PoiInfoEntity info, final boolean isSetTop) {
        return mBehaviorPackage.topFavorite(info, isSetTop);
    }

    /**
     * 收藏点重命名
     * @param detailInfo
     * @param customName
     * @return string
     */
    public String modifyFavorite(final PoiInfoEntity detailInfo, final String customName) {
        return mBehaviorPackage.modifyFavorite(detailInfo, customName);
    }

    /**
     * // 用户处于登录状态，完成同步事件 获取指定Uid的精简信息收藏点列表
     * @param eventType
     * @param exCode
     */
    @Override
    public void notifyFavorite(final int eventType, final int exCode) {
        getSimpleFavoriteList();
    }

    @Override
    public void notifyFavoriteAsync(final int type, final ArrayList<PoiInfoEntity> data, final boolean sorted) {
        mViewModel.updateFavoriteView(data, 0);
    }

    @Override
    public void notifyLoginStatusChanged() {
        //登出时需要更新信息
        getHomeFavoriteInfo();
        getCompanyFavoriteInfo();
        getSimpleFavoriteList();
    }

    /**
     * 重命名时需更新本地数据
     * @param itemId  收藏点唯一码
     * @param customName  自定义名称 重命名时编辑的字段
     */
    public void modifyFavoriteData(final String itemId, final String customName) {
        mBehaviorPackage.modifyFavoriteData(itemId, customName);
    }

    /**
     * 重命名时需更新本地数据
     * @param itemId  收藏点唯一码
     * @param topTime 置顶时间
     */
    public void updateFavoriteTopTime(final String itemId, final long topTime) {
        mBehaviorPackage.updateFavoriteTopTime(itemId, topTime);
    }

    /**
     * 判断SGM账号是否登录
     * @return 是否登录
     */
    public boolean isSGMLogin() {
        final boolean sgmLogin = mAccountPackage.isSGMLogin();
        Logger.d(TAG, "isSGMLogin: " + sgmLogin);
        return sgmLogin;
    }

    /**
     * 登录SGM
     * @param context
     */
    public void sendSGMLogin(Context context) {
        mAccountPackage.sendSGMLoginRequest(context);
    }

    /**
     * 查询专属充电站
     */
    public void queryCollectStation(final Activity activity) {
        final AccessTokenParam param = new AccessTokenParam(
            AutoMapConstant.AccountTokenParamType.ACCOUNT_TYPE_PATAC_HMI,
            AutoMapConstant.AccountTokenParamType.AUTH_TOKEN_TYPE_READ_ONLY,
            null,
            activity,
            null,
            null,
            null,
            null);

        ThreadManager.getInstance().runAsync(() -> {
            final String vehicleBrand = mSearchPackage.getBrandName(CalibrationPackage.getInstance().brand());
            mSearchPackage.queryCollectStation(mAccountPackage.getUserId(), mAccountPackage.getAccessToken(param), vehicleBrand);
        });
    }

    /**
     * 取消收藏当前专属充电站
     * @param activity
     * @param poiInfo
     */
    public void removeCurrentStation(final Activity activity, final PoiInfoEntity poiInfo) {
        final AccessTokenParam param = new AccessTokenParam(
            AutoMapConstant.AccountTokenParamType.ACCOUNT_TYPE_PATAC_HMI,
            AutoMapConstant.AccountTokenParamType.AUTH_TOKEN_TYPE_READ_ONLY,
            null,
            activity,
            null,
            null,
            null,
            null);

        ThreadManager.getInstance().runAsync(() -> {
            final String vehicleBrand = mSearchPackage.getBrandName(CalibrationPackage.getInstance().brand());
            poiInfo.setIsCollect(true);
            mSearchPackage.updateCollectStatus(mAccountPackage.getUserId(), mAccountPackage.getAccessToken(param), vehicleBrand, poiInfo);
        });
    }

    @Override
    public void onNetSearchResult(int taskId, String searchKey, BaseRep result) {
        if(AutoMapConstant.NetSearchKey.QUERY_STATION_LIST.equals(searchKey)){
            Logger.d(TAG, "station list result is  " + GsonUtils.toJson(result));
            if (AutoMapConstant.NetSearchKey.SUCCESS_CODE.equals(result.getResultCode())) {
                mViewModel.notifyConnectStationResult(result);
            }else {
                mViewModel.notifyConnectStationError();
            }
        }
    }
}

