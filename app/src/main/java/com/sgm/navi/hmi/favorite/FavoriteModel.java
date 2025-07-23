package com.sgm.navi.hmi.favorite;

import android.app.Activity;
import android.content.Context;

import com.alibaba.android.arouter.utils.TextUtils;
import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.navi.ForecastAddressDialog;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.position.LocInfoBean;
import com.sgm.navi.service.define.search.FavoriteInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.setting.SettingController;
import com.sgm.navi.service.define.user.account.AccessTokenParam;
import com.sgm.navi.service.define.user.account.AccountProfileInfo;
import com.sgm.navi.service.define.user.forecast.ForecastArrivedDataInfo;
import com.sgm.navi.service.define.user.forecast.OftenArrivedItemInfo;
import com.sgm.navi.service.greendao.setting.SettingManager;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.mapdata.MapDataPackage;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.search.SearchResultCallback;
import com.sgm.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.sgm.navi.service.logicpaket.user.account.AccountPackage;
import com.sgm.navi.service.logicpaket.user.behavior.BehaviorCallBack;
import com.sgm.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.sgm.navi.service.logicpaket.user.forecast.ForecastCallBack;
import com.sgm.navi.service.logicpaket.user.forecast.ForecastPackage;
import com.sgm.navi.service.logicpaket.user.forecast.IForecastAddressCallBack;
import com.sgm.navi.ui.base.BaseModel;

import java.util.ArrayList;

public class FavoriteModel extends BaseModel<FavoriteViewModel> implements BehaviorCallBack,
        SettingUpdateObservable.SettingUpdateObserver, SearchResultCallback, ForecastCallBack, IForecastAddressCallBack {
    private static final String TAG = FavoriteModel.class.getName();
    private final BehaviorPackage mBehaviorPackage;
    private final SettingManager mSettingManager;
    private final AccountPackage mAccountPackage;
    private final SearchPackage mSearchPackage;
    private MapPackage mapPackage;
    private PositionPackage positionPackage;
    private MapDataPackage mapDataPackage;
    private final ForecastPackage mforCastPackage;

    public FavoriteModel() {
        mBehaviorPackage = BehaviorPackage.getInstance();
        mAccountPackage = AccountPackage.getInstance();
        mSearchPackage = SearchPackage.getInstance();
        mSettingManager = new SettingManager();
        mSettingManager.init();

        mapPackage = MapPackage.getInstance();
        positionPackage = PositionPackage.getInstance();
        mapDataPackage = MapDataPackage.getInstance();
        mforCastPackage = ForecastPackage.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mBehaviorPackage.registerCallBack(this);
        mSearchPackage.registerCallBack(TAG, this);
        SettingUpdateObservable.getInstance().addObserver("FavoriteModel", this);
        mforCastPackage.registerCallBack(this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mSearchPackage.unRegisterCallBack(TAG);
        mBehaviorPackage.unRegisterCallBack(this);
        SettingUpdateObservable.getInstance().removeObserver("FavoriteModel", this);
        mforCastPackage.unregisterCallBack(this);
        clearDialog();
    }

    public void clearDialog() {
        if (null != forecastAddressDialog && forecastAddressDialog.isShowing()) {
            forecastAddressDialog.dismiss();
        }
        forecastAddressDialog = null;
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
            setHomeCompanyDisplay(true);
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
            if (Logger.openLog) {
                Logger.d(TAG, "station list result is  ", result);
            }
            if (AutoMapConstant.NetSearchKey.SUCCESS_CODE.equals(result.getResultCode())) {
                mViewModel.notifyConnectStationResult(result);
            }else {
                mViewModel.notifyConnectStationError();
            }
        }
    }

    @Override
    public void onInit(int result) {
        Logger.d(TAG, "onInit: " + result);
    }

    @Override
    public void onSetLoginInfo(int result) {
        Logger.d(TAG, "onSetLoginInfo: " + result);
    }

    @Override
    public void onForecastArrivedData(ForecastArrivedDataInfo data) {
        if (Logger.openLog)
            Logger.d(TAG, "onForecastArrivedData: ", "mCompanyOrHomeType: ", mCompanyOrHomeType,
                    data.getAdCode(), data.getHome(), data.getCompany());
        //判断是否有家或者公司的数据
        switch (mCompanyOrHomeType) {
            case AutoMapConstant.GuessPositionType.HOME:
                OftenArrivedItemInfo mHomeInfo = (data != null ? data.getHome() : null);
                if (!ConvertUtils.isEmpty(mHomeInfo) && !ConvertUtils.isEmpty(mHomeInfo.getWstrAddress())) {
                    showForecastDialog(mCompanyOrHomeType, mHomeInfo);
                } else {
                    mViewModel.toHomeFragment();
                }
                break;
            case AutoMapConstant.GuessPositionType.COMPANY:
                OftenArrivedItemInfo mCompanyInfo = (data != null ? data.getCompany() : null);
                if (!ConvertUtils.isEmpty(mCompanyInfo) && !ConvertUtils.isEmpty(mCompanyInfo.getWstrAddress())) {
                    showForecastDialog(mCompanyOrHomeType, mCompanyInfo);
                } else {
                    mViewModel.toCompanyFragment();
                }
                break;
            default:
                break;
        }
    }

    private ForecastAddressDialog forecastAddressDialog;

    public void showForecastDialog(int type, OftenArrivedItemInfo oftenArrivedItemInfo) {
        ThreadManager.getInstance().postUi(() -> {
            if (forecastAddressDialog == null) {
                forecastAddressDialog = new ForecastAddressDialog(mViewModel.getView().getContext(), this);
            }
            forecastAddressDialog.setForecastAddressInfo(type, oftenArrivedItemInfo);
            if (!forecastAddressDialog.isShowing()) {
                forecastAddressDialog.show();
            }
        });
    }

    private int mCompanyOrHomeType;

    public void getOnlineForecastArrivedData(int type) {
        Logger.i(TAG, "getOnlineForecastArrivedData: type " + type);
        mCompanyOrHomeType = type;
        //登陆了才去预测数据
        if (AccountPackage.getInstance().reallyLogin()) {
            // 获取在线预测常去地点
            ForecastArrivedDataInfo param = new ForecastArrivedDataInfo();
            param.setLevel((int) mapPackage.getZoomLevel(MapType.MAIN_SCREEN_MAIN_MAP)); // 图面比例尺级别
            //先拿到经纬度
            LocInfoBean locInfoBean = positionPackage.getLastCarLocation();
            if (!ConvertUtils.isEmpty(locInfoBean)) {
                param.setUserLoc(new GeoPoint(locInfoBean.getLongitude(), locInfoBean.getLatitude()));
                int adCode = mapDataPackage.getAdCodeByLonLat(locInfoBean.getLongitude(), locInfoBean.getLatitude());
                param.setAdCode(String.valueOf(adCode)); // 所在城市对应 adcode
            }
            AccountProfileInfo accountProfileInfo = AccountPackage.getInstance().getUserInfo();
            if (!ConvertUtils.isEmpty(accountProfileInfo)) {
                param.setUserId(accountProfileInfo.getUid()); // 登录用户UID
            }
            mforCastPackage.getOnlineForecastArrivedData(param);
        } else {
            if (AutoMapConstant.GuessPositionType.HOME == type) {
                mViewModel.toHomeFragment();
            } else if (AutoMapConstant.GuessPositionType.COMPANY == type) {
                mViewModel.toCompanyFragment();
            }
        }
    }

    @Override
    public void AddForecastInfo(int type, OftenArrivedItemInfo oftenArrivedItemInfo) {
        addHomeOrCompanyInfoToSetting(type, oftenArrivedItemInfo);
        getHomeFavoriteInfo();
        getCompanyFavoriteInfo();
    }

    @Override
    public void addressClick(int type) {
        if (AutoMapConstant.HomeCompanyType.HOME == type) {
            mViewModel.toHomeFragment();
        } else {
            mViewModel.toCompanyFragment();
        }
    }

    public void addHomeOrCompanyInfoToSetting(int type, OftenArrivedItemInfo oftenArrivedItemInfo) {
        int favoriteType = (type == AutoMapConstant.HomeCompanyType.HOME) ? 1 : 2;
        PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
        poiInfoEntity.setPid(oftenArrivedItemInfo.getWstrPoiID());
        poiInfoEntity.setName(oftenArrivedItemInfo.getWstrPoiName());
        poiInfoEntity.setAddress(oftenArrivedItemInfo.getWstrAddress());
        poiInfoEntity.setPoint(oftenArrivedItemInfo.getStDisplayCoord());
        FavoriteInfo favoriteInfo = new FavoriteInfo();
        favoriteInfo.setMCommonName(favoriteType);
        poiInfoEntity.setFavoriteInfo(favoriteInfo);
        mBehaviorPackage.addFavorite(poiInfoEntity, favoriteType);
        if (favoriteType == 1) {
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.forecast_set_home));
        } else {
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.forecast_set_company));
        }
    }
}

